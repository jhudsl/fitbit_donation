library(shiny)
library(tidyverse)
library(lubridate)
library(fitbitr)
library(shinyjs)
library(rdrop2)

# UI elements
source('shiny_app/tabs/appCSS.R')
source('shiny_app/tabs/helixLoader.R')
source('shiny_app/tabs/welcomePanel.R')
source('shiny_app/tabs/tagPanel.R')
source('shiny_app/tabs/reportPanel.R')
source('shiny_app/tabs/downloadPanel.R')

# Server-side helpers
source('shiny_app/helperFuncs/showAndHideFuncs.R')
source('shiny_app/helperFuncs/reportGenerator.R')
source("shiny_app/helperFuncs/dropboxHelpers.R")
source("shiny_app/helperFuncs/firebaseHelpers.R")
source("shiny_app/helperFuncs/downloadDays.R")

# API Info from encripted vault. 
source("shiny_app/helperFuncs/loadApiCredentials.R")
source("shiny_app/api_keys.R")

options(shiny.reactlog=TRUE)
ui <- fluidPage(
  useShinyjs(),
  extendShinyjs(text = tabDisableJS),
  title = "Quantified Whole",
  tags$head(
    tags$style(type="text/css", appCSS)
  ),
  tabsetPanel(id = "tabset",
    tabPanel( "Welcome",           welcomePanel() ),
    tabPanel( "Tag Your Data",     tagPanel() ),
    tabPanel( "Shareable Report",  reportPanel() ),
    tabPanel( "Download Your Data",downloadPanel() )
  )
)

server <- function(input, output) {

  # everything needed for the apps apperence is stored in here. 
  state <- reactiveValues(
    alreadyDownloadedDays = NULL, # What days has the user already downloaded in previous logins?
    desiredDays = NULL,           # A vector of dates we want to pull
    daysProfile = NULL,           # Data from the desiredDays
    activityTags = NULL,          # Data on activity tags. Supplied by our viz (but also previous tags eventually.)
    userToken = NULL,             # Oauth token for fitbits api. 
    userName = NULL,              # First name of the logged in user.
    userID = NULL,                # Unique fitbit ID for individual. Used for IDing files. 
    rawFile = NULL,               # Temp raw data file locations for dropbox so we dont write multiple per session. 
    tagFile = NULL                # Temp tag file location. 
  )
  
  # Start with the tabs disabled as the user isn't logged in yet.
  disableTabs()

  # Fitbit authentication button. 
  loginButton <- callModule(shinyLogin, "fitbit_login", api_info = api_keys)
  
  # Tagging interface server-side,
  userTags <- reactive({})
  
  # If the user has yet to login prompt them to, otherwise welcome them. 
  output$userName <- renderText({ 
    name <- reactive({state$userName})
    ifelse(is.null(name()),
      "Login to get tagging!",
      sprintf("Welcome back %s!", name())
    )
  })
 
  # Watch for the user logging in. 
  # When the user has logged in. Set the state for the user token to its returned value. 
  observeEvent(loginButton(), {
    state$userToken = loginButton()
    enableTabs()
    showLoader()
  })
  
  # User info from fitbits api. 
  userInfo <- reactive({
    # Only grab info if the user has logged in and we have their token
    req(state$userToken)
    # Grab user's info from the fitbit api
    getUserInfo(state$userToken)
  })
  
    
  # Once we have some user info lets use it.
  observeEvent(userInfo(), {
    # Grab users name and id from api. 
    state$userName <- userInfo()$firstName
    state$userID   <- userInfo()$encodedId
    
    # Get our user metadata from firebase. 
    userStats <- findUserInFirebase(firebaseToken, userInfo())
    
    # Find what days they have already downloaded. 
    state$alreadyDownloadedDays <- getAlreadyDownloadedDays(userStats)
    
    # If they have not downloaded any data before pull the most recent seven days to get started
    state$desiredDays <- makeDesiredDays(numberOfDays = 7, startDay = Sys.Date())
  })
  
  # On login when desired days are populated or when user re-requests some new days. 
  observeEvent(state$desiredDays, {
    desiredDays <- reactive({state$desiredDays})
    
    # Update user tags reactive object with the module. 
    userTags <- callModule(fitbitTagger, 'tagger', data = state$daysProfile)
    
    # Download desired day's data from fitbit
    state$daysProfile <- getPeriodProfile(token = state$userToken, desired_days = desiredDays())
    
    # Add new dates to the already downloaded list. 
    state$alreadyDownloadedDays <- unique(c(state$alreadyDownloadedDays,  desiredDays()))
    
    # Set up the dropbox file upload temp locations. 
    # Eventually if storage becomes an issue we may want to optimize this by not re-downloading duplicates.
    dbFileNames <- fileNamer(state$userID,  desiredDays()[1], tail( desiredDays(),n=1))
    state$rawFile <- dbFileNames("raw")
    state$tagFile <- dbFileNames("tag")
  })
  
  
  # When the user's day profile downloads...
  observeEvent(state$daysProfile, {
    hideLoader()
    daysProfile <- reactive({state$daysProfile})
    
    # Upload the raw data to dropbox.
    uploadDataToDropbox(daysProfile(), dbToken, state$rawFile)

    # Generate a report plot.
    output$reportPlot <- callModule(
      activityReport,
      'userReport',
      state$daysProfile
    )
  })
  
  # Update the downloads page with actual data.
  output$displayRaw <- renderTable(state$daysProfile %>% head())
  
  # Tag data table and download button
  output$displayTags <- renderTable(state$activityTags)
  output$downloadTags <- downloadHandler(
    filename = "my_activity_tags.csv",
    content = function(file) {
      write.csv(state$activityTags, file)
    }
  )
  
  # Raw data table and download button
  output$downloadRaw <- downloadHandler(
    filename = "my_fitbit_data.csv",
    content = function(file) {
      write.csv(state$daysProfile, file)
    }
  )
  
  # # Watch for users tagging stuff.
  observeEvent(userTags(), {
    state$activityTags <- userTags()
    #Upload tags to the dropbox tags file
    uploadDataToDropbox(state$activityTags, dbToken, state$tagFile)
  })
}

# Run the application
shinyApp(ui = ui,
         server = server,
         options = c("port" = 1410))
