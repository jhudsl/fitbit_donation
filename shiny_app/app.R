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

source("shiny_app/helperFuncs/loadApiCredentials.R")
source("shiny_app/api_keys.R")



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
  
  # Control when we can see the different tabs. 
  observe({
    if(is.null(state$userToken)){
      disableTabs()
    } else{
      enableTabs()
    }
  })

  # Fitbit authentication button. 
  authButton <- callModule(shinyLogin, "fitbit_login", api_info = api_keys)
 
  output$userName <- renderText({ "Login to get tagging!" })
 
  # Watch for the user logging in. 
  # When we have a new user token grab the users info and set up file locations etc.
  observeEvent(authButton(), {
    print("trigger authButton event observer")
    state$userToken = authButton()
    hideLoginMessage()
    showLoader()
  })
  
  # User info from fitbits api. 
  userInfo <- reactive({
    # Only grab info if the user has logged in and we have their token
    req(state$userToken)
   
    print('triggering the userInfo reactive event')
    # Grab user's info from the fitbit api
    getUserInfo(state$userToken)
  })
  
    
  # Once we have some user info lets use it.
  observeEvent(userInfo(), {
    print('triggering userInfo observe event')

    # Grab users name and id from api. 
    # userInfo       <- getUserInfo(state$userToken)
    state$userName <- userInfo()$firstName
    state$userID   <- userInfo()$encodedId
    # Get user's entry in firebase
    userStats <- findUserInFirebase(firebaseToken, userInfo())
    
    # Update the username output. 
    output$userName <- renderText({ 
      newUser <- length(getLoginTimes(userStats)) == 0
      welcomeSuffix <- ifelse(newUser, "", " back ")
      sprintf("Welcome %s%s!", welcomeSuffix, userInfo()$firstName )
    })

    # Find what days they have already downloaded. 
    state$alreadyDownloadedDays <- getAlreadyDownloadedDays(userStats)
    
    # If they have not downloaded any data before pull the most recent seven days to get started
    state$desiredDays <- makeDesiredDays(numberOfDays = 7, startDay = Sys.Date())
  })
  
  # On login when desired days are populated or when user re-requests some new days. 
  observeEvent(state$desiredDays, {
    print("running observe event for desired days.")

    # Download desired day's data from fitbit
    state$daysProfile <- getPeriodProfile(token = state$userToken, desired_days = state$desiredDays)
    
    # Add new dates to the already downloaded list. 
    state$alreadyDownloadedDays <- unique(c(state$alreadyDownloadedDays, state$desiredDays))
    
    # Set up the dropbox file upload temp locations. 
    # Eventually if storage becomes an issue we may want to optimize this by not re-downloading duplicates.
    dbFileNames <- fileNamer(state$userID, state$desiredDays[1], tail(state$desiredDays,n=1))
    state$rawFile <- dbFileNames("raw")
    state$tagFile <- dbFileNames("tag")
  })
  
  
  
  # When the user's day profile downloads...
  observeEvent(state$daysProfile, {
    print('triggering data downloaded observe event')
    
    hideLoader()
    
    # Tagging interface server-side, keep hidden until data shows up.
    userTags <- callModule(fitbitTagger, 'tagger', data = state$daysProfile)
    
    # Upload the raw data to dropbox.
    uploadDataToDropbox(state$daysProfile, dbToken, state$rawFile)

    # Generate a report plot.
    output$reportPlot <- callModule(
      activityReport,
      'userReport',
      state$daysProfile
    )
  })
  
  # Update the downloads page with actual data.
  output$displayRaw <- renderTable(state$daysProfile %>% head())
  
  output$downloadTags <- downloadHandler(
    filename = "my_activity_tags.csv",
    content = function(file) {
      write.csv(state$activityTags, file)
    }
  )
  # Download buttons
  output$displayTags <- renderTable(state$activityTags)
  output$downloadRaw <- downloadHandler(
    filename = "my_fitbit_data.csv",
    content = function(file) {
      write.csv(state$daysProfile, file)
    }
  )
  
  # Watch for users tagging stuff.
  reactive({
    req(userTags())
    observeEvent(userTags(), {
      state$activityTags <- userTags()
      print(state$activityTags)
      #Upload tags to the dropbox tags file
      uploadDataToDropbox(state$activityTags, dbToken, state$tagFile)
    })
  })
}

# Run the application
shinyApp(ui = ui,
         server = server,
         options = c("port" = 1410))
