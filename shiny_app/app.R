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
source('shiny_app/helperFuncs/makeDesiredDays.R')
source('shiny_app/helperFuncs/showAndHideFuncs.R')
source('shiny_app/helperFuncs/reportGenerator.R')
source("shiny_app/helperFuncs/dropboxHelpers.R")
source("shiny_app/helperFuncs/firebaseHelpers.R")
source("shiny_app/helperFuncs/downloadDays.R")

# API Info from encripted vault. 
source("shiny_app/helperFuncs/loadApiCredentials.R")
source("shiny_app/api_keys.R")

# options(shiny.reactlog=TRUE)

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
  # Initialize the app state here. 
  state <- isolate({reactiveValues()})
  
  # Call reducer script to set up reducer function with state context. 
  source("shiny_app/helperFuncs/reducer.R", local = TRUE)
  
  # Run state through empty reducer to initialize
  isolate({reducer()})

  # Start with the tabs disabled as the user isn't logged in yet.
  disableTabs()

  # Fitbit authentication button. 
  loginButton <- callModule(shinyLogin, "fitbit_login", api_info = api_keys)
  
  # Initialize a holder for the user tags function. This allows us to be able to 
  # call an observerEvent on it but also call it within another observe event as well. Hacky and probably not the best way.
  userTags <- reactive({NULL})
  
  # If the user has yet to login prompt them to, otherwise welcome them. 
  output$userName <- renderText({ 
    name <- state$userInfo$name
    ifelse(is.null(name),
      "Login to get tagging!",
      sprintf("Welcome back %s!", name)
    )
  })
  
  # Watch for the user logging in. 
  # When the user has logged in. Set the state for the user token to its returned value. 
  observeEvent(loginButton(), {
    reducer(type = "ADD_FITBIT_TOKEN", payload = loginButton())
    reducer(type = "SET_DESIRED_DAYS", payload = input$desiredDaysPicker)
    enableTabs()
    showLoader()
  })
  
    
  # Once we have a token lets use it to get user info. 
  observeEvent(state$userToken, {
    # Grab users name and id from api and send it to app state
    userInfo <- getUserInfo(state$userToken)
    reducer(type = "SET_USER_INFO", payload = userInfo)

    # Get our user metadata from firebase. 
    userStats <- findUserInFirebase(firebaseToken, userInfo)
    
    # Find what days they have already downloaded. 
    previouslyDownloaded <- getAlreadyDownloadedDays(userStats)
    reducer(type = "ADD_DOWNLOADED_DAYS", payload = previouslyDownloaded)
  })
  
  
  # Watch the date picker and set new days when it is updated. 
  observeEvent(input$desiredDaysPicker, {
    req(state$userToken)
    reducer(type = "SET_DESIRED_DAYS", payload = input$desiredDaysPicker)
  })
  
  
  # On login when desired days are populated or when user re-requests some new days. 
  observeEvent(state$desiredDays, {
    # Download desired day's data from fitbit
    showLoader()
    profile <- getPeriodProfile(token = state$userToken, desired_days = state$desiredDays)
    reducer(type = "ADD_DAYS_PROFILE", payload = profile)
    hideLoader()

    # Set up the dropbox file upload temp locations.
    # Eventually if storage becomes an issue we may want to optimize this by not re-downloading duplicates.
    dbFileNames <- fileNamer(state$userInfo$id, state$desiredDays[1], tail( state$desiredDays,n=1))
    reducer(type = "SET_FILE_PATHS", payload = list(raw = dbFileNames("raw"), tags = dbFileNames("tag")))
  })
  
  
  # When the user's day profile downloads...
  observeEvent(state$daysProfile, {
    hideLoader()
    
    userTags <- callModule(taggingModule, 'tagviz', data = state$daysProfile)
    
    # Add newly downloaded dates to the already downloaded list.
    reducer(type = "ADD_DOWNLOADED_DAYS", payload = state$desiredDays)
    
    # Upload the raw data to dropbox.
    uploadDataToDropbox(state$daysProfile, dbToken, state$filePaths$raw)
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
    reducer(type = "SET_ACTIVITY_TAGS", payload = userTags())
    #Upload tags to the dropbox tags file
    uploadDataToDropbox(userTags(), dbToken, state$filePaths$tags)
  })
  
  # Generate a report plot.
  output$reportPlot <- callModule(
    activityReport,
    'userReport',
    state$daysProfile
  )
 
}

# Run the application
shinyApp( ui = ui, server = server, options = c("port" = 1410) )
