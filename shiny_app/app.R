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
  
  ############################################# Initializations #############################################
  # These are all things that get run once at the start of a given user's session. 
  # They set up the framework for the subsequent reactive sections. 
  
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
  
  
  ############################################# Event Observers #############################################
  # Here we watch for things that can happen as our app is running, sending the results to the appropriate
  # reducer functions. Reducers go at the end after whatever intermediary logic we need beforehand. 
  
  # Watch for the user logging in. 
  observeEvent(loginButton(), {
    # Unlock the tabs and also show a loader for downloading data. 
    enableTabs()
    showLoader()
    
    # Set the state for the user token to its returned value.
    reducer(type = "ADD_FITBIT_TOKEN", payload = loginButton())
    reducer(type = "SET_DESIRED_DAYS", payload = input$desiredDaysPicker)
  })
  
    
  observeEvent(state$userToken, {
    # Grab users name and id from api and send it to app state
    userInfo <- getUserInfo(state$userToken)
    
    # Get our user metadata from firebase. 
    userStats <- findUserInFirebase(firebaseToken, userInfo)
    
    # Find what days they have already downloaded. 
    previouslyDownloaded <- getAlreadyDownloadedDays(userStats)
    
    # Set the user info in the state and also add previously downloaded days. 
    reducer(type = "SET_USER_INFO", payload = userInfo)
    reducer(type = "ADD_DOWNLOADED_DAYS", payload = previouslyDownloaded)
  })
  
  
  observeEvent(input$desiredDaysPicker, {
    # We require user token because otherwise this will fire at startup and we will 
    # accidentally try and grab data with not api token.
    req(state$userToken) 
    # If the user has logged in and subsequently changed the date range from the default update data. 
    reducer(type = "SET_DESIRED_DAYS", payload = input$desiredDaysPicker)
  })
  
  
  observeEvent(state$desiredDays, {
    # Reshow the loader as we're going to start downloading data. 
    showLoader()
    profile <- getPeriodProfile(token = state$userToken, desired_days = state$desiredDays)
   
    # Set up the dropbox file upload temp locations.
    # Eventually if storage becomes an issue we may want to optimize this by not re-downloading duplicates.
    dbFileNames <- fileNamer(state$userInfo$id, state$desiredDays[1], tail( state$desiredDays,n=1))
    
    reducer(type = "ADD_DAYS_PROFILE", payload = profile)
    reducer(type = "SET_FILE_PATHS", payload = list(raw = dbFileNames("raw"), tags = dbFileNames("tag")))
  })
  
  
  # When the user's day profile downloads...
  observeEvent(state$daysProfile, {
    # The downloading is done so we can hide the loader icon. 
    hideLoader()
    # Set up call module with the new data. Double arrow so it impacts the previous userTags variable
    userTags <<- callModule(taggingModule, 'tagviz', data = state$daysProfile)

    # Upload the raw data to dropbox.
    uploadDataToDropbox(state$daysProfile, dbToken, state$filePaths$raw)
    
    # Add newly downloaded dates to the already downloaded list.
    reducer(type = "ADD_DOWNLOADED_DAYS", payload = state$desiredDays)
  })
  
  # userTags() will fire when the user has created a new tag. 
  observeEvent(userTags(), {
    print(userTags())
    reducer(type = "SET_ACTIVITY_TAGS", payload = userTags())
  })
  
  # After new tags have been added to the state send them to dropbox as well. 
  observeEvent(state$activityTags, {
    print(state$activityTags)
    uploadDataToDropbox(state$activityTags, dbToken, state$filePaths$tags)
  })
  
  ############################################# Output Bindings #############################################
  # This is where the actual UI is bound. Basically these just watch our state and make the screen look
  # like is should for a given scenario. 
  
  output$userName <- renderText({ 
    name <- state$userInfo$name
    # If the user has yet to login prompt them to, otherwise welcome them. 
    ifelse(is.null(name),
           "Login to get tagging!",
           sprintf("Welcome back %s!", name)
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
 
  # Generate a report plot.
  output$reportPlot <- callModule(
    activityReport,
    'userReport',
    state$daysProfile
  )
}

# Run the application
shinyApp( ui = ui, server = server, options = c("port" = 1410) )
