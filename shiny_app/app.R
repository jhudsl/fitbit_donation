library(shiny)
library(tidyverse)
library(lubridate)
library(fitbitr)
library(shinyjs)
library(rdrop2)

# UI elements
source('shiny_app/tabs/welcomePanel.R')
source('shiny_app/tabs/tagPanel.R')
source('shiny_app/tabs/reportPanel.R')
source('shiny_app/tabs/downloadPanel.R')

# Server-side helpers
source('shiny_app/helperFuncs/reportGenerator.R')
source("shiny_app/helperFuncs/dropboxHelpers.R")
source("shiny_app/helperFuncs/downloadDays.R")
source("shiny_app/helperFuncs/loadApiCredentials.R")

source("shiny_app/api_keys.R")

appCSS <- "
body {
   font-family: 'optima';
}
  
.container-fluid { 
  max-width: 1000px;
  margin: 0 auto;
}
"

ui <- fluidPage(
  useShinyjs(),
  title = "Quantified Whole",
  tags$head(
    tags$style(type="text/css", appCSS)
  ),
  tabsetPanel(
    tabPanel( "Welcome",           welcomePanel() ),
    tabPanel( "Tag Your Data",     tagPanel() ),
    tabPanel( "Shareable Report",  reportPanel() ),
    tabPanel( "Download Your Data",downloadPanel() )
  )
)

server <- function(input, output) {
  #load dropbox token
  dbToken <- readRDS("./shiny_app/storage-drop.rds")
  
  #Generate a random sessino ID for matching of tags and raw data. 
  sessionID <- paste(sample(letters,14, replace = T), collapse = "")
  
  # everything needed for the apps apperence is stored in here. 
  state <- reactiveValues(
    numberOfDays = 7,   # Number of days to draw from api at once. 
    desiredDays = NULL, # A vector of dates we want to pull (length = state$numberOfDays)
    daysProfile = NULL, # Data from the desiredDays
    activityTags = NULL,# Data on activity tags. Supplied by our viz (but also previous tags eventually.)
    userToken = NULL,   # Oauth token for fitbits api. 
    userName = NULL,    # Full name (First Last) of the logged in user.
    userID = NULL,      # Unique fitbit ID for individual. Used for IDing files. 
    rawFile = NULL,     # Temp raw data file locations for dropbox so we dont write multiple per session. 
    tagFile = NULL      # Temp tag file location. 
  )

  # Fitbit authentication button. 
  authButton <- callModule(shinyLogin, "fitbit_login", api_info = api_keys)
 
  
  # Watch for the user logging in. 
  # When we have a new user token grab the users info and set up file locations etc.
  observeEvent(authButton(), {
    
    state$userToken = authButton()
    
    # Kill the login message for the tagger
    shinyjs::hide(selector = "#tag_login_message", anim = TRUE)
  })
  
  
  # Once we have a user token download some data
  observeEvent(state$userToken, {
    # Grab users name and id from api. 
    userInfo  <- getUserInfo(state$userToken)
    state$userName <- userInfo$fullName
    state$userID <- userInfo$encodedId
    browser()
    fitbitDownload <- downloadDays(token = state$userToken, numberOfDays = state$numberOfDays)
    state$desiredDays <- fitbitDownload$days
    state$daysProfile <- fitbitDownload$data
    
    # Set up the dropbox file upload temp locations. 
    dbFileNames <- fileNamer(sessionID, state$desiredDays[1], state$desiredDays[state$numberOfDays])
    state$rawFile <- dbFileNames("raw")
    state$tagFile <- dbFileNames("tag")
  })

  # When the users day profile downloads...
  observeEvent(state$daysProfile, {

    # set up tagging interface
    userTags <- callModule(fitbitTagger,
                           'tagger',
                           data = state$daysProfile)

    # Upload the raw data to dropbox.
    uploadDataToDropbox(state$daysProfile, dbToken, state$rawFile)

    # Generate a report plot.
    output$reportPlot <- callModule(
      activityReport,
      'userReport',
      state$daysProfile
    )

    # Watch for users tagging stuff.
    observeEvent(userTags(), {
      state$activityTags <- userTags()
      #Upload tags to the dropbox tags file
      uploadDataToDropbox(state$activityTags, dbToken, state$tagFile)
    })

    # Update the downloads page with actual data.
    output$displayRaw <- renderTable(state$daysProfile %>% head())
    output$downloadTags <- downloadHandler(
      filename = "my_activity_tags.csv",
      content = function(file) {
        write.csv(state$activityTags, file)
      }
    )
    # Update the download buttons
    output$displayTags <- renderTable(state$activityTags)
    output$downloadRaw <- downloadHandler(
      filename = "my_fitbit_data.csv",
      content = function(file) {
        write.csv(state$daysProfile, file)
      }
    )
  })
}

# Run the application
shinyApp(ui = ui,
         server = server,
         options = c("port" = 1410))
