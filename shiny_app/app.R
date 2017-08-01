library(shiny)
library(shinyauth)
library(tidyverse)
library(lubridate)
library(fitbitr)
library(shinyjs)
library(rdrop2)

source('shiny_app/tabs/welcomePanel.R')
source('shiny_app/tabs/tagPanel.R')
source('shiny_app/tabs/reportPanel.R')
source('shiny_app/tabs/downloadPanel.R')
source('shiny_app/tabs/report_generator.R')
source("shiny_app/heperFuncs/api_keys.R")
source("shiny_app/heperFuncs/dropboxHelpers.R")
source("shiny_app/heperFuncs/downloadDays.R")

ui <- fluidPage(
  useShinyjs(),
  title = "Quantified Whole",
  tags$head(
    tags$style(type="text/css", ".container-fluid {  max-width: 1000px;margin: 0 auto;}")
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
    numberOfDays = 7,
    desiredDays = NULL,
    daysProfile = NULL,
    activityTags = NULL,
    userToken = NULL,
    rawFile = NULL, #There are temp file locations for dropbox so we dont write multiple per session. 
    tagFile = NULL
  )

  # Fitbit authentication button. 
  authButton <- callModule(shinyauth, "fitbit_login", api_info = api_keys)
 
  # Watch for the user logging in. 
  observeEvent(authButton(), {
    
    state$userToken = authButton()
    
    # Kill the login message for the tagger
    shinyjs::hide(selector = "#tag_login_message", anim = TRUE)
  
  })
  
  # Once we have a user token download some data
  observeEvent(state$userToken, {
    fitbitDownload <- downloadDays(token = state$userToken, numberOfDays = state$numberOfDays)
    state$desiredDays <- fitbitDownload$days
    state$daysProfile <- fitbitDownload$data
  })
  
  # When the users day profile downloads...
  observeEvent(state$daysProfile, {
    
    # set up tagging interface 
    userTags <- callModule(fitbitTagger,
                           'tagger',
                           data = state$daysProfile)
    
    # upload data to dropbox
    print("Uploading to dropbox")
    
    uploadDataToDropbox(
      state$daysProfile, 
      dbToken, 
      sessionID, 
      type = "raw",
      start = state$desiredDays[1],
      end = state$desiredDays[length(state$desiredDays)]
    )
    
    # Generate a report plot.
    output$reportPlot <- callModule(
      activityReport,
      'userReport',
      state$daysProfile
    )
    
    # Watch for users tagging stuff. 
    observeEvent(userTags(), {
      state$activityTags <- userTags()
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
