library(shiny)
library(shinyauth)
library(tidyverse)
library(fitbitViewer)
source("api_helpers.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  h1("Let's query fitbit!"),
  shinyauthUI("fitbit_login"),
  hr(),
  downloadButton('downloadData', 'download this data'),
  hr(),
  fitbitVizOutput('tagviz')
)

server <- function(input, output) {
  source("shiny_app/api_keys.R")
  
  authButton <- callModule(shinyauth,
                           "fitbit_login",
                           api_info = api_keys)
  
  
  
  # logic for what happens after a user has drawn their values. Note this will fire on editing again too.
  observeEvent(authButton(), {
    userToken = authButton()
    
    # Generate a plot of the the day
    number_of_days <- 7
    start_day <- Sys.Date()
    desired_days <- c(start_day)
    for(i in 1:number_of_days){  desired_days <- c(desired_days, start_day - lubridate::days(i)) }
    desired_days <- desired_days %>% as.character()
  
    
    days_profile <- get_interval_profile(userToken, desired_days = desired_days)
    
    output$tagviz <- renderFitbitViz(
      fitbitViewer::fitbitViz(days_profile)
    )
    
    output$downloadData <- downloadHandler(
      filename = "my_fitbit_data.csv",
      content = function(file) {
        write.csv(days_profile, file)
      }
    )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server, options = c("port" = 1410))

