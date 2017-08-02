# Trying out a spiral chart for a report. 

# library(tidyverse)
# 
# data <- read_csv('fitbit_data.csv')


generateReport <- function(data){
  timeLabels <- c("midnight", "1 am",  "2",  "3",  "4",  "5",  "6",  "7",  "8",  "9",  "10", "11", "noon", "1 pm", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")
  minsInDay <- 24*60
  
  data %>% 
    arrange(date) %>% 
    mutate(day = as.integer(difftime(date, min(date), units = "days"))) %>% 
    mutate(minute = time/60,
           totalMins = ((day - 1)*minsInDay) + minute,
           posInDay = 2*minute/minsInDay) %>% 
    ggplot(aes(x = posInDay, y = totalMins, color = value, group = as.character(day))) + 
    viridis::scale_color_viridis(option = 'C') + 
    coord_polar() + 
    geom_line(size = 7.9) +
    theme_minimal() + 
    facet_wrap(~type, ncol=1, strip.position = "bottom") + 
    scale_x_continuous(breaks = 0:23/12, labels = timeLabels) +
    theme(
      axis.text.y = element_blank(),
      text = element_text(family = "Georgia"),
      strip.text = element_text(size = 14, vjust = 0.5)
    ) + 
    labs(x = "", y = "", color = "", title = "My Heartrate and Steps", subtitle = "Data gathered from my Fitbit and visualized using Quantified Whole")
}

# generateReport(data)

activityReportUI <- function(id){
  ns <- NS(id)
  div(
    tags$style(type="text/css", ".shiny-image-output img { width: 100%;}"),
    imageOutput(ns('userReport'), width = "100%")
  )
}


activityReport <-  function(input, output, session, data){
  
  output$userReport <- renderImage({
    outfile <- tempfile(fileext = ".png")
    report <- generateReport(data)
    ggsave(outfile, report, width = 7, height = 10, dpi = 265)
    
     # Return a list containing information about the image
    list(src = outfile,
         contentType = "image/png",
         alt = "This is alternate text",
         width = "100%"
         )
  }, deleteFile = TRUE)
  
  result <- reactive({output$userReport})
  return(result)
  
}

