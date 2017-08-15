#playing with report styles

# Trying out a spiral chart for a report. 

library(tidyverse)
minToTimeOfDay <- function(mins) {
  (lubridate::ymd_hms("2011-06-04 00:00:00") + lubridate::minutes(mins)) %>% 
    format(format = "%I:%M %p")
}
minsInDay <- 60*24
chartWidth  <- 178 #mm
chartHeight <- 254 #mm


reportSetup <- function(data){
  
  uniqueDays <- data$date %>% unique() %>% format(format = "%b %d")
  totalDays <- uniqueDays %>% length()
 
  
  formatedData <- data %>% 
    arrange(date) %>% 
    mutate(day = as.integer(difftime(date, min(date), units = "days"))) %>% 
    mutate(minute = time/60,
           totalMins = ((day - 1)*minsInDay) + minute,
           posInDay = 2*minute/minsInDay) %>% 
    filter(type == "heart rate")
  
  
  full_data <- expand.grid(
    day = seq(0, totalDays - 1),
    minute = seq(0, minsInDay - 1)
  ) %>% 
    as_data_frame() %>% 
    full_join(formatedData, by = c("day", "minute")) %>% 
    fill(value) 
  
  
  
  firstDay <- tail(uniqueDays, 1)
  lastDay <- uniqueDays[1]
  shownDays <- seq(totalDays - 1, 0, by = -3)
  
  #most extreme stat
  highestHR <- full_data %>% filter(value == max(value)) 
  highHRDay <- rev(uniqueDays)[highestHR$day]
  highHRTime <- minToTimeOfDay(highestHR$minute) 
  
  hrSubtitle <- sprintf("My highest heart rate of %i bpm occured at %s on %s.\nData gathered using Fitbit Charge 2.", highestHR$value,highHRTime, highHRDay)
  mainTitle <- sprintf("My heart rate from %s to %s", firstDay, lastDay)
  
  squareChart(full_data, totalDays, shownDays, uniqueDays)
  
  roundChart(formatedData,totalDays)
}

squareChart <- function(full_data, totalDays, shownDays, uniqueDays){
  timeLabels <- c("midnight",  "2am",  "4",   "6",   "8",   "10",  "noon",  "2pm",  "4",  "6", "8", "10", "")
  
  full_data %>% 
    ggplot(aes(x = minute, y = day, fill = value)) + 
    geom_raster() + 
    viridis::scale_fill_viridis(option = 'C') +
    theme_minimal() +
    scale_x_continuous(breaks = seq(0,minsInDay, length.out = 13), labels = timeLabels, expand = c(0, 0)) +
    scale_y_continuous(breaks = shownDays, labels = rev(uniqueDays)[shownDays + 1], expand = c(0, 0)) +
    labs(x = "", y = "", fill = "bpm", 
         title = mainTitle,
         subtitle = hrSubtitle) + 
    theme(
      text = element_text(family = "Garamond", size = 13),
      axis.text.x = element_text(hjust = 0),
      axis.text.y = element_text(vjust = 1, angle = 15)
    )
}


roundChart <- function(formatedData,totalDays){
  # data <- data %>% filter(type != 'heart rate')
  timeLabels <- c("midnight", "1 am",  "2",  "3",  "4",  "5",  "6",  "7",  "8",  "9",  "10", "11", "noon", "1 pm", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")

  lineThickness <- ( chartWidth/ 2.2) /totalDays
  
  maxTotalMins <- max(formatedData$totalMins)
  
  numDaysShown <- 8
  shownDays <- as.integer(seq(totalDays - 1, 0, length.out = numDaysShown))
  yBreaks <- seq(0, maxTotalMins, length.out = numDaysShown)
  yLabels <- uniqueDays[shownDays + 1]

  
  formatedData %>% 
    ggplot(aes(x = posInDay, y = totalMins, color = value, group = as.character(day))) + 
    viridis::scale_color_viridis(option = 'C') + 
    coord_polar() + 
    geom_line(size = lineThickness) +
    theme_minimal() + 
    # facet_wrap(~type, ncol=1, strip.position = "bottom") + 
    scale_x_continuous(breaks = 0:23/12, labels = timeLabels) +
    scale_y_continuous(breaks = yBreaks, labels = yLabels, expand = c(0, 0), limits = c(-maxTotalMins*0.5, maxTotalMins)) +
    # ylim(-maxTotalMins, maxTotalMins) +
    theme(
      # axis.text.y = element_blank(),
      text = element_text(family = "Garamond", size = 13),
      strip.text = element_text(size = 14, vjust = 0.5),
      # panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank()
    ) + 
    labs(x = "", y = "", color = "", title = mainTitle, subtitle = hrSubtitle)
}


# data <- read_csv('fitbit_data.csv')
data <- read_csv('my_fitbit_data_big.csv')


squareChart(data)





