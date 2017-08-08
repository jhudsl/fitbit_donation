# Attempting classification with markov chain model
library(here)
library(tidyverse)
library(lubridate)

rawData <- here::here("tagPrediction/raw_data.csv") %>% read_csv() %>% select(-X1)
tagData <- here::here("tagPrediction/activity_tags.csv") %>% read_csv() %>% select(-X1) %>% mutate(start = round(start/60), end = round(end/60))

#takes data in the standard report format and adds the column totalMins which starts on the first day and counts up. 
makeLong <- function(data){
  minsInDay <- 24*60
  
  data %>% 
    group_by(type) %>% 
    arrange(date) %>% 
    mutate(day = as.integer(difftime(date, min(date), units = "days"))) %>% 
    mutate(minute = time/60,
           totalMins = (day*minsInDay) + minute) 
}

long_data <- rawData %>% 
  makeLong() %>% 
  spread(type, value) %>% 
  select(totalMins, hr = `heart rate`, steps)

# add in the classes at each minute
minsInDay <- 24*60

#expand tag data to cover their ranges. 
expandedTags <- tagData %>% 
  mutate(day = as.integer(difftime(date, min(rawData$date), units = "days")),
         startTotal = (day*minsInDay) + start,
         endTotal = (day*minsInDay) + end) %>% 
  select(tag, start = startTotal, end = endTotal)

library(rucrdtw)

# Pick out a tag from the data. 
tagOfInterest <- expandedTags[4,] #One of two hiking examples
tagStart <- tagOfInterest$start[1]
tagEnd <- tagOfInterest$end[1]

# Subset tag data into series
tagSequence <- long_data %>% filter(totalMins > tagStart & totalMins < tagEnd)

# Remove tag series from full data
leftOverSequence <- long_data %>% filter(!(totalMins > tagStart & totalMins < tagEnd)) 

# Search full data for taged series
dtw_search <- ucrdtw_vv(data = leftOverSequence$hr, query = tagSequence$hr, dtwwindow = 0.05)


foundClosest <- leftOverSequence[dtw_search$location:(dtw_search$location + dim(tagSequence)[1]),]

long_data %>% mutate(
  found = ifelse(totalMins %in% foundClosest$totalMins, "classified", 
                 ifelse(totalMins %in% tagSequence$totalMins, "training","not"))) %>% 
  gather(type, value, -totalMins, -found) %>% 
  ggplot(aes(x = totalMins, y = value, group = 1, color = found)) +
  geom_line() + facet_grid(type~.)

foundClosest %>% 
  gather(type, value, -totalMins) %>% 
  
