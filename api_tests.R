library(plotly)
library(httr)
library(tidyverse)
library(lubridate)
library(zoo)
library(scales)
library(jsonlite)

source("api_helpers.R") #some helpful functions for dealing with the fitbit api.
source("api_keys.R") #contains a list with strings storing the below values. 

# setup a config object for querying api. 
conf <- make_config(api_keys)


#grab data for heart rate at a 1 second granularity for a given day
my_hr <- get_heart_rate(
  config = conf, 
  resolution = 'seconds',
  date = 'today',
  startTime = "00:00",
  endTime = "23:59"
)

#Plot it with rolling average
my_hr %>% 
  mutate(heart_rate_rolling = rollapply(heart_rate,120,mean,align='right',fill=NA)) %>% 
  ggplot(aes(x = time)) + 
  geom_line(aes(y = heart_rate), color = "steelblue", alpha = 0.15, size = 0.75) + 
  geom_line(aes(y = heart_rate_rolling)) + 
  theme_minimal()



#grab data for steps at a minute level as well for funs. 
my_hr_min <- get_heart_rate(
  config = conf, 
  resolution = 'minutes',
  date = 'today',
  startTime = "00:00",
  endTime = "23:59"
)

ggplot(my_hr_min, aes(x = time)) + 
  geom_line(aes(y = heart_rate), color = "steelblue", alpha = 0.85, size = 0.75) + 
  theme_minimal()

