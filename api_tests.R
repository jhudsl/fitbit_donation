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
  token = conf, 
  resolution = 'seconds',
  date = 'today',
  startTime = "00:00",
  endTime = "23:59"
)


get_heart_rate <- function(
  config, 
  resolution = 'seconds',
  date = 'today',
  startTime = "00:00",
  endTime = "23:59"
){
  res <- ifelse(resolution == "seconds", "1sec", "1min")
  query_string <- sprintf("https://api.fitbit.com/1/user/-/activities/heart/date/%s/1d/%s/time/%s/%s.json",
                          date, res, startTime, endTime)
  GET(query_string, config = config) %>% 
    content(as="text") %>% 
    fromJSON() %>% 
    .$`activities-heart-intraday` %>% 
    .$dataset %>% 
    mutate(time = as.numeric(hms(time))) %>% 
    rename(heart_rate = value)
}

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


# We can get steps too
my_steps <- get_steps(
  config, 
  date = 'today',
  startTime = "00:00",
  endTime = "23:59"
)

ggplot(my_steps, aes(x = time)) + 
  geom_point(aes(y = steps), color = "steelblue", alpha = 0.85, size = 0.75) + 
  theme_minimal()


# Or elevation...This one's kinda funky though.
my_elevation <- get_elevation(
  config, 
  date = 'today',
  startTime = "00:00",
  endTime = "23:59"
)

ggplot(my_elevation, aes(x = time)) + 
  geom_point(aes(y = elevation), color = "steelblue", alpha = 0.85, size = 0.75) + 
  theme_minimal()


read_csv("~/Downloads/my_fitbit_data.csv") %>% 
  filter(date == first(date)) %>% 
  select(time, value, type) %>% 
  jsonlite::toJSON() %>% 
  write("~/hopkins/fitbitViz/demo/fitbit_data.json")

