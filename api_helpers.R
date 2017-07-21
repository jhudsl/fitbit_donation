# Helper functions to interface with fitbits api

library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)

#' The httr GET likes for tokens to be in its own proprietary format which ours are not. This function will perform the correct get request based upon either a token string (from shinyauth) or a token object (from httr).
#' @param query_string The query string to be sent in. See given API docs for details. 
#' @param token_or_conf Can be either a string token (e.g. from shinyauth) or a config object (e.g. what you get from httr's normal oauth function). 
#' @return A nice big blob of whatever your get request returns.

fitbit_get <- function(query_string,token_or_conf){
  
  if(class(token_or_conf) == "character"){
    return(
      httr::GET(query_string, httr::add_headers( Authorization = paste("Bearer", token_or_conf) ) )
    )
  } else {
    return(
      GET(query_string, config = token_or_conf)
    )
  }
  
}



##########################################################################
# Config Helper 
##########################################################################
#' Takes a list object with three strings: API credentials of your app's name, key, and secret. Returns a config object to be passed to other api helper functions. 
#'     If it's the first time using the function a browser will open and ask you to authorize the app. After this a token should be stored so you wont need to. 
#' @param api_keys An list with three values: $appname = your apps name on the api dashboard, $key = apps key (7 chars), and $secret (more than 7 chars). 
#' @param scope Array of what parts of the api you want access to. Defaults to getting you "activity"(steps), "heartrate", and "sleep". 
#' @return An httr configuration object that is required anytime the fitbit api is hit. 
#' @export
#' @examples
#' source("api_keys.R") #Kept somewhere super duper secret
#' my_config <- make_config(api_keys)
make_config <- function(
  api_keys,
  scope = c("activity", "heartrate", "sleep")
){
  fitbit_endpoint <- oauth_endpoint( authorize = "https://www.fitbit.com/oauth2/authorize",  
                                     access = "https://api.fitbit.com/oauth2/token")
  myapp <- oauth_app( appname = api_keys$appname,
                      key     = api_keys$key,
                      secret  = api_keys$secret)
  
  #What the heck, why do I need this?! Pull request will be sent
  Sys.setenv("HTTR_SERVER_PORT" = "1410")
  
  token <- oauth2.0_token(fitbit_endpoint, 
                          myapp, 
                          use_basic_auth = TRUE,
                          scope=scope)
  
  config(token = token)
}





#' Grabs heart-rate data from the fitbit api. Can do anything the API will give you. 
#' @param config An oauth config object setup with your token. 
#' @param resolution Can be set to "seconds" for 1 second intervals or "minutes" for 1 minute intervals. Defaults to seconds. 
#' @param date The day for which you want data. Defaults to the current day. Day format is yyyy-MM-dd. 
#' @param startTime HH:MM 24 hour time for when you want to start getting data. Defaults to midnight. 
#' @param endTime HH:MM 24 hour time for when you want to stop getting data. Defaults to 23:59.  
#' @return A dataframe with two rows. time of the day in seconds and beats per minute for that timepoint. 
#' @export
#' @examples
#' my_hr <- get_heart_rate(
#'   config = conf, 
#'   resolution = 'seconds',
#'   date = 'today',
#'   startTime = "00:00",
#'   endTime = "23:59"
#'  )
get_heart_rate <- function(
  token, 
  resolution = 'seconds',
  date = 'today',
  startTime = "00:00",
  endTime = "23:59"
){
  res <- ifelse(resolution == "seconds", "1sec", "1min")
  query_string <- sprintf("https://api.fitbit.com/1/user/-/activities/heart/date/%s/1d/%s/time/%s/%s.json",
                          date, res, startTime, endTime)
  fitbit_get(query_string, token) %>% 
    content(as="text") %>% 
    fromJSON() %>% 
    .$`activities-heart-intraday` %>% 
    .$dataset %>% 
    mutate(time = as.numeric(hms(time))) %>% 
    rename(heart_rate = value)
}


#' A general interface to the activity intraday api. Used in confunction with wrapper functions. 
#' @param config An oauth config object setup with your token. 
#' @param type Which activity api you desire. Options are calories, steps, distance, floors, and elevation. 
#' @param date The day for which you want data. Defaults to the current day. Day format is yyyy-MM-dd. 
#' @param startTime HH:MM 24 hour time for when you want to start getting data. Defaults to midnight. 
#' @param endTime HH:MM 24 hour time for when you want to stop getting data. Defaults to 23:59.  
#' @param resolution Can choose between "1min" and "15min" although I'm not sure why you'd ever want 15 minute. 
#' @return A dataframe with two rows. time of the day in seconds and beats per minute for that timepoint. 
#' @export
#' @examples
#' query_result <- get_activity(
#'   config, 
#'   type = 'calories',
#'   date = date,
#'   startTime = startTime,
#'   endTime = endTime
#' )
get_activity <- function(
  token, 
  type = 'steps',
  date = 'today',
  startTime = "00:00",
  endTime = "23:59",
  resolution = "1min"
){
  query_string <- sprintf("https://api.fitbit.com/1/user/-/activities/%s/date/%s/1d/%s/time/%s/%s.json",
                          type, date, resolution, startTime, endTime)
  
  fitbit_get(query_string, token) %>% 
    content(as="text") %>% 
    fromJSON() 
}


#' Grabs time series data at 1 minute intervals on steps. 
#' @param config An oauth config object setup with your token. 
#' @param date The day for which you want data. Defaults to the current day. Day format is yyyy-MM-dd. 
#' @param startTime HH:MM 24 hour time for when you want to start getting data. Defaults to midnight. 
#' @param endTime HH:MM 24 hour time for when you want to stop getting data. Defaults to 23:59.  
#' @return A dataframe with two rows. time of the day in seconds and steps for the previous minute. 
#' @export
#' @examples
#' my_steps <- get_steps(
#'   config = conf, 
#'   date = 'today',
#'   startTime = "00:00",
#'   endTime = "23:59"
#'  )
get_steps <- function(
  token, 
  date = 'today',
  startTime = "00:00",
  endTime = "23:59"
){
  #grab activity result from the api. 
  query_result <- get_activity(
    token, 
    type = 'steps',
    date = date,
    startTime = startTime,
    endTime = endTime
  )

  query_result$`activities-steps-intraday`$dataset %>% 
    as_data_frame() %>% 
    mutate(time = as.numeric(hms(time))) %>% 
    rename(steps = value)
}


#' Grabs both heartrate and steps for a given date at 1 minute intervals. 
#' @param config An oauth config object setup with your token. 
#' @param date The day for which you want data. Defaults to the current day. Day format is yyyy-MM-dd. 
#' @return A dataframe with two rows. time of the day in seconds and steps for the previous minute. 
#' @export
#' @examples
#' my_steps <- get_steps(
#'   config = conf, 
#'   date = 'today',
#'   startTime = "00:00",
#'   endTime = "23:59"
#'  )
get_day_profile <- function(
  token, 
  date = 'today'
){
  heart_rate <- get_heart_rate(
    token, 
    resolution = 'minutes',
    date = date
  )
  
  steps <- get_steps(
    token, 
    date = date
  )
    
  heart_rate %>% 
    mutate(type = "heart rate") %>% 
    rename(value = heart_rate) %>% 
    bind_rows(
      steps %>% 
        mutate(type = "steps") %>% 
        rename(value = steps) 
    ) %>% 
    mutate(date = ifelse(date == "today", as.character(Sys.Date()), date))
}

#' Grabs both heartrate and steps for a each day in a provided vector at 1 minute intervals. 
#' @param config An oauth config object setup with your token. 
#' @param desired_days A character vector of dates in yyyy-MM-dd format.
#' @return A dataframe with two rows. time of the day in seconds and steps for the previous minute. 
#' @export
#' @examples
#' my_steps <- get_steps(
#'   config = conf, 
#'   date = 'today',
#'   startTime = "00:00",
#'   endTime = "23:59"
#'  )
get_interval_profile <- function(
  token, 
  desired_days
){
  
  days_data <- get_day_profile(token, date = desired_days[1])
  for(day in desired_days[-1]){
    days_data <- days_data %>% 
      bind_rows(
        get_day_profile(token, date = day)
      )
  }
  days_data
}


#' Grabs time series data at 1 minute intervals on elevation. It's important to note that this is a relative measure and not feet above sea-level.
#' @param config An oauth config object setup with your token. 
#' @param date The day for which you want data. Defaults to the current day. Day format is yyyy-MM-dd. 
#' @param startTime HH:MM 24 hour time for when you want to start getting data. Defaults to midnight. 
#' @param endTime HH:MM 24 hour time for when you want to stop getting data. Defaults to 23:59.  
#' @return A dataframe with two rows. time of the day in seconds and elevation at timepoint.
#' @export
#' @examples
#' my_elevation <- get_elevation(
#'   config = conf, 
#'   date = 'today',
#'   startTime = "00:00",
#'   endTime = "23:59"
#'  )
get_elevation <- function(
  config, 
  date = 'today',
  startTime = "00:00",
  endTime = "23:59"
){
  #grab activity result from the api. 
  query_result <- get_activity(
    config, 
    type = 'elevation',
    date = date,
    startTime = startTime,
    endTime = endTime
  )
  
  query_result$`activities-elevation-intraday`$dataset %>% 
    as_data_frame() %>% 
    mutate(time = as.numeric(hms(time))) %>% 
    rename(elevation = value)
}

