# Helped functions to interface with fitbits api

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
  config, 
  type = 'steps',
  date = 'today',
  startTime = "00:00",
  endTime = "23:59",
  resolution = "1min"
){
  query_string <- sprintf("https://api.fitbit.com/1/user/-/activities/%s/date/%s/1d/%s/time/%s/%s.json",
                          type, date, resolution, startTime, endTime)
  
  GET(query_string, config = config) %>% 
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
  config, 
  date = 'today',
  startTime = "00:00",
  endTime = "23:59"
){
  #grab activity result from the api. 
  query_result <- get_activity(
    config, 
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

