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
