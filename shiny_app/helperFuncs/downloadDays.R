# Takes a fitbit token number of desired days into the past and downloads them from fitbit
downloadDays <- function(token, numberOfDays){

  startDay <- Sys.Date() #start on today (probably needs to change eventually)
  endDay <- startDay - lubridate::days(numberOfDays)

  desiredDays <- as.character(seq(startDay, endDay, "-1 days"))
 
  return(list(
    data = getPeriodProfile(token, desired_days = desiredDays),
    days = desiredDays)
    )
}
