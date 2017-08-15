# action emmiters that act on our state

# app is defined in our app earlier. 

addUserToken <- function(token){
  app$dispatchAction(
    list(type = "ADD_FITBIT_TOKEN", token)
  )
}

setDesiredDays <- function(dateRange){
  app$dispatchAction(
    list(type = "SET_DESIRED_DAYS", makeDateRange(dateRange))
  )
}

setUserInfo <- function(userInfo){
  # Currently we're just pulling this from their profile. 
  userName <- userInfo$firstName
  userID   <- userInfo$encodedId
  
  app$dispatchAction(
    list(type = "SET_USER_INFO", list(name = userName, id = userID))
  )
}

addDownloadedDays <- function(days){
  app$dispatchAction(
    list(type = "ADD_DOWNLOADED_DAYS", days)
  )
}

addDaysProfile <- function(daysProfile){
  app$dispatchAction(
    list(type = "ADD_DAYS_PROFILE", daysProfile)
  )
}

setfilePaths <- function(rawPath, tagsPath){
  app$dispatchAction(
    list(type = "SET_USER_INFO",  list(raw = rawPath, tags = tagsPath))
  )
}

setActivityTags <- function(tags){
  app$dispatchAction(
    list(type = "SET_ACTIVITY_TAGS",  tags)
  )
}

