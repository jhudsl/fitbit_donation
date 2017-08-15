# source(here::here('shiny_app/helperFuncs/rredux.R'))
# Every single state entry will get passed an action. It's given reducer will then deal with it. 
# The default or initial value will be given as a default function parameter. This is because on startup
# the state will be run through with an empty action. 

alreadyDownloadedDays_reducer <- function(alreadyDownloadedDays = c(), action){
  if(action$type == "ADD_DOWNLOADED_DAYS"){
    newDays <- action$payload
    return(unique(c(alreadyDownloadedDays, newDays)))
  } else {
    return(alreadyDownloadedDays)
  }
}

desiredDays_reducer <- function(desiredDays = c(), action){
  if(action$type == "SET_DESIRED_DAYS"){
    newDesiredDays <- action$payload
    return(newDesiredDays)
  } else {
    return(desiredDays)
  }
}

daysProfile_reducer <- function(daysProfile = NULL, action){
  if(action$type == "ADD_DAYS_PROFILE"){
    newDaysProfile <- action$payload
    return(newDaysProfile)
  } else {
    return(daysProfile)
  }
}

activityTags_reducer <- function(activityTags = NULL, action){
  if(action$type == "SET_ACTIVITY_TAGS"){
    newActivityTags <- action$payload
    return(newActivityTags)
  } else {
    return(activityTags)
  }
}


userToken_reducer <- function(token = NULL, action){
  if(action$type == "ADD_FITBIT_TOKEN"){
    newToken <- action$payload
    return(newToken)
  } else if( action$type == "REMOVE_FITBIT_TOKEN" ){
    newToken <- NULL
    return(newToken)
  } else {
    return(token)
  }
}

userInfo_reducer <- function(info = list(name = NULL, id = NULL), action){
  if(action$type == "SET_USER_INFO"){
    newInfo <- action$payload
    return(newInfo)
  } else if( action$type == "REMOVE_USER_INFO" ){
    newInfo <- list(name = NULL, id = NULL)
    return(newInfo)
  } else {
    return(info)
  }
}

filePaths_reducer <- function(paths = list(raw = NULL, tags = NULL), action){
  if(action$type == "SET_FILE_PATHS"){
    newPaths <- action$payload
    return(newPaths)
  } else if( action$type == "REMOVE_FILE_PATHS" ){
    newPaths <- list(raw = NULL, tags = NULL)
    return(newInfo)
  } else {
    return(paths)
  }
}


# Takes the current state and runs the provided action through all of its componenets 
# Mutates state because that's how reactiveValues needs it, but keeps it all localized in one place 
# for easier bug tracking. 
# reducer <- function(state = reactiveValues(), action = list(type = "INITIALIZE")){
reducer <- function(state = list(), action = list(type = "INITIALIZE")){
    print(paste("running the action", action$type))
  
  # What days has the user already downloaded in previous logins?
  state$alreadyDownloadedDays = alreadyDownloadedDays_reducer(state$alreadyDownloadedDays, action)
  
  # A vector of dates we want to pull
  state$desiredDays = desiredDays_reducer(state$desiredDays, action)
  
  # Data from the desiredDays
  state$daysProfile = daysProfile_reducer(state$daysProfile, action)
  
  # Data on activity tags. Supplied by our viz (but also previous tags eventually.)
  state$activityTags = activityTags_reducer(state$activityTags, action)
  
  # Oauth token for fitbits api.
  state$userToken = userToken_reducer(state$userToken, action)
  
  # Name and fitbit id of the user from the fitbit api. 
  state$userInfo = userInfo_reducer(state$userInfo, action)
  
  # Where we want to store the data from the user. 
  state$filePaths = filePaths_reducer(state$filePaths, action)
}



# appState <- stateManager$new(actionReducer = reducer)
# appState$value
# appState$dispatchAction(list(type = "ADD_FITBIT_TOKEN", payload = "oihadoifahdoiufhasp9dufhadioufhadiufh"))
# appState$value
# appState$dispatchAction(list(type = "ADD_DOWNLOADED_DAYS", payload = c(1,2,3)))
# appState$value



