

# Defines what we store per individual in our firebase database. 
# We have all the demographic info inside fitbit so we will just grab their name 
# and also store info on when they logged in and what days weve downloaded for them. 
# This way when they return we won't redownload days and we'll know to look for tags already made. 
userInfoTemplate <- function(userInfoList){
  
  userId <- userInfoList$encodedId
  putBody <- list()
  putBody[[userId]] <- list(
    fullName = userInfoList$fullName,
    firstName = userInfoList$firstName,
    logins = c( Sys.Date() ),
    daysPulled = c()
  )
  
  putBody
}


# Takes firebase token and the list of user info returned by the fitbit api and 
# sets up a new user in the users/ section of our firebase database. 
addUser <- function(firebase_token, userInfoList){
  
  # Set up list for basic user info structure.  
  putBody <- userInfoTemplate(userInfoList)
  
  queryUrl <- "https://fitbitdatadonation.firebaseio.com/users.json"
  
  result <- httr::PUT(url = queryUrl,
                      body = putBody,
                      config = httr::config(token = firebase_token),
                      encode = "json") 
}


# Queries our users section of the database and trys to fetch a users info. 
# If there is no response we create a new user entry into the database. 
getUserData <- function(firebase_token, userId, userInfoList){
  
  queryUrl <- sprintf("https://fitbitdatadonation.firebaseio.com/users/%s.json", userId)
  result <- httr::GET(url = queryUrl,
                      config = httr::config(token = firebase_token),
                      encode = "json") %>% httr::content()
  
  # If our query returns empty.
  if(is.null(result)){
    result <- addUser(firebase_token, userInfoList)
  }
  
  result
}

