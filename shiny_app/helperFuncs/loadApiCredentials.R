library(secret)
# We are using the R package secrets to encript our api values. 
# No need to be scared about commiting something bad anymore! 


# This is the location of our "vault" in the project directory. 
vault <- here::here("apiVault")

# If you need to add a user you do it here. 
# add_user('n.strayer@vanderbilt.edu', local_key()$pubkey, vault = vault)

## This is the logic to get a token and then store it. The most important "secret" part of this is the json file. 
# firebase_token <- googleAuthR::gar_auth_service(
#   json_file="fitbitdatadonation-firebase-adminsdk-p35yx-30b97f62d7.json",
#   scope = c("https://www.googleapis.com/auth/firebase.database", "https://www.googleapis.com/auth/userinfo.email"))
# 
# add_secret("firebase_token", firebase_token, users = c("n.strayer@vanderbilt.edu"), vault = vault)

firebase_token <- get_secret("firebase_token", key = local_key(), vault = vault)

## We can now use this token with simple httr requests
# req <- PUT(url = "https://fitbitdatadonation.firebaseio.com/testing.json",
#            body = list(x = "A simple text string another time"),
#            config = config(token = firebase_token), 
#            encode = "json")