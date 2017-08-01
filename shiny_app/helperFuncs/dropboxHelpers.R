# data <- read_csv('fitbit_data.csv')
# dbToken <-  readRDS("./shiny_app/storage-drop.rds")
# uniqueID <- "oiashdfoiahjf"
# type <- "raw"
uploadDataToDropbox <- function(data, dbToken, sessionID, type, start, end){
  csvDest <- tempfile(pattern = sprintf("%s_%s_%s_%s_", sessionID, type, start, end), fileext = ".csv")
  write_csv(data, csvDest) #write the csv to server
  drop_upload(csvDest, "apps/fitbitDonation/userData/", dtoken = dbToken) #upload to dropbox too.
}

