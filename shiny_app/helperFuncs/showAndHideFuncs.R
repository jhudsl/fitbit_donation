#Spinning helix loader
hideLoader <- function(){
  print("hiding loader")
  shinyjs::hide(selector = "#tag_loader", anim = TRUE)
}

showLoader <- function(){
  print("showing loader")
  shinyjs::show(selector = "#tag_loader")
}


# Tagging interface header.
hideTagger <- function(){
  shinyjs::hide(selector = "#tagger-tagviz")
}
showTagger <- function(){
  shinyjs::show(selector = "#tagger-tagviz")
}

hideLoginMessage <- function(){
  shinyjs::hide(selector = "#tag_login_message")
}