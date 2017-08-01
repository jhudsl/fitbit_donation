tagPanel <- function(){
  div(id = 'tab_panel',
      h3(id = "tag_login_message", "Please login to get tagging!"),
      fitbitTaggerUI('tagger', height = "500px")
  )
}