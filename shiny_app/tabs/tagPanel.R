tagPanel <- function(){
  div(id = 'tab_panel',
      div(id = "tag_login_message",
          h3("Please login to get tagging!")
      ),
      div(id = "tag_loader", 
          div(class = "centered", h1("We're fetching your data!")),
          div(class = "centered", helixLoader())
      ),
      fitbitTaggerUI('tagger', height = "500px")
  )
}