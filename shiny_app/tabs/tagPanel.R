tagPanel <- function(){
  div(
    div(id = "tag_downloading_animation",
        div(class = "centered", h1("We're fetching your data!")),
        div(class = "centered", helixLoader())
    ),
    div(id = 'tag_visualization',
        fitbitTaggerUI('tagger', height = "500px")
    )
  )
}