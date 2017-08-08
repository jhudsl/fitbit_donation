
welcomePanel <- function(){
  div(
    h1("Quantified Whole"),
    p("Every minute you have your fitbit on you are collecting troves of data. Never before has more information on activity patterns of people been present in the world. The one problem: it's stuck in the fitbit app."),
    p("This app aims to help free the data from the fitbit app to let you explore it as you want while also contributing your data to future studies."),
    h3("What does the app do?"),
    p("When you log in with fitbit below you are giving us permission to go in and pull your recent heartrate and step data. We then display this to you in the tab \"Tag Your Data\". Here you see an overview of your data and tag your data over time."),
    h3("What do I get out of this?"),
    p("Once you've tagged all your data we will generate a report for you on the next page (\"Shareable Report\") that you can either keep for posterity sake or tweet out to spread the word about the app. The more participants and data the more benefit to science!"),
    h3("Next Steps"),
    p("Once we have enough data from users we will start generating activity predictions based upon your data. For instance the app may predict you did interval training between 7 and 8 am followed by yoga at 8:30 to 9."),
    h3("Okay, I'm ready"),
    p("Just click the login button below and navigate over to the tag tab to get started!"),
    shinyLoginUI("fitbit_login"),
    p(
      a(href = "http://jhudatascience.org/", img(src = "https://raw.githubusercontent.com/jhudsl/drawyourprior/master/WWW/jhu_logo.png", height = 40) ),
      align= "right"
    )
  )
}
