# This application predicts the next word based
# on selected phrase/word
# So, type a word or phrase
# Click the submit button
# And see the predicted next word

library(shiny)
library(data.table)
source("WordPredict.R", local=TRUE)

shinyUI(fluidPage(theme = "bootstrap.css",
  titlePanel("Next word prediction"),
  sidebarLayout(
    sidebarPanel(
    textInput("wordPhrase", "Word/Phrase:"),    
    submitButton("Predict")
   
 ),  
 mainPanel(
   verbatimTextOutput("predict"),
   plotOutput("wordcloud", width = 400, height = 300),
   
   tabsetPanel( 
     
     tabPanel("Summary", includeHTML("include.html")),
     tabPanel("Methods Used", includeHTML("methods.html")),
     tabPanel("Data Source", includeHTML("DataSource.html"))
   )
   
   )
 
  )
))

