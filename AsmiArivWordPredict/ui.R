# This application predicts the next word based
# on selected phrase/word
# So, type a word or phrase
# Click the submit button
# And see the predicted next word

library(shiny)
library(data.table)
source("WordPredict.R", local=TRUE)

shinyUI(fluidPage(
  titlePanel("Next word prediction"),
    fluidRow(
    column(3, wellPanel(
    textInput("wordPhrase", "Word/Phrase:"),    
    submitButton("Predict")
 
 )),  
  
  column(6,
    p('This application predicts the next word based on selected phrase/word.
	So, type a word or phrase. Click the submit button. And see the 
	predicted next word'),
        verbatimTextOutput("predict"),
	plotOutput("wordcloud", width = 400, height = 300)
    
    )
  )
))

