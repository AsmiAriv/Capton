# This application predicts the next word based
# Selected phrase/word
# So, type a word or phrase
# Click the submit button
# And see the predicted next word

library(shiny)
library(data.table)
library(wordcloud)
source("WordPredict.R", local=TRUE)

shinyServer(
  function(input, output) {
      output$predict <- renderText({paste("Predicted Next Word:",predict(input(input$wordPhrase)))})
      output$wordcloud <- renderPlot({wordcloudPlot(predict(input(input$wordPhrase)))})
      
  }
)