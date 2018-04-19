#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# 12 cols across in fluid
shinyUI(fluidPage(
  # Display the ngrams inline with their headings: https://stackoverflow.com/questions/39250200/rshiny-textoutput-and-paragraph-on-same-line
  #Seem to have to style each output separately :/ probably a way to do this better
  tags$head(
    tags$style("#p2_1{display:inline}"),
    tags$style("#p2_1g{display:inline}"),
    
    tags$style("#p3_2{display:inline}"),
    tags$style("#p4_3{display:inline}"),
    tags$style("#p5_4{display:inline}"),
    tags$style("#p6_5{display:inline}"),

    tags$style("#p3_2len{display:inline}"),
    tags$style("#p5_4len{display:inline}"),
    
    tags$style("#textDisplay{display:inline}"),
    #tags$style("#p5_4len{display:inline}"),

    tags$style("#n1{display:inline}"),
    tags$style("#n2{display:inline}"),
    tags$style("#n3{display:inline}"),
    tags$style("#n4{display:inline}"),
    tags$style("#n5{display:inline}")
  ),
  
  # Application title
  titlePanel("DS10 Capstone predictive text"),
  
  #  
  fluidRow(
  #   column(9,
  #     textInput("usertext", 
  #               "Enter a phrase", 
  #               "")
  #   ),
  #   column(3,
  #     p(strong("Predicted: "), textOutput("textDisplay"))
  #   )
  # ),  
  column(12,
      textInput("usertext", 
                "Enter a phrase", 
                ""),
             p(style="display:inline","The app displays the last 1-gram to 3-gram you enter."),
             p(style="display:inline","The predicted word from each ngram length is in",strong("bold."), " There needs to be at least 3 words entered before the 3-gram displays a prediction."),
             br(),br(),
             p(style="display:inline","The algorithm tries to find the prediction from the longest ngram you have entered from a database of 1-grams to 4-grams."),
             br(), br(),
          p(style="display:inline"," If you enter a 3-gram and a 4-gram is found in the database, the prediction is the most-likely last word in the 4-gram. If a 4-gram is not found, the prediction is the most-likely word at the end of a 3-gram, etc..This approach is known as 'Stupid Backoff'."),
            br(), br(),
             p(style="display:inline",strong("Prediction: "), textOutput("textDisplay"))
      
   )),

  fluidRow(
    #br(),
    h4(" N-gram input (right to left)"),
    column(12,

           # p(strong("1-gram: ", style="display:inline"), textOutput("n1"  ) ),
           # p(strong("1-pred: ", style="display:inline"), textOutput("p2_1") ),
           # p(strong("2_1g  : ", style="display:inline"), textOutput("p2_1g") ),
           
            p(style="display:inline", "1-gram: ", textOutput("n1"), strong(textOutput("p2_1"))  ),
            p(style="display:inline", "2-gram: ", textOutput("n2"), strong(textOutput("p3_2"))  ),
            p(style="display:inline", "3-gram: ", textOutput("n3"), strong(textOutput("p4_3"))  )
            #p(style="display:inline", "4-gram: ", textOutput("n4"), strong(textOutput("p5_4"))  )   #add comma if 

    ) #col
  ) #fluidRow 
)
)  #fluidpage
