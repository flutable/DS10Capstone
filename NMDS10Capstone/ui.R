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
    tags$style("#inputlen{display:inline}"),
    tags$style("#p3_2len{display:inline}"),
    tags$style("#p5_4len{display:inline}"),

    tags$style("#n1{display:inline}"),
    tags$style("#n2{display:inline}"),
    tags$style("#n3{display:inline}"),
    tags$style("#n4{display:inline}"),
    tags$style("#n5{display:inline}")
  ),
  
  # Application title
  titlePanel("DS10 Capstone predictive text"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    column(9,
      textInput("usertext", 
                "Enter a phrase", 
                "")
    ),
    column(3,
      p(strong("Predicted: "), textOutput("textDisplay"))
    )
  ),  
  fluidRow(
    br(),
    h4(" N-gram input (right to left)"),
    column(12,
           h6(strong("inputlen: ", style="display:inline"), textOutput("inputlen"  ) ),
           h5(strong("1-gram: ", style="display:inline"), textOutput("n1"  ) ),
           h5(strong("1-pred: ", style="display:inline"), textOutput("p2_1") ),
           h5(strong("2_1g:   ", style="display:inline"), textOutput("p2_1g") ),
           
           h5(strong("2-gram: ", style="display:inline"), textOutput("n2")   ),
           h5(strong("2-pred: ", style="display:inline"), textOutput("p3_2") ),
           h6(strong("p3_2 len: ", style="display:inline"), textOutput("p3_2len") ),           
           
           h5(strong("3-gram: ", style="display:inline"), textOutput("n3")   ),
           h5(strong("3-pred: ", style="display:inline"), textOutput("p4_3") ),
           
           h5(strong("4-gram: ", style="display:inline"), textOutput("n4")   ),
           h5(strong("4-pred: ", style="display:inline"), textOutput("p5_4") ),
           h6(strong("p5_4 len: ", style="display:inline"), textOutput("p5_4len") ),
              
           h5(strong("5-gram: ", style="display:inline"), textOutput("n5")   ),
           h5(strong("5-pred: ", style="display:inline"), textOutput("p6_5") )
           
        
    ) #col
  ) #fluidRow 
)
)  #fluidpage
