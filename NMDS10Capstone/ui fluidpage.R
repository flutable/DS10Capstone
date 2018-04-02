#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("DS10 Capstone predictive text"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput("usertext", 
                "Enter a phrase", 
                ""),
      p(strong("1-gram: "), textOutput("n1"), br()),
      h5("2-gram: "), textOutput("n2"),
      h5("3-gram: "), textOutput("n3"),
      h5("4-gram: "), textOutput("n4"),
      h5("5-gram: "), textOutput("n5"),
      width=6
    ),
   
  
    # Show a plot of the generated distribution
    mainPanel(
       textOutput("textDisplay")
    )
  )
))
