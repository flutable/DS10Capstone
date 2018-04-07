library(tidyverse)
library(tidytext)
library(jsonlite)
library(data.table)
library(quanteda)
library(sqldf)
# Debugging notes
# shiny::runApp(display.mode="showcase") shows code
# cat(file=stderr()," message", variable)

shinyServer(function(input, output) {
  # Utility functions ----
  getNgram <- function(userinput, nglength){
    #get the most recent nglength-gram from user input
    ut <- as.data.table(data_frame(txt = userinput));
    ng <- ut %>% unnest_tokens(ngram, txt, token="ngrams", n=nglength)
    return (as.character(ng[.N]))   #.N is a data table variable holding the number of observations.
    #We want to display the most recent 1/2/3/4/5gram, hence .N holds
           #the index of the most recent -gram.
  } #getNgram
  
  SanitiseInput <- function(inputtext) {
    #remove all non-predictable characters from input
    return(gsub("[[:digit:]]|[[:punct:]]", "", x=inputtext, perl=FALSE))
  }

  RemBlank <- function(st) {
    # Remove blank output from being displayed
    if (st=="character(0)" || is.na(st)) {return("")} else {return(st)}
  }
  
  # Prediction ----
  #Create reactive conductor so we use usertext only once
   utR <- reactive({ SanitiseInput(input$usertext) })

   n1 <-  reactive({      getNgram(utR(), 1)     }) #reactive necessary as input$usertext is reactive
   n2 <-  reactive({ word(getNgram(utR(), 2), 1) }) #word extracts  
   n3 <-  reactive({ word(getNgram(utR(), 3), 1) }) 
   n4 <-  reactive({ word(getNgram(utR(), 4), 1) }) 
   n5 <-  reactive({ word(getNgram(utR(), 5), 1) })

    p2_1g   <- reactive({ ngram2[.(n1() ) ][order(-pr)][1,] })  #lookup n1, order by descending pr, first result only
    p2_1    <- reactive({ RemBlank(p2_1g()$w2) })               #Prediction is the NEXT word; remove blanks for displaying in app 
    n2stem  <- reactive({ RemBlank(p2_1g()$w1) })               #Stem of 2gram ie a word entered, not the prediction
    
    p3_2g   <- reactive({ p3_2g <- ngram3[.(n2(), n1() )][order(-pr)][1,]  })
    p3_2    <- reactive({ RemBlank(p3_2g()$w3) }) 
    n3stem  <- reactive({ stri_join(RemBlank(p3_2g()$w1), SPACE, RemBlank(p3_2g()$w2), collapse=" ") })
    
    p4_3g   <- reactive({ ngram4[.(n3(), n2(), n1() ) ][order(-pr)][1,] })
    p4_3    <- reactive({ RemBlank(p4_3g()$w4) })
    n4stem <- reactive({ stri_join(RemBlank(p4_3g()$w1),  SPACE, RemBlank(p4_3g()$w2), SPACE, RemBlank(p4_3g()$w3), collapse=" ") })
    
    p5_4g   <- reactive({ ngram5[.(n4() , n3() , n2(), n1() )][order(-pr)][1,] })
    p5_4    <- reactive({ RemBlank(p5_4g()$w5) })
    n5stem  <- reactive({ stri_join(RemBlank(p5_4g()$w1), SPACE, RemBlank(p5_4g()$w2), SPACE, RemBlank(p5_4g()$w3), SPACE, RemBlank(p5_4g()$w4), collapse=" ") })

   output$textDisplay <- renderText({
       if      ( !is.na(p5_4g()$w5 )) { prediction <- p5_4() }  # p5_4g()$w5need to check na, since ngram may not exist.
       else if ( !is.na(p4_3g()$w4 )) { prediction <- p4_3() }  # p4_3g()$w4  smoothing will fix this
       else if ( !is.na(p3_2g()$w3 )) { prediction <- p3_2() }  #p3_2g()$w3
       else if ( !is.na(p2_1g()$w2 )) { prediction <- p2_1() }  #p2_1g()$w2)
    })
 
  #Need () as n1() is a reactive object, not just a string/ngram   
   output$n1   <- renderText({ n2stem() })   #existing 1gram
   output$p2_1 <- renderText({ p2_1()   }) #prediction with blanks removed p2_1g()$w2
   output$n2   <- renderText({ n3stem() }) 
   output$p3_2 <- renderText({ p3_2()   }) #prediction p3_2g()$w3
   output$n3   <- renderText({ n4stem() }) 
   output$p4_3 <- renderText({ p4_3()   }) #prediction   p4_3g()$w4
   output$n4   <- renderText({ n5stem() }) 
   output$p5_4 <- renderText({ p5_4()   }) #prediction p5_4g()$w5

   #output$textDisplay <- renderText({ prediction })
})
