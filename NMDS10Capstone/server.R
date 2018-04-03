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
  #constants
  InsufficientInput <- "Insufficient input" #...to make a prediction; input string not long enough.
  
  output$textDisplay <- renderText({
    
    paste0(input$usertext, " ", "####")  #initially, just put a random
     
  })
  
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
  # GetNgramlastword <- function(ngram, n) {
  #   #last word in split string is no.of tokens in ngram,hence "n"
  #   ifelse(length(ngram) > 0, result <- str_split(ngram, boundary("word"))[[1]][n], 
  #                             result <- "NoNgram")
  #   return(result)
  # }
  # GetNgramfirstwords <- function(ng, n){
  #    #Specify the ngram, and the order of the ngram eg "of the", 2  yields "of"; "of the fish",3 yields "of the"
  #   ifelse(length(ng) > 0, result <- glue::collapse(str_split(ng, boundary("word"))[[1]][1:n-1], sep=" "), 
  #                             result <- "NoNgram")
  #   return(as.character(result))
  # }
  
  # Prediction ----
  #Create reactive conductor so we use usertext only once
   utR <- reactive({ SanitiseInput(input$usertext) })
   inputlen <- reactive ({inputlen <- length( paste0(utR()) )})
   #inputlen <- length( paste0(utR())) 

   n1 <-  reactive({      getNgram(utR(), 1)     }) #reactive necessary as input$usertext is reactive
   n2 <-  reactive({ word(getNgram(utR(), 2), 1) }) #word extracts  
   n3 <-  reactive({ word(getNgram(utR(), 3), 1) }) 
   n4 <-  reactive({ word(getNgram(utR(), 4), 1) }) 
   n5 <-  reactive({ word(getNgram(utR(), 5), 1) })

   #Have we seen this ngram before? Lookup ngram and extract it.

    p2_1g   <- reactive({ ngram2[.(n1() ) ][order(-pr)][1,] })
    p2_1    <- reactive({ p2_1g()$w2 })
    p2      <- reactive({ stri_join(p2_1g()$w1, p2_1g()$w2, collapse= " ") }) #ngram + prediction
    
    p3_2g   <- reactive({ p3_2g <- ngram3[.(n2(), n1() )][order(-pr)][1,]  })
    p3_2    <- reactive({ p3_2g$w3 }) 
    p3_2len <- reactive({ length(p3_2) })
    
    p4_3g   <- reactive({ ngram4[.(n3(), n2(), n1() ) ][order(-pr)][1,] })
    p4_3    <- reactive({ p4_3g$w4 })
    
    p5_4g   <- reactive({ ngram5[.(n4() , n3() , n2(), n1() )][order(-pr)][1,] })
    p5_4    <- reactive({ p5_4g$w5 })
    p5_4len <- reactive({ length( p5_4g ) })

    # Pure MLE method
    # reactive({
    #   if       ( !is.na(p5_4g()$w5 )) {prediction <- p5_4g()$w5}  #need to check na, since ngram may not exist.
    #   else if ( !is.na(p4_3g()$w4 )) {prediction <- p4_3g()$w4}  #   smoothing will fix this
    #   else if ( !is.na(p3_2g()$w3 )) {prediction <- p3_2g()$w3}
    #   else if ( !is.na(p2_1g()$w2 )) {prediction <- p2_1g()$w2}
    # })
   # return( prediction)
   
  #get predictions from each of the ngrams
 
    
  #output$n1 <- renderText({ getNgram(input$usertext, 1) }) #must be in renderText to be reactive
   output$n1    <- renderText({ n1()   })   #Need () as n1() is a reactive object, not just a string/ngram
   output$p2_1  <- renderText({ p2_1() })
   output$p2_1g <- renderText({ p2() })
 #  
   output$n2   <- renderText({ n2() }) 
   #output$p3_2 <- renderText({ p3_2() })
 #  
   output$n3   <- renderText({ n3() }) 
   #output$p4_3 <- renderText({ p4_3() })
 #  
   output$n4   <- renderText({ n4() }) 
  # output$p5_4 <- renderText({ p5_4() })
 #  
   output$n5   <- renderText({ n5() })
 #  output$n6_5 <- renderText({ p6_5()})
 #  # output$inputlen <- renderText({inputlen})
 #  # output$inputlen <- renderText({inputlen})
 #  
 #  #output$p3_2len <- renderText({p3_2len()})
 # # output$p6_5len <- renderText({p6_5len()})
 # output$inputlen <- renderText({inputlen()})
})
