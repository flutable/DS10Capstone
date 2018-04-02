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
  GetNgramlastword <- function(ngram, n) {
    #last word in split string is no.of tokens in ngram,hence "n"
    ifelse(length(ngram) > 0, result <- str_split(ngram, boundary("word"))[[1]][n], 
                              result <- "NoNgram")
    return(result)
  }
  GetNgramfirstwords <- function(ng, n){
     #Specify the ngram, and the order of the ngram eg "of the", 2  yields "of"; "of the fish",3 yields "of the"
    ifelse(length(ng) > 0, result <- glue::collapse(str_split(ng, boundary("word"))[[1]][1:n-1], sep=" "), 
                              result <- "NoNgram")
    return(as.character(result))
  }
  
  # Prediction ----
  #Create reactive conductor so we use usertext only once
   utR <- reactive({ SanitiseInput(input$usertext) })
   inputlen <- reactive ({inputlen <- length( paste0(utR()) )})
   #inputlen <- length( paste0(utR())) 

   n1 <-  reactive({getNgram(utR(), 1) }) #reactive necessary as input$usertext is reactive
   n2 <-  reactive({getNgram(utR(), 2) })
   n3 <-  reactive({getNgram(utR(), 3) }) 
   n4 <-  reactive({getNgram(utR(), 4) }) 
   n5 <-  reactive({getNgram(utR(), 5) })
  
   #1. need to assemble the sqldf search string here. This doesn't work:
   #     sqldf("select * from twitngram2 where ngram like 'n1()' limit 1 ")   # the reactive part is never evaluated for some reason
   #2. names mean "given n1, predict from 2-gram data"; "given n2, predict from 3-gram data'
   n1_pred <- reactive({paste0("select * from twitngram2 where ngram like '", n1(),"%' limit 1") })  #correct string is produced
   n2_pred <- reactive({paste0("select * from twitngram3 where ngram like '", n2(),"%' limit 1") })  
   n3_pred <- reactive({paste0("select * from twitngram4 where ngram like '", n3(),"%' limit 1") })  
   n4_pred <- reactive({paste0("select * from twitngram5 where ngram like '", n4(),"%' limit 1") })  
   n5_pred <- reactive({paste0("select * from twitngram5 where ngram like '", n5(),"%' limit 1") })  
   
   #Names mean 'predict 2-gram from 1 gram'
   p2_1g <- reactive({ p2_1g <- sqldf( n1_pred() )[[1]] })  #search for 2gram, [[]] gets just the gram, not the count
   
   p2_1  <- reactive({ ifelse(p2_1g() != "", p2_1 <- str_split(p2_1g(), boundary("word"))[[1]][2], #extract last word of 2gram
                                             p2_1 <- InsufficientInput)  
                    })
   
   p3_2g <- reactive({ p3_2g <- sqldf( n2_pred() )[[1]] }) #[[1]][1] })
  
   p3_2  <- reactive({ ifelse(p3_2g() != "", p3_2 <- str_split(p3_2g(), boundary("word"))[[1]][3], #extract last word of 3gram
                                             p3_2 <- InsufficientInput)  
                    })
   p3_2len <- reactive ({length(sqldf( n2_pred() )[[1]])})
   
   p4_3g <- reactive({ p4_3g <- sqldf( n3_pred() )[[1]] }) #[[1]][1] })
  
   p4_3  <- reactive({ ifelse(p4_3g() != "", p4_3 <- str_split(p4_3g(), boundary("word"))[[1]][4], #extract last word of 4gram
                                             p4_3 <- InsufficientInput)  
                    })
     
   p5_4g <- reactive({ p5_4g <- sqldf( n4_pred() )[[1]] }) #[[1]][1] })
  
   p5_4  <- reactive({ ifelse(p5_4g() != "", p5_4 <- str_split(p5_4g(), boundary("word"))[[1]][5], #extract last word of 5gram
                                             p5_4 <- InsufficientInput)  
   p5_4len <- reactive({length( p5_4g() ) })     
                    })
   p6_5g <- reactive({ p6_5g <- sqldf( n5_pred() )[[1]] }) #[[1]][1] })
  
   p6_5 <- reactive({ ifelse(p6_5g() != "", p6_5 <- str_split(p6_5g(), boundary("word"))[[1]][6], #extract last word of 5gram
                                             p6_5len <- InsufficientInput )
                    })
   p6_5len <- reactive({length( p6_5g() ) }) 
   
  #get predictions from each of the ngrams
 
    
  #output$n1 <- renderText({ getNgram(input$usertext, 1) }) #must be in renderText to be reactive
  output$n1    <- renderText({ n1()   })   #Need () as n1() is a reactive object, not just a string/ngram
  output$p2_1  <- renderText({ p2_1() })
  output$p2_1g <- renderText({ p2_1g() })
  
  output$n2   <- renderText({ n2() }) 
  output$p3_2 <- renderText({ p3_2() })
  
  output$n3   <- renderText({ n3() }) 
  output$p4_3 <- renderText({ p4_3() })
  
  output$n4   <- renderText({ n4() }) 
  output$p5_4 <- renderText({ p5_4() })
  
  output$n5   <- renderText({ n5() })
  output$n6_5 <- renderText({ p6_5()})
  # output$inputlen <- renderText({inputlen})
  # output$inputlen <- renderText({inputlen})
  # output$inputlen <- renderText({inputlen})
  # output$inputlen <- renderText({inputlen})
  
  output$p3_2len <- renderText({p3_2len()})
  output$p6_5len <- renderText({p6_5len()})
  output$inputlen <- renderText({inputlen()})
})
