library(shiny)
library(tidyverse)
library(tidytext)
library(jsonlite)
library(data.table)
library(sqldf)
library(glue) #collapse()
library(stringi)

# Load pre-processed data into shiny app
source("constants.R")
source("utility.R")

loadData <- function() {
  #load files containing each ngram data
  # Select small or large dataset. Small is already in 1-5 grams, about 100000 lines.
  DATALOC = "./data/"
  if (DATASIZE == SMALL) {
    ngram1File <- paste0(DATALOC, "smallng1.ng")
    ngram2File <- paste0(DATALOC, "smallng2.ng")
    ngram3File <- paste0(DATALOC, "smallng3.ng")
    ngram4File <- paste0(DATALOC, "smallng4.ng")
    ngram5File <- paste0(DATALOC, "smallng5.ng")
  } else { #all data
    ngram1File <- paste0(DATALOC, "ngram1.ng") 
    ngram2File <- paste0(DATALOC, "ngram2.ng")
    ngram3File <- paste0(DATALOC, "ngram3.ng")
    ngram4File <- paste0(DATALOC, "ngram4.ng")
    ngram5File <- paste0(DATALOC, "ngram5.ng")
  }
  
  if (!exists("ngram1")) {
    ngram1 <<- fread(ngram1File, sep=",", header=TRUE)
    print("Ngram1 data loaded")
  }
  if (!exists("ngram2")) {
    ngram2 <<- fread(ngram2File, sep=",", header=TRUE) #34s to read
    print("Ngram2 data loaded")
  }
  if (!exists("ngram3")) {
    ngram3 <<- fread(ngram3File, sep=",", header=TRUE) #2 mins to read
    print("Ngram3 data loaded")
  }
  if (!exists("ngram4")) {
    ngram4 <<- fread(ngram4File, sep=",", header=TRUE)
    print("Ngram4 data loaded")
  }
  # if (!exists("ngram5")) {
  #   ngram5 <<- fread(ngram5File, sep=",", header=TRUE)
  #   print("Ngram5 data loaded")                    
  # }
  print("Ngram data loaded")
}

indexData <- function() {
  setDT(ngram1);
  setDT(ngram2); 
  setDT(ngram3);
  setDT(ngram4); 
#  setDT(ngram5); 
  #ngram1[, i1 := 1:nrow(ngram1)]
  setkey(ngram1, w1)
  setkey(ngram2, w1, w2)
  setkey(ngram3, w1, w2, w3)
  setkey(ngram4, w1, w2, w3, w4)
#  setkey(ngram5, w1, w2, w3, w4, w5)
}

loadData()
indexData()

