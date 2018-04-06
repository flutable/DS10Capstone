# utility functions
  MapWordToInt <- function(word) {
    return(ngram1[.(word)]$i1)
  }
  MapIntToWord <- function(i){
    return(ngram1[i1==i]$w1)
  }
  
  MapStringToInt <- function(st){
    return(as.integer(sapply(str_split(tolower(st),boundary("word")), MapWordToInt)))
  }
  
  MapIntToString <- function(intvec) {
    return(paste0(sapply(intvec, MapIntToWord), collapse=" "))
  }
MapWords <- function(ng)  {
  # Pre-prepare ngram 1: ngram1[, index := 1:nrow(ngram1)] #by reference
  ngcols <- grep("w[1-5]?", names(ng)) # returns an integervec with valid cols
  #Do this column by column
  # Integer  0 or both integer and text 1
  INT = 0
  BOTH = 1
  NGTYPE = BOTH
  if (NGTYPE==BOTH){
      for (word_column in 1:length(ngcols) ) {
        colname <- paste0("w",word_column)
        #col <- ng[, ngcols[word_column], with=FALSE] # extract the col, it keeps its name
        col <- ng[, colname, with=FALSE]
        names(col) <- "w"
        col$w <- ngram1[.(col$w)]$i1    #replace word text with its index
       # names(col) <- colname
        intcolname <- paste0("i",word_column)
        ng[, (intcolname) := col$w]  #integer ngrams only
      }
  } else {
    #only integer
    # colname <- paste0("i",word_column)
    # col <- ng[, colname, with=FALSE]
    # names(col) <- "i"
    # col$i <- ngram1[.(col$w)]$index    #replace word text with its index
    # names(col) <- colname
    # ng[, (colname) := col$w]  #integer ngrams only
  }  
}
 
  perplexity <- function(ng){
    # Collins method
    return(2 ^ (-sum(ng$lpr)/nrow(ng)))
  }
  
    getNgram <- function(userinput, nglength){
    #get the most recent nglength-gram from user input (from final word backwards)
    # eg "at the end of the"
    #   1: "the"
    #   2: "of the"
    #   3: "end of the"
    #   4: "the end of the"
    #   5: "at the end of the
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
    #NOT vectorised
    ifelse(length(ngram) > 0, result <- str_split(ngram, boundary("word"))[[1]][n], 
                              result <- "NoNgram")
    return(result)
  }
  GetNgramfirstwords <- function(ng, n){
     #Specify the ngram, and the order of the ngram eg "of the", 2  yields "of"; "of the fish",3 yields "of the"
    #NOT vectorised
    ifelse(length(ng) > 0, result <- glue::collapse(str_split(ng, boundary("word"))[[1]][1:n-1], sep=" "), 
                              result <- "NoNgram")
    return(as.character(result))
  }

  
  # Helper functions
downloadFile <- function( URL, dataFile ) {
  if (!file.exists(dataFile)) {
    download.file(URL, destfile=dataFile)
  }
}

readData <- function( filename, lines) {
  # f <- file(filename)
  #datalines <- readLines(f, n=lines)  #was readLines
  #datalines <- read_file(f)
  datalines <- read_lines(filename, n_max=lines)
  # on.exit(close(f))                           #close connection if read fail
  return(datalines)
}
