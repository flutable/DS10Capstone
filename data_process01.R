library(tidyverse)
library(tidytext)
library(jsonlite)
library(data.table)
library(quanteda)
library(sqldf)
library(glue) #collapse()
library(stringi)

# 
# <!-- Does the link lead to an HTML page describing the exploratory analysis of the training data set? -->
#   <!-- Has the data scientist done basic summaries of the three files? Word counts, line counts and basic data tables? -->
#   <!-- Has the data scientist made basic plots, such as histograms to illustrate features of the data? -->
#   <!-- Was the report written in a brief, concise style, in a way that a non-data scientist manager could appreciate? -->
#   ```
# TODO
# 1. Remove foreign characters using iconv()
# 2. remove number tokens (or at least find tokens containing numbers)
# 3. print memory usage at different points
# 4. garbage collect gc() at certain points

# testing variables
PREPROCESSING = 0         #1 means pre-process, 0 means testing
                          #  Should only have to pre-process once to reproduce all files
                          #  Then switch to PREPROCESSING=0 for testing the algorithm

cLines   = -1             #-1 means read everything in the blog/twitter/news raw files. Only makes sense for DATASIZE=large

TWITTER  = 1              #1 means process this file, 0 means skip
BLOG     = 1
NEWS     = 1

SMALL    = 0
LARGE    = 1

DATASIZE = LARGE   #0 is small data set, 1 is all the data
SPACE    = " "     #a single space     

# Data locations
#There are some odd profanities here. 'Australians' is one!
profanityURL <- "https://raw.githubusercontent.com/zacanger/profane-words/master/words.json"

rawdataFile  <- "Coursera-SwiftKey.zip"
rawdataLoc   <- "./data_raw/"
cleandataLoc <- "./data_clean/"

blogFile <- paste0(rawdataLoc, "en_US.blogs.txt")
newsFile <- paste0(rawdataLoc, "en_US.news.txt")
twitFile <- paste0(rawdataLoc, "en_US.twitter.txt")
profanityJSON <- paste0(rawdataLoc,   "profanity.json")
profanityFile <- paste0(cleandataLoc, "profanity.txt")

# Select small or large dataset. Small is already in 1-5 grams, about 100000 lines.
if (DATASIZE == SMALL) {
  ngram1File <- paste0(cleandataLoc, "smallng1.ng")
  ngram2File <- paste0(cleandataLoc, "smallng2.ng")
  ngram3File <- paste0(cleandataLoc, "smallng3.ng")
  ngram4File <- paste0(cleandataLoc, "smallng4.ng")
  ngram5File <- paste0(cleandataLoc, "smallng5.ng")
} else { #all data
  ngram1File <- paste0(cleandataLoc, "ngram1.ng") 
  ngram2File <- paste0(cleandataLoc, "ngram2.ng")
  ngram3File <- paste0(cleandataLoc, "ngram3.ng")
  ngram4File <- paste0(cleandataLoc, "ngram4.ng")
  ngram5File <- paste0(cleandataLoc, "ngram5.ng")
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


# Download raw data
#dataf <- paste0("./", rawdataLoc, "/", rawdataFile)
#downloadFile(rawdataURL, dataf)


# Get some profanity ----
if (PREPROCESSING) {
  preproctime = Sys.time()
  downloadFile( profanityURL, profanityJSON)
  profanity <- fromJSON(profanityJSON)          # It's in JSON format, so strip out all the tags
  # remove asterisk from profanity (we are not yet doing stemming. \ escapes, \* resolves to asterisk)
  profanity <- gsub("\\*", "", profanity, ignore.case=TRUE, perl=TRUE)
  profanity <- unique(profanity)                # remove duplicates
  
  # Save text-format profanity for later
  write(profanity, file=profanityFile)
  profdf <- data_frame(profanity)
  } else {
  #Get existing data
  if (!exists("profdf")) {
    profdf <- fread(profanityFile, sep="\n", header=FALSE)
  }} #profanity preprocessing

 # IF PREPROCESSING ----
if (PREPROCESSING) {
  if (DATASIZE==LARGE) {    # Only do this on large dataset; it creates the small dataset too.
    # read data
    nLines = cLines  #n=-1 reads everything
    
    #read text, tolower, remove digits/punctuation/foreign chars
    #
    if (TWITTER) {
      twitData <- readData(twitFile, nLines)
      twitData <- iconv(twitData, from="UTF-8", to="ASCII","")  #last "" means remove non-convertible characters.
      twitData <- tolower(twitData)
      
      twitData <- gsub("[[:digit:]]", "", x=twitData, perl=FALSE)
      twitData <- gsub("[[:punct:]]", "", x=twitData, perl=FALSE)
      twitData <- gsub("[[:cntrl:]]", "", x=twitData, perl=FALSE)
      twitData <- gsub("^\ +",        "", x=twitData, perl=FALSE)  # remove spaces at start of line
      twitdf   <- data_frame(text=twitData)   #must have text= parameter, else unnest_tokens or tokenize doesn't work
      twitdf   <- sqldf("select text from twitdf where length(text) > 1")   #only get valid lines, else fread won't work :/
    }
    
    #read blog text
    if (BLOG){
      blogData <- readData(blogFile, nLines)  
      blogData <- iconv(blogData, from="UTF-8", to="ASCII","")  
      blogData <- tolower(blogData)
      blogData <- gsub("[[:digit:]]", "", x=blogData, perl=FALSE)
      blogData <- gsub("[[:punct:]]", "", x=blogData, perl=FALSE)
      blogData <- gsub("[[:cntrl:]]", "", x=blogData, perl=FALSE)
      blogData <- gsub("^\ +",        "", x=blogData, perl=FALSE)    
      blogdf   <- data_frame(text=blogData)
      blogdf   <- sqldf("select text from blogdf where length(text) > 1") 
    }  
    #read News text
    if (NEWS) {
      newsData <- readData(newsFile, nLines)  
      newsData <- iconv(newsData, from="UTF-8", to="ASCII","")  
      newsData <- tolower(newsData)
      newsData <- gsub("[[:digit:]]", "", x=newsData, perl=FALSE)
      newsData <- gsub("[[:punct:]]", "", x=newsData, perl=FALSE)
      newsData <- gsub("[[:cntrl:]]", "", x=newsData, perl=FALSE)  
      newsData <- gsub("^\ +",        "", x=newsData, perl=FALSE)
      newsdf   <- data_frame(text=newsData)
      newsdf   <- sqldf("select text from newsdf where length(text) > 1") 
    }  
    #Quiz 1 ----
    # fsize = file.size(blogFile)
    # print(paste0("blogs file size: ",fsize))
    # 
    # #1.2
    # zTwitLen <- length(twitData)
    # print(paste0("en_US.twitter.txt has ",zTwitLen, " lines of text" ))
    # 
    # #1.3
    # maxLLtwit <- max(str_length(twitData))
    # maxLLblog <- max(str_length(blogData))
    # maxLLnews <- max(str_length(newsData))
    # print(paste0("Max twitter line length: ", maxLLtwit))
    # print(paste0("Max blog    line length: ", maxLLblog))
    # print(paste0("Max news    line length: ", maxLLnews))
    # 
    # #1.4
    # nlineslove <- sum(str_count(twitData, "love")) 
    # nlineshate <- sum(str_count(twitData, "hate")) 
    # print(paste0("Love to hate ratio: ", nlineslove/nlineshate))
    # 
    # #1.5
    # #one tweet mentions "biostats". what is the full tweet?
    # str_subset(twitData,"biostats")
    # #[1] "i know how you feel.. i have biostats on tuesday and i have yet to study =/"
    # 
    # # 1.6
    # # find how many tweets contain "A computer once beat me at chess, but it was no match for me at kickboxing"
    # sum(str_count(twitData, "A computer once beat me at chess, but it was no match for me at kickboxing"))
    
    
    # Merge all data to create complete corpus ----
    L <- list(twitdf, blogdf, newsdf)
    #rm(twitdf); rm(blogdf); rm(newsdf)
    corpus <- rbindlist(L)
    rm(L)
    gc()
    # Save master corpus
    fwrite(x=corpus, file=paste0(cleandataLoc,"corpusNewsBlogTwit.txt"))
    
    #Create smaller corpora for test and training ----
    # Divide into test and training corpora
    corpusrows <- 1:nrow(corpus)
    training   <- sample(corpusrows, 0.7 * nrow(corpus), replace=FALSE)
    corpustraining <- corpus[training,]
    corpustest     <- corpus[-training,]
    fwrite(x=corpustraining, file=paste0(cleandataLoc, "corpustraining.txt"))
    fwrite(x=corpustest,     file=paste0(cleandataLoc, "corpustest.txt"))
    
    #  Create a smaller corpus of 400000 lines
    smallsample <- sample(corpusrows, 400000, replace=FALSE)
    smallcorpus <- corpus[smallsample,]
    
    #Divide smaller corpus into test and training sets
    smallcorpusrows <- 1:nrow(smallcorpus)
    smalltraining   <- sample(smallcorpusrows, 0.7 * nrow(smallcorpus), replace=FALSE)
    smalltrainingcorpus <- smallcorpus[smalltraining,  ]
    smalltestcorpus     <- smallcorpus[-smalltraining, ]
    
    fwrite(x=smalltrainingcorpus, file=paste0(cleandataLoc, "smalltrainingcorpus.txt"))
    fwrite(x=smalltestcorpus,     file=paste0(cleandataLoc, "smalltestcorpus.txt"))
    
    # Clean up
    rm(corpus)
    gc()
    print(paste0("Preprocessing time: ", difftime(Sys.time(), preproctime, units = 'sec')))  
  } #DATASIZE=LARGE
  
  # Finding ngrams ----
  # Assign the corpus depending on whether we're using the small or large datasets
  if (DATASIZE==SMALL) {
    #all the sampling has been done before.
    corpus <- smalltrainingcorpus
  } else {
    corpus <- corpustraining
  }
  
  findNgramtime <- Sys.time()
  
  ngram1 <- corpus %>%
      unnest_tokens(ngram, text, token="ngrams", n= 1) %>%
      count(ngram, sort=TRUE) 
  print(paste0("Find ngram1: ", difftime(Sys.time(), findNgramtime, units = 'sec'))) #292 sec 
 
  # Handling out-of-vocabulary items
  # Replace all tokens with count == 1 by token unk, then recompute all ngrams.
  #  1. Find n==1 tokens
  unkwords <- ngram1[n==1, c("ngram") ]

  #  2. modify unkwords so that only tokens with a space on either side are selected
  unkwords$ngram <- paste0(SPACE, unkwords$ngram, SPACE)  
  
  # 3. modify all unkwords: <space>unkword<space>; shorter unkwords embedded in other words are not affected
  tempreplacement <- paste0(SPACE, "unk", SPACE)
  unkwords <- data.frame(unkwords[ , replacement:= tempreplacement])
  
  # 4.Now replace each occurrence of all elements of unkwords in original corpus  #6 ytimes slower than stri_replace_all_fixed
  #   Approximately 25 items per second using 1 core on an i6700. 
  #   Total lines=4,269,678 => 47 hours on 1 core, approx 8 hours on 7 cores.
  corpus <- stri_replace_all_fixed(str=corpus$text, pattern=unkwords$ngram, replacement=unkwords$replacement,
                                   vectorize_all=FALSE )
  
  
 # Ngrams2-5 will have UNK already incorporated  
  # Recompute ngram1 with UNK so we can compute OOV probabilities. This removes unigrams with count ==1, as they're all UNK.
    ngram1 <- corpus %>%
      unnest_tokens(ngram, text, token="ngrams", n= 1) %>%
      count(ngram, sort=TRUE) 
  print(paste0("Find ngram1 with UNK: ", difftime(Sys.time(), findNgramtime, units = 'sec'))) #292 sec 
  
  findNgramtime <- Sys.time()
  ngram2 <- corpus %>%
      unnest_tokens(ngram, text, token="ngrams", n= 2) %>%
      count(ngram, sort=TRUE) 
  print(paste0("Find ngram2: ", difftime(Sys.time(), findNgramtime, units = 'sec'))) # sec 

  findNgramtime <- Sys.time()
  ngram3 <- corpus %>%
      unnest_tokens(ngram, text, token="ngrams", n= 3) %>%
      count(ngram, sort=TRUE) 
  print(paste0("Find ngram3: ", difftime(Sys.time(), findNgramtime, units = 'sec'))) # sec 

  findNgramtime <- Sys.time()
  ngram4 <- corpus %>%
      unnest_tokens(ngram, text, token="ngrams", n= 4) %>%
      count(ngram, sort=TRUE) 
  print(paste0("Find ngram4: ", difftime(Sys.time(), findNgramtime, units = 'sec'))) #292 sec 

  findNgramtime <- Sys.time()
  ngram5 <- corpus %>%
      unnest_tokens(ngram, text, token="ngrams", n= 5) %>%
      count(ngram, sort=TRUE) 
  print(paste0("Find ngram5: ", difftime(Sys.time(), findNgramtime, units = 'sec'))) #292 sec 

  # Calculate MLE and log probabilities
  # -----------------------------------
  # p - probability = count(AB)/count(A) for bigrams, count(ABC)/count(AB) for trigrams, etc
  #     For 1grams, the frequency is the total occurences/total no. of tokens.
  # lpr - log probability
  time_probs <- Sys.time() #31 seconds for small dataset, approx 10 minutes for large dataset
  total_tokens = sum(ngram1$n)                             #includes all previous 1-count unigrams replaced with UNK
  setDT(ngram1)
  ngram1 <- ngram1[, pr  := ngram1[ngram==ngram1$ngram, ]$n/total_tokens] #Use data.table to add (:=) the pr column
  ngram1 <- ngram1[, lpr := log(pr)]
  
  setDT(ngram2)
  ngram2 <- ngram2[, pr  := ngram2[ngram==ngram2$ngram, ]$n/ngram1[ngram==GetNgramfirstwords(ngram2$ngram,2)]$n]
  ngram2 <- ngram2[, lpr := log(pr)]
  
  setDT(ngram3)
  ngram3 <- ngram3[, pr  := ngram3[ngram==ngram3$ngram, ]$n/ngram2[ngram==GetNgramfirstwords(ngram3$ngram,3),]$n]
  ngram3 <- ngram3[, lpr := log(pr)]
  
  setDT(ngram4)
  ngram4 <- ngram4[, pr  := ngram4[ngram==ngram4$ngram, ]$n/ngram3[ngram==GetNgramfirstwords(ngram4$ngram,4),]$n]
  ngram4 <- ngram4[, lpr := log(pr)]
  
  setDT(ngram5)  
  ngram5 <- ngram5[, pr  := ngram5[ngram==ngram5$ngram, ]$n/ngram4[ngram==GetNgramfirstwords(ngram5$ngram,5),]$n]
  ngram5 <- ngram5[, lpr := log(pr)]
  print(paste0("string: ", difftime(Sys.time(), time_probs, units = 'sec')))    

  #Split ngrams #1370seconds on i6700
  # paste(x$w1, x$w2, x$w3, x$w4, x$w5,sep=" ") reassembles.
  setnames(ngram1,"ngram","w1")
  
  ngram2[, w1 := word(ngram,1)]
  ngram2[, w2 := word(ngram,2)]

  ngram3[, w1 := word(ngram,1)]
  ngram3[, w2 := word(ngram,2)]
  ngram3[, w3 := word(ngram,3)]
  
  ngram4[, w1 := word(ngram,1)]
  ngram4[, w2 := word(ngram,2)]
  ngram4[, w3 := word(ngram,3)]
  ngram4[, w4 := word(ngram,4)]
  
  ngram5[, w1 := word(ngram,1)]
  ngram5[, w2 := word(ngram,2)]
  ngram5[, w3 := word(ngram,3)]
  ngram5[, w4 := word(ngram,4)]
  ngram5[, w5 := word(ngram,5)]

  #  Remove full ngrams
  ngram2[, ngram := NULL]
  ngram3[, ngram := NULL]
  ngram4[, ngram := NULL]
  ngram5[, ngram := NULL]
  
  #Save files
  fwrite(x=ngram1, file=ngram1File)
  fwrite(x=ngram2, file=ngram2File)
  fwrite(x=ngram3, file=ngram3File)
  fwrite(x=ngram4, file=ngram4File)
  fwrite(x=ngram5, file=ngram5File)
 
} #end PREPROCESSING

  # Utility functions -----
  ## getNgram, SanitiseInput
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

 # Testing starts here. Make sure PREPROCESSING is 0. Faster to read the ngrams than recreate them.
if (!exists("ngram1")) {
  ngram1 <- fread(ngram1File, sep=",", header=TRUE)
}
if (!exists("ngram2")) {
  ngram2 <- fread(ngram2File, sep=",", header=TRUE) #34s to read
}
if (!exists("ngram3")) {
  ngram3 <- fread(ngram3File, sep=",", header=TRUE) #2 mins to read
}
if (!exists("ngram4")) {
  ngram4 <- fread(ngram4File, sep=",", header=TRUE)
}
if (!exists("ngram5")) {
  ngram5 <- fread(ngram5File, sep=",", header=TRUE)
}

# trivial amount of time
  # setDT(ngram1); setkey(ngram1, w1)
  # setDT(ngram2); setkey(ngram2, w2, w1)
  # setDT(ngram3); setkey(ngram3, w3, w2, w1)
  # setDT(ngram4); setkey(ngram4, w4, w3, w2, w1)
  # setDT(ngram5); setkey(ngram5, w5, w4, w3, w2, w1)
  setDT(ngram1); setkey(ngram1, w1)
  setDT(ngram2); setkey(ngram2, w1, w2)
  setDT(ngram3); setkey(ngram3, w1, w2, w3)
  setDT(ngram4); setkey(ngram4, w1, w2, w3, w4)
  setDT(ngram5); setkey(ngram5, w1, w2, w3, w4, w5)
  
  perplexity <- function(ng){
    # Collins method
    return(2 ^ (-sum(ng$lpr)/nrow(ng)))
  }
# perplexity measures ----
  #for small training set
# > perplexity(ngram1)
# [1] 28891.82
# > perplexity(ngram2)
# [1] 2829.741
# > perplexity(ngram3)
# [1] 334.0508
# > perplexity(ngram4)
# [1] 113.9896
# > perplexity(ngram5)
# [1] 60.59071
#perplexity full corpus
#   > perplexity(ngram1)
# [1] 28891.82
# > perplexity(ngram2)
# [1] 3187.805
# > perplexity(ngram3)
# [1] 11.90226
# > perplexity(ngram4)
# [1] 0.977296
# > perplexity(ngram5)
# [1] 0.9940812
   

 predMLE <- function(usertext) {
   # Predict next word! 
   char0 <- "character(0)"  # string value when ngram not found
    #usertext <- "i" #am a good example of a person who will" #
    n1 <- char0; n2 <- char0; n3 <- char0; n4 <-char0; n5<-char0
    prediction <-""
    
    #Look for ngrams from final word backwards (ie get last unigram, last bigram, last trigram etc)
    n1 <-  getNgram(usertext, 1)  
    if (n1 != char0) { n2 <-  getNgram(usertext, 2) }  # proceed only if an ngram is found in what the user types
    if (n2 != char0) { n3 <-  getNgram(usertext, 3) } 
    if (n3 != char0) { n4 <-  getNgram(usertext, 4) }
    if (n4 != char0) { n5 <-  getNgram(usertext, 5) }
    #eg "I think you are a really
    n2 <- word(n2,1)
    n3 <- word(n3,1)
    n4 <- word(n4,1)
    n5 <- word(n5,1)
    
     timeDtLookup <- Sys.time()
    
    #Data.table lookups ----
    #Have we seen this ngram before? Lookup ngram and extract it.
    #MLE predicted 2gram
    timeDtLookup <- Sys.time()
    p2_1g   <- ngram2[.(n1) ][order(-pr)][1,]
    p2_1    <- p2_1g$w2
    print(paste0("p2_1g: ", p2_1g$ngram, ": ", p2_1))
    
    #MLE predicted 3gram
    p3_2g   <- p3_2g <- ngram3[.(n2, n1)][order(-pr)][1,]
    p3_2    <- p3_2g$w3
    p3_2len <- length(p3_2)
    print(paste0("p3_2g: ", p3_2g$ngram, ": ", p3_2 ))
    
    #MLE predicted 4gram
    p4_3g   <- ngram4[.(n3, n2, n1) ][order(-pr)][1,]
    p4_3    <- p4_3g$w4
    print(paste0("p4_3g: ", p4_3g$ngram, ": ", p4_3))

    #MLE predicted 5gram  
    p5_4g   <- ngram5[.(n4, n3, n2, n1)][order(-pr)][1,]
    p5_4    <- p5_4g$w5
    p5_4len <- length( p5_4g )
    print(paste0("p5_4g: ", p5_4g$ngram, ": ", p5_4))

    print(paste0("DataTable lookup time: ", difftime(Sys.time(), timeDtLookup, units = 'sec')))   
    # Pure MLE method
    if       ( !is.na(p5_4g$w5)) {prediction <- p5_4g$w5}  #need to check na, since ngram may not exist.
     else if ( !is.na(p4_3g$w4)) {prediction <- p4_3g$w4}  #   smoothing will fix this
     else if ( !is.na(p3_2g$w3)) {prediction <- p3_2g$w3}
     else if ( !is.na(p2_1g$w2)) {prediction <- p2_1g$w2}
    
    return( prediction)
    
    }# function pred
 
 predSB <- function(usertext) {
   # Predict next word using stupid backoff 
   char0 <- "character(0)"  # string value when ngram not found
    #usertext <- "i" #am a good example of a person who will" #
    n1 <- char0; n2 <- char0; n3 <- char0; n4 <-char0; n5<-char0
    countn1 <- 0; countn2 <-0; countn3 <- 0; countn4 <-0; countn5 <- 0
    # l <- list( p2, p3, p4,p5, countn1, countn2, countn3, countn4, countn5, S)
    # rm(l)
    
    #Look for ngrams from final word backwards (ie get last unigram, last bigram, last trigram etc)
    n1 <-  getNgram(usertext, 1)  
    if (n1 != char0) { n2 <-  getNgram(usertext, 2) }  # proceed only if an ngram is found in what the user types
    if (n2 != char0) { n3 <-  getNgram(usertext, 3) } 
    if (n3 != char0) { n4 <-  getNgram(usertext, 4) }
    if (n4 != char0) { n5 <-  getNgram(usertext, 5) }

    #Stupid backoff method ----
    # Take DT, subset rows using i, then calculate j, grouped by by.
    # Score = count(ngram order X)/count(ngram order X-1)
    system.time({
    if (n5 != char0) {  
      p5       <- ngram5[like(ngram, paste0(SPACE, n4))]
      countn5  <- nrow(p5)
    }
    if (n4 != char0) { 
      p4      <- ngram4[like(ngram, paste0(SPACE, n3))]  
      countn4 <- nrow(p4) 
    }
    if (n3 != char0) {  
      p3       <- ngram3[like(ngram, paste0(SPACE, n2))] 
      countn3  <- nrow(p3)
    }
    if (n2 != char0) {  
      p2      <-  ngram2[like(ngram, paste0(SPACE, n1))] 
      countn2 <- nrow(p2)
    }
    if (n1 != char0) {
      
      countn1  <- ngram1[ngram==n1]$n/nrow(ngram1) 
    }
    }) #system.time
    S <- 0
    if  (countn5 > 0) {
       S <- countn5/countn4 
       prediction <- GetNgramlastword(p5$ngram)
    } else if (countn4 > 0) {
      S <- 0.4 * countn4/countn3 
      prediction <- GetNgramlastword(p5$ngram)
    } else if (countn3 > 0) {
      S <- 0.4 * countn3/countn2
      prediction <- GetNgramlastword(p5$ngram)
    } else if (countn2 > 0) { S <- 0.4 * countn2/countn1 
    prediction <- GetNgramlastword(p5$ngram)
    } else {S <- 0.4 * countn1}
    return(prediction)

    }# function predSB

# summary statistics ----
# unique tokens after profanity filtering for all 3
# 90% coverage 
# line counts for all 3
# top 10 2- and 3-grams for all 3
# distribution of 


# #TODO refactor into a single function
# Create data frame for basic data
#dt <-data.frame(file="",length="",uniqueTokens="",coverage50="", coverage90="", stringsAsFactors=FALSE)

# if (TWITTER) {
#   twitCount <- tidy_twit %>%
#     count(word, sort=TRUE)
#   twitUniqueTokens <- length(twitCount$word)
#   twitLines <- length(twitData)
#   
#   # 90% coverage by occurrence => sum of occurrences = 90% of observed data
#   twit90 <- 0.9 * length(tidy_twit$word)
#   # find the index in twitcount where the cumulative sum of occurences ~= 90%
#   cumsumocc <- cumsum(twitCount$n)    #get all cumulative sums
#   twit90coverage <- max(which(cumsumocc <= twit90))     #get the largest index where the cumulative sum is <= the 90% figure.
#   #
#   twit50 <- 0.5 * length(tidy_twit$word)
#   # find the index in twitcount where the cumulative sum of occurences ~= 90%
#   cumsumocc <- cumsum(twitCount$n)    #get all cumulative sums
#   twit50coverage <- max(which(cumsumocc <= twit50))     #get the largest index where the cumulative sum is <= the 90% figure.
#   
#   twitCount <- tidy_twit %>%
#     count(word, sort=TRUE)
#   twitUniqueTokens <- length(twitCount$word)
#   twitLines <- length(twitData)
#   
#   dt[2,] <- rbind("en_US.twitter.txt", twitLines, twitUniqueTokens, twit50coverage, twit90coverage)
# }
# # #TODO refactor into a single function
# if (NEWS) {
#   newsCount <- tidy_news %>%
#     count(word, sort=TRUE)
#   newsUniqueTokens <- length(newsCount$word)
#   newsLines <- length(newsData)
#   # 90% coverage by occurrence => sum of occurrences = 90% of observed data
#   news90 <- 0.9 * length(tidy_news$word)
#   # find the index in twitcount where the cumulative sum of occurences ~= 90%
#   cumsumocc <- cumsum(newsCount$n)    #get all cumulative sums
#   news90coverage <- max(which(cumsumocc <= news90))     #get the largest index where the cumulative sum is <= the 90% figure.
#   #
#   news50 <- 0.5 * length(tidy_news$word)
#   # find the index in twitcount where the cumulative sum of occurences ~= 90%
#   cumsumocc <- cumsum(newsCount$n)    #get all cumulative sums
#   news50coverage <- max(which(cumsumocc <= news50))     #get the largest index where the cumulative sum is <= the 90% figure.
#   
#   dt[3,] <- rbind("en_US.news.txt",    newsLines, newsUniqueTokens, news50coverage, news90coverage)
# }  
# # #TODO refactor into a single function
# if (BLOG) {
#   blogCount <- tidy_blog %>%
#     count(word, sort=TRUE)
#   blogUniqueTokens <- length(blogCount$word)
#   blogLines <- length(blogData)
#   
#   # 90% coverage by occurrence => sum of occurrences = 90% of observed data
#   blog90 <- 0.9 * length(tidy_blog$word)
#   # find the index in twitcount where the cumulative sum of occurences ~= 90%
#   cumsumocc <- cumsum(blogCount$n)    #get all cumulative sums
#   blog90coverage <- max(which(cumsumocc <= blog90))     #get the largest index where the cumulative sum is <= the 90% figure.
#   #
#   blog50 <- 0.5 * length(tidy_blog$word)
#   # find the index in twitcount where the cumulative sum of occurences ~= 90%
#   cumsumocc <- cumsum(blogCount$n)    #get all cumulative sums
#   blog50coverage <- max(which(cumsumocc <= blog50))     #get the largest index where the cumulative sum is <= the 90% figure.
#   
#   dt[4,] <- rbind("en_US.blog.txt",    blogLines, blogUniqueTokens, blog50coverage, blog90coverage)
# }  

#   names(dt) <- c("File name", "Lines", "Unique Tokens", "Tokens covering 50%", "Tokens covering 90%")           
# Filter profanity - do this at prediction time, else data prep needs ~ 3100 * 4E6
# i <- 0; j <- 0
#   for (i in 1:length(testprof) ){
#     #print(testprof[i])
#     for (j in 1:length(testdf)){
#       #print(testdf[j])
#       testdf[j] <<- gsub(testprof[i], "", testdf[j])
#       #print(testdf[j])
#     }
#   }
# Archived stuff ----
 
    # sqldf lookups ----    
    # pre-prepare the SQLdf strings.
    # Assumptions
    # 1. Only want the first element of the subset, hence limit 1
    # 2. a space is the delimiter betweentokens, so the sqldf LIKE keyword has the parameter ngram followed by a space
    # 3. ngrams have already been pre-sorted so returned string has the highest probability/log pr already.
    n1_pred <- paste0("select * from ngram2 where ngram like '", n1," %' limit 1")   #n1gram prediction
    n2_pred <- paste0("select * from ngram3 where ngram like '", n2," %' limit 1")   
    n3_pred <- paste0("select * from ngram4 where ngram like '", n3," %' limit 1")   
    n4_pred <- paste0("select * from ngram5 where ngram like '", n4," %' limit 1")   
    #n5_pred <- paste0("select * from ngram5 where ngram like '", n5," %' limit 1")      
    # timeSqlLookup <- Sys.time()
    # 
    # #Names p2_1g 'predict 2-gram from 1 gram', p2_1 is the
    # p2_1g   <- sqldf( n1_pred )[[2]]   #search for 2gram, [[2]] gets gram, not the count. character(empty) if not found
    # p2_1    <- GetNgramlastword(p2_1g, 2)
    # print(paste0("p2_1g: ", p2_1g, ": ", p2_1))
    # 
    # p3_2g   <- sqldf( n2_pred )[[2]]
    # p3_2    <- GetNgramlastword(p3_2g, 3)
    # p3_2len <- length(p3_2)
    # print(paste0("p3_2g: ", p3_2g, ": ", p3_2 ))
    # 
    # p4_3g   <- sqldf( n3_pred )[[2]]
    # p4_3    <- GetNgramlastword(p4_3g, 4)
    # print(paste0("p4_3g: ", p4_3g, ": ", p4_3))
    # 
    # p5_4g   <- sqldf( n4_pred )[[2]]
    # p5_4    <- GetNgramlastword(p5_4g, 5)
    # p5_4len <- length( p5_4g )
    # print(paste0("p5_4g: ", p5_4g, ": ", p5_4))
    # 
    # # p6_5g   <- sqldf( n5_pred )[[2]]
    # # p6_5    <-  GetNgramlastword(p6_5g,6)
    # # p6_5len <- length( p6_5g )
    # # print(paste0("p6_5g: ", p6_5g, ": ", p6_5))
    # 
    # print(paste0("SQL lookup time: ", difftime(Sys.time(), timeSqlLookup, units = 'sec'))) 
    #

 ##### Don't mix tidyverse & data.table ----
  # mutate() changes things from data.table back to data.frame, you get all sorts of errors.
    # ngram1 <- setDT(ngram1) %>% 
  #             mutate(pr=ngram1[ngram==ngram1$ngram, ]$n/total_tokens) %>% 
  #             mutate(lpr=log(pr))
  # ngram2 <- setDT(ngram2) %>% 
  #             mutate(pr= (ngram2[ngram==ngram2$ngram, ]$n/ngram1[ngram==GetNgramfirstwords(ngram2$ngram,2)]$n) ) %>% 
  #             mutate(lpr=log(pr))
  # ngram3 <- ngram3 %>% 
  #             mutate(pr=ngram3[ngram==ngram3$ngram, ]$n/ngram2[ngram==GetNgramfirstwords(ngram3$ngram,3),]$n ) %>% 
  #             mutate(lpr=log(pr))
  # ngram4 <- ngram4 %>% 
  #             mutate(pr=ngram4[ngram==ngram4$ngram, ]$n/ngram3[ngram==GetNgramfirstwords(ngram4$ngram,4),]$n ) %>% 
  #             mutate(lpr=log(pr))
  # ngram5 <- ngram5 %>% 
  #             mutate(pr=ngram5[ngram==ngram5$ngram, ]$n/ngram4[ngram==GetNgramfirstwords(ngram5$ngram,5),]$n ) %>% 
  #             mutate(lpr=log(pr))

  # remove all ngrams with only one occurrence 
  # ng1_1 <- ngram1[n >1, ]
  # ng2_1 <- ngram2[n >1, ]
  # ng3_1 <- ngram3[n >1, ]
  # ng4_1 <- ngram4[n >1, ]
  # ng5_1 <- ngram5[n >1, ]
  # fwrite(x=ng1_1, file="ng1_1.ng")
  # fwrite(x=ng2_1, file="ng2_1.ng")
  # fwrite(x=ng3_1, file="ng3_1.ng")
  # fwrite(x=ng4_1, file="ng4_1.ng")
  # fwrite(x=ng5_1, file="ng5_1.ng")
  # 
  # format(object.size(ngram1), units="auto")
  # format(object.size(ng1_1),  units="auto")
  # format(object.size(ngram2), units="auto")
  # format(object.size(ng2_1),  units="auto")
  # format(object.size(ngram3), units="auto")
  # format(object.size(ng3_1),  units="auto")
  # format(object.size(ngram4), units="auto")
  # format(object.size(ng4_1),  units="auto")
  # format(object.size(ngram5), units="auto")
  # format(object.size(ng5_1),  units="auto")
 
 
# stdt <- as.data.table(data_frame(txt=userinput))   #Usertext is what is typed, txt is the column name
#   stdt1 <- stdt %>% unnest_tokens(ngram, txt, token="ngrams", n=1)  #ngram specifies the output column name
#   stdt2 <- stdt %>% unnest_tokens(ngram, txt, token="ngrams", n=2)
#   stdt3 <- stdt %>% unnest_tokens(ngram, txt, token="ngrams", n=3)
#   stdt4 <- stdt %>% unnest_tokens(ngram, txt, token="ngrams", n=4)
#   stdt5 <- stdt %>% unnest_tokens(ngram, txt, token="ngrams", n=5)
  
# n-gram stats
# dt <-data.frame(file="",length="",uniqueTokens="",coverage50="", coverage90="", stringsAsFactors=FALSE)
# dt[1,] <- rbind("en_US.twitter.txt", twitLines, twitUniqueTokens, twit50coverage, twit90coverage)
# dt[2,] <- rbind("en_US.news.txt",    newsLines, newsUniqueTokens, news50coverage, news90coverage)
# dt[3,] <- rbind("en_US.blog.txt",    blogLines, blogUniqueTokens, blog50coverage, blog90coverage)
# names(dt) <- c("File name", "Lines", "Unique Tokens", "Tokens covering 50%", "Tokens covering 90%")           

# cngrams <- data.frame(c(ctwitngram2, cnewsngram2, cblogngram2),
#                       c(ctwitngram3, cnewsngram3, cblogngram3))
# names(cngrams) <- c("2-grams", "3-grams")
#
 
  # ut <- as.data.table(data_frame(txt = userinput))
  # ng1 <- ut %>% unnest_tokens(ngram, txt, token="ngrams", n=1)
  # ctwitngram2 <- length(twitngram2$n)
  # ctwitngram3 <- length(twitngram3$n)
  # 
  # tsizes <- data.frame("1gram"=as.integer(object.size(twitngram1)),
  #                      "2gram"=as.integer(object.size(twitngram2)),
  #                       "3gram"=as.integer(object.size(twitngram3)),
  #                       "4gram"=as.integer(object.size(twitngram4)),
  #                       "5gram"=as.integer(object.size(twitngram5)))
 

# dt <- cbind(dt, cngrams)
# print(paste0(c("NLines: ", nLines, " 2/3grams.")))
# 
# print(difftime(Sys.time(), time1, units = 'sec'))

#---------End find ngrams
#   twitngram2 <- twitdf %>%
#     unnest_tokens(ngram,text, token="ngrams", n= 2) %>%
#     count(ngram, sort=TRUE) 
#   
#   twitngram3 <- twitdf %>%
#     unnest_tokens(ngram,text, token="ngrams", n= 3) %>%
#     count(ngram, sort=TRUE)
#   twitngram4 <- twitdf %>%
#     unnest_tokens(ngram,text, token="ngrams", n= 4) %>%
#     count(ngram, sort=TRUE) 
#   
#   twitngram5 <- twitdf %>%
#     unnest_tokens(ngram,text, token="ngrams", n= 5) %>%
#     count(ngram, sort=TRUE)
# if (TWITTER){
#   time5gram <- Sys.time()
#   twitngram1 <- twitdf %>%
#     unnest_tokens(ngram,text, token="ngrams", n= 1) %>%
#     count(ngram, sort=TRUE) 
#   
#   twitngram2 <- twitdf %>%
#     unnest_tokens(ngram,text, token="ngrams", n= 2) %>%
#     count(ngram, sort=TRUE) 
#   
#   twitngram3 <- twitdf %>%
#     unnest_tokens(ngram,text, token="ngrams", n= 3) %>%
#     count(ngram, sort=TRUE)
#   twitngram4 <- twitdf %>%
#     unnest_tokens(ngram,text, token="ngrams", n= 4) %>%
#     count(ngram, sort=TRUE) 
#   
#   twitngram5 <- twitdf %>%
#     unnest_tokens(ngram,text, token="ngrams", n= 5) %>%
#     count(ngram, sort=TRUE)
#  # print(difftime(Sys.time(), time1, units = 'sec'))
#   twitngram1 <- SetDT(twitngram1) # was: as.data.table(twitngram1). SetDT makes an in-place copy; a.d.t is slower
#   twitngram2 <- SetDT(twitngram2)
#   twitngram3 <- SetDT(twitngram3)
#   twitngram4 <- SetDT(twitngram4)
#   twitngram5 <- SetDT(twitngram5)
#   
#   # t1 <- as.data.table(twitngram1)
#   # t2 <- as.data.table(twitngram2)
#   # t3 <- as.data.table(twitngram3)
#   # t4 <- as.data.table(twitngram4)
#   # t5 <- as.data.table(twitngram5)
# } # if TWITTER
# 
# 
# #In
# if (BLOG){
#   blogngram2 <- blogdf %>%
#     unnest_tokens(ngram,text, token="ngrams", n= 2) %>%
#     count(ngram, sort=TRUE)
#   
#   blogngram3 <- blogdf %>%
#     unnest_tokens(ngram,text, token="ngrams", n= 3) %>%
#     count(ngram, sort=TRUE)
#   
#   cblogngram2 <- length(blogngram2$n)
#   cblogngram3 <- length(blogngram3$n)
# }  
# if (NEWS) {
#   newsngram2 <- newsdf %>%
#     unnest_tokens(ngram,text, token="ngrams", n= 2) %>%
#     count(ngram, sort=TRUE)
#   
#   newsngram3 <- newsdf %>%
#     unnest_tokens(ngram,text, token="ngrams", n= 3) %>%
#     count(ngram, sort=TRUE)
#   
#   cnewsngram2 <- length(newsngram2$n)
#   cnewsngram3 <- length(newsngram3$n)
# }


#   zcorpus <- data.frame(text=c("the cat sat onthe large mat", "the dog howled at the door","the mouse said hell no"), stringsAsFactors=FALSE)
# zunkwords <- data.frame(ngram=c("onthe","mat","hell"), replacement=c("unk", "unk", "unk"), stringsAsFactors=FALSE)
# zcorpus2 <- FindReplace(data=zcorpus, Var="text", replaceData=zunkwords,
#                         from="ngram", to="replacement", exact=FALSE)

  # system.time({
  # unkcorpus <- FindReplace( data=corpusold, Var="text", replaceData=unkwords,
  #                           from="ngram", to="replacement", exact=FALSE)
  # })
 
#combine vars to plot them
# want n=2 n=3
# file1
# file2

# Explore data
# ctidy_twit <- tidy_twit %>% 
#   count(word, sort=TRUE)

# ggtwit <- tidy_twit %>%
#   count(word, sort=TRUE) %>%
#   filter(n > 100000 )%>%
#   mutate(word=reorder(word,n)) %>%
#   ggplot(aes(word,n)) +
#   geom_col() +
#   xlab(NULL) +
#   coord_flip()

# ggtwit2 <- twitngram2 %>%
#    filter(n> 1000) %>%
#     mutate(ng2=reorder(ngram,n)) %>%
#   ggplot(aes(x=n)) +
#   geom_histogram(binwidth=1000) +
#   xlab("Occurrences") +
#   ggtitle("2-grams in Twitter data") +
#   coord_flip()
# ggtwit2
# 
# ggtwit3 <- twitngram3 %>%
#   filter(n> 1000) %>%
#   mutate(ng2=reorder(ngram,n)) %>%
#   ggplot(aes(x=n)) +
#   geom_histogram(binwidth=1000) +
#   xlab("Occurrences") +
#   ggtitle("3-grams in Twitter data") +
#   coord_flip()
# ggtwit3
# 
# ggblog2 <- blogngram2 %>%
#   filter(n> 1000) %>%
#   mutate(ng2=reorder(ngram,n)) %>%
#   ggplot(aes(x=n)) +
#   geom_histogram(binwidth=5) +
#   xlab("Occurrences") +
#   ggtitle("2-grams in Blog data") +
#   coord_flip()
# ggblog2
# 
# ggblog3 <- blogngram3 %>%
#   filter(n> 10) %>%
#   mutate(ng2=reorder(ngram,n)) %>%
#   ggplot(aes(x=n)) +
#   geom_histogram(binwidth=5) +
#   xlab("Occurrences") +
#   ggtitle("3-grams in Blog data") +
#   coord_flip()
# ggblog3
# 
# 
# ggnews2 <- newsngram2 %>%
#   filter(n> 10) %>%
#   mutate(ng2=reorder(ngram,n)) %>%
#   ggplot(aes(x=n)) +
#   geom_histogram(binwidth=5) +
#   xlab("Occurrences") +
#   ggtitle("2-grams in News data") +
#   coord_flip()
# ggnews2
# 
# ggnews3 <- newsngram3 %>%
#   filter(n> 10) %>%
#   mutate(ng2=reorder(ngram,n)) %>%
#   ggplot(aes(x=n)) +
#   geom_histogram(binwidth=5) +
#   xlab("Occurrences") +
#   ggtitle("3-grams in News data") +
#   coord_flip()
# ggnews3
# 
# 
# # ggtwit3 <- twitngram3 %>%
# #   filter(n> 10) %>%
# #   mutate(ng2=reorder(ngram,n)) %>%
# #   ggplot(aes(ng2,n)) +
# #   geom_col() +
# #   xlab(NULL) +
# #   coord_flip()
# ggtwit3 <- twitngram3 %>%
#   filter(n> 10) %>%
#   mutate(ng2=reorder(ngram,n)) %>%
#   ggplot(aes(x=n)) +
#   geom_histogram(binwidth=2) +
#   xlab(NULL) +
#   coord_flip()
# 
# ggtwit3
 
 # old profanity filtering, only works on word tokens
#  names(profdf) <- "word"         # column names must be the same for joins
# 
# #Unnest tokens, then anti-join
# if (TWITTER) {
#   #Tokenise twitter
#   tidy_twit <- twitdf %>%
#     unnest_tokens(word, text)  #word is the column name
#   # #test
#   # f <- tidy_twit$word=="fuck"
#   # print(c("Fucks in twitter before", sum(f)))
#   
#   # Remove profanity
#   tidy_twit <- tidy_twit %>%
#     anti_join(profdf,by=c("word"))
#   # f <- tidy_twit$word=="cunt"
#   # print("Fucks in twitter after", sum(f))
# }
# if (BLOG) {
#   tidy_blog <- blogdf %>%
# 
#         unnest_tokens(word, text)
#   blogdf <- blogdf %>%
#     anti_join(profdf,by=c("word"))
# }
# 
# if (NEWS){
# tidy_news <- newsdf %>%
#   unnest_tokens(word, text)
# 
# newsdf <- newsdf %>%
#   anti_join(profdf,by=c("word"))
# }

