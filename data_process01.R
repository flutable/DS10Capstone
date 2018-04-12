library(tidyverse)
library(tidytext)
library(jsonlite)
library(data.table)
library(quanteda)
library(sqldf)
library(glue) #collapse()
library(stringi)
library(doParallel)

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

#Clear environment
rm(list=ls())

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

source("utility.R")


# Get some profanity ----
if (PREPROCESSING) {
  preproctime = Sys.time()
  # downloadFile( profanityURL, profanityJSON)
  # profanity <- fromJSON(profanityJSON)          # It's in JSON format, so strip out all the tags
  # # remove asterisk from profanity (we are not yet doing stemming. \ escapes, \* resolves to asterisk)
  # profanity <- gsub("\\*", "", profanity, ignore.case=TRUE, perl=TRUE)
  # profanity <- unique(profanity)                # remove duplicates
  # 
  # # Save text-format profanity for later
  # write(profanity, file=profanityFile)
  # profdf <- data_frame(profanity)
  # } else {
  #Get existing data
  if (!exists("profdf")) {
    profdf <- fread(profanityFile, sep="\n", header=FALSE)
    names(profdf) <- "term"
  }
  profanitypattern <- paste(profdf$term, collapse="|")
  } #profanity preprocessing

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
      twitData <- as.data.table(twitData)
      names(twitData) <- "text"
      setindex(twitData,text)
      profanityIndex <- grep(profanitypattern, twitData$text)
      # select non-profane lines
      twitData <- twitData[!profanityIndex, ]  #Data table, so in-place
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
  
    
    # Merge all data to create complete corpus ----
    L <- list(twitdf, blogdf, newsdf, blogData, newsData, twitData)
    #rm(twitdf); rm(blogdf); rm(newsdf)
    corpus <- rbindlist(L)
    rm(twitdf); rm(blogdf); rm(newsdf); rm(twitData); rm(newsData); rm(twitData)
    gc()
    #remove profanity
    corpus <- as.data.table(corpus)
    names(corpus) <- "text"
    setindex(corpus,text)
    profanityIndex <- grep(profanitypattern, corpus$text)
    # select non-profane lines
    corpus <- corpus[!profanityIndex, ]  #Data table, so in-place
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
    rm(corpus); rm(smallcorpus); rm(smalltestcorpus); rm(smalltrainingcorpus)
    rm(corpustest); rm(corpustraining)
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
  ## UNK TESTING
    corpus <- fread(paste0(cleandataLoc , "corpustraining.txt"), sep=",", header=TRUE)
  
  findNgramtime <- Sys.time()
  
  ngram1 <- corpus %>%
      unnest_tokens(ngram, text, token="ngrams", n= 1) %>%
      count(ngram, sort=TRUE) 
  print(paste0("Find ngram1: ", difftime(Sys.time(), findNgramtime, units = 'sec'))) #292 sec 
  ngram1 <- as.data.table(ngram1) 
  # Handling out-of-vocabulary items
  # Replace all tokens with count == 1 by token unk, then recompute all ngrams.
  #  1. Find n==1 tokens
  unkwords <- ngram1[n==1, c("ngram") ]
  #unkwords <- ngram1[n==1, ]

   fwrite(x=ngram1, file=ngram1File)
  #  2. modify unkwords so that only tokens with a space on either side are selected
  unkwords$ngram <- paste0(SPACE, unkwords$ngram, SPACE)  
  
  # 3. modify all unkwords: <space>unkword<space>; shorter unkwords embedded in other words are not affected
  tempreplacement <- paste0(SPACE, "unk", SPACE)
  unkwords <- data.frame(unkwords[ , replacement:= tempreplacement])
  
  
  # parallel processing of unknown words. Divide corpus into partitions
  partitions = detectCores() - 1 # leave 1 for system.
  #partition the corpus into 
  partitionSize = round(nrow(corpus)/partitions)
  
  cl <- makeCluster(partitions)
  registerDoParallel(cl)
  # 4.Now replace each occurrence of all elements of unkwords in original corpus  #6 ytimes slower than stri_replace_all_fixed
  #   Approximately 25 items per second using 1 core on an i6700. 
  #   Total lines=4,269,678 => 47 hours on 1 core, approx 8 hours on 7 cores.
  
  RemUnkWords <- function(partitionNum, corp) {
    print(paste0("Partition: ", partitionNum))
    partition_start  <- partitionNum * partitionSize-(partitionSize) + 1
    partition_finish <- partitionNum * partitionSize 
    corpPart <- corpus[partition_start:partition_finish, ]
    corpPart <- stringi::stri_replace_all_fixed(str=corpPart$text, pattern=unkwords$ngram, replacement=unkwords$replacement,
                                     vectorize_all=FALSE )
  }
  unktime <- Sys.time()
  x <- foreach(i = 1:partitions) %dopar% RemUnkWords(i, corpus)
  print(paste0("Unkwords: ", difftime(Sys.time(), unktime, units = 'sec'))) #292 sec
  stopCluster(cl)
  
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

# Setup with text ngrams
  # setDT(ngram1); setkey(ngram1, w1)
  # setDT(ngram2); setkey(ngram2, w2, w1)
  # setDT(ngram3); setkey(ngram3, w3, w2, w1)
  # setDT(ngram4); setkey(ngram4, w4, w3, w2, w1)
  # setDT(ngram5); setkey(ngram5, w5, w4, w3, w2, w1)
  ngram1File <- paste0(cleandataLoc, "ngram1both.ng")
  ngram2File <- paste0(cleandataLoc, "ngram2both.ng")
  ngram3File <- paste0(cleandataLoc, "ngram3both.ng")
  ngram4File <- paste0(cleandataLoc, "ngram4both.ng")
  ngram5File <- paste0(cleandataLoc, "ngram5both.ng")
  
    fwrite(x=ngram1, file=ngram1File)
  fwrite(x=ngram2, file=ngram2File)
  fwrite(x=ngram3, file=ngram3File)
  fwrite(x=ngram4, file=ngram4File)
  fwrite(x=ngram5, file=ngram5File)
 
  #setup with integer and text ngrams
  ngram1 <- fread(ngram1File, sep=",", header=TRUE)
  ngram2 <- fread(ngram2File, sep=",", header=TRUE) #34s to read
  ngram3 <- fread(ngram3File, sep=",", header=TRUE) #2 mins to read
  ngram4 <- fread(ngram4File, sep=",", header=TRUE)
  ngram5 <- fread(ngram5File, sep=",", header=TRUE)
  setDT(ngram1);
  setDT(ngram2); 
  setDT(ngram3);
  setDT(ngram4); 
  setDT(ngram5); 
  #ngram1[, i1 := 1:nrow(ngram1)]
  setkey(ngram1, w1)
  setkey(ngram2, w1, w2)
  setkey(ngram3, w1, w2, w3)
  setkey(ngram4, w1, w2, w3, w4)
  setkey(ngram5, w1, w2, w3, w4, w5)
  
  MapWords(ngram2)
  MapWords(ngram3)
  MapWords(ngram4)
  MapWords(ngram5)
  
  setkey(ngram1, w1, i1)
  setkey(ngram2, w1, w2, i1, i2)
  setkey(ngram3, w1, w2, w3, i1, i2, i3)
  setkey(ngram4, w1, w2, w3, w4, i1, i2, i3, i4)
  setkey(ngram5, w1, w2, w3, w4, w5, i1, i2, i3, i4, i5)
  #just key on integers
  setkey(ngram1, i1)
  setkey(ngram2, i1, i2)
  setkey(ngram3, i1, i2, i3)
  setkey(ngram4, i1, i2, i3, i4)
  setkey(ngram5, i1, i2, i3, i4, i5)
 

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
#     ngram5 <- fread(ngram5File, sep=",", header=TRUE)
  
 

#  ngram1[index==c1[1]$w]
#     w1   n           pr       lpr  index
# 1: nov 230 3.515237e-05 -10.25582 101734
# > ngram1[index==c1[2]$w]
#              w1   n           pr      lpr index
# 1: conversation 495 7.565401e-05 -9.48934 30090
# > ngram1[index==c1[3]$w]
#     w1   n           pr       lpr index
# 1: dec 260 3.973746e-05 -10.13322 35262
# > ngram1[index==c1[4]$w]
#         w1   n           pr       lpr  index
# 1: obvious 313 4.783779e-05 -9.947695 102789
# > ngram1[index==c1[5]$w]
#        w1   n          pr       lpr  index
# 1: raised 492 7.51955e-05 -9.495419 118447


 predMLEi <- function(usertext) {
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
    n1 <- MapWordToInt(n1)
    n2 <- MapWordToInt(word(n2,1))
    n3 <- MapWordToInt(word(n3,1))
    n4 <- MapWordToInt(word(n4,1))
    n5 <- MapWordToInt(word(n5,1))
    print(c(MapIntToWord(n1), MapIntToWord(n2), MapIntToWord(n3), MapIntToWord(n4), MapIntToWord(n5)))
    
     timeDtLookup <- Sys.time()
    
    #Data.table lookups ----
    #Have we seen this ngram before? Lookup ngram and extract it.
    #MLE predicted 2gram
    timeDtLookup <- Sys.time()
    #p2_1g   <- ngram2[i2==n1 ][order(-pr)][1,]
    p2_1g   <- ngram2[i1==n1 ][order(-pr)][1,]
    p2_1    <- MapIntToWord(p2_1g$i2)  #predicted word is the next token
    print(paste0("p2_1g: ", MapIntToString(c(p2_1g$i1, p2_1g$i2)), " Prediction: ", p2_1))
    #print()
    #MLE predicted 3gram
#    p3_2g   <- p3_2g <- ngram3[i3==n2 & i2==n1][order(-pr)][1,]
    p3_2g   <- ngram3[i1==n2 & i2==n1 ][order(-pr)][1,]

    p3_2    <- MapIntToWord(p3_2g$i3)
    #p3_2len <- length(p3_2)
    print(paste0("p3_2g: ", MapIntToString(c(p3_2g$i1, p3_2g$i2, p3_2g$i3)), " Prediction: ", p3_2 ))
    
    #MLE predicted 4gram
#    p4_3g   <- ngram4[i4==n3 & i3==n2 & i2==n1 ][order(-pr)][1,]
        p4_3g   <- ngram4[i1==n3 & i2==n2 & i3==n1 ][order(-pr)][1,]

    p4_3    <- MapIntToWord(p4_3g$i4)
    
    print(paste0("p4_3g: ", MapIntToString(c(p4_3g$i1, p4_3g$i2, p4_3g$i3, p4_3g$i4)), " Prediction: ", p4_3))

    #MLE predicted 5gram 
    # i and w order is as the user types"at the end of the"
    # calculated ngrams                  n5  n4 n3  n2  n1    
    # this function (and predMLE) finds the ngram in word order "the end of the"
    #so word i1 = the/n4, i2 = end/n3, i3 = of/n2 i4 = the/n1
    p5_4g   <- ngram5[i1==n4 & i2==n3 & i3==n2 & i4==n1][order(-pr)][1,]
    p5_4    <- MapIntToWord(p5_4g$i5)
    p5_4len <- length( p5_4g )
    print(paste0("p5_4g: ", MapIntToString(c(p5_4g$i1,p5_4g$i2,p5_4g$i3,p5_4g$i4,p5_4g$i5)), " Prediction: ", p5_4))

    print(paste0("DataTable lookup time: ", difftime(Sys.time(), timeDtLookup, units = 'sec')))   
    # Pure MLE method
    if       ( !is.na(p5_4g$i5)) {prediction <- MapIntToWord(p5_4g$i5)}  #need to check na, since ngram may not exist.
     else if ( !is.na(p4_3g$i4)) {prediction <- MapIntToWord(p4_3g$i4)}  #   smoothing will fix this
     else if ( !is.na(p3_2g$i3)) {prediction <- MapIntToWord(p3_2g$i3)}
     else if ( !is.na(p2_1g$i2)) {prediction <- MapIntToWord(p2_1g$i2)}
    
    return( prediction)
    
    }# function pred
 
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
    print(c(n1, n2, n3, n4, n5))

     timeDtLookup <- Sys.time()

    #Data.table lookups ----
    #Have we seen this ngram before? Lookup ngram and extract it.
    #MLE predicted 2gram
    timeDtLookup <- Sys.time()
    p2_1g   <- ngram2[.(n1) ][order(-pr)][1,]
    p2_1    <- p2_1g$w2
    print(paste0("p2_1g: ", paste0(c(p2_1g$w1, p2_1g$w2), collapse= " ")," Prediction: ", p2_1))

    #MLE predicted 3gram
    p3_2g   <- ngram3[.(n2, n1)][order(-pr)][1,]
    p3_2    <- p3_2g$w3
    p3_2len <- length(p3_2)
    print(paste0("p3_2g: ", paste0(c(p3_2g$w1, p3_2g$w2, p3_2g$w3), collapse=" "), " Prediction: ", p3_2 ))

    #MLE predicted 4gram
    p4_3g   <- ngram4[.(n3, n2, n1) ][order(-pr)][1,]
    p4_3    <- p4_3g$w4
    print(paste0("p4_3g: ", paste0(c(p4_3g$w1,p4_3g$w2,p4_3g$w3,p4_3g$w4), collapse= " "), " Prediction: ", p4_3))

    #MLE predicted 5gram
    p5_4g   <- ngram5[.(n4, n3, n2, n1)][order(-pr)][1,]
    p5_4    <- p5_4g$w5
    p5_4len <- length( p5_4g )
    print(paste0("p5_4g: ", paste0(c(p5_4g$w1,p5_4g$w2,p5_4g$w3,p5_4g$w4,p5_4g$w5),collapse=" "), " Prediction: ", p5_4))

    print(paste0("DataTable lookup time: ", difftime(Sys.time(), timeDtLookup, units = 'sec')))
    # Pure MLE method
    if       ( !is.na(p5_4g$w5)) {prediction <- p5_4g$w5}  #need to check na, since ngram may not exist.
     else if ( !is.na(p4_3g$w4)) {prediction <- p4_3g$w4}  #   smoothing will fix this
     else if ( !is.na(p3_2g$w3)) {prediction <- p3_2g$w3}
     else if ( !is.na(p2_1g$w2)) {prediction <- p2_1g$w2}

    return( prediction)
}