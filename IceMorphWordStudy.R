# Take in a form; return the parsing from the icemorph_corpus and frequency in each text.
# textlist points to a directory of .txt files, each of which is a text in the corpus
# Problem: texts themselves aren't normalized? 
# Make a list of dataframes containing the frequency of each word type in each text
# Is it possible to apply the normalization function from the Haskell parser to the texts? If so, 
# NB that lists in R are really more like hash tables, in having a TAG:value structure
### Returns a list (i.e., a hash table) where each tag is a word type in the corpus, and each value is a dataframe containing the name, total number of tokens, and number of tokens of that word type for each text
### Returns a list (i.e., a hash table) where each tag is the name of a text in the corpus, and each value is a dataframe containing basic frequency statistics

### Actually, what you need to do is get the length of each text, and use those numbers to get the frequency of a lemma

MakeIceMorphWordStudy <- (directory1, directory2){
  
  setwd(directory1) # directory1 is where the current .json format parsed corpus lives
  library(rjson) # Load .json processing functions
  parsed.corpus = paste("icemorph_corpus_", as.character(Sys.Date()), ".json", sep="") # set filename for the day's parsing run
  icemorph <- fromJSON(file = parsed.corpus) # read in the parsed corpus
  form.types.vector <- unique(sapply(icemorph, "[[", "form_norm")) # reduce the forms to types
  text.directory = directory2 # directory2 is where all the .txt files that constitute the corpus are
  global.lemma.freq <- as.data.frame(sort(table(unlist(sapply(icemorph, "[[", "lemma"))), decreasing=T))
  names(global.lemma.freq) <- "Token Frequency"
  global.pos.freq <- as.data.frame(sort(table(unlist(sapply(icemorph, "[[", "pos"))), decreasing=T))
  names(global.pos.freq) <- "Token Frequency"
  global.declension.freq <- as.data.frame(sort(table(unlist(sapply(icemorph, "[[", "declension"))), decreasing=T))
  names(global.declension.freq) <- "Token Frequency"
  list.corpus.statistics <- list(global.lemma.freq, global.pos.freq, global.declension.freq)
  names(list.corpus.statistics) <- c("Lemma.Frequency", "POS.Frequency", "Declension.Frequency")
  
  list.word.freq.per.text <- list() ## This list will contain a number of dataframes equal to the number of word types, where each dataframe contains the frequency of that word per text
  list.of.text.bounds <- list() ## A list that will contain the starting and ending token, and number of tokens for each text.
  list.of.texts <- list() # A list that will contain vectors of all the texts themselves
  
  text.start <- 1 # initialize the counter
  for(text in text.directory){
    text.name <- unlist(strsplit(text, split=".", fixed=TRUE))[1] ## assuming that all text names are of the form NAME.txt, breaks that file name at "." and takes the NAME part
    text <- scan(file=text, what="char", sep="\n") # read in .txt file
    words.list <- strsplit(text, "\\W+") # break up all words in .txt file using any whitespace chars; returns a list
    words.vector <- unlist(words.list) # converts list of words into a vector
    words.vector <- words.vector[which(words.vector!="")] # eliminates any potentially empty items in the list
    
    text.tokens <- length(words.vector) ## get number of tokens in the text
    text.end <- text.start + text.tokens - 1 ## set the value of the final token in the text
    temp.list <- list(c(text.start, text.end, text.tokens)) # put the value of the first and last token, and total number of tokens, in a vector into a list
    names(temp.list) <- text.name # set the name of that item in the list as the name of the text being looked at
    list.of.texts <- append(list.of.texts, temp.list) # add that text to the master list
    text.start <- text.start + text.tokens # Set new starting point for following text. 
  }
  
  for(word in form.types.vector){
    wordstudy <- data.frame()
    for(text in list.of.texts){
      text.name = names(text)
      text.tokens <- list.of.texts$text.name[3] # retrieve total number of tokens in text
      word.freq.in.text <- length(which(sapply(icemorph[list.of.texts$text.name[1]:list.of.texts$text.name[2]], "[[", "form_norm") == word))
      lemma.value <- icemorph[which(sapply(icemorph, "[[", "form_norm") == word)][[1]]$lemma
      lemma.freq.in.text <- length(which(sapply(icemorph[list.of.texts$text.name[1]:list.of.texts$text.name[2]], "[[", "lemma") == lemma.value))
      temp <- data.frame("Text"=text.name, "Words in Text"=text.tokens, "Word Frequency"=word.freq.in.text, "Lemma Frequency"=lemma.freq.in.text)
      wordstudy <- rbind(wordstudy, temp)
    }
    temp.list <- list(wordstudy)
    names(temp.list) <- word
    list.word.freq.per.text <- append(list.word.freq.per.text, temp.list)
  }
  
  text.statistics <- list() # A list to contain two dataframes for each text: one with global statistics on that text, and one with the frequency and relative frequency of each item in that text
  
  for(text in list.of.texts){
    text.name <- names(text)
    words.freq.table <- as.data.frame(sort(table(unlist(sapply(icemorph[list.of.texts$text.name[1], list.of.texts$text.name[2]], "[[", "form_norm"))), decreasing=T))
    text.tokens <- list.of.texts$text.name[3]
    text.types <- length(rownames(words.freq.table))
    vocabulary.density <- text.tokens / text.types
    word.hapax.legomena <- length(which(words.freq.table == 1))
    lemma.hapax.legomena <- unique(sapply(icemorph[list.of.texts$text.name[1], list.of.texts$text.name[2]], "[[", "lemma"))
    
    Global.Stats <- data.frame("Total Word Count" = text.tokens, "Unique Word Count" = text.types, "Vocabulary Density" = vocabulary.density, "Words Occurring Only Once" = word.hapax.legomena, "Lemmas Occurring Only Once" = lemma.hapax.legomena)
    words.freq.rel.table <- data.frame(words.freq.table, "Relative Frequency" = words.freq.table[1] / text.tokens)
    names(words.freq.rel.table) <- c("Frequency", "Relative Frequency")
    temp.list <- list(list(Global.Stats, words.freq.rel.table))
    names(temp.list) <- text.name
    text.statistics <- append(text.statistics, temp.list)
  }
  
  wordstudy.list <- list(list.corpus.statistics, list.word.freq.per.text, text.statistics)
  
  return(wordstudy.list)
  
}

############################### Preliminary Junk and Bigram Log Likelihood Ratio Test Below

# ΙceMorphWordStudy <- function(form, textlist){ 
#   
#   
#   
#   for(text in textlist){
#     text <- scan(file=textfile, what="char", sep="\n") # read in .txt file
#     words.list <- strsplit(text, "\\W+") # break up all words in .txt file using any whitespace chars; returns a list
#     words.vector <- unlist(words.list) # converts list of words into a vector
#     words.vector <- words.vector[which(words.vector!="")] # eliminates any potentially empty items in the list
#     words.vector <- tolower(words.vector) # normalize all words to lowercase (I'm assuming that the .txt files have capitalization)
#     text.tokens <- length(words.vector) # N= total number of tokens in text
#     number.form <- length(which(words.vector == form))
#     text.and.form.freq <- data.frame("Words in Corpus" = text.tokens, "Form Frequency" = number.form)
#   }
#   
# }
#
# IceMorphTextStatistics <- function(textfile){
#   text <- scan(file=textfile, what="char", sep="\n") # read in .txt file
#   words.list <- strsplit(text, "\\W+") # break up all words in .txt file using any whitespace chars; returns a list
#   words.vector <- unlist(words.list) # converts list of words into a vector
#   words.vector <- words.vector[which(words.vector!="")] # eliminates any potentially empty items in the list
#   words.vector <- tolower(words.vector) # normalize all words to lowercase (I'm assuming that the .txt files have capitalization)
#   Frequency <- sort(table(words.vector), decreasing=T) # make a frequency table of words in the text
#   
#   words.freq.table <- data.frame(Frequency)
#   text.tokens <- length(words.vector) # N= total number of tokens in text
#   text.types <- length(rownames(words.freq.table))
#   vocabulary.density <- text.tokens / text.types
#   hapax.legomena <- length(which(words.freq.table == 1))
#   
#   Global.Stats <- data.frame("Total Word Count" = text.tokens, "Unique Word Count" = text.types, "Vocabulary Density" = vocabulary.density, "Words Occurring Only Once" = hapax.legomena)
#   words.freq.rel.table <- data.frame(words.freq.table, "Relative Frequency" = words.freq.table[1] / text.tokens)
#   names(words.freq.rel.table) <- c("Frequency", "Relative Frequency")
#   
#   bigram.vector<-c()
#   window<-1
#   for(word in words.vector){
#     current.bigram <- paste(words.vector[window], words.vector[window+1], sep=" ")
#     if(current.bigram %in% bigram.vector == FALSE) {
#       bigram.vector<-append(current.bigram, bigram.vector)
#     }
#     window<-window+1
#   }
#   
#   bigram.freq.list<-sort(table(bigram.vector), decreasing=T)
#   
#   
#   
#   
#   likelihoodRatio <- function(c1, c2, c12, N){
#     p = c2 / N
#     p1 = c12 / c1
#     p2 = (c2 - c12) / (N-c1)
#     ## Computing the number of combinations. The number of Combinations of n objects 
#     ##taken r at a time is 
#     ##nCr = n(n - 1)(n - 2) ... (n - r + 1)/r! = n! / r!(n - r)! = nPr / r!
#     ##comb = factorial(n) / factorial(r)*factorial((n-r))
#     ## binomialDist = comb * p^r * (1-n)^(n-r)
#     binomialDist1 = dbinom(c12, c1, p)
#     binomialDist2 = dbinom(c12, c1, p1)
#     binomialDist3 = dbinom((c2 - c12), (N - c1), p)
#     binomialDist4 = dbinom((c2 - c12), (N - c1), p2)
#     H1 = binomialDist1*binomialDist3
#     H2 = binomialDist2*binomialDist4
#     likelihood = -2 * log(H1 / H2)
#     return(likelihood)
#   }
#   
#   likelihoodRatioTable <- function(bigramvector){ # computes the likelihood co-occurrence ratio for each bigram type in the text. Perhaps ideally computed just on the entire corpus?
#     
#     bigram.freq.list<-sort(table(bigramvector), decreasing=T)
#     
#     Neg2logλ<-c()
#     c1<-c()
#     c2<-c()
#     c1c2<-c()
#     w1<-c()
#     w2<-c()
#     
#     bigram.types <- unique(bigramvector)
#     
#     for(bigram in bigram.types){
#       separated<-strsplit(bigram, "\\W+")
#       separated<-unlist(separated)
#       word1.freq<-words.freq.table[separated[1]]
#       word2.freq<-words.freq.table[separated[2]]
#       word1word2.freq<-bigram.freq.list[bigram]
#       w1<-append(separated[1], w1)
#       w2<-append(separated[2], w2)
#       c1<-append(word1.freq, c1)
#       c2<-append(word2.freq, c2)
#       c1c2<-append(word1word2.freq, c1c2)
#       Neg2logλ<-append(likelihoodRatio(word1.freq, word2.freq, word1word2.freq, N), Neg2logλ)
#     }
#     return(data.frame(Neg2logλ, c1, c2, c1c2, w1, w2))
#   }
#   
#   likelihoodRatioFromText <- likelihoodRatioTable(bigram.vector)
#   
#   
# }


