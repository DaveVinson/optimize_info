# ***************************************************************************************
# ***************************************************************************************
# ************************ Analyzing Yelp, Inc. Business Reviews ************************ 
# *********** Written by David W. Vinson  for the purpose of demonstration **************
# ******************** PLEASE DO NOT DISTRIBUTE WITHOUT PERMISSION **********************  
# ***************************************************************************************
# *************************************************************************************** 
info_funct = function(i,revs) { 
  
  library(rjson)
  library(tm)
  library(RWeka)
  library(entropy)

  tip = fromJSON(revs$u[i])
  tiptext = tip$text
  #################### info measures ########################
  tiptext = tolower(tip$text)
  ts = Corpus(VectorSource(tiptext))
  # remove stopwords
  ts <- tm_map(ts, removeWords, stopwords("english"))
  # eliminate extra whitespace
  ts <- tm_map(ts, stripWhitespace)
  # eliminate punctuation
  removepunct <- function(x) { return(gsub("[[:punct:]]","",x)) }
  ts <- tm_map(ts, removepunct)
  # eliminate numbers
  removenum <- function(x) { return(gsub("[0-9]","",x)) }
  ts <- tm_map(ts, removenum)
  
  tiptext = sapply(ts, '[', 1) 
  ########################################################
  
  date = as.numeric(as.POSIXct(tip$date))
  len = length(MC_tokenizer(tiptext))
  diversity = length(unique(MC_tokenizer(tiptext)))/length(MC_tokenizer(tiptext))
  
  #build unigrams per reviewer
  tip_df <- data.frame(V1 = tiptext, stringsAsFactors = FALSE)
  tip_corp <- Corpus(DataframeSource(tip_df))
  tip_unigram <- TermDocumentMatrix(tip_corp)
  #inspect(tdm_unigram)
  tip_uni_as_matrix = data.matrix(tip_unigram, rownames.force = NA)
  
  #build bigrams per reviewer
  BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
  tip_bigrams <- TermDocumentMatrix(tip_corp, control = list(tokenize = BigramTokenizer))
  #inspect(tdm_bigrams)
  tip_bi_as_matrix = data.matrix(tip_bigrams, rownames.force = NA)
  
  # across unigrams
  rows.to.keep<-which(rownames(uni_full) %in% rownames(tip_uni_as_matrix)) # selects rows to keep (matched)
  across_uni_mat_net = as.matrix(uni_full[rows.to.keep,]) #extracts matrix of matched rows
  uni = mean(-log2(across_uni_mat_net/sum(uni_full)))
  
  #across bigrams
  rows.to.keep.bigs<-which(rownames(big_full) %in% rownames(tip_bi_as_matrix)) # selects rows to keep (matched)
  across_big_mat_net = as.matrix(big_full[rows.to.keep.bigs,]) #extracts matrix of matched rows
  big = mean(-log2(across_big_mat_net/sum(big_full)))
  
  #chan caps UID measures
  unichan =  sd(-1*log2(across_uni_mat_net/sum(uni_full)))
  bigchan =  sd(-1*log2(across_big_mat_net/sum(big_full)))
  
  
 results <<- data.frame(tip$business_id,tip$user_id,
              tip$votes$useful,tip$votes$funny,tip$votes$cool,
              date,len,diversity,uni,big,
              unichan,bigchan)
}

