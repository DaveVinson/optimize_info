# ***************************************************************************************
# ***************************************************************************************
# ********** extract network and info measures from Yelp, Inc. Business Reviews *********
# ******* Written by David W. Vinson for the purpose of demonstration *******************
# ******************** PLEASE DO NOT DISTRIBUTE WITHOUT PERMISSION **********************  
# ***************************************************************************************
# ***************************************************************************************

# notes: Depending on your processor/memory combination, the multicore process may not be of benefit for this code
# reasons include the amount of total RAM needed to process each review
# More RAM may be needed than can be provided by a single core at a time.  
# The computer will automatically pull from both as needed, so a lack of RAM (due to core division)
# can substantially slow down your code. 

#read.json may work faster.! 
#load revs 
revs = read.csv('/Users/Dave/Documents/yelp_dataset_mil/yelp_academic_dataset_review.json',sep='\n',quote = "")
colnames(revs) = list('u')  
revs$u = as.character(revs$u)

##### load across revs uni and bigrams ####
#review unigrams
setwd('/Users/Dave/Documents/yelp_dataset_mil/')

across_uni_matrix <- read.table('unis.csv', sep=',', header=FALSE) #READ OUTSIDE LOOP #load across unigram frequencies
uni_full <- as.matrix(across_uni_matrix[, -1]) #convert to matrix
row.names(uni_full) <- across_uni_matrix[, 1] #name the rows the appropriate words

#review bigrams
across_bigram_matrix <- read.table('bigs.csv', sep=',', header=FALSE) #READ OUTSIDE LOOP #load across unigram frequencies
big_rows = as.matrix(paste(across_bigram_matrix$V1,across_bigram_matrix$V2, sep=' ')) # problems with loading bigs #sep by space works.  
big_full <- as.matrix(across_bigram_matrix[,-(1:2)]) #convert to matrix
row.names(big_full) <- big_rows[,1] #name the rows the appropriate words


# #load tips
# tips = read.csv('/Users/Dave/Documents/yelp_dataset_mil/yelp_academic_dataset_tip.json',sep='\n',quote = "")
# colnames(tips) = list('u')  
# tips$u = as.character(tips$u)
# 
# #tip unigrams
# across_tip_uni_matrix <- read.table('/Users/Dave/Documents/yelp_dataset_mil/tip_unis.csv', sep=',', header=FALSE) #READ OUTSIDE LOOP #load across unigram frequencies
# uni_full <- as.matrix(across_tip_uni_matrix[, -1]) #convert to matrix
# row.names(uni_full) <- across_tip_uni_matrix[, 1] #name the rows the appropriate words
# 
# #tip bigrams
# across_bigram_tip_matrix <- read.table('/Users/Dave/Documents/yelp_dataset_mil/tip_bigs.csv', sep=',', header=FALSE) #READ OUTSIDE LOOP #load across unigram frequencies
# big_tip_rows = as.matrix(paste(across_bigram_tip_matrix$V1,across_bigram_tip_matrix$V2, sep=' ')) # problems with loading bigs #sep by space works.  
# big_full <- as.matrix(across_bigram_tip_matrix[,-(1:2)]) #convert to matrix
# row.names(big_full) <- big_tip_rows[,1] #name the rows the appropriate words


#function will need to be changed slightly depending on tips/revs (only the first line)
source('/Users/Dave/Documents/yelp_dataset_mil/info_funct.R') 

#build the cluster
library(pscl)
library(snow)
library(doSNOW)
library(foreach)
library(nws)

c1.tmp=makeCluster(4)
  #makeCluster(2)
registerDoSNOW(c1.tmp)

start.time=Sys.time()
foreach (i = 1:20, .combine='rbind',.packages=c("rjson","tm","RWeka","entropy")) %dopar% {#%dopar% to parallel proc. the work. i = (iterations,processors) otherwise i  = 4:1 (number of iterations)
  info_funct(i, revs)
}

# library(rjson)
# library(tm)
# library(RWeka)
# library(entropy)

clusterApply(c1.tmp, 1:20, info_funct, revs)
end.time=Sys.time()
end.time-start.time
stopCluster(c1.tmp)