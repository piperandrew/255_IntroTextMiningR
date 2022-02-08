######### LLCU 255 Introduction to Literary Text Mining ##########
######### by Andrew Piper ####################
######### CC By 4.0 License ##################

################################################################################
#############   CORPUS COMPARISON - Distinctive features      ##################
################################################################################

#In this script we will learn how to ingest two separate corpora and compare them

#Part2. We will then learn methods for testing *every* feature of interest (usually words)
#And ranking words by their "distinctiveness" -- how much more frequent is Feature X in Corpus A than Corpus B?
#Rather than condition on one feature, we will look at all available features
#Keep in mind this is akin to running multiple hypothesis tests so we need to do some sort of
#correction because we will inevitably find some variables that differ accidentally
#(if you run enough tests one of them will accidentally return a positive result)
#for more reading see:
#https://en.wikipedia.org/wiki/Bonferroni_correction

library("tm")
library("slam")
setwd("~/Data")

#####For this exercise we are going to compare Sherlock Holmes stories to canonical "literary" short stories

### Datasets used here:
#Sherlock Holmes Collection. https://doi.org/10.6084/m9.figshare.17425568.v1 
#Short Stories Collection. https://doi.org/10.6084/m9.figshare.17425571.v1 

##### Ingest both corpora #####

#### Corpus A ####
corpus1 <- VCorpus(DirSource("SherlockHolmes", encoding = "UTF-8"), readerControl=list(language="English"))
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
f<-content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus1 <- tm_map(corpus1, f, "[[:punct:]]")
corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace)) 
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf))) #(1,Inf) refers to the range of word lengths kept

#### Corpus B ####
corpus2 <- VCorpus(DirSource("ShortStories_English", encoding = "UTF-8"), readerControl=list(language="English"))
corpus2 <- tm_map(corpus2, content_transformer(tolower))
corpus2 <- tm_map(corpus2, content_transformer(removeNumbers))
f<-content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus2 <- tm_map(corpus2, f, "[[:punct:]]")
corpus2 <- tm_map(corpus2, content_transformer(stripWhitespace)) 
corpus2.dtm<-DocumentTermMatrix(corpus2, control=list(wordLengths=c(1,Inf))) #(1,Inf) refers to the range of word lengths kept

############   DISTINCTIVE FEATURES - ALL   ##############
#Here we are going to discover "distinctive features" of each of our corpora
#For our purposes we are going to use what is called a log-likelihood test (called G-squared or "Dunning's" test)
#The original article is here: https://aclanthology.org/J93-1003.pdf
#Blog post: http://tdunning.blogspot.com/2008/03/surprise-and-coincidence.html

#The way we will model this is to ask how much more likely is WORD X to appear in CORPUS A than B?
#The way calculate this is by making what are known as "contingency tables".
#This allows us to compare the rate of a given word relative to the overall number of words in that corpus
#They have the following structure:

#         Corpus A    Corpus B
#Word       x           y
#NotWord    z           w

###### EXAMPLE: #######

#So what we are comparing is the rate at which a word occurs in Corpus A v. Corpus B given all the words in Corpus A and B. Simple Example:
#Let's say that one group of texts (A) uses the word "sandwich" 100 times.
#The other group (B) uses it 200 times.
#At first glance this looks like B uses the word more.
#But it turns out that B uses a lot more words overall: 
#Let's say B has 10,000 words total, while A only uses 1,000 words.
#So sandwich occurs 100 times for every 1,000 words in A but 200 times for every 10,000 words in B.
#Thus the likelihood that group A uses sandwich is much higher (1/10 v. 2/100).
#In this example x = 100, z = 900, y = 200, w = 9,800.


#####  Subset Corpora by intersecting words #####

#***NOTE: we use raw counts not scaled counts for this measure, i.e. corpus1.dtm

#To run this we first need to subset our two corpora by words they have in common
#(If I use a word and you never do then the likelihood of me using it is infinity more than you...)

#First reduce our corpora to non-sparse words
#Note: we have not removed stopwords. Generally I recommend keeping them in for these tests
#Remember: adjust the integer below based on the size of your data and length of documents
#Because we are using *short* stories we require words to be only in at least 20% of documents
#thus we set it at 0.8. Change this depending on the nature of your documents and how many words
#you want to test (the more words the longer it takes...)
corpus1.sparse<-removeSparseTerms(corpus1.dtm, 0.8)
corpus2.sparse<-removeSparseTerms(corpus2.dtm, 0.8)

#keep only words in both sets
keep<-intersect(colnames(corpus1.sparse), colnames(corpus2.sparse))

#subset 1 and 2 by these words
#this is your DTM1 and DTM2
dtm1<-corpus1.dtm[,colnames(corpus1.dtm) %in% keep]
dtm2<-corpus2.dtm[,colnames(corpus2.dtm) %in% keep]

#first get individual word counts for each corpus
word1<-col_sums(dtm1)
word2<-col_sums(dtm2)
#then get total counts for each corpus
all1<-sum(word1)
all2<-sum(word2)

#entropy function
H = function(k) {N = sum(k); return(sum(k/N*log(k/N+(k==0))))}

#store empty results in a table
results <- data.frame(word = colnames(dtm1), 
                      group1=word1,
                      group2=word2,
                      G2 = 0,
                      fisher.OR = 0,
                      fisher.p = 0)
#create loop to go through every word
for (j in 1:ncol(dtm1)){
  print(j)
  #create contingency table for each word
  cont.table<-data.frame(c(word1[j], all1-word1[j]), c(word2[j], all2-word2[j]))
  #get straight odds ratio
  fish<-fisher.test(cont.table)
  #Dunning's
  LLR = 2*sum(cont.table)*(H(cont.table)-H(rowSums(cont.table))-H(colSums(cont.table)))
  results$G2[j] = LLR
  results$fisher.OR[j] = fish$estimate
  results$fisher.p[j] = fish$p.value
}

#keep only words that pass a significance threshold
#because we just ran as many tests as there are words we need to do a Bonferroni Correction
#which divides our cut-off of significance by the total number of tests we ran

#establish correction
#the first value is your threshold and the denominator is the number of words tested
cut<-0.05/ncol(dtm1)

#keep only words whose p-value is below the cut
results<-results[results$fisher.p < cut,]

#create a new column that turns the G2 score negative if it applies to group2
#this gives you a way of sorting for G2 by group or just strongest words in general
#negative values indicate distinctive words for group2, positive for group 1
results$G2_Sorted<-vector(mode="numeric", length=nrow(results))
for (i in 1:nrow(results)){
  if (results$fisher.OR[i] < 1){
    results$G2_Sorted[i]<--results$G2[i]
  } else {
    results$G2_Sorted[i]<-results$G2[i]
  }
}
results<-results[order(-results$G2_Sorted),]

#save your results
write.csv(results, file="MyResults.csv", row.names = F)

