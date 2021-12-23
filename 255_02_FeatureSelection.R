######### LLCU 255 Introduction to Literary Text Mining ##########
######### by Andrew Piper ####################
######### CC By 4.0 License ##################

########### Feature Selection #############

##### Recreate your Document Term Matrix #####
library("tm")
library("slam")
setwd("~/Data")
corpus1 <- VCorpus(DirSource("txtlab_Novel150_English", encoding = "UTF-8"), readerControl=list(language="English"))
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
f<-content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus1 <- tm_map(corpus1, f, "[[:punct:]]")
corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace)) 
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf))) #(1,Inf) refers to the range of word lengths kept
dtm.scaled<-corpus1.dtm/row_sums(corpus1.dtm)
dtm.tfidf<-weightTfIdf(corpus1.dtm, normalize = TRUE)


###############  Visualizing Zipf's Law   ##################
#Zipf's law says the frequency of a word is inversely proportional to its rank
#What this means in practice is that most word tokens in a text consist of very few word types
#Colloquially == very few words account for the bulk of the words in a document
#This fact means we have to deal with different kinds of words differently

#The simplest way to represent Zipf's law is as a 1/f function, meaning the 
#second word appears 1/2 times as often as the first, and the third word appears
#1/3 times as much, etc., so that the nth most frequent word appears 1/n times
#as often as the most frequent word.

#here is an example of what that distribution would look like
v<-1/seq(2,1001,1)
plot(v, xlab="Words", ylab="Frequency")

#to observe the extent to which your data approximates Zipf's law do the following

#sort your words by raw counts in descending order
top.words<-sort(col_sums(corpus1.dtm), decreasing = T)
top.words.tfidf<-sort(col_sums(dtm.tfidf), decreasing = T)

#inspect the top 10
top.words[1:10]

#create a graph of the top 1000 words
options(scipen = 999)
plot(top.words[1:1000], xlab="words", ylab="frequency")
text(1:3, unname(top.words[1:3]), labels=c(names(top.words[1:3])), cex=.7, pos=4)


############# Stopwords #############
#https://en.wikipedia.org/wiki/Stop_word

#stopwords refers to words that appear very frequently 
#but are thought to carry less semantic importance to a document's meaning
#there is no "right" or universal stopword list
#it is usually derived as an arbitrary set of the most frequent words in a dataset or language
#one way to think of it is as the upper part of the elbow of Zipf's law curve

#here is a list of TM library stopwords
stopwords("en") #notice how the punctuation is still there


#### Keeping ONLY Stopwords ####

#subset your DTM by *keeping* only stopwords

#create a variable of your stopwords w punctuation removed
stop<-stopwords("en")
stop<-unlist(strsplit(stop,"[[:punct:]]"))
stop<-unique(stop)

#if you want to add additional words
#stop<-append(stop, c("INSERTWORD", "INSERTWORD", "ETC"))

#keep only stopwords in your DTM
dtm.stop<-as.matrix(dtm.scaled[ ,which(colnames(dtm.scaled) %in% stop)])
dtm.stop.tfidf<-as.matrix(dtm.tfidf[ ,which(colnames(dtm.tfidf) %in% stop)])


########## REMOVING STOPWORDS #############

#keep all words NOT in the stopword list
dtm.nostop<-dtm.scaled[ ,which(!colnames(dtm.scaled) %in% stop)]


########## Removing the long tail ##########
#The second problem is we still have *a lot* of variables (i.e. word types)
#Now we consider methods for reducing the number of words / variables in our DTM
#think of this as dealing with the other end of Zipf's law (i.e. the long tail)

### Approach 1: Top Words ###
#this just fixes a nice round number of words to keep by their frequency in your data
#advantages = straightforward and fixed variable space
#disadvantages = arbitrary cut-off and some words may be very frequent but this frequency
#may be due to small numbers of documents (ex = proper names)

#first create a variable of the top N words (default = 10K)
top.words2<-sort(col_means(dtm.nostop), decreasing = T)[1:10000]

#subset your dtm by these words
dtm.top10k<-dtm.scaled[,which(colnames(dtm.scaled) %in% names(top.words2))]
#view
dtm.top10k<-as.matrix(dtm.top10k)

## Approach 2: Remove Sparse Terms ###
# "Sparse" words are words that appear in very few documents
# we can set thresholds for saying we only keep words that appear in X% of documents in our data
# this is a preferred method to the above because it conditions on words *common* across your data
# think of this as a way to look at the middle part of Zipf's law

#to remove words that appear in X% of documents
#note: the integer here (default = 0.4) is the inverse of what you want to keep
#eg: .4 = you want to keep words that appear in at least 60% of your documents
dtm.sparse<-removeSparseTerms(dtm.nostop, 0.4)

#inspect the top 5 and bottom 5 words of your matrix
sort(col_means(dtm.sparse), decreasing = T)[1:5]
sort(col_means(dtm.sparse), decreasing = T)[(length(colnames(dtm.sparse))-4):length(colnames(dtm.sparse))]

#notice how this includes words like "said" and also chapter headings. ugh.
#so we want to remove these things. Here's how.

#first remove words that are less than 3 letters.
dtm.nostop<-dtm.nostop[, which(!nchar(colnames(dtm.nostop)) < 3)]

#then create a custom list of words
stop.xtra<-c("said", "one", "will")

#append a list of roman numerals
stop.xtra<-append(stop.xtra, tolower(as.roman(1:1000)))

#remove these words
dtm.nostop<-dtm.nostop[, which(!colnames(dtm.nostop) %in% stop.xtra)]

#rerun remove sparse terms
dtm.sparse<-removeSparseTerms(dtm.nostop, 0.4)

#recheck word list
sort(col_means(dtm.sparse), decreasing = T)[1:5]
sort(col_means(dtm.sparse), decreasing = T)[(length(colnames(dtm.sparse))-4):length(colnames(dtm.sparse))]

#you're good to go!

#subset your tfidf table by these words
dtm.sparse.tfidf<-dtm.tfidf[,colnames(dtm.tfidf) %in% colnames(dtm.sparse)]
#view
dtm.sparse.tfidf.m<-as.matrix(dtm.sparse.tfidf)
sort(col_means(dtm.sparse.tfidf), decreasing = T)[1:5]
sort(col_means(dtm.sparse.tfidf), decreasing = T)[(length(colnames(dtm.sparse.tfidf))-4):length(colnames(dtm.sparse.tfidf))]

#Plot the sparse words' frequencies
top.sparse<-sort(col_means(dtm.sparse), decreasing = T)
options(scipen = 999)
plot(top.sparse[1:1000], xlab="words", ylab="frequency")
text(1:3, unname(top.sparse[1:3]), labels=c(names(top.sparse[1:3])), cex=.7, pos=4)
top.sparse[1:200]


#### What are the most similar documents using these feature spaces? ####

#first transform your document term matrix into a 'distance matrix'
library(proxy)
#make document similarity matrix using "cosine similarity"
sim.d<-as.matrix(simil(as.matrix(dtm.sparse), method = "cosine"))
#find most similar items to target text
row.names(sim.d)
sort(sim.d[row.names(sim.d) == "EN_1813_Austen,Jane_PrideandPrejudice_Novel.txt",], decreasing = F)

#make term similarity matrix using "cosine similarity"
sim.t<-as.matrix(simil(t(as.matrix(dtm.sparse)), method = "cosine"))
#find most similar items to target word
sort(sim.t[which(row.names(sim.t) == "heart"),], decreasing = T)[1:100]





