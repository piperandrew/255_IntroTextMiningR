######### LLCU 255 Introduction to Literary Text Mining ##########
######### by Andrew Piper ####################
######### CC By 4.0 License ##################

#####################################################
##############   Preparing your data    #############
#####################################################

#The first thing you will need to do is install your libraries
#You only need to do this the first time.
#install.packages("tm")
#install.packages("slam")

#Dataset used here:
#txtlab_Novel150_English 
#https://doi.org/10.6084/m9.figshare.17425562.v1 

#load libraries (you need to do this every time)
library("tm")
library("slam")

#Set your working directory
#this is the folder *above* where your texts are located
setwd("~/Data")

########  Reading in Your Data: the TM Library   ###########

#Read in your corpus
#the name in "" after DirSource is the name of your folder where your texts are
#set the language appropriately
corpus1 <- VCorpus(DirSource("txtlab_Novel150_English", encoding = "UTF-8"), readerControl=list(language="English"))

#Inspect your data (see a portion of the actual text)
strwrap(corpus1[[26]])[1:5] #the second number in brackets 1:5 refers to the first five lines


########## Normalizing Your Data 1: Textual Normalization ###########

#make all lowercase
corpus1 <- tm_map(corpus1, content_transformer(tolower))
#remove numbers
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
#remove punctuation
#corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
f<-content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus1 <- tm_map(corpus1, f, "[[:punct:]]")
#strip white space
corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace)) 

#Option: lemmatize your data (not often recommended)
#corpus1.lemma <- tm_map(corpus1, lemmatize_strings)
#corpus1.lemma <- tm_map(corpus1, PlainTextDocument)

#inspect
strwrap(corpus1[[26]])[1:5]

###########  Make a document term matrix ##############

#run the function on your corpus variable
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf))) #(1,Inf) refers to the range of word lengths kept

#if you want to generate a table of ngrams (multiple words in sequence)

#first create function that defines the n in ngrams. 2 = bigrams or 2 words in a row
#BigramTokenizer2 <- function(x)unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

#you can also create a function that captures 1- and 2grams
#BigramTokenizer12 <- function(x)unlist(lapply(ngrams(words(x), 1:2), paste, collapse = " "), use.names = FALSE)

#rerun the DTM function w the bigram function inside
#dtm.bigram <- DocumentTermMatrix(corpus1, control=list(tokenize = BigramTokenizer2, wordLengths=c(1,Inf)))


############  What does a document term matrix tell us? ##################

#a document term matrix is a table that consists of rows = documents and columns = words (or terms).
#the values of the cells are the word counts
#thus we get a table of word frequencies for all documents in your collection

#you can see one by doing the following (we are going to make it really small because right now
#it is too large to view)
#then just click on dtm.example in the upper right pane and it will be visualized here
dtm.example<-as.matrix(corpus1.dtm[ ,which(colnames(corpus1.dtm) %in% stopwords("en"))])


##########  Normalizing Your Data 2: Mathematical Normalization ########

#Because word counts vary considerably from word to word and
#from document to document for many tasks it is important 
#to adjust the raw frequencies above (but not always!)
#the first method does so by adjusting frequencies according to the
#document lengths; the second does so by adjusting word frequencies by
#their document frequency (i.e. in how many documents they appear)
#see the book for further explanation

#Method 1: Scaling
#divide the counts by the total number of words in each document.
dtm.scaled<-corpus1.dtm/row_sums(corpus1.dtm)

#Method 2: Tf-Idf
#tfidf = term frequency * inverse document frequency
#this weights words by how infrequent they are in the corpus (i.e. across all documents)
#the more infrequent they are across documents the higher the word's score and vice versa
dtm.tfidf<-weightTfIdf(corpus1.dtm, normalize = TRUE)

##### Now inspect your DTM #####
dtm.example.scaled<-as.matrix(dtm.example/row_sums(corpus1.dtm))


############################################################################
######## Beginning to understand your data: some initial metrics ###########
############################################################################

#review titles
row.names(corpus1.dtm)

#How many documents do you have?
nrow(corpus1.dtm)

#How many word types?
ncol(corpus1.dtm)

#How many words overall?
sum(corpus1.dtm)

#Generate a list of word counts for each document
row_sums(corpus1.dtm) #if you want to save this as a variable (for example to export as a table, then do: wc<-row_sums(corpus1.dtm))

#Sort in descending order
sort(row_sums(corpus1.dtm), decreasing = T)

#observe top or bottom
sort(row_sums(corpus1.dtm), decreasing = T)[1:10] #change decreasing=F to see bottom

#create a histogram
#a histogram can tell you the distribution of some variable
#in this case we want to know the distribution of book length (or word count)
#how long are our books and what length are most of our books between?
options(scipen=999)
hist(row_sums(corpus1.dtm), xlab="word count", main="Histogram of Word Counts")

#what is the average length of a novel?
#the summary function does something similar without the visualisation
summary(row_sums(corpus1.dtm))

#How can you find out which is the longest novel?
which.max(row_sums(corpus1.dtm))
row_sums(corpus1.dtm)[13]

