######### LLC 255 - Word Embeddings ###########
######### by Andrew Piper ####################
######### CC By 4.0 License ##################

#Datasets:
#20C_Poetry_WordEmbeddings_English. https://doi.org/10.6084/m9.figshare.17435000.v1 
#NYTimes_Novels_WordEmbeddings. https://doi.org/10.6084/m9.figshare.17435054.v1 

setwd("~/Data")

#####  Method 1 ######
library(word2vec)

####  build model ####
#takes as input a single document of all documents from a directory that have been concatenated
#to generate a single doc from a directory of documents see Method 2 below
#type = word2vec model (cbow or skip-gram)
#window = skip length between words -- larger equals larger context of words that can be similar
#dim = dimensions of the word vectors, 50 is default, large models use 300
nyt<-word2vec("NYT.txt", type = "cbow", window=5, threads=3, dim=50, min_count=10)

#save model
write.word2vec(nyt, "NYT.bin", type = "bin")

#### if you already have a model or have downloaded one START HERE ###

#read model
#if the model is .txt file see below
model1<-read.word2vec("20CPOetryAll.bin", normalize = T) #normalize = T!!!
model2<-read.word2vec("NYT_Model.bin", normalize = T) #normalize = T!!!

#create matrix
emb1 <- as.matrix(model1)
emb2 <- as.matrix(model2)

#find nearest words
#top_n = the number of similar words to return
predict(model1, c("human"), type = "nearest", top_n = 10)
predict(model2, c("human"), type = "nearest", top_n = 10)

#find which words differ in two models
m1<-predict(model1, c("human"), type = "nearest", top_n = 10)
m2<-predict(model2, c("human"), type = "nearest", top_n = 10)
#change variable after $ to match the term of interest
m1<-m1$human
m2<-m2$human

#which terms in m1 are NOT in m2
m1[which(!m1$term2 %in% m2$term2),2]

#which terms in m2 are NOT in m1
m2[which(!m2$term1 %in% m2$term2),2]

#find similarity between two terms
word2vec_similarity(emb1["human", ], emb1["nature", ], top_n = 1)
word2vec_similarity(emb2["human", ], emb2["nature", ], top_n = 1)

#create artificial vectors by adding or subtracting word vectors
vector <- emb1["king", ] - emb1["man", ] + emb1["woman", ]
predict(model1, vector, type = "nearest", top_n = 10)

#### If the model is a .txt file START HERE #####

#sample from https://nlp.stanford.edu/projects/glove/
#read model
emb1 <- read.csv("glove.6B.50d.txt", sep=" ", quote="", header=F)
row.names(emb1)<-emb1[,1]
emb1<-as.matrix(emb1[,-1])

#find nearest N words to target word
word2vec_similarity(emb1["human", ], emb1, top_n = 10, type="cosine")

#find nearest N words to target vector
vector <- emb1["king", ] - emb1["man", ] + emb1["woman", ]
word2vec_similarity(vector, emb1, top_n = 10, type="cosine")

