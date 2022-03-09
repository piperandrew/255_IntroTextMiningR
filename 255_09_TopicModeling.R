######### LLCU 255 Introduction to Literary Text Mining ##########
######### by Andrew Piper ####################
######### CC By 4.0 License ##################

############################################
###########   Topic Modeling   #############
############################################

#the following allows you to run a topic model on a set of documents
#topic models allow you to understand the relationship between words and topics and topics and documents
#it can be a useful tool for exploring the topic distribution of ideas in your data
#or for identifying clusters of documents that focus on particular topics
#or for understanding the kinds of topics and their semantic identity that are present in your documents
#at the end you will run a distinctive topics test similar to your feature comparison code

#you can bypass Steps 1 & 2 by loading the prebuilt topic modeling workspace "txtlab_Novel150_English_TopicModel_k50.RData"
#you do this by going to Session->Load Workspace and choosing the above file.
#then jump to section "Step 4: Explore your model" (ca. line 91)

library("tm")
library("proxy")
library("stats")
library("topicmodels")
library("slam")

###### Step 1: Create DTM ######

setwd("")
corpus1 <- VCorpus(DirSource("NYT_FanFic_Queer_Combined", encoding = "UTF-8"), readerControl=list(language="English"))
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
f<-content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus1 <- tm_map(corpus1, f, "[[:punct:]]")
corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace))
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf))) #(1,Inf) refers to the range of word lengths kept
dtm1.scaled<-corpus1.dtm/row_sums(corpus1.dtm)

#because there are an unknown number of stop and stop-like words (such as proper names in Fiction)
#we recommend that you only keep words based on a custom dictionary that has been manually reviewed
#i.e. all proper names, stopwords, geographic locations have been removed.
#remember this is the opposite of what we normally do which is remove words we *don't want*. Here we 
#keep words we do want.

#ingest dictionary
keep<-read.csv("NYT_FanFic_Queer_Dic.csv", header=F, stringsAsFactors = F)

#subset your dtm by the dictionary
dtm<-corpus1.dtm[,which(colnames(corpus1.dtm) %in% keep$V1)]

#remove rows with all 0 values (this is caused by only keeping non sparse words)
row_zero<-row_sums(dtm)
length(which(row_zero == 0))

# if length > 0 run these lines
row_sub<-as.vector(which(row_zero == 0))
corpus2<-dtm[-row_sub,]
corpus2<-as.matrix(corpus2)

#if length = 0 then run this line
#corpus2<-as.matrix(dtm)


########### Step 2: Run Model ############

#1. define number of topics
#this is an arbitrary number that aligns with your theory about the distribution of topics in your documents
#e.g. if you have many scientific journals you may want as many topics as there are scientific fields
#for fiction you typically choose a range of numbers and observe outputs. So k=30,50,70,90,110 is a good range
#depending on the size of your data. There are diagnostics you can run on choosing a good model, but that's for later.
k=50

#2. the key parameter to experiment with is "alpha"
#this determines whether you are looking for topics that are more unique to individual documents
#or whether you are looking for topics that are more well-distributed across the entire corpus
#a low alpha will give you very distinct topics (documents will tend to have one strong topic and the rest are meaningless)
#a high alpha will give you several topics that are associated in similar manner with a single document
#50/k is recommended for a high alpha, 0.001 for a low alpha
control_LDA_Gibbs<-list(alpha=(50/k), estimate.beta=TRUE, iter=1000, burnin=20, best=TRUE, seed=2)

#3. run topic model
#this can take awhile (as in hours) depending on how many texts you have. Be patient.
topicmodel<-LDA(corpus2, method="Gibbs", k=k, control = control_LDA_Gibbs) # k = # topics


########   Step 3: MAKE PROBABILITY TABLE   ##########

#this is the main data object you will be engaging with along with the output "topicmodel"
probabilities<-posterior(topicmodel)


########   Step 4: EXPLORE YOUR MODEL    ##############


###################################
### WORD - TOPIC RELATIONSHIPS ####
###################################

#topic models allow you to better understand the relationship between words and topics and
#topics and documents. 
#- Which words are more likely to appear in which topics?
#- Which topics are more likely to appear in which documents?

#this is a table of the words per topic
#the integer changes the number of words shown (remember, in a topic model all words are in every topic with
#vanishingly small probabilities)
#the words are ranked by their probability of being in a given topic (thus the first word in a topic is the 
#most likely word to appear in that topic)
term_dis<-terms(topicmodel, 20) 

#to see ALL words per topic
term_dis_all<-terms(topicmodel, ncol(corpus2)) 

#to save this as a table
#save in this format: FanFic_k50_Alpha1.0.csv
write.csv(term_dis, file="NameOfCorpus_HowManyTopics_MyAlpha.csv")

#to observe the probabilities associated with words for each topic - we call this the "topic to WORDS probabilities table"
topic_word_probs<-as.data.frame(probabilities$terms)
#select a topic
topic.no<-7

#subset by your topic
prob_sample<-topic_word_probs[topic.no,]

#sort in descending order
title.g<-paste("Word Probabilities\nTopic ", topic.no, sep="")
prob_sample<-sort(prob_sample, decreasing=T)
plot(t(prob_sample), main=title.g, xlab="Words", ylab="Probability")

#inspect the probabilities of the top 20 words for this topic
prob_sample[1:20] 
plot(seq(1:20), unname(prob_sample)[1:20], xlab="Words", ylab="Probability")


############################################
#####  TOPIC - DOCUMENT RELATIONSHIPS  #####
############################################

#this tells you the top topic per document
topic_dis_1<-topics(topicmodel, 1) 

#this tells you all the topic probabilities per document
top_dis_all<-topics(topicmodel, k)

#to save the top topic per document
#write.csv(topic_dis_1, file="Corpus_HowManyTopics_Document_Top1_MyAlpha.csv")

#to observe the topic probabilities by document - we call this the topic to DOCUMENT probabilities table
topic_doc_probs<-as.data.frame(probabilities$topics)

#to check the distribution of topics across documents
#this is asking if all topics qre equally represented across your documents
#or whether some topics appear more often than others
#x-axis = topic number. y-axis = frequency of that topic as the "top topic" in a document
plot(table(topic_dis_1), main="Topic Distribution", xlab="Topics", ylab="Frequency")

##### WHAT TOPICS ARE ASSOCIATED W/ A SINGLE DOCUMENT? #####

#First choose a document to inspect in two ways:

#A. input document number - this is the document you want to discover top topics for
test.doc<-30

#B. OR input document name
myDoc<-c("1177881.txt")
test.doc<-which(row.names(corpus2) == myDoc)

#get filename of that document
row.names(corpus2)[test.doc]

#then subset by that document
prob_sample<-topic_doc_probs[test.doc,]

#sort topics from highest to lowest
prob_sample_sort<-sort(prob_sample, decreasing = T)

#observe distribution of topics in a single document (relative strength/weakness of each topic in that doc)
#NOTE: x-axis is rank not the actual topic number
title.d<-paste("Topic to Document Probabilities\nfor Document ", test.doc, sep="")
plot(t(prob_sample_sort), main = title.d, xlab="Topic Ranks", ylab="Probability")

#inspect leading topics for that document
prob_sample_sort[1:5]

#refer to your topic-word table to see the words associated with a given top topic
#this shows the words associated with the top Nth topic -- chang the integer in brackets
#to get the top 1,2,3,4th topic etc for this document.
#output are the top 20 words associated with that topic
term_dis[,as.numeric(names(prob_sample_sort))[1]]


########   WHAT ARE THE DOCUMENTS ASSOCIATED WITH A PARTICULAR TOPIC  ########

#first, define your topic
topic.no<-7

#subset all documents by that topic
prob_sample<-topic_doc_probs[,topic.no]

#then plot in descending order to understand the distribution
#warning again: the document #s do not refer to row numbers of actual docs, just their rank
title.f<-paste("Topic to Document Probabilities\nTopic ", topic.no, sep="")
prob_sample_sort<-sort(prob_sample)
plot(prob_sample_sort, main = title.f,xlab = "Documents", ylab = "Probability")

#find the document that exhibits this topic most strongly out of all documents
row.names(topic_doc_probs)[which(topic_doc_probs[,topic.no] == max(prob_sample))]

#observe documents that appear in the 99th percentile for this topic
cut<-unname(quantile(topic_doc_probs[,topic.no], .99))
row.names(topic_doc_probs)[which(topic_doc_probs[,topic.no] > cut)]

#subset by the top documents with this topic
top.docs<-topic_doc_probs[row.names(topic_doc_probs)[which(topic_doc_probs[,topic.no] > cut)],]

#this orders the table from highest to lowest - change the integer to the desired topic number
#with this information you can now go back to the original documents and read them and see how well that
#topic is actually represented
top.docs<-top.docs[order(-top.docs$`7`),]



#################################################################
###############  DISCOVER DISTINCTIVE TOPICS   ##################
#################################################################

#In this section we will identify which topics are more associated
#with which class of writing. For this exercise, we are using two
#datasets: a corpus of NYT reviewed fiction and a corpus of 
#Queer Fanfic from Archive of Our Own as discussed here:
#https://txtlab.org/wp-content/uploads/2021/12/QueerFans2021.pdf

#We will be using the same techniques as in the code 03_CorpusComparison2
#Here instead of testing words we will be working with "topics" as features
#and because we are working with probabilities, we will be using the Wilcoxon rank sum test
#rather than the G2 test

#load metadata
setwd("")
meta<-read.csv("TopicModel_NYT_FanFic_Meta.csv")

#separate the topic probability table by metadata
group1<-topic_doc_probs[row.names(topic_doc_probs) %in% meta$Filename[meta$Class == "FAN"],]
group2<-topic_doc_probs[row.names(topic_doc_probs) %in% meta$Filename[meta$Class == "NYT"],]

#For each topic we performa rank sum test (Wilcoxon) to observe the degree to which the topic 
#is more likely to show up in one corpus versus another.

#create a loop that goes through each column and compares the median probability of the topic
#in group 1 v. group 2 and store in a table
distinctive.df<-NULL
for (i in 1:ncol(topic_doc_probs)){
  model<-wilcox.test(group1[,i], group2[,i])
  p<-model$p.value
  median1<-median(group1[,i])
  median2<-median(group2[,i])
  median.ratio<-median(group1[,i])/median(group2[,i])
  median.diff.abs<-abs(median(group1[,i])-median(group2[,i]))
  median.diff<-median(group1[,i])-median(group2[,i])
  topic.no<-i
  temp.df<-data.frame(topic.no, median1, median2, median.ratio, median.diff, p)
  distinctive.df<-rbind(distinctive.df, temp.df)
}

#plot results
library(ggplot2)
ggplot(distinctive.df, aes(y=reorder(topic.no, -median.diff), x=median.diff)) +
  geom_point() +
  theme_classic() +
  geom_vline(xintercept=0) +
  xlab("Difference in Median Probability") +
  ylab("Topic Number") +
  labs(title="Distinctive Topics", subtitle = "Queer Fanfic and NYTimes Fiction")






