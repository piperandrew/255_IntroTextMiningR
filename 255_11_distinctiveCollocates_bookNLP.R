#########    LLCU 255 Introduction to Literary Text Mining    ##################
#########                 by Andrew Piper                     ##################
#########                CC By 4.0 License                    ##################

################################################################################
########################    Distinctive Collocates    ##########################
########################      using bookNLP data      ##########################
################################################################################

#A 'collocate' is a word that appears near another word ("co-located").
#Using the theory of distributional semantics that a word's meaning is
#determined by its usage, we can measure words that are distinctive of 
#a given keyword in a given corpus. If we want to know how
#a word is used *differently* in different types of texts (or different communities of writers)
#we can use this method to better understand the differences in the semantic fields
#surrounding a given keyword. 
#Alternately, we can use this method to understand the different semantic fields surrounding
#two related keywords in the same collection of texts, such as "mother" and "father".

#To do this we need two steps:
#A. Build the list of collocates for a given keyword in a given corpus
#B. Use our distinctive words test to identify words that are semantically distinctive of each keyword

##############################     A. Collocate Builder    #################################

#this script takes as input:
#- a directory of bookNLP .tokens files (or .csv)
#- a "keyword" (can also be a word type)
#- a window size of +/- N words (default = 9)

#it outputs a table of word counts associated with the keyword

#because you can either compare:
#- the same keyword in two different collections (e.g. collocates of "mother" in children's lit v. adult lit)
#- or two keywords in the same collection (e.g. collocates of mother v. father in children's lit)
#you will run the following script TWICE by changing either the KEYWORD or the txt files DIRECTORY
#notice at the end where you save your outputted list of words -- you need to create 2 variables, one for each run

#set working directory to bookNLP files location -- set to the directory where they are actually located
#change this for second run if testing 2 collections
setwd("~/Data/bookNLP_gut_child_select")
#setwd("~/Data/bookNLP_cont_ADULT") #example of comparison data

#get list of files in target directory
f.names<-list.files()

#create loop to go through and ingest each file and turn into one large table
book.df<-NULL
for (i in 1:length(f.names)){
  print(i)
  
  #if tab separated
  a<-read.csv(f.names[i], sep="\t", quote = "", stringsAsFactors = F)
  
  #if comma separated
  #a<-read.csv(f.names[i], stringsAsFactors = F)
  
  #add filename column
  a$filename<-f.names[i]
  #add to meta-table
  book.df<-rbind(book.df, a)
}

#remove punctuation
book.df<-book.df[-grep("[[:punct:]]", book.df$lemma),]

### define keyword for collocate analysis
#this can either be a word or a word type or other type of data in bookNLP
#this script uses the character gender annotation as an example
#change this on second run if comparing two different keywords

#keyword<-c("male")
#keyword<-c("female")
keyword<-c("love")

#if looking at a specific character use this
#keyword<-55

#define window +/- (Default = 9)
n<-9

#create index of locations of keyword in the vector

#EXAMPLE: This is to detect collocates of male/female identified characters
key.index<-which(book.df$gender == keyword) # position of keyword

#EXAMPLE: This is to detect collocates of a keyword
#key.index<-which(book.df$lemma == keyword) # position of keyword

#EXAMPLE: This is to detect collocates of a characterID
#key.index<-which(book.df$characterId == keyword) # position of keyword

#Next get all words +/- of where the keywords are
before<-sapply(key.index, function (x) seq(from=(x-10), to=(x-1), by=1))
after<-sapply(key.index, function (x) seq(from=(x+1), to=(x+10), by=1))

#combine and remove duplicates and keywords
key.all<-append(before, after)
key.all<-unique(key.all)
key.all<-key.all[!key.all %in% key.index]
key.all<-key.all[key.all > 0]

#save as vector of words
collocate.v<-book.df$lemma[key.all]

#remove all words that are proper names
collocate.v<-collocate.v[!collocate.v %in% book.df$lemma[book.df$ner == "PERSON"]]

##### remember to adjust the following lines based on whether first or second run!!!

#For your FIRST keyword/text collection
#collocate.df1<-data.frame(sort(table(collocate.v), decreasing = T))

#For your SECOND keyword/text collection
collocate.df2<-data.frame(sort(table(collocate.v), decreasing = T))


#####################      B. Identify Distinctive Collocates      ######################

#this script runs the same distinctive words test using the G2 test from earlier
#The way we will model this is to ask:
#how much more likely is COLLOCATE X to appear as a collocate in CORPUS A than B? Or near KEYWORD A than B?
#The way calculate this is by making what are known as "contingency tables".
#This allows us to compare the rate of a given word relative to the overall number of words in that corpus
#They have the following structure:

#           Corpus A/Keyword A    Corpus B/Keyword B
#Collocate       x                  y
#NotCollocate    z                  w

#we match the two data frames of word counts and we only keep words that are in both

#first turn into strings not factors
collocate.df1$collocate.v<-as.character(collocate.df1$collocate.v)
collocate.df2$collocate.v<-as.character(collocate.df2$collocate.v)

#then sort alphabetically
collocate.df1<-collocate.df1[order(collocate.df1$collocate.v),]
collocate.df2<-collocate.df2[order(collocate.df2$collocate.v),]

#merge
colnames(collocate.df1)<-c("collocate.v", "Freq")
colnames(collocate.df2)<-c("collocate.v", "Freq")
collocate.all<-merge(collocate.df1, collocate.df2, by="collocate.v", all = T)
collocate.all[is.na(collocate.all)]<-0

#remove rows where at least one column is not > X (default = 50)
collocate.all<-collocate.all[collocate.all$Freq.x > 49 | collocate.all$Freq.y > 49,]

#first get individual word counts for each corpus
word1<-collocate.all$Freq.x
word2<-collocate.all$Freq.y

#then get total counts for each corpus
all1<-sum(word1)
all2<-sum(word2)

#entropy function
H = function(k) {N = sum(k); return(sum(k/N*log(k/N+(k==0))))}

#store empty results in a table
results <- data.frame(word = collocate.all$collocate.v, 
                      group1=word1,
                      group2=word2,
                      G2 = 0,
                      fisher.OR = 0,
                      fisher.p = 0)
#create loop to go through every word
for (j in 1:nrow(results)){
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
cut<-0.05/nrow(results)

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


######## Inspect Target Collocates ############
#let's say you're interested in better understanding the contexts in which certain collocates occur
#in other words, given distinctive collocates what are they telling us?
#one way to answer that is to look at sentences where both your keyword and collocate are present
#and assess their meaning. (You can see the problem of infinite regress: run collocat analysis on distinctive collocates!)

#first choose a given collocate & keyword
col.key<-c("love")
keyword<-c("mother")

#subset your bookNLP table by all *sentences* that have *both* your keyword and your collocate
sent1<-book.df[book.df$lemma == keyword,]
sent2<-book.df[book.df$lemma == col.key,]

#get list of works that contain these sentences
sent3<-unique(append(sent1$filename, sent2$filename))

#subset by these books
book.sub<-book.df[book.df$filename %in% sent3,]

#now for each book subset by sentences with both collocate and keyword
#this table gives you sentences that contain both the keyword and target collocate to inspect
inspect.df<-NULL
for(i in 1:nlevels(factor(book.sub$filename))){
  #subset by i book
  sub<-book.sub[book.sub$filename == levels(factor(book.sub$filename))[i],]
  #subset sentences by keyword and collocate
  sent1.v<-sub$sentenceID[sub$lemma == keyword]
  sent2.v<-sub$sentenceID[sub$lemma == col.key]
  sent.v<-sent1.v[sent1.v %in% sent2.v]
  #save sentence as table
  temp.df<-sub[sub$sentenceID %in% sent.v,]
  inspect.df<-rbind(inspect.df, temp.df)
}


