#########    LLCU 255 Introduction to Literary Text Mining    ##################
#########                 by Andrew Piper                     ##################
#########                CC By 4.0 License                    ##################

################################################################################
########################    Working with bookNLP data    #######################
########################                                 #######################
################################################################################

#this script works with the .tokens output from the original version of bookNLP (https://github.com/dbamman/book-nlp)
#it does not work with the updated version (https://github.com/booknlp/booknlp)

##### In this script you can:
#A. Measure the average sentence length for every document in a corpus
#B. Measure the average word length
#C. Discover distinctive Place Names
#D. Discover distinctive Parts-of-Speech

#Each of these scripts assumes you have two comparison corpora

#set your working directory: this is where your directories of bookNLP files are stored 
setwd("/Users/akpiper/Data")

######## A & B. Comparing Sentence and Word Length ###########
#for this exercise you are going to look at two different corpora of texts
#and compare their sentence and word lengths

#get filenames Corpus 1
file.n1<-list.files("bookNLP_gut_child")

#get filenames for Corpus 2
file.n2<-list.files("bookNLP_gut_folk")

#create a loop to go through each file and ingest the bookNLP table

#### CORPUS 1 ####

#change your working directory to Corpus 1
setwd("/Users/akpiper/Data/bookNLP_gut_child")

#create empty final table
corpus1.df<-NULL

#run loop
for (i in 1:length(file.n1)){
  print(i)
  
  #ingest bookNLP table
  a<-read.csv(file.n1[i], sep="\t", quote = "")
  
  #measure mean sentence length
  sent.length<-mean(table(a$sentenceID))
  
  #measure mean word length
  word.length<-mean(nchar(a$originalWord))
  
  #save filename
  fileID<-file.n1[i]
  
  #store in table
  temp.df<-data.frame(fileID, sent.length, word.length)
  corpus1.df<-rbind(corpus1.df, temp.df)
}

#### CORPUS 2 #####

setwd("/Users/akpiper/Data/bookNLP_gut_folk")
#create empty final table
corpus2.df<-NULL
#run loop
for (i in 1:length(file.n2)){
  print(i)
  #ingest bookNLP table
  a<-read.csv(file.n2[i], sep="\t", quote = "")
  #measure mean sentence length
  sent.length<-mean(table(a$sentenceID))
  #measure mean word length
  word.length<-mean(nchar(a$originalWord))
  #save filename
  fileID<-file.n2[i]
  #store in table
  temp.df<-data.frame(fileID, sent.length, word.length)
  corpus2.df<-rbind(corpus2.df, temp.df)
}

#compare the two corpora using our standard methods of t.test or wilcox.test
hist(corpus1.df$sent.length)
hist(corpus2.df$sent.length)
shapiro.test(corpus1.df$sent.length)
shapiro.test(corpus2.df$sent.length)
boxplot(corpus1.df$sent.length, corpus2.df$sent.length)
t.test(corpus1.df$sent.length, corpus2.df$sent.length)
wilcox.test(corpus1.df$sent.length, corpus2.df$sent.length)
median(corpus1.df$sent.length)
median(corpus2.df$sent.length)


########## C. Find distinctive place names ##########
#Place names are discoverable using the NER "location" tag
#As you will see it is not 100% accurate so for any application review your errors

setwd("/Users/akpiper/Data")

#get filenames Corpus 1
file.n1<-list.files("bookNLP_gut_child")

#get filenames for Corpus 2
file.n2<-list.files("bookNLP_gut_folk")

#create a loop to go through each file and ingest the bookNLP table

#### CORPUS 1 ####

#change your working directory to Corpus 1
setwd("/Users/akpiper/Data/bookNLP_gut_child")

#create empty final vector
place.final.v1<-vector()

#run loop
for (i in 1:length(file.n1)){
  print(i)
  
  #ingest bookNLP table
  a<-read.csv(file.n1[i], sep="\t", quote = "")
  
  #subset by locations
  a.place<-a[a$ner == "LOCATION",]
  
  #if there are placenames
  if (nrow(a.place) > 1){
  #concatenate multi-word places
  bi.v<-vector()
  remove.v<-vector()
  for (j in 1:(nrow(a.place)-1)){
    if (a.place$tokenId[j]+1 == a.place$tokenId[j+1]){
      bi.v<-append(bi.v,paste(a.place$originalWord[j],a.place$originalWord[j+1], sep=" ", collapse = " "))
      v<-c(a.place$tokenId[j],a.place$tokenId[j+1])
      remove.v<-append(remove.v, v)
    }
  }
  
  #remove those tokens from the place table
  a.place<-a.place[!a.place$tokenId %in% remove.v,]

  #then extract single word places
  one.v<-a.place$originalWord
  
  #combine into single vector
  place.v<-append(bi.v, one.v)
  
  #combine into meta-vector
  place.final.v1<-append(place.final.v1, place.v)
  }
}

#### CORPUS 2 #####

#change your working directory to Corpus 1
setwd("/Users/akpiper/Data/bookNLP_gut_folk")

#create empty final vector
place.final.v2<-vector()

#run loop
for (i in 1:length(file.n2)){
  print(i)
  
  #ingest bookNLP table
  a<-read.csv(file.n2[i], sep="\t", quote = "")
  
  #subset by locations
  a.place<-a[a$ner == "LOCATION",]
  
  #if there are placenames
  if (nrow(a.place) > 1){
    #concatenate multi-word places
    bi.v<-vector()
    remove.v<-vector()
    for (j in 1:(nrow(a.place)-1)){
      if (a.place$tokenId[j]+1 == a.place$tokenId[j+1]){
        bi.v<-append(bi.v,paste(a.place$originalWord[j],a.place$originalWord[j+1], sep=" ", collapse = " "))
        v<-c(a.place$tokenId[j],a.place$tokenId[j+1])
        remove.v<-append(remove.v, v)
      }
    }
    
    #remove those tokens from the place table
    a.place<-a.place[!a.place$tokenId %in% remove.v,]
    
    #then extract single word places
    one.v<-a.place$originalWord
    
    #combine into single vector
    place.v<-append(bi.v, one.v)
    
    #combine into meta-vector
    place.final.v2<-append(place.final.v2, place.v)
  }
}

#Tally each of the list of place names for each corpus
df1<-data.frame(table(place.final.v1))
df2<-data.frame(table(place.final.v2))

#run a distinctive words test to compare place names
keep<-intersect(as.character(df1[,1]), as.character(df2[,1]))

#subset 1 and 2 by these words
#this is your DTM1 and DTM2
dtm1<-df1[df1$place.final.v1 %in% keep,]
dtm2<-df2[df2$place.final.v2 %in% keep,]

#first get individual word counts for each corpus
word1<-dtm1$Freq
word2<-dtm2$Freq
#then get total counts for each corpus
all1<-sum(word1)
all2<-sum(word2)

#entropy function
H = function(k) {N = sum(k); return(sum(k/N*log(k/N+(k==0))))}

#store empty results in a table
results <- data.frame(word = dtm1$place.final.v1, 
                      group1=word1,
                      group2=word2,
                      G2 = 0,
                      fisher.OR = 0,
                      fisher.p = 0)
#create loop to go through every word
for (j in 1:nrow(dtm1)){
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
cut<-0.05/nrow(dtm1)

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


########## D. Find distinctive parts of speech ##########
#Parts of speech are discoverable using the "pos" column

setwd("/Users/akpiper/Data")

#get filenames Corpus 1
file.n1<-list.files("bookNLP_gut_child")

#get filenames for Corpus 2
file.n2<-list.files("bookNLP_gut_folk")

#create a loop to go through each file and ingest the bookNLP table

#### CORPUS 1 ####

#change your working directory to Corpus 1
setwd("/Users/akpiper/Data/bookNLP_gut_child")

#create empty final vector
pos.df1<-NULL

#run loop
for (i in 1:length(file.n1)){
  print(i)
  
  #ingest bookNLP table
  a<-read.csv(file.n1[i], sep="\t", quote = "")
  
  #table the POS tags
  df<-data.frame(table(a$pos))

  #merge with meta table
  if (length(pos.df1) > 0){
    pos.df1<-merge(pos.df1, df, by="Var1", all=T)
  } else {
    pos.df1<-df
  } 
}

#sum values into single table
row.names(pos.df1)<-pos.df1$Var1
pos.df1<-pos.df1[,-1]
sum.v<-rowSums(pos.df1, na.rm = T)
pos.df1<-data.frame(row.names(pos.df1), sum.v)
colnames(pos.df1)<-c("pos", "freq")

#### CORPUS 2 ####

#change your working directory to Corpus 1
setwd("/Users/akpiper/Data/bookNLP_gut_folk")

#create empty final vector
pos.df2<-NULL

#run loop
for (i in 1:length(file.n2)){
  print(i)
  
  #ingest bookNLP table
  a<-read.csv(file.n2[i], sep="\t", quote = "")
  
  #table the POS tags
  df<-data.frame(table(a$pos))
  
  #merge with meta table
  if (length(pos.df2) > 0){
    pos.df2<-merge(pos.df2, df, by="Var1", all=T)
  } else {
    pos.df2<-df
  } 
}

#sum values into single table
row.names(pos.df2)<-pos.df2$Var1
pos.df2<-pos.df2[,-1]
sum.v<-rowSums(pos.df2, na.rm = T)
pos.df2<-data.frame(row.names(pos.df2), sum.v)
colnames(pos.df2)<-c("pos", "freq")

#first get individual word counts for each corpus
word1<-pos.df1$freq
word2<-pos.df2$freq
#then get total counts for each corpus
all1<-sum(word1)
all2<-sum(word2)

#entropy function
H = function(k) {N = sum(k); return(sum(k/N*log(k/N+(k==0))))}

#store empty results in a table
results <- data.frame(word = pos.df1$pos, 
                      group1=word1,
                      group2=word2,
                      G2 = 0,
                      fisher.OR = 0,
                      fisher.p = 0)
#create loop to go through every word
for (j in 1:nrow(pos.df1)){
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
cut<-0.05/nrow(pos.df1)

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


