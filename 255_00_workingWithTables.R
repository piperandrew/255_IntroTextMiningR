######### LLCU 255 Introduction to Literary Text Mining ##########
######### by Andrew Piper ####################
######### CC By 4.0 License ##################

########### Working with Tables #############

#txtlab_Novel150_English 
#https://doi.org/10.6084/m9.figshare.17425562.v1

#In this section you will get comfortable manipulating tables in R and understanding
#the data they contain. Before you begin to ingest and analyze documents it is important
#to see how much information is already encoded in your metadata.

#As an example we will be using the metadata to the Novel150 data set

#set your working directory
setwd("~/Data")

#load table
#you should see a data frame with 9 columns
a<-read.csv("txtlab_Novel150_English.csv", stringsAsFactors = T)

#what are those columns?
colnames(a)

######### CATEGORICAL DATA #############
#There are two kinds of data stored in our data frame:
#numerical data and categorical data
#numerical data for example refers to the dates of publication or word counts
#categorical data refers to the author's gender or the novel's point of view
#R will refer to categorical data as "factors" that contain different levels.
#"gender" is a factor that contains two levels in our data for example (though you could code it for more).

#to call a column use the $ sign.
a$gender

#notice how it will list the filenames but also say "150 levels:..." What does that mean?
#strings = strings of letters or numbers (i.e. words or numbers)
#factors = categories which can have multiple levels
#so what has happened here is that R has turned the filenames into 150 separate levels
#of a single variable called "filenames." Sometimes this is useful, sometimes not.
#You can tell if a word is a factor or a string by whether it has quotes around it.
#Notice how the titles don't have quotes. That's how you know they are factors

#this prints the first title as a string not a factor (and thus has quotes)
#ie. we can change things from one "type" to another by using the "as" function
#(you could go in reverse and turn a string into a factor using as.factor())
as.character(a$filename[1])

#factors are useful if you have a variable of interest with multiple levels.

#one example are authors. You may have multiple authors in your data set and you
#should have a general idea about how much author repetition you have. 
#In general if you want to generalize about larger social practices you don't want too many
#books by the same author

#to observe this you use the following two functions:

#this tells us how many "levels" there are in the "author" factor
nlevels(a$author)

#this is akin to asking how many unique authors there are
length(unique(a$author))

#to observe the list of authors
levels(a$author)

#to find out who has the most books
#the table function adds up categories for you, in this case author names
#the sort function puts them in order from lowest to highest
#change decreasing = T for the opposite order
#notice how we can put functions inside of functions
sort(table(a$author), decreasing = F)

#so you can see no author has more than 3 books in our data set

#Another example is look at the column called "gender".
levels(a$gender)

#similarly we can use the table function to see what the ratio is
table(a$gender)


##########   NUMERICAL DATA     #########

#our data also contains two columns of numerical data. How do we handle that?

#first, you could take the mean of the publication dates
mean(a$date)

#1862 is the mean publication date in this data. That's very useful to know. It tells us
#where the centre of gravity is in our data. We can get fancier with summary:

summary(a$date)

#Now we know the earliest date associated with a novel and the latest
#This is useful to assess the historical boundaries of your data. If your data stops
#at 1930 you cannot talk about the "20C novel"

#We can also assess how well the mean and median line up -- the less they do so the more
#skew there is in the data. Here they are almost the same.

#a third way you can handle numerical data is to assess the overall distribution of values
#are some periods cover better than others?

#First we can use a histogram. 

hist(a$date)

#In general we see how we have more books from later decades in our data, though 
#there is decent representation across the entire timeline.
#Depending on your research goals you would either want a sample that approximates 
#titles published (and is thus skewed towards later periods because as time passes 
#there are always more novels) OR you might want to sample evenly across all periods 
#to ensure that your findings aren't skewed by a particular period. 
#See the section on data selection for addressing these complex issues.

#because there aren't so many dates we could also just plot each year individually
plot(table(a$date))

#this lets us see that we have no more than 4 novels from the same year, very few extended gaps
#and once again pretty good balance across the whole period.

#now try it yourself with the novels' word counts

#you should get a mean of 123,240
#you should get a maximum value of 356,109 and minimum of 23,275

#try making a histogram. What do you find?


####### Manipulating Your Data #######

#The two things I want to look at here are performing math on columns
#and subsetting your tables by various categories

#### Example 1 ####

#So for example, what happens if you realized that every single book had in fact 
#exactly 500 words of boiler plate in its front matter and so your word counts were 
#over-estimated. If you wanted to remove 500 from the length values you go:

a$adjusted.length<-a$length-500

#notice how I created a new column in case I made a mistake and preserved it as a new variable.
#I might also want to compare the old and the new columns so it makes sense to keep both.

#You should see that the new column has the same values minus 500 for each word count. R
#just performs the -500 on all values in the vector, which is very convenient.

#a few other useful functions for exploring your data

#this sums all columns in a matrix
# it is not appropriate for a data frame because some of your columns aren't numbers
colSums() 
#the same thing for rows
rowSums()


###### Example 2 ######

#let's say you wanted to study decades not years.
#let's go ahead and transform years to decades by removing the final number and adding a 0

#first we'll convert the date column to a column of strings to utilize the substring function
a$decade<-as.character(a$date)

#then we transform the 4th digit to a 0
substring(a$decade, 4, 4) <- "0"

#and convert back to integers
a$decade<-as.numeric(a$decade)

#now we can see decade-level counts of novels
plot(table(a$decade))

#### Subsetting Tables ####

#very often you will want to subset your data in some way or compare portions of it
#R has a very easy way to do this
#for example, if we wanted to compare the avg. length of novels for men and women authors
#we could first subset our table into two smaller tables and then take the mean of the word count column
#(there is a faster way to do this but we'll worry about this later)

#first subset the bigger table into two smaller tables by gender
#R syntax uses brackets for subsetting, with rows before the comma and columns after
#the following syntax says: 
#subset table a by the gender column *such that* gender == "female"
#i.e. only keep rows that meet this criteria
#and keep all columns (i.e. there is no condition after the comma in brackets)
f<-a[a$gender == "female",]
m<-a[a$gender == "male",]

#then calculate the means of the two new tables
mean(f$length)
mean(m$length)

#in a later section we'll learn how to run a statistical test to see if these
#differences are "statistically significant" 
#i.e. when is a difference in quantity *meaningful*?

#here is a faster way to do these calculations
#we use the "apply" family of functions, which are really useful (and confusing)
#here we use "tapply" which takes three values: 
#a. the column with the value you want to measure
#b. the column with the categories they belong to
#c. the calculation you want to run (i.e. "mean" or "sum")
#so to measure the avg. novel length by "gender":
tapply(a$length, a$gender, mean)


###### SAMPLE QUESTIONS ######

#1. What is the ratio of first-person to third-person novels in our sample?

#2. What is the avg length of first and third person novels?

#3. List the avg. length of novels by decade. What do you observe?

###### Extra Credit:
#While this gets ahead of ourselves, another thing you can observe with tables
#are *associations*. For example, with this data we could ask the question:
#are women more likely to be associated with first or third person novels?
#in later segments you'll learn how to conduct such a test of association










