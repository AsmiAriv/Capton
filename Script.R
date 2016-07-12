#Capstone project Preparation:

#Working directory:

setwd("C:\\R\\Coursera\\CapstoneProject\\Dataset\\final\\en_US")

#Libraries for text mining
library(e1071)
library(textir)
library(tm)
library(VGAM)
library(ngram)


#Creating a subdirectory and loading the subset of the data
dir.create("sample") 
setwd("C:\\R\\Coursera\\CapstoneProject\\Dataset\\final\\en_US\\sample")

#Subsetting the data for a sample creation
set.seed(124)
sam_twitter <- twitter[rbinom(length(twitter)*0.005, length(twitter),0.5)]

length(sam_twitter)
[1] 11800

set.seed(124)
sam_blogs <- blogs[rbinom(length(blogs)*0.01, length(blogs),0.5)]
length(sam_blogs)
[1] 8992

set.seed(124)
sam_news <- news[rbinom(length(news)*0.1, length(news),0.5)]
length(sam_news)
[1] 7725

#Writing the sample in CSV files in sample directory
write.csv(sam_twitter, file="twitter.csv", row.names=F)
write.csv(sam_blogs, file="blogs.csv", row.names=F)
write.csv(sam_news, file="news.csv", row.names=F)

#Preprocessing the text to construct DocumentTermMatrix

Method1########################################
#Storing the data as Corpus object in order to cleane the data
data <- Corpus(DirSource("C:/R/Coursera/CapstoneProject/Dataset/final/en_US/sample") , readerControl=list(reader=readPlain,language="en_US"))

#Removing unwanted elements from the data
# convert to lower case
data <- tm_map(data, tolower)

# remove punctuation
data <- tm_map(data, removePunctuation)

# remove numbers
data <- tm_map(data, removeNumbers)

# remove extra whitespace
data <- tm_map(data, stripWhitespace)

#Cannot remove stop words as they may be required 


bigram <- ngram_asweka(as.character(data), min=2, max=2)
bigfreq <- data.frame(table(bigram))
big10 <- head(bigsort,10)
barplot(big10$Freq, names.arg=big10$bigram, las=2)

###################################################

Method2####################################################
twt <- read.csv("twitter.csv", stringsAsFactors=FALSE)
nws <- read.csv("news.csv", stringsAsFactors=FALSE)
blg <- read.csv("blogs.csv", stringsAsFactors=FALSE)

data <- rbind(twt,nws,blg)
 class(data)
[1] "data.frame"

dataCh <- as.character(data)
proData <- preprocess(dataCh, case="lower", remove.punct=TRUE, 
				remove.numbers =TRUE, fix.spacing=TRUE)
#Without stop words
corpdata <- Corpus(VectorSource(proData))
dataStop <- tm_map(corpdata, removeWords, stopwords("english"))
text <- sapply(dataStop, as.character)

#Bigram Analysis
ng2 <- ngram(proData, n=2)
ng2freq <- get.phrasetable(ng2)

big10 <- head(ng2freq,10)
barplot(big10$freq, names.arg=big10$ngrams, las=2, main = "Top 10 bigrams")

#Investigating without stop words
ng2Stop <- ngram(text, n=2)
ng2stfreq <- get.phrasetable(ng2Stop)

big2st10 <- head(ng2stfreq,10)
barplot(big2st10$freq, names.arg=big2st10$ngrams, las=2, main = "Top 10 bigrams sans stop words")

#Trigram Analysis
ng3 <- ngram(proData, n=3)
ng3freq <- get.phrasetable(ng3)

big3Top <- head(ng3freq,10)
barplot(big3Top$freq, names.arg=big3Top$ngrams, las=2, main = "Top 10 trigrams")

#Investigating without stop words
ng3Stop <- ngram(text, n=3)
ng3stfreq <- get.phrasetable(ng3Stop)

big3st10 <- head(ng3stfreq,10)
barplot(big3st10$freq, names.arg=big3st10$ngrams, las=2, main = "Top 10 trigrams without sans words")






#############################################################

#Converting to PlainTextDocument format
myCorpus<- tm_map(data, PlainTextDocument)


#Either weightTf or weightTfIdf for weighting.
#wighttf gives a DocumentTermMatrix in term frequency format
myTdm<- DocumentTermMatrix(myCorpus, control = list(weighting = weightTf, minWordLength=4))

dim(myTdm)
[1]     3 19020

inspect(myTdm[1:3,1:20])

#Removing the sparse terms
myTdm <- removeSparseTerms(myTdm, 0.98)   
inspect(myTdm[1:5,1:20])  

#Converting to matrix
temp = as.matrix(myTdm)


##################For practice:#############################


Reading lines:
con <- file("en_US.twitter.txt")
text <- readLines(con,5)
close(con)

Writing Lines:

con <- file("Sample.txt") #The file already must have been created 
writeLines(text, con)
close(con)

Estimating the number of lines in a file
con <- file("en_US.twitter.txt",open="r")
size <- 30000
lines <- 0
(while((linesread <- length(readLines(con,size))) > 0) 
 lines <- lines+linesread)
 close(con)
 lines

For quiz1:
blogs <- readLines("en_US.blogs.txt")
length(blogs)
[1] 899288

news <- readLines("en_US.news.txt")
length(news)
[1] 77259

twitter <- readLines("en_US.twitter.txt")
length(twitter)
[1] 2360148

#Lines with longest character
max(nchar(blogs))
[1] 40835

max(nchar(news))
[1] 5760

max(nchar(twitter))
[1] 213

#love / hate (all lowerscase) in twitter
love_count <- sum(grepl("love", twitter))
hate_count <- sum(grepl("hate", twitter))
love_count / hate_count
[1] 4.108592

#word "biostats" matching in twitter
biostats <- grep("biostats", twitter)
twitter[biostats]
[1] "i know how you feel.. i have biostats on tuesday and i have yet to study =/"

#No. of tweets in "A computer once beat me at chess, but it was no match for me at kickboxing"
sum(grepl("A computer once beat me at chess, but it was no match for me at kickboxing",twitter) 

 






