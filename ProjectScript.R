#Final Project

#######################Data loading and subsetting################
setwd("C:\\R\\Coursera\\CapstoneProject\\Project")
load("data.txt") or load("text.R")

con1 <- file("en_US.blogs.txt", "rb")
blogs <- readLines(con1)
close(con1)
length(blogs)

con2 <- file("en_US.news.txt", "rb")
news <- readLines(con2)
close(con2)
length(news)

con3 <- file("en_US.twitter.txt", "r")
twitter <- readLines(con3)
close(con3)
length(twitter)

#Libraries for text mining
library(tm)
library(ggplot2)
library(RWeka)
library(SnowballC)
library(NLP)
library("flexclust")


#Subsetting the data for a sample creation
set.seed(124)
sam_twitter <- twitter[rbinom(length(twitter)*0.005, length(twitter),0.5)]

length(sam_twitter)


set.seed(124)
sam_blogs <- blogs[rbinom(length(blogs)*0.01, length(blogs),0.5)]
length(sam_blogs)


set.seed(124)
sam_news <- news[rbinom(length(news)*0.01, length(news),0.5)]
length(sam_news)

rm(blogs, news, twitter)

#Creating a common dataset
data <- c(sam_twitter, sam_blogs, sam_news)
rm(sam_news, sam_blogs, sam_twitter)

save(data, file="data.txt")
rm(data)
#######################Writing different functions################

#Function to tokenize the data
Tokenizedata <- function(data){
corpdata <- Corpus(VectorSource(data))
#Removing unwanted elements from the data
# convert to lower case
corpdata <- tm_map(corpdata, tolower)
# remove punctuation
corpdata <- tm_map(corpdata, removePunctuation)
# remove numbers
corpdata <- tm_map(corpdata, removeNumbers)
# remove extra whitespace
corpdata <- tm_map(corpdata, stripWhitespace)
# remove profanity
#corpdata <- tm_map(corpdata, removeWords, profanity)
# remove stopwords
#corpdata <- tm_map(corpdata, removeWords, stopwords("english"))
# stem the data
#corpdata <- tm_map(corpdata, stemDocument)
#Removing other characters
#tm_map (corpdata, toSpace "\"|/|@|\\|" ) 
#Converting to PlainTextDocument
corpdata <- tm_map(corpdata, PlainTextDocument)
corpdata
}

#Function to build tdms
tdm <- function(data, n){
ngramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), n), paste, collapse = " "), use.names = FALSE)

  tdm <- TermDocumentMatrix(data, control = list(tokenize = ngramTokenizer))
tdm <- removeSparseTerms(tdm,.999)
tdm
}


#Function for normalizing the column

Norm.row <- function(temp) {
rowcount= nrow(temp)
colcount  = ncol(temp)

for(i in 1:colcount){
colsum = sum(temp[,i])
if(colsum !=0){
colmean = mean(temp[,i])
colsd      = sd(temp[,i])*sqrt(rowcount-1)
temp[,i] = (temp[,i]-colmean)/colsd
}
} 
temp
}

#Function for normalizing rows

Norm.col <- function(temp) {
rowcount= nrow(temp)
colcount  = ncol(temp)
for(i in 1:rowcount){
t = sum(temp[i,])
if(t>0){
rowmean = mean(temp[i,])
rowsd     = sd(temp[i,])*sqrt(colcount-1)
temp[i,] = (temp[i,]-rowmean)/rowsd
}
} 
temp
}


###clustering, classifications, prediction

#Clustering and classifications
classifier <- function(mat){
grwords <- kmeans(mat, centers=30, nstart=3) #Need to work on this
Mat_c <- cbind(mat,grwords$cluster)
trainset = sample(1:nrow(Mat_c), trunc(0.7*nrow(Mat_c)))
classifier = naiveBayes(Mat_c[trainset, 1:ncol(Mat_c)], as.factor(Mat_c[trainset, ncol(Mat_c)+1]))
classifier
}

#Test prediction
testPred <- function(){
testpredicted = predict(classifier,Mat_c[-trainset, ncol(Mat_c)])
table(testpredicted,Mat4_c[-trainset, 23])
prop<-prop.table(table(testpredicted,Mat_c[-trainset, ncol(Mat_c)+1]))
mean(testpredicted==Mat_c[-trainset, ncol(Mat_c)+1]) # Accuracy
mean
}


#We can also use support vector machine- best for text prediction:
syntax: classifier <- svm(Class~., data=train, kernel="polynomial", degree = 2, cost=1, scale=F)

#So, we can write a function for combination of words

predict <- function(x){
l <- sapply((gregexpr("\\W+", x)), length)+1
corpdata <- Tokenizedata(x)

if(l==4){
tdm <- tdm(corpdata,4)
mat <- as.matrix(tdm)
mat <- t(mat)
testpredicted = predict(classifier4,mat)
}

else if(l==3){
tdm <- tdm(corpdata,3)
mat <- as.matrix(tdm)
mat <- t(mat)
testpredicted = predict(classifier3,mat)
}

else if(l==2){
tdm <- tdm(corpdata,2)
mat <- as.matrix(tdm)
mat <- t(mat)
testpredicted = predict(classifier2,mat)
}

else {
tdm <- TermDocumentMatrix(corpdata)
mat <- as.matrix(removeSparseTerms(tdm1,.999))
mat <- t(mat)
testpredicted = predict(classifier1,mat)
}

testpredicted
}



#Function for searching the matching words
input <- function(x, Mat){
str <- strsplit(x, split=" ")[[1]]

for(i in seq_along(str)){
index <- grep(str[i], rownames(Mat), ignore.case=T, value=T)
#lines <- rownames(Mat[index,])
l[i] <- list(index)
}

words <- sapply(l, function(x)strsplit(x, split=" "))
words <- unlist(words)
words <- setdiff(words,str)
unique(words)
}



#Function for dotproduct
dotproduct <- function(temp, str) {
rowcount= nrow(temp)
colcount  = ncol(temp)
dotproducts = numeric(rowcount)

for( i in 1:rowcount){
dotproducts[i] =  sum(temp[grep(str,rownames(temp)),]*temp[i,])
}

ordering = order(dotproducts)
ordering
}

#Alternatively, try doc.scores <- t(word.vector) %*% mat


#Tokenizing the data
corpdata <- Tokenizedata(data)
rm(data)

#Tdm for unigram
tdm1 <- TermDocumentMatrix(corpdata)
Mat1 <- as.matrix(removeSparseTerms(tdm1,.999))

# Tdm for bigram
tdm2 <- tdm(corpdata, 2)
Mat2 <- as.matrix(tdm2)

#Tdm for trigram
tdm3 <- tdm(corpdata,3)
Mat3 <- as.matrix(tdm3)


#Tdm for quadigram
tdm4 <- tdm(corpdata,4)
Mat4 <- as.matrix(tdm4)












#Need to find a way to display the next word (rather than all), may consider dotproduct for finding the exact index based on closeness 

rownames(Mat4[grep("at", rownames(Mat4)),]) #This will provide all the words along with all matching rownames
unlist(strsplit(rownames(Mat4[grep("at", rownames(Mat4)),]),split=" ",fixed=TRUE)) # This splits each rowname into words



#Storing the models using Markov Chains

#Designing back off models to estimate the probability of unobserved n-grams

