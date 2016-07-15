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

profane <- file("profanity.txt", "r")
profWord <- readLines(profane)
close(profane)

#Libraries for text mining
library(tm)
library(ggplot2)
library(RWeka)
library(SnowballC)
library(NLP)
library(e1071)


#Subsetting the data for a sample creation
set.seed(124)
sam_twitter <- twitter[rbinom(length(twitter)*0.0005, length(twitter),0.5)]

length(sam_twitter)


set.seed(124)
sam_blogs <- blogs[rbinom(length(blogs)*0.001, length(blogs),0.5)]
length(sam_blogs)


set.seed(124)
sam_news <- news[rbinom(length(news)*0.001, length(news),0.5)]
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
corpdata <- tm_map(corpdata, removeWords, profWord)
# remove stopwords
corpdata <- tm_map(corpdata, removeWords, stopwords("english"))
# stem the data
corpdata <- tm_map(corpdata, stemDocument)
#Converting to PlainTextDocument
corpdata <- tm_map(corpdata, PlainTextDocument)
# remove non-english characters/words
corpdata <- sapply(corpdata, function(row) iconv(row, "latin1", "ASCII", sub=""))
corpdata <- Corpus(VectorSource(corpdata))
#Converting to PlainTextDocument 
corpdata <- tm_map(corpdata, PlainTextDocument)
corpdata
}



#Function to build tdms
tdm <- function(data, n){
ngramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), n), paste, collapse = " "), use.names = FALSE)

  tdm <- TermDocumentMatrix(data, control = list(tokenize = ngramTokenizer, weighting = weightTfIdf))
tdm <- removeSparseTerms(tdm,.999)
tdm
}


#Function for normalizing the column

Norm.col <- function(temp) {
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

Norm.row <- function(temp) {
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


###############clustering, classifications, prediction################
#Normalize columnwise

#Clustering and classifications

#K-means and naiveBayes
clust <- function(mat, n){
grwords <- kmeans(mat, centers=n, nstart=3)
mat <- cbind(mat,grwords$cluster)
mat
}
classifier <- function(mat){
set.seed(70)
trainset = sample(1:nrow(mat), trunc(0.7*nrow(mat)))
classifier = naiveBayes(mat[trainset, 1:(ncol(mat)-1)], as.factor(mat[trainset, ncol(mat)]))
trainpredicted = predict(classifier,mat[trainset, 1:(ncol(mat)-1)])
trm <- mean(trainpredicted==mat[trainset, ncol(mat)])
testpredicted = predict(classifier,mat[-trainset, 1:(ncol(mat)-1)])
tm <- mean(testpredicted==mat[-trainset, ncol(mat)])
list(classifier=classifier, trainAccuracy=trm, testAccuracy=tm, mat=mat)
}


#K-means and Support Vector Machine
classifier <- function(mat, n){
grwords <- kmeans(mat, centers=n, nstart=3)
mat <- cbind(mat,grwords$cluster)
set.seed(70)
trainset = sample(1:nrow(mat), trunc(0.7*nrow(mat)))
classifier <- svm(as.factor(mat[trainset, ncol(mat)])~mat[trainset, 1:(ncol(mat)-1)], kernel="linear",cost=0.1, scale=F)
trainpredicted = predict(classifier,mat[trainset, 1:(ncol(mat)-1)])
trm <- mean(trainpredicted==mat[trainset, ncol(mat)])
testpredicted = predict(classifier,mat[-trainset, 1:(ncol(mat)-1)])
tm <- mean(testpredicted==mat[-trainset, ncol(mat)])
list(classifier=classifier, trainAccuracy=trm, testAccuracy=tm, mat=mat)
}



################Function for dotproduct
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


##########Test prediction
testPred <- function(x,...){
trainset <- x$trainset
mat <- x$mat
classifier <- x$classifier
testpredicted = predict(classifier,mat[-trainset, 1:(ncol(mat)-1)])
table(testpredicted,mat[-trainset, ncol(mat)])
m <- mean(testpredicted==mat[-trainset, ncol(mat)]) # Accuracy
m
}


############################input/predict functions########################
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

input <- function(x,...){
classifier <- classifier4$classifier
mt <<- classifier4$mat
tdata <- x
corpdata <- Tokenizedata(tdata)
tdm4_t <- tdm(corpdata,4)
Mat4_t <- as.matrix(tdm4_t)
#Mat_t_n <- Norm.col(Mat4_t)

pred <- predict(classifier, Mat4_t)
pred <- as.matrix(pred)
m <- cbind(Mat4_t, pred)
mtes <- as.matrix(head(mt[(mt[,ncol(mt)]==pred[,1]),ncol(mt)],nrow(pred)))
mytest <- cbind(m,rownames(mtes))
mytest[,(ncol(mytest)-1):ncol(mytest)]
}                                            
am sorry for the    "4" "at the top of"     
i am sorry for      "4" "for the first time"
sorry for the error "4" "i would love to"

#######################Tokenizing and creating tdms######################
#Tokenizing the data
corpdata <- Tokenizedata(data)
rm(data)

#Tdm for unigram
tdm1 <- TermDocumentMatrix(corpdata, control = list(weighting = weightTfIdf))
Mat1 <- as.matrix(removeSparseTerms(tdm1,.99))

# Tdm for bigram
tdm2 <- tdm(corpdata, 2)
Mat2 <- as.matrix(tdm2)

#Tdm for trigram
tdm3 <- tdm(corpdata,3)
Mat3 <- as.matrix(tdm3)


#Tdm for quadigram
tdm4 <- tdm(corpdata,4)
Mat4 <- as.matrix(tdm4)

#######################################################################



  



#Need to find a way to display the next word (rather than all), may consider dotproduct for finding the exact index based on closeness 

rownames(Mat4[grep("at", rownames(Mat4)),]) #This will provide all the words along with all matching rownames
unlist(strsplit(rownames(Mat4[grep("at", rownames(Mat4)),]),split=" ",fixed=TRUE)) # This splits each rowname into words



#Storing the models using Markov Chains

#Designing back off models to estimate the probability of unobserved n-grams

