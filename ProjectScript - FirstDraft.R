#Final Project

#######################################Loading Libraries######################################

#Libraries for text mining
library(tm)
library(SnowballC)
library(NLP)
library(e1071)


#########################################Data loading and subsetting#############################

setwd("C:\\R\\Coursera\\CapstoneProject\\Project")

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

########################################Developing functions######################################

############Function to tokenize the data

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
corpdata <- tm_map(corpdata, removeWords, c("NA","NA.1","NA.2","NA.3","NA.4"))
corpdata <- sapply(corpdata, function(row) iconv(row, "latin1", "ASCII", sub=""))
corpdata <- Corpus(VectorSource(corpdata))
#Converting to PlainTextDocument 
corpdata <- tm_map(corpdata, PlainTextDocument)
corpdata
}



###########Function to build tdms/matrix

tdm <- function(data, n){
ngramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), n), paste, collapse = " "), use.names = FALSE)

  tdm <- TermDocumentMatrix(corpdata, control = list(tokenize = ngramTokenizer, weighting = function(x) weightSMART(x, spec = "apc")))
tdm <- removeSparseTerms(tdm,.999)
tdm
}


a <- a + (1-a)*(tf)/max(tf) - this is called maximum tf normalization in order to  mitigate the anomaly of higher frequency of terms in 
a large document merely due to the repeatition over and over again. Here a is a smoothing term (ranges from 0 to 1), we choose 0.5.

p <- max{0, log((N-df)/df)}, we can also use t <- log(N/df), here p is probability term for idf (inverse document frequency)

c <- cosine normalization, we can use n for none, u for pivoted unique, or b for byte size 




##############Function for normalizing the column

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


################Function for dotproduct/result

dotproduct <- function(temp, str) {
dp<- (temp[grep(str,rownames(temp)),]) %*% t(temp)
dp <- apply(dp,2,sum)
dp <- dp[order(dp, decreasing=T)]
res <- head(dp)
if(sum(res)==0){return("No match found! sorry, try some other word")}

res <- res[2:5]
return(res)
}


###Another option

output <- function(tdm, str){
 op <- head(findAssocs(x=tdm,term=str,corlimit=0.1)[[1]],4)
if(length(op)==0){return("No match found! sorry, try some other word")}

return(op)
}

##################################################Algorithm/steps#################################

1. Load all libraries(just one, "tm")
2. Load the data
3. Tokenize and cleane the data
4. Create TDM with ngrams
5. Create matrices for each ngram
6. Normalize each matrix columnwise
7. Check if the input is a single/multiple word
8. Supply the matrix and the word to dotproduct function
10. AND print the results


Example of unigram:
Dataset has already been created "corpdata"

tdm <- TermDocumentMatrix(corpdata,control = list(weighting = function(x) weightSMART(x, spec = "apc")))
tdm1 <- removeSparseTerms(tdm, 0.999)
mt <- as.matrix(tdm1)
mt_n <- Norm.col(mt)
df <- data.frame(mt_n)

###Optional
# mts <- apply(mt,1,sum); mts_n <- scale(mts, center=T, scale=T); 
###

dp <- dotproduct(mt_n, "aaa")





##############################################################NEED TO WORK ON HOW TO USE NGRAM OF HIGHER DEGREE##################################################

############################input/predict functions########################
#So, we can write a function for combination of words

predict <- function(x){
l <- sapply((gregexpr("\\W+", x)), length)+1
corpdata <- corpdata
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
Redundant functions:

words <- function(df=df,dp) {

words <- rownames(df[tail(dp$order,5),])
words <- words[1:4]
score <- tail(dp$dp,5)
score <- score[1:4]

res <- rbind(words,score)
res
}

###Alternatively

dotproduct <- function(temp, str) {
rowcount= nrow(temp)
colcount  = ncol(temp)
dotproducts = numeric(rowcount)

for( i in 1:rowcount){
dotproducts[i] =  sum(temp[grep(str,rownames(temp)),]*temp[i,])
}
ordering = order(dotproducts)
#dp <- sort(dotproducts, decreasing=T)
list(order=ordering, dp=dotproducts)
}


words <- function(df=df,dp) {

words <- rownames(df[tail(dp$order,5),])
words <- words[1:4]
score <- tail(dp$dp,5)
score <- score[1:4]

res <- rbind(words,score)
res
}






Need to look at these:

Ufreq[which(Ufreq$grams=="happiest"),] 





Websites to refer:
http://norvig.com/spell-correct.html

https://en.wikipedia.org/wiki/Trie

http://stackoverflow.com/questions/11449115/algorithms-theory-behind-predictive-autocomplete
