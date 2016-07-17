#Final Project

#######################################Loading Libraries######################################

#Libraries for text mining
library(tm)
library(SnowballC)
library(NLP)
library(e1071)


#########################################Data loading and subsetting#############################

setwd("C:\\R\\Coursera\\CapstoneProject\\Project")
setwd("C:\\R\\Coursera\\CapstoneProject\\Project\\Data")


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


load data:
wordlist <- readRDS("wordlist.rds")
mat1 <- readRDS("mat1.rds")
mat2 <- readRDS("mat2.rds")
mat3 <- readRDS("mat3.rds")
mat4 <- readRDS("mat4.rds")


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



###########Function to build tdms/matrix

tdm <- function(data, n){
ngramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), n), paste, collapse = " "), use.names = FALSE)

  tdm <- TermDocumentMatrix(corpdata, control = list(tokenize = ngramTokenizer, weighting = function(x) weightSMART(x, spec = "apn")))
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
input <- function(str){
    str <- gsub("\\d+",         "", str, perl = TRUE)
    str <- gsub("[^\\w\\s'#]+", "", str, perl = TRUE)
    str <- gsub("(['#])\\1", "\\1", str, perl = TRUE)
    str <- gsub("_+",           "", str, perl = TRUE)
    str <- gsub("\\s+",        " ", str, perl = TRUE)
    str <- tolower(str)

words <- strsplit(str, split="\\s+")[[1]]
if(length(words)>1){
index <- vector(mode="list")
    for (num in length(words):1) {
	idx <- match(words[num], wordlist)
        #print(paste("word idx:", idx, " -> ", words[num]))
        if (!is.na(idx)) index <- c(idx, index)
	}
	return(wordlist[unlist(index)])
    

}
    
return(words) 
}






for(i in seq_along(words){
index <- grep(str[i], wordlist)

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
tdm1 <- TermDocumentMatrix(corpdata, control = list(weighting = function(x) weightSMART(x, spec = "apc")))
mat1 <- as.matrix(removeSparseTerms(tdm1,.99))

# Tdm for bigram
tdm2 <- tdm(corpdata, 2)
mat2 <- as.matrix(tdm2)

#Tdm for trigram
tdm3 <- tdm(corpdata,3)
mat3 <- as.matrix(tdm3)


#Tdm for quadigram
tdm4 <- tdm(corpdata,4)
mat4 <- as.matrix(tdm4)



########################################################################
Steps for Shiny app

mat1 <- readRDS("mat1.rds")
mat2 <- readRDS("mat2.rds")
mat3 <- readRDS("mat3.rds")
mat4 <- readRDS("mat4.rds")

transform into data table with relevant weighted frequencies:

dtable <- function(mat){

matav <- apply(mat, 1, mean)
matav <- matav[order(matav)]
m <- as.matrix(matav)
gram_dtable <- data.table(m, keep.rownames=TRUE)
setnames(gram_dtable, c("words", "probs"))
}

splitwords <- function(gram,n){
	gram <- data.frame(gram)
	colmn<-c()
	col1 <-c()
	for(j in 1:n){
	
	colmn[j] <- paste0("word",j)
	}

gram[,colmn] = NA

 
nr <- nrow(gram)

for (i in 1:nr){
	
	 word <- strsplit(gram$words[i], split=" ")[[1]]
 	
	for (k in 1:length(word)){
             col1 <- paste0("word",k)		
	     gram[,col1][i] <- word[k]
		}
	}
gram$scores <- gram$probs
gram$words <- NULL
gram$probs <- NULL
gram
}




gram1 <- readRDS("gram1.rds")
gram2 <- readRDS("gram2.rds")
gram3 <- readRDS("gram3.rds")
gram4 <- readRDS("gram4.rds")



#Function for returning the cleaned words for word match
input <- function(str){
    str <- gsub("\\d+",         "", str, perl = TRUE)
    str <- gsub("[^\\w\\s'#]+", "", str, perl = TRUE)
    str <- gsub("(['#])\\1", "\\1", str, perl = TRUE)
    str <- gsub("_+",           "", str, perl = TRUE)
    str <- gsub("\\s+",        " ", str, perl = TRUE)
    str <- tolower(str)

words <- strsplit(str, split="\\s+")[[1]]
if(length(words)>1){
index <- vector(mode="list")
    for (num in length(words):1) {
	idx <- match(words[num], wordlist)
        #print(paste("word idx:", idx, " -> ", words[num]))
        if (!is.na(idx)) index <- c(idx, index)
	}
	return(wordlist[unlist(index)])
    

}
    
return(words) 
}







Algorithm
1. Search for the last word in gram4 > colum1>column2>column3>column4
2. If found in column1>column2, column2>column3, column3>column4
3. If coulmn4> next row column1
4. If not found in gram4, search for the word in gram3 colum1>colun2>column3
5. If found in column1>column2, column2>column3
6. If coulmn3> next row column1 
7. If not found in gram3, search for the word in gram2 colum1>colun2
8. If coulmn2> next row column1
9. If not found in gram2, search for the word in gram1 colum1> next row column1 


Logic:

gram4:

w1 > col1 yes, subset rows, >> w2 > col2 yes, subset rows, >> w3>col3 yes, subset rows, >> predict w4 > col4
w1 > col1 no >> w2 > col1 yes, subset rows, >> w3>col2 yes, subset rows, >> predict w4 > col3
w2 > col1 no, >> w2>col2 yes, subset rows, >> w3>col3 yes, subset rows, >> predict w4 > col4

search <- function(words){

l <- length(words)

if (l>=3){
sub_set <- gram4
words <- words[(length(words)-2):length(words)]
for(i in 1:(length(words)-1)){
  if(words[i] %in% sub_set[,i]){
	mch <- match(sub_set[,i], words[i])
	rn <- row.names((sub_set[!is.na(mch),]))
    	sub_set <- sub_set[rn,]
     if(words[i+1] %in% sub_set[,i+1]){
	mch <- match(sub_set[,i+1], words[i+1])
	rn <- row.names((sub_set[!is.na(mch),]))
    	sub_set <- sub_set[rn,]
	if(i==2) {return(sub_set$word4[1])}
	if(words[i+2] %in% sub_set[,i+2]){
	  mch <- match(sub_set[,i+2], words[i+2])
	  rn <- row.names((sub_set[!is.na(mch),]))
    	  sub_set <- sub_set[rn,]
	  return(sub_set$word4[1])
		}
	    }				
	}
  }
words <- words[2:3]
l <- length(words)
return(l)
 }

if (l==2){
sub_set <- gram3    
if(words[1] %in% sub_set[,1]){
    mch <- match(sub_set[,1], words[1])
    rn <- row.names((sub_set[!is.na(mch),]))
    sub_set <- sub_set[rn,]
    if(words[2] %in% sub_set[,2]){
	mch <- match(sub_set[,2], words[2])
	rn <- row.names((sub_set[!is.na(mch),]))
    	sub_set <- sub_set[rn,]
	return(sub_set$word3[1])
    	}
	
      }
words <- words[3]
l <- 1
return(l)
}

if (l==1){
   sub_set1 <- gram2
   sub_set <- gram1
  
   if(words[1] %in% sub_set1[,1]){      
   mch <- match(sub_set1[,1], words[1])
   rn <- row.names((sub_set1[!is.na(mch),]))
   sub_set1 <- sub_set1[rn,]
   return(sub_set1$word2[1])
	}
   if(words[1] %in% sub_set2[,1]){      
   mch <- match(sub_set1[,1], words[1])
   rn <- row.names((sub_set1[!is.na(mch),]))
   return(sub_set1$word2[1+rn[1]])
	}


  }

}

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



