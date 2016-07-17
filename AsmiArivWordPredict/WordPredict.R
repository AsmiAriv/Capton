library(tm)
library(data.table)

wordlist <- readRDS("wordlist.rds")
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

if(is.na(match(words, wordlist))) { words <- wordlist[floor(runif(1,5,2200))]
return(words)}   

return(words) 
}


#############################################(l>=3)########
predict <- function(words){
l <- length(words)

if (l>=3){
sub_set <- gram4
words <- words[(length(words)-2):length(words)]
for(i in 1:(length(words)-1)){
  if(!is.na(match(words[i], sub_set[,i]))){
	rn <- row.names((sub_set[sub_set[,i]==words[i],]))
    	sub_set <- sub_set[rn,]
     if(!is.na(match(words[i+1],sub_set[,i+1]))){
	rn <- row.names((sub_set[sub_set[,i+1]==words[i+1],]))
    	sub_set <- sub_set[rn,]
	if(i==2) {return(sub_set$word4[1])}
	if(!is.na(match(words[i+2], sub_set[,i+2]))){
	  rn <- row.names((sub_set[sub_set[,i+2]==words[i+2],]))
    	  sub_set <- sub_set[rn,]
	  return(sub_set$word4[1])
		}
	    }				
	}
  }
words <- words[2:3]
l <- length(words)
}

if (l==2){
sub_set <- gram3    
if(!is.na(match(words[1], sub_set[,1]))){
    mch <- match(sub_set[,1], words[1])
    rn <- as.numeric(row.names((sub_set[!is.na(mch),])))
    sub_set <- sub_set[rn,]
    	if(!is.na(match(words[2], sub_set[,2]))){
	rn <- row.names((sub_set[sub_set[,2]==words[2],]))
    	sub_set <- sub_set[rn,]
	return(sub_set$word3[1])
    	}
	
      }

words <- words[2]
l <- 1
}


if (l==1){
   sub_set1 <- gram2
   sub_set <- gram1
  
   if(!is.na(match(words, sub_set1[,1]))){      
   rn <- as.numeric(row.names((sub_set1[sub_set1$word1==words,])))
   sub_set1 <- sub_set1[rn,]
   return(sub_set1$word2[1])
	}
   else if(!is.na(match(words, sub_set[,1]))){      
   rn <- as.numeric(row.names((sub_set[sub_set$word==words,])))
   if(!is.na(sub_set$word[(rn[1]+1)]))return(sub_set$word[(rn[1]+1)])
   return(sub_set$word[(rn[1]-1)])
	}
     

  }

}

#######################################################################
wordcloudPlot <- function(str){ words <- c()
    rn <- as.numeric(row.names((gram1[gram1$word==str,])))   
for (i in rn:(rn-10){
if(!is.na(gram1$word[(rn+1)]))words[i] <- gram1$word[(rn[1]+1)]
}	
wordcloud(words)
}