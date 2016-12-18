###################
## read in stopwords from core nlp
###################

# read in the stopwords.txt file 
# download https://github.com/stanfordnlp/CoreNLP/blob/master/data/edu/stanford/nlp/patterns/surface/stopwords.txt
stopwords <- read.table(file="../stopwords.txt",quote="",stringsAsFactors=FALSE)


###################
## read in first 300 positive and first 300 negative reviews
###################

# store directory that holds unzipped positive and negative review folders
# download reviews from http://www.cs.cornell.edu/people/pabo/movie-review-data/review_polarity.tar.gz
overallDirectory <- "../review_polarity/txt_sentoken/"

# read in names of all negative review files
negFilenames <- list.files(paste(overallDirectory,"neg",sep=""), pattern="*.txt", full.names=TRUE)

# read in names of all positive review files
posFilenames <- list.files(paste(overallDirectory,"pos",sep=""), pattern="*.txt", full.names=TRUE)

# obtain filenames of first 300 negative reviews
negTrain <- negFilenames[c(grep("cv0", negFilenames),grep("cv1", negFilenames),grep("cv2", negFilenames))]

# obtain filenames of first 300 positive reviews
posTrain <- posFilenames[c(grep("cv0", posFilenames),grep("cv1", posFilenames),grep("cv2", posFilenames))]


ReviewCorpus <- function(trainFiles) {
  # returns corpus of reviews 
  # Args: 
  #    trainFiles: a character vector with full filepaths
  #                to training reviews
  # 
  # Returns: a character vector where each element represents a review
	corpus <- c()
	for (i in 1:length(trainFiles)) {
		sentences <- readLines(trainFiles[i])
		thisDoc <- c()
		for (j in 1:length(sentences)) {
			thisDoc <- paste(thisDoc,sentences[j], sep=" ")
		}
		corpus <- c(corpus, thisDoc)
	}
	return(corpus)
}

# extract text of negative reviews
negCorpus <- ReviewCorpus(negTrain)

# extract text of positive reviews
posCorpus <- ReviewCorpus(posTrain)

##########
## extract unigrams in training reviews
###########

UnigramCounts <- function(corpus) {
  # counts the number of times each unigram appears in a review
  # Args:
  #    corpus: a character vector, with each element containing
  #            the text of a review
  # Returns:
  #    a data frame with one row per unigram in each review
	
  # create empty data frame
  wordCount <- data.frame(tokenList=character(),
                          freq=numeric(), 
                          stringsAsFactors=FALSE) 
                          
  # iterate through reviews                      
  for (i in 1:length(corpus)) { 
	if (i %% 50 == 0) {
		print(i)
    }
    # split unigrams by spaces
    tokenList <- strsplit(corpus[i]," ")
    
    # count times each unigram appears in this review
    tokenCt <-as.data.frame(table(tokenList),stringsAsFactors=FALSE)
    
    # append all unigram counts from this review to wordCount dataframe
    wordCount <- rbind(wordCount, tokenCt)
	
  }  
  return(wordCount)  
}

# count number of times unigrams appear in negative reviews
negUnigramCounts <- UnigramCounts(negCorpus)

# count number of times unigrams appear in positive reviews
posUnigramCounts <- UnigramCounts(posCorpus)

# limit unigram counts to stopwords
negStopwordCounts <- negUnigramCounts[negUnigramCounts$tokenList %in% stopwords$V1,]
posStopwordCounts <- posUnigramCounts[posUnigramCounts$tokenList %in% stopwords$V1,]

###############
## is p(stopword) different for positive and negative reviews?
## compare p(stopword|neg) and p(stopword|pos)
###############

# count number of reviews with stopwords
negStopwordCountsCorpus <- as.data.frame(table(negStopwordCounts$tokenList),stringsAsFactors=FALSE)
posStopwordCountsCorpus <- as.data.frame(table(posStopwordCounts$tokenList),stringsAsFactors=FALSE)

# list of all unique stopwords in positive and negative reviews
posAndNegStopwords <- unique(c(negStopwordCountsCorpus$Var1,posStopwordCountsCorpus$Var1))

# run a test to compare p(stopword|neg) and p(stopword|pos)
# null hypothesis = p(stopword|neg) - p(stopword|pos) = 0
# test statistic = z = (pHat_pos - pHat_neg) / sqrt(pHat*(1-pHat)*(1/nPos - 1/nNeg)) 
# return p(Z <= z), assuming Z~N(0,1)
# if p(Z <= z) < 0.025 or > 0.975, we can reject the null hypothesis
# at the 95% level of significance
# reference: https://onlinecourses.science.psu.edu/stat414/node/268

# stores p(Z <= z) for each stopword
pValue <- c()

# iterate through stopwords in positive reviews
for (i in 1:length(posAndNegStopwords)) {
	
	thisStopword <- posAndNegStopwords[i]
	
    # count times thisStopword appears in positive reviews
    if (nrow(posStopwordCountsCorpus[posStopwordCountsCorpus$Var1== thisStopword,])==0) {
		posCount <- 0
	} else {
		posCount <- posStopwordCountsCorpus$Freq[posStopwordCountsCorpus $Var1== thisStopword]
	}
	
	# count times thisStopword appears in negative reviews
	if (nrow(negStopwordCountsCorpus[negStopwordCountsCorpus$Var1== thisStopword,])==0) {
		negCount <- 0
	} else {
		negCount <- negStopwordCountsCorpus$Freq[negStopwordCountsCorpus $Var1== thisStopword]
	}
	
	# prepare data for proportion test
	pHatPos <- posCount / length(posCorpus)
	pHatNeg <- negCount / length(negCorpus)
	pHat <- (posCount + negCount) / (length(posCorpus) + length(negCorpus))
	nPos <- length(posCorpus)
	nNeg <- length(negCorpus)
	
	#calculate z
	z <- (pHatNeg - pHatPos) / sqrt( (pHat*(1-pHat)*(1/nPos + 1/nNeg) ))
	pValue <- c(pValue,pnorm(z)) 
	 
}

###############
## identify stopwords where we reject null hypothesis
## that stopword status and review type are independent
###############

# bind stopwords and their associated p-values
significant <- data.frame(posAndNegStopwords, pValue,stringsAsFactors=FALSE)

# identify stopwords with p values < 0.05
significant <- significant[is.na(significant$pValue)==FALSE & (significant$pValue < 0.025 | significant$pValue > .975),]

# merge on counts of reviews with each stop word
significant <- merge(significant, negStopwordCountsCorpus,by.x="posAndNegStopwords",by.y="Var1",all.x=TRUE)
significant <- merge(significant, posStopwordCountsCorpus,by.x="posAndNegStopwords",by.y="Var1",all.x=TRUE)

# clean up data frame for easier review
significant <- significant[order(significant$pValue),]
names(significant)[names(significant)=="Freq.x"] <- "countNegReviews"
names(significant)[names(significant)=="Freq.y"] <- "countPosReviews"
significant[is.na(significant)] <- 0
significant <- significant[order(significant$posAndNegStopwords),]

write.table(significant,row.names=FALSE,sep=",",file="significant_stopwords_chart_git.txt")
