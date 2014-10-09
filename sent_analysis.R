### sent_analysis.R file ###
library(tm) # load R package tm: framework for text mining applications
library(RTextTools) # load R package RTextTools: Automatic Text Classification via Supervised Learning
library(rminer)  # load R package rminer: Application of Data Mining Methods
library(slam) # load R package slam: Data structures and algorithms for sparse arrays and matrices
library(XLConnect) # load XLConnect package: Manipulation of Excel files

twt<-read.csv("twt_sample.csv",stringsAsFactors=FALSE) # read a CSV file containing tweets about stock market
twt$created_at<-strptime(twt$created_at,"%Y-%m-%d %H:%M:%S") # convert "created_at" column from character to time
twt<-twt[order(twt$created_at),] # order messages by creation time

# pre-process some usual expressions in stock market microblog conversations (punctuation removal will eliminate these expressions)
twt$text<-gsub("(^|[[:space:]])(@)([[:space:]])","\\1at\\3",twt$text) # substitute the isolated "@" by "at"
twt$text<-gsub("(^|[[:space:]])(@)([$]?[0-9.]+)([[:space:]]|[[:punct:]]|$)","\\1at \\3\\4",twt$text) # transform @<number> into "at <number>":
twt$text<-gsub("(^|[[:space:]])([+])([$]?[.]?[[:digit:]]+)","\\1more \\3",twt$text) # substitute "+<number>" by "more <number>"
twt$text<-gsub("(^|[[:space:]])([-])([$]?[.]?[[:digit:]]+)","\\1less \\3",twt$text) # substitute "-<number>" by "less <number>"
twt$text<-gsub("(^|[[:space:]])([~])([[:space:]]?)([$]?[.]?[[:digit:]]+)","\\1nearly \\4",twt$text) # substitute "~<number>" by "nearly <number>"
twt$text<-gsub("(^|[[:space:]])([#])([[:space:]]?)(([[:digit:]])+)","\\1number \\4",twt$text) # substitute "#<number>" by "number <number>"
twt$text<-gsub("([[:digit:]])([%])","\\1 percent",twt$text) # substitute "<number>%" by "<number> percent"
twt$text<-gsub("[$]?[[:digit:]]*[.]?[[:digit:]]+","num",twt$text) # substitute all numbers by the word "num"
twt$text<-gsub("[$][[:alpha:]]+","cshtg",twt$text) # substitute cashtags by the word "cshtg"

# create a list containing the emoticons of each tweet
m<-gregexpr("(^|[[:space:]]|[[:punct:]])(([<>]?[:;=][-o*']?([)]|[(]|[]]|[[]|[dDpP}]|[/:{|\\]))|(([)]|[]]|[(]|[[]|[dDpP}]|[/:{|\\])[-o*']?[:;=][<>]?))([[:punct:]]|$|[[:space:]])",twt$text)
l_emot<-regmatches(twt$text,m)
twt$text<-gsub("(^|[[:space:]]|[[:punct:]])(([<>]?[:;=][-o*']?([)]|[(]|[]]|[[]|[dDpP}]|[/:{|\\]))|(([)]|[]]|[(]|[[]|[dDpP}]|[/:{|\\])[-o*']?[:;=][<>]?))([[:punct:]]|$|[[:space:]])","\\1 ",twt$text) # remove these emoticons from text
# create a list containing the hashtags of each tweet
m<-gregexpr("#\\w+",twt$text)
l_hstg<-regmatches(twt$text,m)
twt$text<-gsub("#\\w+","",twt$text) # remove hashtags from text

DTM <- create_matrix(twt$text, language="english", removeStopwords=FALSE, removePunctuation=TRUE, removeSparseTerms=0.99, toLower=TRUE, stemWords=TRUE, weighting=weightTf) # creation of a Document-Term Matrix with stemming, conversion to lowercase, removal of punctuation and sparse terms and utilization of "Term frequency" weighting for unigrams
total<-nrow(twt) # total number of tweets
n_training<-round(0.75*total) # number of messages of the training set (first 75%)

### machine learning methods for sentiment analysis
# application of Support Vector Machines (SVM) method
container <- create_container(DTM,twt$sentiment,trainSize=1:n_training, testSize=(n_training+1):total, virgin=FALSE) # creation of a container using the Document-Term Matrix, the sentiment labels and the definition of the training and test set size
svm_model <- train_model(container,"SVM") # train SVM model
svm_results <- classify_model(container,svm_model) # predict test set using SVM model
# evaluation metrics
svm_eval=mmetric(as.factor(twt$sentiment[(n_training+1):total]),svm_results[,1],metric=c("ACC","TPR","PRECISION","F1"))
print(mean(svm_eval[c("F11","F12")])) # macro-averaged F-score

# application of Naive Bayes (NB) method
df<-as.data.frame(as.matrix(DTM),stringsAsFactors=FALSE) # convert Document-Term matrix to data.frame
rownames(df)<-NULL
df<-cbind(df,as.factor(twt$sentiment)) # add a sentiment column
colnames(df)[ncol(df)]<-"sentiment"
nb_model=fit(sentiment~.,df[1:n_training,],model="naivebayes",task="class") # train NB model
nb_results<-predict(nb_model,df[(n_training+1):total,]) # predict test set using NB model
# evaluation metrics
nb_eval=mmetric(as.factor(twt$sentiment[(n_training+1):total]),nb_results,metric=c("ACC","TPR","PRECISION","F1"))
print(mean(nb_eval[c("F11","F12")])) # macro-averaged F-score

### unsupervised sentiment classification
# process financial lexicon (FIN) created by Loughran and McDonald
FIN_n<-read.csv("http://www3.nd.edu/~mcdonald/Data/Finance_Word_Lists/LoughranMcDonald_Negative.csv", stringsAsFactors = FALSE)[,1] # read negative word list
FIN_p<-read.csv("http://www3.nd.edu/~mcdonald/Data/Finance_Word_Lists/LoughranMcDonald_Positive.csv", stringsAsFactors = FALSE)[,1] # read positive word list
lex_FIN<-as.data.frame(rbind(cbind(FIN_p,rep(1,length(FIN_p))), cbind(FIN_n,rep(-1,length(FIN_n)))),stringsAsFactors=FALSE) # create a data frame with all words and respective sentiment label (1 for positive and -1 for negative)
colnames(lex_FIN)<-c("stem","sentiment")
lex_FIN$stem<-tolower(lex_FIN$stem) # convert to lowercase
lex_FIN$stem<-stemDocument(lex_FIN$stem) # stem all words
lex_FIN<-unique(lex_FIN) # remove duplicated elements
# process Harvard General Inquirer (GI) lexicon
download.file(url="http://www.wjh.harvard.edu/~inquirer/inquirerbasic.xls", destfile="inquirerbasic.xls", mode="wb") # download GI lexicon
GI_book = loadWorkbook("inquirerbasic.xls") # load XLS file containing GI lexicon
GI_sheet = readWorksheet(GI_book,sheet=1,header=TRUE) # read lexicon sheet
GI_n<-GI_sheet$Entry[!is.na(GI_sheet$Negativ)] # select all words from "Negativ" category
GI_p<-GI_sheet$Entry[!is.na(GI_sheet$Positiv)] # select all words from "Positiv" category
lex_GI<-as.data.frame(rbind(cbind(GI_p,rep(1,length(GI_p))), cbind(GI_n,rep(-1,length(GI_n)))),stringsAsFactors=FALSE) # create a data frame with all words and respective sentiment label (1 for positive and -1 for negative)
colnames(lex_GI)<-c("stem","sentiment")
lex_GI$stem<-tolower(lex_GI$stem) # convert to lowercase
lex_GI$stem<-gsub("#(.*)$","",lex_GI$stem) # process repeated words (e.g., ABOUT#1, ABOUT#2)
lex_GI$stem<-stemDocument(lex_GI$stem) # stem all words
lex_GI<-unique(lex_GI) # remove duplicated elements
lex_GI<-lex_GI[!(lex_GI$stem %in% lex_GI$stem[duplicated(lex_GI$stem)]),] # exclude stems appearing in both lists
# creation of a DocumentTextMatrix with:
#	- minimum length of letters = 2 (minimum length of GI and FIN words)
#	- without the removal of sparse words to match all GI and FIN items
DTM_lex <- create_matrix(twt$text, language="english", removeStopwords=FALSE, minWordLength=2, removePunctuation=TRUE, toLower=TRUE, stemWords=TRUE, weighting=weightTf)
DTM_GI<-DTM_lex[,which(colnames(DTM_lex) %in% lex_GI$stem)] # creation of a DocumentTextMatrix containing only GI terms
DTM_FIN<-DTM_lex[,which(colnames(DTM_lex) %in% lex_FIN$stem)] # creation of a DocumentTextMatrix containing only FIN elements
l_GI<-lex_GI[lex_GI$stem %in% colnames(DTM_GI),] # exclude GI elements absent from all tweets
l_FIN<-lex_FIN[lex_FIN$stem %in% colnames(DTM_FIN),] # exclude FIN elements absent from all tweets
l_GI<-l_GI[order(l_GI$stem),] # order GI lexicon (DocumentTextMatrix columns are ordered alphabetically)
l_FIN<-l_FIN[order(l_FIN$stem),]# order FIN lexicon (DocumentTextMatrix columns are ordered alphabetically)
# score each message by summing each word sentiment in that tweet
#		word sentiment = number of occurrences of the word * word sentiment (1 for positive,
#				-1 for negative, 0 for neutral ou not present in lexicon)
total_GI<-tcrossprod_simple_triplet_matrix(DTM_GI, t(as.simple_triplet_matrix(l_GI$sentiment)))
total_FIN<-tcrossprod_simple_triplet_matrix(DTM_FIN, t(as.simple_triplet_matrix(l_FIN$sentiment)))
total_GI[total_GI>0]<-"positive" # tweets with positive score are considered positive
total_GI[total_GI<0]<-"negative" # tweets with negative score are considered negative
# evaluation metrics
GI_eval=mmetric(as.factor(twt$sentiment[(n_training+1):total]),as.factor(total_GI[(n_training+1):total]),metric=c("ACC","TPR","PRECISION","F1"))
print(mean(GI_eval[c("F11","F12")]))
total_FIN[total_FIN>0]<-"positive" # tweets with positive score are labeled positive
total_FIN[total_FIN<0]<-"negative" # tweets with negative score are labeled negative
# evaluation metrics
FIN_eval=mmetric(as.factor(twt$sentiment[(n_training+1):total]),as.factor(total_FIN[(n_training+1):total]),metric=c("ACC","TPR","PRECISION","F1"))
print(mean(FIN_eval[c("F11","F12")])) # macro-averaged F-score

### inclusion of emoticons and hashtags features in a SVM classifier
# function that calculates emoticon score (number of positive emoticons - number of negative emoticons)
tot_emot <- function (vec) {
	if (length(vec)>0) {
		emot_pos<-length(vec[grepl("(^|[[:space:]]|[[:punct:]])(([<>]?[:;=8][-o*']?([)]|[]]|[dDpP}]))|(([)]|[]]|[dDpP}])[-o*']?[:;=8][<>]?))([[:punct:]]|$|[[:space:]])",vec)])
		emot_neg<-length(vec[grepl("(^|[[:space:]]|[[:punct:]])(([<>]?[:;=8][-o*']?([(]|[[]|[/:{|\\]))|(([(]|[[]|[/:{|\\])[-o*']?[:;=8][<>]?))([[:punct:]]|$|[[:space:]])",vec)])
		return(emot_pos-emot_neg)
		} else return (0)
	}
emot_score<-unlist(lapply(l_emot, tot_emot))
DTM<-cbind(DTM,emot_score) # add the emoticon score
colnames(DTM)[ncol(DTM)]<-"emot_score"
# function to verify the presence of each element of a list of hashtags
vrf_hstg <- function (vec, n_hstg) {
	return(as.numeric(n_hstg %in% vec))
	}
hstg<-unique(unlist(l_hstg)) # hashtags list
DTM_hstg <- unlist(lapply(l_hstg,vrf_hstg,hstg))
DTM_hstg <- as.simple_triplet_matrix(matrix(DTM_hstg,ncol=length(hstg),byrow=TRUE))
colnames(DTM_hstg)<-hstg
DTM<-cbind(DTM,DTM_hstg) # add Hashtag matrix
# utilization of SVM method
container <- create_container(DTM,twt$sentiment,trainSize=1:n_training, testSize=(n_training+1):total, virgin=FALSE) # creation of a container
svm_model <- train_model(container,"SVM")
svm_results <- classify_model(container,svm_model)
# evaluation metrics
svm_eval=mmetric(as.factor(twt$sentiment[(n_training+1):total]),svm_results[,1],metric=c("ACC","TPR","PRECISION","F1"))
print(mean(svm_eval[c("F11","F12")])) # macro-averaged F-score
