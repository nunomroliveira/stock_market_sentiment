### sentim_analysis.R file ###
library(tm) # load R package tm: framework for text mining applications
library(RTextTools) # load R package RTextTools: Automatic Text Classification via Supervised Learning
library(rminer)  # load R package rminer: Application of Data Mining Methods
library(slam) # load R package slam: Data structures and algorithms for sparse arrays and matrices

load("preproc.RData") # load preprocessing objects
total<-nrow(twt) # total number of tweets
n_training<-round(0.75*total) # number of messages of the training set (first 75%)

### machine learning methods for sentiment analysis
# application of Support Vector Machines (SVM) method
container <- create_container(DTM,twt$sentiment,trainSize=1:n_training, testSize=(n_training+1):total, virgin=FALSE) # creation of a container using the Document-Term Matrix, the sentiment labels and the definition of the training and test set size
svm_model <- train_model(container,"SVM") # train SVM model
svm_results <- classify_model(container,svm_model) # predict test set using SVM model

# application of Naive Bayes (NB) method
df<-as.data.frame(as.matrix(DTM),stringsAsFactors=FALSE) # convert Document-Term matrix to data.frame
rownames(df)<-NULL
df<-cbind(df,as.factor(twt$sentiment)) # add a sentiment column
colnames(df)[ncol(df)]<-"sentiment"
nb_model=fit(sentiment~.,df[1:n_training,],model="naivebayes",task="class") # train NB model
nb_results<-predict(nb_model,df[(n_training+1):total,]) # predict test set using NB model

### unsupervised sentiment classification
lex_GI<-read.csv("GI.csv",stringsAsFactors=FALSE) # read CSV file containing the processed GI lexicon
lex_FIN<-read.csv("FIN.csv",stringsAsFactors=FALSE) # read CSV file containing the processed FIN lexicon
# creation of a Document-Text matrix with:
#	- minimum length of letters = 2 (minimum length of GI and FIN words)
#	- without the removal of sparse words to match all GI and FIN items
DTM_lex <- create_matrix(twt$text, language="english", removeStopwords=FALSE, minWordLength=2, removePunctuation=TRUE, toLower=TRUE, stemWords=TRUE, weighting=weightTf)
DTM_GI<-DTM_lex[,which(colnames(DTM_lex) %in% lex_GI$stem)] # creation of a Document-Text matrix containing only GI terms
DTM_FIN<-DTM_lex[,which(colnames(DTM_lex) %in% lex_FIN$stem)] # creation of a Document-Text matrix containing only FIN elements
l_GI<-lex_GI[lex_GI$stem %in% colnames(DTM_GI),] # exclude GI elements absent from all tweets
l_FIN<-lex_FIN[lex_FIN$stem %in% colnames(DTM_FIN),] # exclude FIN elements absent from all tweets
l_GI<-l_GI[order(l_GI$stem),] # order GI lexicon (Document-Text matrix columns are ordered alphabetically)
l_FIN<-l_FIN[order(l_FIN$stem),]# order FIN lexicon (Document-Text matrix columns are ordered alphabetically)
# score each message by summing each word sentiment in that tweet
#		word sentiment = number of occurrences of the word * word sentiment (1 for positive,
#				-1 for negative, 0 for neutral ou not present in lexicon)
total_GI<-tcrossprod_simple_triplet_matrix(DTM_GI, t(l_GI$sentiment))
total_FIN<-tcrossprod_simple_triplet_matrix(DTM_FIN, t(l_FIN$sentiment))
# tweets with positive score are labeled positive; tweets with negative score are labeled negative
total_GI[total_GI>0]<-"positive"
total_GI[total_GI<0]<-"negative"
total_FIN[total_FIN>0]<-"positive"
total_FIN[total_FIN<0]<-"negative"

save(list=c("total", "n_training", "svm_results", "nb_results", "total_GI", "total_FIN", "DTM_GI", "DTM_FIN"), file="sent_analysis.RData") # save R objects required for future tasks
