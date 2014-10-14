### emot_hstg.R file ###
library(tm) # load R package tm: framework for text mining applications
library(RTextTools) # load R package RTextTools: Automatic Text Classification via Supervised Learning
library(rminer)  # load R package rminer: Application of Data Mining Methods

load("preproc.RData") # load preprocessing objects
load("sent_analysis.RData") # load sentiment analysis objects

# function that calculates emoticon score (number of positive emoticons - number of negative emoticons)
scr_emot <- function (vec) {
	if (length(vec)>0) {
		emot_pos<-length(vec[grepl("(^|[[:space:]]|[[:punct:]])(([<>]?[:;=8][-o*']?([)]|[]]|[dDpP}]))|(([)]|[]]|[dDpP}])[-o*']?[:;=8][<>]?))([[:punct:]]|$|[[:space:]])",vec)])
		emot_neg<-length(vec[grepl("(^|[[:space:]]|[[:punct:]])(([<>]?[:;=8][-o*']?([(]|[[]|[/:{|\\]))|(([(]|[[]|[/:{|\\])[-o*']?[:;=8][<>]?))([[:punct:]]|$|[[:space:]])",vec)])
		return(emot_pos-emot_neg)
		} else return (0)
	}
emot_score<-unlist(lapply(l_emot, scr_emot))
DTM<-cbind(DTM,emot_score) # add the emoticon score to Document-Term matrix
colnames(DTM)[ncol(DTM)]<-"emot_score"

# function that verifies the presence or absence of each element of a list of hashtags
prs_hstg <- function (vec, n_hstg) {
	return(as.numeric(n_hstg %in% vec))
	}
hstg<-unique(unlist(l_hstg)) # hashtags vector
DTM_hstg <- unlist(lapply(l_hstg,prs_hstg,hstg))
DTM_hstg <- matrix(DTM_hstg,ncol=length(hstg),byrow=TRUE)
colnames(DTM_hstg)<-hstg
DTM<-cbind(DTM,DTM_hstg) # add hashtag matrix to Document-Term matrix

# utilization of SVM method
container <- create_container(DTM,twt$sentiment,trainSize=1:n_training, testSize=(n_training+1):total, virgin=FALSE) # creation of a container
svm_model <- train_model(container,"SVM")
svm_results <- classify_model(container,svm_model)
# evaluation metrics
svm_eval=mmetric(as.factor(twt$sentiment[(n_training+1):total]),svm_results[,1],metric=c("ACC","TPR","PRECISION","F1"))
print(mean(svm_eval[c("F11","F12")])) # macro-averaged F-score
