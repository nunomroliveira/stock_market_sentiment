### evaluation.R file ###
library(tm) # load R package tm: framework for text mining applications
library(rminer)  # load R package rminer: Application of Data Mining Methods

load("preproc.RData") # load preprocessing objects
load("sent_analysis.RData") # load sentiment analysis objects

# evaluation metrics
svm_eval=mmetric(as.factor(twt$sentiment[(n_training+1):total]),svm_results[,1],metric=c("ACC","TPR","PRECISION","F1")) # SVM model evaluation 
nb_eval=mmetric(as.factor(twt$sentiment[(n_training+1):total]),nb_results,metric=c("ACC","TPR","PRECISION","F1")) # NB model evaluation
GI_eval=mmetric(factor(twt$sentiment[(n_training+1):total],levels=c("0","negative","positive")),as.factor(total_GI[(n_training+1):total]),metric=c("ACC","TPR","PRECISION","F1")) # GI model evaluation
FIN_eval=mmetric(factor(twt$sentiment[(n_training+1):total],levels=c("0","negative","positive")),as.factor(total_FIN[(n_training+1):total]),metric=c("ACC","TPR","PRECISION","F1")) # FIN model evaluation
tab_eval<-rbind(c(svm_eval[c("ACC","PRECISION2","TPR2","F12","PRECISION1","TPR1","F11")],mean(svm_eval[c("F11","F12")])),
	c(nb_eval[c("ACC","PRECISION2","TPR2","F12","PRECISION1","TPR1","F11")],mean(nb_eval[c("F11","F12")])),
	c(GI_eval[c("ACC","PRECISION3","TPR3","F13","PRECISION2","TPR2","F12")],mean(GI_eval[c("F12","F13")])),
	c(FIN_eval[c("ACC","PRECISION3","TPR3","F13","PRECISION2","TPR2","F12")],mean(FIN_eval[c("F12","F13")]))) # create matrix with evaluation results
colnames(tab_eval)<-c("Accuracy","Prec_Pos","Recall_Pos","F1_Pos","Prec_Neg","Recall_Neg","F1_Neg","F_avg")
rownames(tab_eval)<-c("SVM","NB","GI","FIN")
tab_eval<-round(tab_eval)

# exclude tweets without lexicon items in evaluation metrics calculation
num_GI<-rowSums(as.matrix(DTM_GI)[(n_training+1):total,]) # sum term frequency of GI items for each tweet of test set
print(length(num_GI[num_GI==0])/length(num_GI)*100) # test set tweets without GI items (% values)
GI_eval_exc=mmetric(factor(twt$sentiment[(n_training+1):total][num_GI>0],levels=c("0","negative","positive")),as.factor(total_GI[(n_training+1):total][num_GI>0]),metric=c("ACC","TPR","PRECISION","F1")) # GI model evaluation
num_FIN<-rowSums(as.matrix(DTM_FIN)[(n_training+1):total,]) # sum term frequency of FIN items for each tweet of test set
print(length(num_FIN[num_FIN==0])/length(num_FIN)*100) # test set tweets without FIN items (% values)
FIN_eval_exc=mmetric(factor(twt$sentiment[(n_training+1):total][num_FIN>0],levels=c("0","negative","positive")),as.factor(total_FIN[(n_training+1):total][num_FIN>0]),metric=c("ACC","TPR","PRECISION","F1")) # FIN model evaluation
tab_eval_exc<-rbind(c(svm_eval[c("ACC","PRECISION2","TPR2","F12","PRECISION1","TPR1","F11")],mean(svm_eval[c("F11","F12")])),
	c(nb_eval[c("ACC","PRECISION2","TPR2","F12","PRECISION1","TPR1","F11")],mean(nb_eval[c("F11","F12")])),
	c(GI_eval_exc[c("ACC","PRECISION3","TPR3","F13","PRECISION2","TPR2","F12")],mean(GI_eval_exc[c("F12","F13")])),
	c(FIN_eval_exc[c("ACC","PRECISION3","TPR3","F13","PRECISION2","TPR2","F12")],mean(FIN_eval_exc[c("F12","F13")]))) # create matrix with evaluation results
colnames(tab_eval_exc)<-c("Accuracy","Prec_Pos","Recall_Pos","F1_Pos","Prec_Neg","Recall_Neg","F1_Neg","F_avg")
rownames(tab_eval_exc)<-c("SVM","NB","GI","FIN")
tab_eval_exc<-round(tab_eval_exc)
