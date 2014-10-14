### proc_lexicons.R file ###
library(tm) # load R package tm: framework for text mining applications
library(SnowballC) # load SnowballC: An R interface to the C libstemmer library that implements Porter's word stemming algorithm
library(XLConnect) # load XLConnect package: Manipulation of Excel files

# process financial lexicon (FIN) created by Loughran and McDonald
FIN_n<-read.csv("http://www3.nd.edu/~mcdonald/Data/Finance_Word_Lists/LoughranMcDonald_Negative.csv", stringsAsFactors = FALSE)[,1] # read negative word list
FIN_p<-read.csv("http://www3.nd.edu/~mcdonald/Data/Finance_Word_Lists/LoughranMcDonald_Positive.csv", stringsAsFactors = FALSE)[,1] # read positive word list
lex_FIN<-as.data.frame(rbind(cbind(FIN_p,rep(1,length(FIN_p))), cbind(FIN_n,rep(-1,length(FIN_n)))),stringsAsFactors=FALSE) # create a data frame with all words and respective sentiment label (1 for positive and -1 for negative)
colnames(lex_FIN)<-c("stem","sentiment")
lex_FIN$stem<-tolower(lex_FIN$stem) # convert to lowercase
lex_FIN$stem<-stemDocument(lex_FIN$stem) # stem all words
lex_FIN<-unique(lex_FIN) # remove duplicated elements
write.csv(lex_FIN,"FIN.csv",row.names=FALSE) # write the processed FIN lexicon to a CSV file

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
write.csv(lex_GI,"GI.csv",row.names=FALSE) # write the processed GI lexicon to a CSV file
