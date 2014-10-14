### preprocessing.R file ###
library(tm) # load R package tm: framework for text mining applications
library(RTextTools) # load R package RTextTools: Automatic Text Classification via Supervised Learning

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
save(list=c("DTM", "twt", "l_emot", "l_hstg"), file="preproc.RData") # save R objects required for future tasks
