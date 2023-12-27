# BDR 18P
# Naive Bayes
# As in Bret Lanz "Machine Learning with R" 2nd ed
# Packages required: "tm", "SnowballC", "wordcloud", "e1071"

# "tm" is a text mining package
# if needed: install.packages("tm")

# for stemming
# Snowball is a open source algorithm
# SnowballC is a package for interfacing R to a C library
# it gets to tm through stemDocument procedure
# if needed: install.packages("SnowballC")

# if needed: install.packages("wordcloud")

# End package installing

# Read data
# Omit fileEncoding if you get errors
sms_raw<-read.csv("/home/victhor/repositories/bigdata/examples/data/sms_spam.csv",stringsAsFactors=FALSE,fileEncoding="UTF-8")

str(sms_raw)

names(sms_raw)

sms_raw$type<-factor(sms_raw$type)

str(sms_raw$type)

table(sms_raw$type)

round(prop.table(table(sms_raw$type))*100,digits=1)

library(tm)

# Create corpus. VCorpus creates it in RAM (volatile).
# It requires a source type argument. VectorSourse transforms
# vector into source. Elements in vector are documents.

sms_corpus<-VCorpus(VectorSource(sms_raw$text))

is.vector(sms_corpus)
is.data.frame(sms_corpus)
is.list(sms_corpus)

print(sms_corpus)

inspect(sms_corpus[1:4])

sms_corpus[[1]][1]
sms_corpus[[1]][2]
sms_corpus[[1]][3]


replacePunctuation<-function(x){
  gsub("[[:punct:]]+"," ",x)
}

sms_corpus_clean<-tm_map(sms_corpus,content_transformer(function(x) chartr("�ë�","     ",x)))
sms_corpus_clean<-tm_map(sms_corpus_clean,content_transformer(tolower))
sms_corpus_clean<-tm_map(sms_corpus_clean,removeNumbers)
sms_corpus_clean<-tm_map(sms_corpus_clean,removeWords,stopwords())
sms_corpus_clean<-tm_map(sms_corpus_clean,content_transformer(replacePunctuation))

stopwords()

getTransformations()

lapply(sms_corpus_clean[1:100],as.character)


library(SnowballC)

sms_corpus_clean<-tm_map(sms_corpus_clean,stemDocument)
# add mc.cores=1 if errors

sms_corpus_clean<-tm_map(sms_corpus_clean,stripWhitespace)

# Create Document/Term matrix

sms_dtm<-DocumentTermMatrix(sms_corpus_clean)

# Alternative: Preprocessing included, some differences in result:

sms_dtm1<-DocumentTermMatrix(sms_corpus_clean, control=list(tolower=TRUE,
                                                            removeNumbers=TRUE,stopwords=TRUE,removePunctuation=TRUE,stemming=TRUE))


str(sms_dtm)
n<-dim(sms_dtm)[1]

nt<-round(n*0.75)

sms_dtm_train<-sms_dtm[1:nt,]
sms_dtm_test<-sms_dtm[(nt+1):n,]

sms_train_labels<-sms_raw[1:nt,1]
sms_test_labels<-sms_raw[(nt+1):n,1]

# Check for same proportions:
round(prop.table(table(sms_train_labels))*100,digits=3)
round(prop.table(table(sms_test_labels))*100,digits=3)

# Word clouds

library(wordcloud)

wordcloud(sms_corpus_clean,min.freq=round(n*0.01),random.order=FALSE)

wordcloud(sms_corpus_clean[sms_raw$type=="ham"],min.freq=round(n*0.01),random.order=FALSE)
wordcloud(sms_corpus_clean[sms_raw$type=="spam"],min.freq=round(n*0.01),random.order=FALSE)



sms_freq_words<-findFreqTerms(sms_dtm_train,5)

str(sms_freq_words)


sms_dtm_freq_train<-sms_dtm_train[,sms_freq_words]
sms_dtm_freq_test<-sms_dtm_test[,sms_freq_words]

convert_counts<-function(x) ifelse(x>0,"Yes","No")

sms_train<-apply(sms_dtm_freq_train,MARGIN=2,convert_counts)
sms_test<-apply(sms_dtm_freq_test,MARGIN=2,convert_counts)

str(sms_train)

library(e1071)

sms_classifier<-naiveBayes(sms_train,sms_train_labels)

sms_test_pred<-predict(sms_classifier,sms_test)

# Confusion matrix
tab<-table(sms_test_labels,sms_test_pred)
tab
round(prop.table(tab)*100,digits=2)

TN<-tab[1,1]
FP<-tab[1,2]
FN<-tab[2,1]
TP<-tab[2,2]

# Misclassification error
ERR<-(FP+FN)/(TP+TN+FP+FN)
cat("ERR=",ERR,"\n")

# Accuracy
ACC<-(TP+TN)/(TP+TN+FP+FN)
cat("ACC=",ACC,"\n")

# True possitive rate
# Also called recall (REC) or sensitivity (SEN)
TPR<-TP/(FN+TP)
REC<-SEN<-TPR
cat("TPR=REC=SEN=",TPR,"\n")

# False positive rate
FPR<-FP/(FP+TN)
cat("FPR=",FPR,"\n")

# Precission
PRE<-TP/(TP+FP)
cat("PRE=",PRE,"\n")

# Specificity
SPE<-TN/(TN+FP)
cat("SPE=",SPE,"\n")

# F1 score
F1<-2*PRE*REC/(PRE+REC)
cat("F1=",F1,"\n")

# Next, using Laplace estimator
sms_classifier2<-naiveBayes(sms_train,sms_train_labels,laplace=1)

sms_test_pred2<-predict(sms_classifier2,sms_test)

# Confusion matrix
tab2<-table(sms_test_labels,sms_test_pred2)
tab2
round(prop.table(tab2)*100,digits=2)