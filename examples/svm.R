# BDR 20P
# Support Vector Machines
# As in Bret Lanz "Machine Learning with R" 2nd ed
# Packages required: "kernlab"
library(kernlab)


# OCR (character recognition)

letters<-read.csv("/home/victhor/repositories/bigdata/examples/data/letterdata.csv",stringsAsFactors=TRUE)
# 26 english capital letters (printed, added distorsion)

ls()
str(letters)
dim(letters)
n<-nrow(letters)

nt<-round(n*0.80)
nt

letters_train<-letters[1:nt,]
letters_test<-letters[(nt+1):n,]

prop.table(table(letters_train$letter))
prop.table(table(letters_test$letter))


letter_classifier<-ksvm(letter~.,data=letters_train,kernel="vanilladot")

letter_classifier

letter_predictions<-predict(letter_classifier,letters_test)

# Confusion matrix
table(letter_predictions,letters_test$letter)

agreement<-letter_predictions==letters_test$letter

prop.table(table(agreement))

# Same with another kernel

letter_classifier2<-ksvm(letter~.,data=letters_train,kernel="rbfdot")

letter_classifier2

letter_predictions2<-predict(letter_classifier2,letters_test)

# Confusion matrix
table(letter_predictions2,letters_test$letter)

agreement2<-letter_predictions2==letters_test$letter

prop.table(table(agreement2))