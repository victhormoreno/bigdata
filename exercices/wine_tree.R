# BDR 20P
# Regression trees
# As in Bret Lanz "Machine Learning with R" 2nd ed
# Packages required: "rpart", "rpart.plot", "RWeka"

# If not installed
install.packages("rpart")
install.packages("rpart.plot")
install.packages("RWeka")


library(rpart)


# Predicting quality of wines

wine<-read.csv("/home/victhor/repositories/bigdata/exercices/data/whitewines.csv")

ls()
str(wine)
dim(wine)
n<-nrow(wine)
ncol(wine)
names(wine)

hist(wine$quality)

nt<-3750

wine_train<-wine[1:nt,]
wine_test<-wine[(nt+1):n,]

mean(wine_train$quality)
mean(wine_test$quality)

# Regression tree
m_rpart<-rpart(quality~.,data=wine_train)

m_rpart
summary(m_rpart)

library(rpart.plot)

rpart.plot(m_rpart,digits=3)
rpart.plot(m_rpart,digits=3,fallen.leaves=FALSE,type=3,extra=101)


p_rpart<-predict(m_rpart,wine_test)

cor(wine_test$quality,p_rpart)

MSE<-mean((wine_test$quality-p_rpart)^2)
MSE
MAE<-mean(abs(wine_test$quality-p_rpart))
MAE


## Model tree M5 prime algorithm

library(RWeka)

m_m5p<-M5P(quality~.,data=wine_train)

m_m5p
summary(m_m5p)

p_m5p<-predict(m_m5p,wine_test)

cor(wine_test$quality,p_m5p)

hist(p_m5p)
hist(wine_test$quality)

MSE<-mean((wine_test$quality-p_m5p)^2)
MSE
MAE<-mean(abs(wine_test$quality-p_m5p))
MAE