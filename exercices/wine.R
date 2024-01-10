library(neuralnet)

# Predicting whine pH

whine<-read.csv("/home/victhor/repositories/bigdata/exercices/data/whitewines.csv")

ls()
str(whine)
dim(whine)
n<-nrow(whine)
ncol(whine)
names<-names(whine)

par(mfrow=c(3,3))
for (k in 1:9) hist(whine[,k],col="yellow",xlab="",main=names[k])

# For normalizing numerical data
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

# Apply normalization to all numerical columns
whine_n<-as.data.frame(lapply(whine,normalize))

par(mfrow=c(3,3))
for (k in 1:9) hist(whine_n[,k],col="pink",xlab="",main=names[k])

n*0.75

nt<-773

whine_train<-whine_n[1:nt,]
whine_test<-whine_n[(nt+1):n,]

mean(whine_train$pH)
mean(whine_test$pH)

# Size 1 hidden layer
set.seed(34)

whine_model<-neuralnet(pH~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+
                         chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density,data=whine_train)

plot(whine_model)

whine_test_results<-compute(whine_model,whine_test[1:8])

strenght_pred<-as.vector(whine_test_results$net.result)

cor(whine_test$pH,strenght_pred)

RMSE<-sqrt(mean((whine_test$pH-strenght_pred)^2))
RMSE

# Size 5 hidden layer
whine_model2<-neuralnet(pH~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+
                          chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density,data=whine_train,hidden=5)

plot(whine_model2)

whine_test_results2<-compute(whine_model2,whine_test[1:8])

strenght_pred2<-as.vector(whine_test_results2$net.result)

cor(whine_test$pH,strenght_pred2)

RMSE<-sqrt(mean((whine_test$pH-strenght_pred2)^2))
RMSE