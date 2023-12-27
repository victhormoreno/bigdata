# BDR 20P
# Neural Networks
# As in Bret Lanz "Machine Learning with R" 2nd ed
# Packages required: "neuralnet"
library(neuralnet)

# Predicting concrete strength

concrete<-read.csv("/home/victhor/repositories/bigdata/examples/data/concrete.csv")

ls()
str(concrete)
dim(concrete)
n<-nrow(concrete)
ncol(concrete)
names<-names(concrete)

par(mfrow=c(3,3))
for (k in 1:9) hist(concrete[,k],col="yellow",xlab="",main=names[k])

# For normalizing numerical data
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

# Apply normalization to all numerical columns
concrete_n<-as.data.frame(lapply(concrete,normalize))

par(mfrow=c(3,3))
for (k in 1:9) hist(concrete_n[,k],col="pink",xlab="",main=names[k])

n*0.75

nt<-773

concrete_train<-concrete_n[1:nt,]
concrete_test<-concrete_n[(nt+1):n,]

mean(concrete_train$strength)
mean(concrete_test$strength)

# Size 1 hidden layer
set.seed(34)

concrete_model<-neuralnet(strength~cement+slag+ash+water+
                            superplastic+coarseagg+fineagg+age,data=concrete_train)

plot(concrete_model)

concrete_test_results<-compute(concrete_model,concrete_test[1:8])

strenght_pred<-as.vector(concrete_test_results$net.result)

cor(concrete_test$strength,strenght_pred)

RMSE<-sqrt(mean((concrete_test$strength-strenght_pred)^2))
RMSE

# Size 5 hidden layer
concrete_model2<-neuralnet(strength~cement+slag+ash+water+
                             superplastic+coarseagg+fineagg+age,data=concrete_train,hidden=5)

plot(concrete_model2)

concrete_test_results2<-compute(concrete_model2,concrete_test[1:8])

strenght_pred2<-as.vector(concrete_test_results2$net.result)

cor(concrete_test$strength,strenght_pred2)

RMSE<-sqrt(mean((concrete_test$strength-strenght_pred2)^2))
RMSE