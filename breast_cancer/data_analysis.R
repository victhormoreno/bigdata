library(rpart)
library(rpart.plot)
library(class)
library(gmodels)
#library(caret)
library(e1071)
library(ROCR)
library(reshape)
library(ggplot2)
library(reshape2)
library(dplyr)
library(fmsb)
#library(GGally)
library(PerformanceAnalytics)
library(gridExtra)
library(MASS)
library(glmnet)
library(gbm)
library(boot)
library(data.table)
library(ROCR)

path <- "/home/victhor/repositories/bigdata/breast_cancer/wisc_bc_data.csv"
data <- read.csv(path)

Y <- ifelse(data$diagnosis == 'M', 1, 0)
# X <- data[,c(3:32)]
#Scatterplot between all variables
# scatter plot between means
# scatter plot between standard deviations
# Worst-case scatterplot 

#Scatterplot between all variables
chart.Correlation(X[,c(1:30)], histogram=TRUE, col="grey10", pch=1)

# scatter plot between means
chart.Correlation(X[,c(1:10)], histogram=TRUE, col="grey10", pch=1)

# scatter plot between standard deviations
chart.Correlation(X[,c(11:20)], histogram=TRUE, col="grey10", pch=1)

# Worst-case scatterplot 
chart.Correlation(X[,c(21:30)], histogram=TRUE, col="grey10", pch=1)

# Calcular matriz de correlación de Pearson
cor_matrix <- cor(X, method = "pearson")

# Obtén las posiciones de las correlaciones mayores a 0.9
high_cor_positions <- which(cor_matrix > 0.9 & cor_matrix < 1, arr.ind = TRUE)

# Imprime los pares de variables con correlación mayor a 0.9
for (i in 1:nrow(high_cor_positions)) {
  row_index <- high_cor_positions[i, 1]
  col_index <- high_cor_positions[i, 2]
  variable1 <- colnames(cor_matrix)[row_index]
  variable2 <- colnames(cor_matrix)[col_index]
  correlation <- cor_matrix[row_index, col_index]
  cat(sprintf("Variables: %s y %s, Correlación: %.2f\n", variable1, variable2, correlation))
}



