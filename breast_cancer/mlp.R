library(neuralnet)

path <- "/home/victhor/repositories/bigdata/breast_cancer/wisc_bc_data.csv"
breast_cancer_data <- read.csv(path)
breast_cancer_data$diagnosis <- as.factor(ifelse(breast_cancer_data$diagnosis == "B", 1, 0))


normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

set.seed(123)  # Para reproducibilidad
indices_entrenamiento <- sample(1:nrow(breast_cancer_data), 0.8 * nrow(breast_cancer_data))
breast_cancer_train <- breast_cancer_data[indices_entrenamiento, ]
breast_cancer_test <- breast_cancer_data[-indices_entrenamiento, ]

# Multi-Layer Perception Simple
model<-neuralnet(diagnosis~radius_mean+texture_mean+smoothness_mean+compactness_mean+concavity_mean,data=breast_cancer_train,hidden = 3)

plot(model)
predicciones <- predict(model, newdata = breast_cancer_test)

# Convierte las predicciones a clases (B o M)
clases_predichas <- ifelse(predicciones > 0.5, "B", "M")
clases_predichas<-clases_predichas[, 1]
# Crear la matriz de confusión
confusion_matrix <- table(clases_predichas, breast_cancer_test$diagnosis)

# Imprimir la matriz de confusión
print("Matriz de Confusión:")
print(confusion_matrix)

# Calcular la precisión, sensibilidad y especificidad
precision <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
sensibilidad <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
especificidad <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])

# Imprimir métricas de rendimiento
print(paste("Precisión:", precision))
print(paste("Sensibilidad:", sensibilidad))
print(paste("Especificidad:", especificidad))

