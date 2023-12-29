library(kernlab)

# Cargar el conjunto de datos (asegúrate de tener correctamente preparados tus datos)
breast_cancer_data <- read.csv("/home/victhor/repositories/bigdata/breast_cancer/wisc_bc_data.csv")
breast_cancer_data <- breast_cancer_data[, -c(1)]  # Excluir las columnas 1 (id) y 2 (diagnosis)
breast_cancer_data$diagnosis <- as.factor(ifelse(breast_cancer_data$diagnosis == "B", 1, 0))




# Dividir el conjunto de datos en entrenamiento y prueba
set.seed(123)  # Para reproducibilidad
indices_entrenamiento <- sample(1:nrow(breast_cancer_data), 0.8 * nrow(breast_cancer_data))
breast_cancer_train <- breast_cancer_data[indices_entrenamiento, ]
breast_cancer_test <- breast_cancer_data[-indices_entrenamiento, ]

breast_cancer_test_labels <- breast_cancer_test$diagnosis
breast_cancer_test_features <- breast_cancer_test[, -which(names(breast_cancer_test) == "diagnosis")]


kernels <- c("vanilladot", "rbfdot", "polydot")
for (k in kernels) {
  svm_model <- ksvm(diagnosis~radius_mean+texture_mean+smoothness_mean+concavity_mean+symmetry_mean+points_mean,data=breast_cancer_train,kernel=k)
  
  # Realizar predicciones en el conjunto de prueba
  svm_test_pred <- predict(svm_model, breast_cancer_test)
  
  # Evaluar el rendimiento del modelo
  confusion_matrix <- table(breast_cancer_test_labels, svm_test_pred)
  print(paste("Kernel:", k))
  print("Confusion Matrix:")
  print(confusion_matrix)
  # Calcular la precisión, sensibilidad y especificidad
  precision <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  sensibilidad <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  especificidad <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
  
  # Imprimir métricas de rendimiento
  print(paste("Precisión:", precision))
  print(paste("Recall:", sensibilidad))
  
  # Calcular la puntuación F
  f_score <- 2 * (precision * sensibilidad) / (precision + sensibilidad)
  
  # Imprimir la puntuación F
  print(paste("Puntuación F:", f_score))
  
}
