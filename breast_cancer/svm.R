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
  svm_model <- ksvm(diagnosis~.,data=breast_cancer_train,kernel=k)
  
  # Realizar predicciones en el conjunto de prueba
  svm_test_pred <- predict(svm_model, breast_cancer_test)
  
  # Evaluar el rendimiento del modelo
  tab <- table(breast_cancer_test_labels, svm_test_pred)
  print(paste("Kernel:", k))
  print("Confusion Matrix:")
  print(tab)
  
  print("Accuracy:")
  print(round(sum(diag(tab))/sum(tab), digits=4))
  cat("\n")
}
