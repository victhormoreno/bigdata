library(e1071)
path <- "/home/victhor/repositories/bigdata/breast_cancer/wisc_bc_data.csv"
breast_cancer_data <- read.csv(path)

# Dividir el conjunto de datos en entrenamiento y prueba
set.seed(123)  # Para reproducibilidad
indices_entrenamiento <- sample(1:nrow(breast_cancer_data), 0.8 * nrow(breast_cancer_data))
breast_cancer_train <- breast_cancer_data[indices_entrenamiento, ]
breast_cancer_test <- breast_cancer_data[-indices_entrenamiento, ]

# Separar etiquetas de clase y características
breast_cancer_train_labels <- breast_cancer_train$diagnosis
breast_cancer_train_features <- breast_cancer_train[, -which(names(breast_cancer_train) == "diagnosis")]

breast_cancer_test_labels <- breast_cancer_test$diagnosis
breast_cancer_test_features <- breast_cancer_test[, -which(names(breast_cancer_test) == "diagnosis")]



# Con laplace
breast_cancer_classifier <- naiveBayes(breast_cancer_train_features, breast_cancer_train_labels,laplace = 1)
breast_cancer_test_pred <- predict(breast_cancer_classifier, breast_cancer_test_features)
confusion_matrix <- table(breast_cancer_test_labels, breast_cancer_test_pred)
print("Confusion Matrix:")
print(confusion_matrix)
round(prop.table(confusion_matrix) * 100, digits = 2)

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

# usekernel
breast_cancer_classifier <- naiveBayes(breast_cancer_train_features, breast_cancer_train_labels, usekernel = TRUE)
breast_cancer_test_pred <- predict(breast_cancer_classifier, breast_cancer_test_features)
confusion_matrix <- table(breast_cancer_test_labels, breast_cancer_test_pred)
print("Confusion Matrix:")
print(confusion_matrix)
round(prop.table(confusion_matrix) * 100, digits = 2)

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



# sin laplace
breast_cancer_classifier <- naiveBayes(breast_cancer_train_features, breast_cancer_train_labels)
breast_cancer_test_pred <- predict(breast_cancer_classifier, breast_cancer_test_features)
confusion_matrix <- table(breast_cancer_test_labels, breast_cancer_test_pred)
print("Confusion Matrix:")
print(confusion_matrix)
round(prop.table(confusion_matrix) * 100, digits = 2)

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
