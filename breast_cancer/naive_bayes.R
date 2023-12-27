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

# Entrenar el modelo Naive Bayes
breast_cancer_classifier <- naiveBayes(breast_cancer_train_features, breast_cancer_train_labels)

# Realizar predicciones en el conjunto de prueba
breast_cancer_test_pred <- predict(breast_cancer_classifier, breast_cancer_test_features)

# Evaluar el rendimiento del modelo
tab <- table(breast_cancer_test_labels, breast_cancer_test_pred)
print("Confusion Matrix:")
print(tab)

round(prop.table(tab) * 100, digits = 2)

# Con laplace
breast_cancer_classifier <- naiveBayes(breast_cancer_train_features, breast_cancer_train_labels,laplace = 1)
breast_cancer_test_pred <- predict(breast_cancer_classifier, breast_cancer_test_features)
tab <- table(breast_cancer_test_labels, breast_cancer_test_pred)
print("Confusion Matrix:")
print(tab)
round(prop.table(tab) * 100, digits = 2)
