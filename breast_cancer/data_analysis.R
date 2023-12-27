# Importing data 
path <- "/home/victhor/repositories/bigdata/breast_cancer/wisc_bc_data.csv"
data <- read.csv(path)
print(names(data))

# Data frame distribution
cat("Casos benignos (B):", table(data$diagnosis)["B"], "\n")
cat("Casos malignos (M):", table(data$diagnosis)["M"], "\n")

# Data visualization
# Cargar la biblioteca
library(ggplot2)

# Seleccionar las columnas numéricas (excluir id y diagnosis)
numeric_columns <- data[, sapply(data, is.numeric)]
numeric_columns <- numeric_columns[, -c(1, 2)]  # Excluir las columnas 1 (id) y 2 (diagnosis)

# Calcular la matriz de correlación de Pearson
library(corrplot)
corr_matrix <- cor(numeric_columns, method = "pearson")
corrplot(corr_matrix, method = "color")
