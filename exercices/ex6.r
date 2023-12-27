mydet <- function(matrix) {
  n <- nrow(matrix)
  
  if (n == 1) {
    return(matrix[1, 1])
  }
  
  determinant <- 0
  for (i in 1:n) {
    sign <- (-1)^(i+1)
    sub_matrix <- matrix[-1, -i, drop = FALSE]
    determinant <- determinant + sign * matrix[1, i] * mydet(sub_matrix)
  }
  
  return(determinant)
}

# Example usage:
M <- matrix(c(4, 4, 3, 0, -3, -3, 1, 2, -1, 2, 0, -1, -1, 1, 2, 3), nrow = 4, byrow = TRUE)
result <- mydet(M)
cat("The determinant of the matrix is:", result, "\n")
