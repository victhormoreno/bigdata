det_rec <- function(matrix, index, is_row) {
  cat("Your matrix:\n")
  print(matrix)
  
  det_value <- det(matrix)
  cat("The determinant of your matrix is:", det_value, "\n")
  
  if (is_row) {
    cat("Developed by row", index, ": ")
    
    for (i in 1:ncol(matrix)) {
      if (i != index) {
        coefficient <- matrix[index, i]
        sub_matrix <- matrix[-index, -i, drop = FALSE]
        det_sub <- det(sub_matrix)
        cat(coefficient, "*", det_sub)
        
        if (i < ncol(matrix) - 1) {
          cat(" + ")
        }
      }
    }
  } else {
    cat("Developed by column", index, ": ")
    
    for (i in 1:nrow(matrix)) {
      if (i != index) {
        coefficient <- matrix[i, index]
        sub_matrix <- matrix[-i, -index, drop = FALSE]
        det_sub <- det(sub_matrix)
        cat(coefficient, "*", det_sub)
        
        if (i < nrow(matrix) - 1) {
          cat(" + ")
        }
      }
    }
  }
  
  cat("\n")
}

data <- c(4, 4, 3, 0, -3, -3, 1, 2, -1, 2, 0, -1, -1, 1, 2, 3)
M <- matrix(data, nrow = 4, byrow = TRUE)
det_rec(M, 3, FALSE)
