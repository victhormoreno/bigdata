mydet <- function(mat) {
  n <- nrow(mat)
  
  if (n == 1) {
    return(mat[1, 1])
  }else{
    det_val<-0
    for(j in 1:n){
      sign <- (-1)^(1+j)
      minor_mat <- mat[-1,-j,drop=FALSE]
      det_val <- det_val + sign * mat[1,j] * mydet(minor_mat)
    }
    return(det_val)
  }
}

# Example usage:
M <- matrix(c(4, 4, 3, 0, -3, -3, 1, 2, -1, 2, 0, -1, -1, 1, 2, 3), nrow = 4, byrow = TRUE)
result <- mydet(M)
cat("The determinant of the matrix is:", result, "\n")
