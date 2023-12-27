compute_roots <- function(a, b, c) {
  # Calculate the discriminant
  discriminant <- b^2 - 4*a*c
  
  # Check if the discriminant is non-negative (real roots)
  if (discriminant >= 0) {
    # Calculate the two real roots
    root1 <- (-b + sqrt(discriminant)) / (2*a)
    root2 <- (-b - sqrt(discriminant)) / (2*a)
    return(c(root1, root2))
  } else {
    # Calculate the two complex roots
    root1 <- (-b + sqrt(as.complex(discriminant))) / (2*a)
    root2 <- (-b - sqrt(as.complex(discriminant))) / (2*a)
    return(c(root1, root2))
  }
}

# Example usage:
a <- 4
b <- -1
c <- 5

roots <- compute_roots(a, b, c)
print(paste("Roots:", roots))
