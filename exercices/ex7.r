# Function to generate Fabius sieve sequence
fabius_sieve <- function(N) {
  sequence <- 1:N
  for (i in 2:N) {
    sequence <- sequence[-(i * (1:(N %/% i)))]
  }
  return(sequence)
}

N <- 100000
fabius_sequence <- fabius_sieve(N)
plot(fabius_sequence, type = "b", pch = 19, col = "blue",main = "Fabius Sieve Sequence", xlab = "Index", ylab = "Value")
grid()

data <- data.frame(index = seq_along(fabius_sequence), value = fabius_sequence)

# Step 1: Regression to estimate k
fit_k <- lm(log(value) ~ log(index), data = data)
k <- coef(fit_k)[2]

# Step 2: Regression to estimate a (using the guessed value for k)
data$log_index <- log(data$index)
data$log_value <- log(data$value)
data$predicted <- exp(predict(fit_k, newdata = data))

fit_a <- lm(log(value) ~ log(index) - 1, data = data)

# Extract the coefficients
a <- exp(coef(fit_a))

# Plot the sequence and regression line
plot(data$index, data$value, type = "b", pch = 19, col = "blue",
     main = "Fabius Sieve Sequence with Regression Line",
     xlab = "Index", ylab = "Value")

# Add gridlines for better visibility
grid()

# Add the regression line to the plot
lines(data$index, exp(predict(fit_a, newdata = data)), col = "red")

# Show the plot
legend("topright", legend = paste("Regression Line (k =", round(k, 2), ", a =", round(a, 2), ")"), col = "red", lty = 1, cex = 0.8)