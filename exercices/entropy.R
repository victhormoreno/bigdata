# Function to compute information gain
information_gain <- function(p1, p2) {
  - (p1 * log2(p1) + p2 * log2(p2))
}

# Function to compute Gini index
gini_index <- function(p1, p2) {
  1 - (p1^2 + p2^2)
}

# Generate sample data
set.seed(123)
n <- 1000
X1 <- runif(n)
X2 <- runif(n)
g <- 2
Y <- ifelse(X2 < X1^g, "A", "B")

# Initialize vectors to store results
t_values <- seq(0.01, 0.99, by = 0.01)
info_gain_values <- numeric(length(t_values))
gini_index_values <- numeric(length(t_values))

# Compute information gain and Gini index for different t values
for (i in seq_along(t_values)) {
  t <- t_values[i]
  # Split data based on t
  part1 <- Y[X1 <= t]
  part2 <- Y[X1 > t]
  
  # Compute class probabilities for each part
  p1_A <- sum(part1 == "A") / length(part1)
  p1_B <- sum(part1 == "B") / length(part1)
  p2_A <- sum(part2 == "A") / length(part2)
  p2_B <- sum(part2 == "B") / length(part2)
  
  # Compute information gain and Gini index
  info_gain_values[i] <- information_gain(p1_A, p1_B) * length(part1) / n +
    information_gain(p2_A, p2_B) * length(part2) / n
  
  gini_index_values[i] <- gini_index(p1_A, p1_B) * length(part1) / n +
    gini_index(p2_A, p2_B) * length(part2) / n
}

# Plot the information gain and Gini index functions
par(mfrow = c(2, 1))
plot(t_values, info_gain_values, type = "l", col = "blue", xlab = "t", ylab = "Information Gain",
     main = "Information Gain vs t")
abline(v = t_values[which.max(info_gain_values)], col = "red", lty = 2)

plot(t_values, gini_index_values, type = "l", col = "green", xlab = "t", ylab = "Gini Index",
     main = "Gini Index vs t")
abline(v = t_values[which.max(gini_index_values)], col = "red", lty = 2)
