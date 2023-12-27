# Function to compute the n-order Taylor polynomial for exp(x) at x=0
texp <- function(n, x) {
  sum(x^(0:n) / factorial(0:n))
}

# Define the range of x values
x_values <- seq(-2, 2, length.out = 100)

# Calculate y values for exp(x)
y_exp <- exp(x_values)

# Plot exp(x) and its Taylor polynomials of different orders
plot(x_values, y_exp, type = "l", col = "blue", lty = 2, ylim = c(0, 8), ylab = "y", xlab = "x")

# Plot Taylor polynomials of orders 1, 2, 3, and 4
for (order in 1:4) {
  y_taylor <- sapply(x_values, function(x) texp(order, x))
  lines(x_values, y_taylor, col = rainbow(4)[order], lty = 1)
}

title("Exp Taylor Functions")

# Add legend
legend("topright", legend = c("exp(x)", "Taylor (order 1)", "Taylor (order 2)", "Taylor (order 3)", "Taylor (order 4)"), 
       col = c("blue", rainbow(4)), lty = c(2, 1, 1, 1, 1))


# Function to compute the n-order Taylor polynomial for cos(x) at x=0
tcos <- function(n, x) {
  sum((-1)^(0:n) * x^(2*(0:n)) / factorial(2*(0:n)))
}

# Function to compute the n-order Taylor polynomial for sin(x) at x=0
tsin <- function(n, x) {
  sum((-1)^(0:n) * x^(1 + 2*(0:n)) / factorial(1 + 2*(0:n)))
}


# Define the range of x values
x_values <- seq(-2*pi, 2*pi, length.out = 200)

# Calculate y values for cos(x) and sin(x)
y_cos <- cos(x_values)
y_sin <- sin(x_values)

# Plot cos(x) and its Taylor polynomials of different orders
par(mfrow = c(1, 2))
plot(x_values, y_cos, type = "l", col = "blue", lty = 2, ylim = c(-2, 2), ylab = "y", xlab = "x", main = "Cosine Function")

# Plot Taylor polynomials of orders 1, 2, 3, and 4 for cos(x)
for (order in 1:4) {
  y_taylor_cos <- sapply(x_values, function(x) tcos(order, x))
  lines(x_values, y_taylor_cos, col = rainbow(4)[order], lty = 1)
}

# Add legend for cos(x)
legend("topright", legend = c("cos(x)", "Taylor (order 1)", "Taylor (order 2)", "Taylor (order 3)", "Taylor (order 4)"), 
       col = c("blue", rainbow(4)), lty = c(2, 1, 1, 1, 1))

# Create a new plot for sin(x) and its Taylor polynomials
plot(x_values, y_sin, type = "l", col = "red", lty = 2, ylim = c(-2, 2), ylab = "y", xlab = "x", main = "Sine Function")

# Plot Taylor polynomials of orders 1, 2, 3, and 4 for sin(x)
for (order in 1:4) {
  y_taylor_sin <- sapply(x_values, function(x) tsin(order, x))
  lines(x_values, y_taylor_sin, col = rainbow(4)[order], lty = 1)
}

# Add legend for sin(x)
legend("topright", legend = c("sin(x)", "Taylor (order 1)", "Taylor (order 2)", "Taylor (order 3)", "Taylor (order 4)"), 
       col = c("red", rainbow(4)), lty = c(2, 1, 1, 1, 1))

# Reset the plotting area to default
par(mfrow = c(1, 1))

