# Define the range of x values
x_values <- seq(-3, 3, length.out = 100)

# Calculate y values for cosh(x) and sinh(x)
y_cosh <- cosh(x_values)
y_sinh <- sinh(x_values)

# Calculate asymptotic lines
asymptotic_upper <- exp(x_values)/2
asymptotic_lower <- -exp(x_values)/2

# Plot the functions
plot(x_values, y_cosh, type = "l", col = "green", ylab = "y", xlab = "x", ylim = c(-3, 3))
lines(x_values, y_sinh, col = "red")

# Add asymptotic lines
lines(x_values, asymptotic_upper, col = "blue", lty = 2)
lines(x_values, asymptotic_lower, col = "blue", lty = 2)

# Add title
title("Hyperbolic Functions")

# Add legend
legend("topleft", legend = c("cosh(x)", "sinh(x)", "asymptotic Lines"), 
       col = c("green", "red", "blue"), lty = c(1, 1, 2))

