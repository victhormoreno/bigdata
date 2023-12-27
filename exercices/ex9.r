# Function to generate a 1D random walk with regular left-right displacements
generate_1d_walk_regular <- function(N) {
  steps <- sample(c(-1, 1), N, replace = TRUE)
  positions <- cumsum(steps)
  return(positions)
}

# Function to generate a 1D random walk with normal (gaussian) displacements
generate_1d_walk_normal <- function(N) {
  steps <- rnorm(N)
  positions <- cumsum(steps)
  return(positions)
}

# Function to generate a 2D random walk with regular left-right displacements
generate_2d_walk_regular <- function(N) {
  x_steps <- sample(c(-1, 1), N, replace = TRUE)
  y_steps <- sample(c(-1, 1), N, replace = TRUE)
  x_positions <- cumsum(x_steps)
  y_positions <- cumsum(y_steps)
  return(data.frame(x = x_positions, y = y_positions))
}

# Function to generate a 2D random walk with normal (gaussian) displacements
generate_2d_walk_normal <- function(N) {
  x_steps <- rnorm(N)
  y_steps <- rnorm(N)
  x_positions <- cumsum(x_steps)
  y_positions <- cumsum(y_steps)
  return(data.frame(x = x_positions, y = y_positions))
}

# Plot several 1D random walks
experiments = 4
par(mfrow = c(1,2))
walk_data <- generate_2d_walk_regular(1000000)
plot(walk_data$x, walk_data$y, type = 'l', main = 'Random regular walk for N=1000000')
walk_data <- generate_2d_walk_normal(1000000)
plot(walk_data$x, walk_data$y, type = 'l',main = 'Random normal walk for N=100000')


for (i in 1:experiments) plot(generate_1d_walk_regular(100), type = 'l', col = i)
for (i in 1:experiments) plot(generate_1d_walk_regular(1000), type = 'l', col = i)
for (i in 1:experiments) plot(generate_1d_walk_regular(100000), type = 'l', col = i)


for (i in 1:experiments) plot(generate_1d_walk_normal(100), type = 'l', col = i)
for (i in 1:experiments) plot(generate_1d_walk_normal(1000), type = 'l', col = i)
for (i in 1:experiments) plot(generate_1d_walk_normal(100000), type = 'l', col = i)


# Plot several 2D random walks
for (i in 1:experiments) {
  walk_data <- generate_2d_walk_regular(100000)
  plot(walk_data$x, walk_data$y, type = 'l', col = i)
} 
for (i in 1:experiments) {
  walk_data <- generate_2d_walk_regular(1000)
  plot(walk_data$x, walk_data$y, type = 'l', col = i)
} 
for (i in 1:experiments) {
  walk_data <- generate_2d_walk_regular(100000)
  plot(walk_data$x, walk_data$y, type = 'l', col = i)
} 

for (i in 1:experiments) {
  walk_data <- generate_2d_walk_normal(100)
  plot(walk_data$x, walk_data$y, type = 'l', col = i)
}

for (i in 1:experiments) {
  walk_data <- generate_2d_walk_normal(1000)
  plot(walk_data$x, walk_data$y, type = 'l', col = i)
}

for (i in 1:experiments) {
  walk_data <- generate_2d_walk_normal(100000)
  plot(walk_data$x, walk_data$y, type = 'l', col = i)
}

