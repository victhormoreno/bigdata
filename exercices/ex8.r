# Exercise one: Simulation to estimate the probability of getting a lucky number

# Function to transform a non-negative integer into a list of digits
  int_to_digits <- function(num) {
    num_str = strsplit(as.character(num), '')[[1]]
    if(length(num_str)<6){
      ceros_faltantes <- 6 - length(num_str)
      return (c(rep(0, ceros_faltantes), as.numeric(num_str)))
    } else{
      return (as.numeric(num_str[1:6]))
    }   
  }

# Function to check if a number is lucky
is_lucky <- function(num) {
  digits <- int_to_digits(num)
  sum_first_half <- sum(digits[1:3])
  sum_second_half <- sum(digits[4:6])
  return(sum_first_half == sum_second_half)
}

# Simulation
set.seed(123)  # Set seed for reproducibility
num_simulations <- 1000
lucky_numbers_sim <- sum(replicate(num_simulations, is_lucky(sample(0:999999, num_simulations, replace = TRUE))))

# Estimate probability from simulation
probability_sim <- lucky_numbers_sim / num_simulations
cat("Exercise one: Estimated probability from simulation:", probability_sim, "\n")

# Exercise two: Exact probability by counting on the whole population

# Counting lucky numbers in the whole population
all_numbers <- 0:999999
lucky_numbers_exact <- sum(sapply(all_numbers, is_lucky))

# Exact probability
probability_exact <- lucky_numbers_exact / length(all_numbers)
cat("Exercise two: Exact probability by counting on the whole population:", probability_exact, "\n")

# Exercise three: Exact probability using probability theory (random variables and convolution theorem)

# Define probability mass function for a single digit (0 to 9)
prob_single_digit <- rep(1/10, 10)

# Convolve the probability mass function for three digits
prob_three_digits <- convolve(prob_single_digit, prob_single_digit, type = "open")
prob_three_digits <- convolve(prob_three_digits, prob_single_digit, type = "open")

# Probability of being lucky
probability_theory <- sum(prob_three_digits^2)
cat("Exercise three: Exact probability using probability theory (random variables and convolution theorem):", probability_theory, "\n")

