library(learningr)
library(ggplot2)
library(lattice)
library(corrplot)

# Import data
election_data <- obama_vs_mccain
summary(election_data)

# Correlation matrix
library(corrplot)

election_data <- election_data[!(election_data$State %in% c("Alaska", "Hawaii")), ]
corr_matrix <- cor(election_data[, c("Obama", "McCain", "Unemployment", "Income", "Population", "Catholic", "Protestant", "Other", "Non.religious", "Black", "Latino", "Urbanization")])
corrplot(corr_matrix, method = "color")



# Scatter plots using base R
pairs(election_data[, c("Obama", "McCain", "Turnout", "Unemployment", "Income", "Population")])

# Scatter plots using lattice
xyplot(McCain ~ Obama, data = obama_vs_mccain, main = "Obama vs. McCain", xlab = "Obama", ylab = "McCain")

# Scatter plots using ggplot2
ggplot(election_data, aes(x = Obama, y = McCain)) +  geom_point() +  facet_grid(. ~ Region)  



# Select relevant columns for the analysis
religion_data <- election_data[, c('State', 'Catholic', 'Protestant', 'Other', 'Non.religious')]
religion_data_long <- tidyr::gather(religion_data, key = 'Religion', value = 'Percentage', -State)
ggplot(religion_data_long, aes(x = State, y = Percentage, fill = Religion)) +
  geom_bar(stat = 'identity') +
  labs(title = 'Religion Distribution in Different States (Obama vs. McCain)',
       x = 'State',
       y = 'Percentage') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Linear regression model
model <- lm(Obama ~ Turnout + Unemployment + Income + Population + Catholic + Protestant + Other + Non.religious + Black + Latino + Urbanization, data = election_data)
summary(model)
