# Load the required libraries
library(ggplot2)
library(dplyr)

# Read the data from the CSV file
data <- read.csv("recommendations.csv")

# Fit a logistic regression model to analyze the effect of recommendation mode on whether a recommendation is followed
model <- glm(RecommendationFollowed ~ Mode, data = data, family = binomial)

# Create a new data frame to use for plotting
new_data <- data.frame(Mode = c("Auditory", "Visual"))

# Predict the probabilities of following the recommendation for each mode
new_data$prob <- predict(model, new_data, type = "response")

# Create the plot
ggplot(new_data, aes(x = Mode, y = prob)) +
  geom_bar(stat = "identity", fill= 'purple', width=0.25) +
  labs(x = "Recommendation Mode", y = "Probability of Following Recommendation", title = "Effect of Recommendation Mode on Whether a Recommendation is Followed") +
  theme_minimal()
  

#Trying different plots for beta distribution

##No:1

# Define the parameters of the normal distribution
mu <- 0.80
sigma <- 0.1

# Check that the mean and standard deviation satisfy the conditions for the formulas to work
if (mu < 0 || mu > 1 || sigma <= 0) {
  stop("The mean and standard deviation of the normal distribution do not satisfy the conditions for the formulas to work.")
}

# Calculate the parameters of the beta distribution
alpha <- (mu^2 * (1 - mu) / sigma^2 - mu) / (mu - 1)
beta <- alpha * (1 - mu) / mu

# Generate a sequence of values between 0 and 1
x <- seq(0, 1, length.out = 100)

# Calculate the probability density function (PDF) for the beta distribution
y <- dbeta(x, shape1 = alpha, shape2 = beta)

# Plot the beta distribution
ggplot(data.frame(x, y), aes(x = x, y = y)) +
  geom_line() +
  ggtitle("Beta Distribution")

# Define the mean and variance of the distribution
mu <- 0.60 # for example
variance <- 0.025 # for example

# Calculate the values of alpha and beta
alpha <- ( (1 - mu) / variance - 1 / mu ) * mu^2
beta <- alpha * (1 / mu - 1)

# Generate some random data from the distribution
set.seed(123) # for reproducibility
data <- rbeta(1000, alpha, beta)

# Plot the distribution
hist(data, breaks = 20, freq = FALSE, main = "Beta Distribution", xlab = "Marks")
curve(dbeta(x, alpha, beta), add = TRUE, col = "purple", lwd = 2)


##No:2

library(stats)
set.seed(123) # for reproducibility
n <- 1000 # number of observations
alpha=shape1 <- 3
beta=shape2 <- 4
grades <- rbeta(n, alpha, beta) * 100

hist(grades, main = "Distribution of Grades", xlab = "Grade", col = "blue")

mean(grades)


##No.3
set.seed(100)

priors <- tibble(n = 1:50) %>%
  group_by(n) %>%
  mutate(alpha = rbeta(1, 67, 75), beta = rbeta(1, 10, 30))

gen_prior_pred <- function(n, alpha, beta) {
  x <- seq(0, 1, 0.01) # use a finer resolution for better visualization
  y <- dbeta(x, alpha, beta)
  
  d <- tibble(n = n, alpha = alpha, beta = beta,
              x = x,
              y = y)
  
  return(d)
}

prior_student_marks <- pmap_df(priors, gen_prior_pred)

ggplot(prior_student_marks, aes(x, y, group = n)) + 
  geom_path(alpha = 0.5) +
  theme_minimal() +
  labs(x = "Student Marks", y = "Density") +
  ggtitle("Distribution of Student Marks (Beta)")
  
  
No: 4 
set.seed(100)

priors <- tibble(n = 1:50) %>%
  group_by(n) %>%
  mutate(alpha = rbeta(1, 50, 100), beta = rbeta(1, 10, 30))

gen_prior_pred <- function(n, alpha, beta) {
  x <- seq(0, 1, 0.1) # use a finer resolution for better visualization
  y <- dbeta(x, alpha, beta)
  d <- tibble(n = n, alpha = alpha, beta = beta,
              x = x,
              y = y)
  return(d)
}

prior_student_marks <- pmap_df(priors, gen_prior_pred)
# Plot histogram
ggplot(prior_student_marks, aes(x, group = n, fill = alpha)) + 
  stat_density(binwidth = 0.1, alpha = 0.5, aes(y = ..density..)) +
  theme_minimal() +
  labs(x = "Student Marks", y = "Density") +
  ggtitle("Beta Distribution of Student Marks") +
  scale_fill_gradient(low = "blue", high = "red")
  
  
No: 5
  set.seed(100)

priors <- tibble(n = 1:50) %>%
  group_by(n) %>%
  mutate(alpha = rbeta(1, 50, 100), beta = rbeta(1, 10, 30))

gen_prior_pred <- function(n, alpha, beta) {
  x <- seq(0, 100, 1) # use a finer resolution for better visualization
  y <- dbeta(x, alpha, beta)
  d <- tibble(n = n, alpha = alpha, beta = beta,
              x = x,
              y = y)
  return(d)
}

prior_student_marks <- pmap_df(priors, gen_prior_pred)

ggplot(prior_student_marks, aes(x, y, group = n, fill = beta)) +
  geom_histogram(aes(y = ..count..), binwidth = 10, alpha = 0.5) +
  labs(x = "Student Marks", y = "Frequency", title = "Histogram of Student Marks") +
  theme_minimal() 