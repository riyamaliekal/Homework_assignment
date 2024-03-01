# Load the required libraries
library(ggplot2)
library(dplyr)

# Read the data from the CSV file
data <- read.csv("C:/Users/ASUS/Documents/GitHub/Homework_assignment/recommendations.csv")

# Fit a logistic regression model to analyze the effect of recommendation mode on whether a recommendation is followed
model <- glm(RecommendationFollowed ~ Mode, data = data, family = binomial)
summary(model)

# Create a new data frame to use for plotting
new_data <- data.frame(Mode = c("Auditory", "Visual"))

# Predict the probabilities of following the recommendation for each mode
new_data$prob <- predict(model, new_data, type = "response")

# Create the plot
ggplot(new_data, aes(x = Mode, y = prob)) +
  geom_bar(stat = "identity", fill= 'purple', width=0.25) +
  labs(x = "Recommendation Mode", y = "Probability of Following Recommendation", title = "Effect of Recommendation Mode on Whether a Recommendation is Followed") +
  theme_minimal()

model_visual <- glm(RecommendationFollowed ~ Competent + Intelligent + Thoughtful + CompositeIntellect, data = data[data$Mode == "Visual",], family = "binomial")
summary(model_visual)
anova(model_visual)

model_auditory <- glm(RecommendationFollowed ~ Competent + Intelligent + Thoughtful + CompositeIntellect, data = data[data$Mode == "Auditory",], family = "binomial")
summary(model_auditory)
anova(model_auditory)




#Baysian Model
library(tibble)

getAlphaBeta <- function(mu, sigma) {
  alpha <- (mu^2 * (1 - mu) / sigma^2 - mu) / (mu - 1)
  beta <- alpha * (1 - mu) / mu
  return(list(alpha=alpha, beta=beta))}


params <- getAlphaBeta(mu=0.5, sigma=0.1)
alpha <- params$alpha
beta <- params$beta


# function that computes likelihood for a given aplha and beta]
priors <- tibble(n = 1:50) %>%
  group_by(n) %>%
  mutate(alpha = 5, beta = 0.2,  
         mu = rbeta(1, alpha, beta))

ggplot(priors, x=alpha)


gen_prior_pred <- function(n, alpha, beta) {
  x <- seq(0, 100, 0.1)
  d <- tibble(n = n, alpha = alpha, beta = beta,
              x = x,
              y = dbeta(x, alpha, beta))
  return(d)}
  
gen_prior_pred(50,95,0.5)

prior_llh <- pmap_df(select(priors, -mu), gen_prior_pred)



alpha <- 5
beta <- 2

x <- seq(0, 1, length.out = 1000)
y <- dbeta(x, alpha, beta)

df <- data.frame(x, y)

ggplot(df, aes(x = x, y = y)) +
  geom_line() +
  ggtitle("Beta Distribution with alpha = 5, beta = 2")

# Define the parameter values for the Beta distribution
alpha <- 10
beta <- 2

# Generate a sequence of values between 0 and 1
x <- seq(0, 1, length.out = 100)

# Calculate the probability density function (PDF) for the Beta distribution
y <- dbeta(x, shape1 = alpha, shape2 = beta)

#informative prior distribution is alpha=5 and beta=2, 
#as we can convert the normal distribution to beta distribution by the equation= mu

# Plot the prior distribution
ggplot(data.frame(x, y), aes(x = x, y = y)) +
  geom_line() +
  ggtitle("Prior Distribution for PS947 Grades")


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




# Create table of mu and sigma values 

priors <- tibble(n = 1:50) %>% group_by(n) %>% 
  mutate(mu = rnorm(1, 67, 5), sigma = rexp(1, 0.5)) 

# Function that converts normal distribution parameters to beta distribution parameters 

get_alpha_beta <- function(mu, sigma) { 
  alpha <- ( (1 - mu) / sigma^2 - 1 / mu ) * mu^2  
  beta <- alpha * (1 / mu - 1) 
  return(list(alpha=alpha, beta=beta)) 
} 

#Apply function to each prior sample 

prior_params <- pmap()

# Function that computes likelihood for a given alpha and beta 16 

gen_prior_pred <- function(n, alpha, beta) { 
  x <- seq(0, 100, 0.1) 
  d <- tibble(n = n, alpha = alpha, beta = beta, 
              x = x, 
              y = dbeta(x, alpha, beta)) 
  
  return(d) 
} 

# Apply function to each set of alpha and beta parameters 25 

prior_llh <- pmap_df(prior_params, gen_prior_pred)




library(stats)
set.seed(123) # for reproducibility
n <- 1000 # number of observations
alpha=shape1 <- 3
beta=shape2 <- 4
grades <- rbeta(n, alpha, beta) * 100

hist(grades, main = "Distribution of Grades", xlab = "Grade", col = "blue")

mean(grades)




shape1 <- mean^2 / var - mean
shape2 <- (1 - mean)^2 / var - mean

# Define mean and variance
mean <- 67
var <- 2

# Generate alpha and beta parameters from a beta distribution
SIPriors <- tibble(n = 1:50) %>% group_by(n) %>% 
  mutate(shape1 = mean^2 / var - mean,
         shape2 = (1 - mean)^2 / var - mean,
         alpha = rbeta(1, shape1, shape2),
         beta = rbeta(1, shape1, shape2))

# Creating a function which generates prior predictions:

GenPriorPred <- function(n, alpha, beta, shape1, shape2) {
  x <- seq(0, 1, 0.1)
  d <- tibble(n = 1, alpha = alpha, beta = beta,
              shape1 = shape1, shape2 = shape2,
              marks = x, 
              density = dbeta(x, shape2, shape1))
  return(d) }

SIPriorsPlot <- pmap_df(SIPriors, GenPriorPred)

ggplot(SIPriorsPlot, aes(marks, density, group = n)) + geom_path(alpha = 0.25)



# Define the parameters of the beta distribution
alpha <- 5
beta <- 2

# Generate a sequence of possible student marks
x <- seq(0, 100, 0.1)

# Calculate the probability density function for each mark
y <- dbeta(x, alpha, beta)

# Plot the probability distribution
plot(x, y, type = "l", xlab = "Student Marks", ylab = "Density",
     main = paste("Beta Distribution with alpha =", alpha, "and beta =", beta))





priors <- tibble(n = 1:50) %>%
  group_by(n) %>%
  mutate(alpha = max(5, runif(1, 1, 10)), beta = max(0.2, runif(1, 1, 10)))

gen_prior_pred <- function(n, alpha, beta) {
  mu <- alpha / (alpha + beta) # calculate the mean of the beta distribution
  x <- seq(0, 100, 0.05)
  y <- dbeta(x, alpha, beta)
  d <- tibble(n = n, alpha = alpha, beta = beta,
              marks = x, 
              density = y,
              mu = mu)
  return(d)
}

prior_student_marks <- pmap_df(priors, gen_prior_pred)

ggplot(prior_student_marks, aes(marks, density, group = n, color = alpha)) + 
  geom_path(alpha = 0.5) +
  theme_minimal() +
  labs(x = "Student Marks", y = "Density", color = "alpha") +
  ggtitle("Distribution of Student Marks (Beta)")




set.seed(100)

priors <- tibble(n = 1:50) %>% 
  group_by(n) %>%
  mutate(alpha = rbeta(1, 67, 75), beta = rbeta(1, 10, 30))

gen_prior_pred <- function(n, alpha, beta) {
  x <- seq(0, 100, 0.05)
  y <- dbeta(x, min = alpha - beta, max = alpha + beta)
  
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
  ggtitle("Distribution of Student Marks")

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