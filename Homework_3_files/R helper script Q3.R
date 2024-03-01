# The parameters used in a beta distribution are alpha and beta.
# The alpha parameter is the shape parameter that controls the skewness of a beta distribution.
# The beta parameter is the shape parameter that controls the spread of a beta distribution.

# The changes that needs to be made to the dependent variables are, 
# 1. to standardise the data, so that mean=0 and sd=1 ; This is a common preprocessing step to ensure that the data is on a consistent scale.
# 2. convert the parameters into alpha and beta using the following equation: 

#alpha <- (mu^2 * (1 - mu) / sigma^2 - mu) / (mu - 1)
#beta <- alpha * (1 - mu) / mu

#where, mu=mean, sigma^2= standard deviation



# Example of a beta distribution plot with alpha=10 beta=8

# The approximate range of the scores is 40%-60%. 

library(ggplot2)

# Define parameter values for the Beta distribution
alpha <- 10   
beta <- 8   

# Generate a sequence of student marks between 0 and 100 
marks <- seq(0, 100, length.out = 100)

# Calculate the probability density function (PDF) for the Beta distribution
# We divide marks by 100 to scale to [0, 1], because this is a beta distribution
pdf <- dbeta(marks / 100, shape1 = alpha, shape2 = beta)  

# Create data frame for plotting
df <- data.frame(marks = marks, pdf = pdf)

# Plot the Beta distribution
ggplot(df, aes(x = marks, y = pdf)) +
  geom_line(color = "blue") +
  ggtitle("Example Beta Distribution of Student Marks") +
  labs(x = "Student Marks", y = "Density") +
  theme_minimal()



# To create a set of informative and a set of weakly-informative priors for the parameters in our model.
# The informative priors taken are alpha (5,10) and beta(2,8). These priors give an approximate estimation of possible probability distribution of marks. 
# The weakly informative priors taken are alpha (1,0.5), beta(1,0.5). These priors are not representing the possible outcomes. 

# Load required packages
library(ggplot2)

# Define parameter values for informative priors
alpha_inf <- c(5, 10)  
beta_inf <- c(2, 8)   

# Define parameter values for weakly-informative priors
alpha_weak <- c(1, 0.5)  
beta_weak <- c(1, 0.5)   

# Generate range of probabilities (0 to 1)
probs <- seq(0, 1, by = 0.01)

# Calculate PDF for informative priors
pdf_inf_1 <- dbeta(probs, alpha_inf[1], beta_inf[1])
pdf_inf_2 <- dbeta(probs, alpha_inf[2], beta_inf[2])

# Calculate PDF for weakly-informative priors
pdf_weak_1 <- dbeta(probs, alpha_weak[1], beta_weak[1])
pdf_weak_2 <- dbeta(probs, alpha_weak[2], beta_weak[2])

# Create data frames for informative and weakly-informative priors
df_inf <- data.frame(probs, pdf_inf_1, pdf_inf_2)
df_weak <- data.frame(probs, pdf_weak_1, pdf_weak_2)

# Plot informative priors
ggplot(df_inf, aes(x = probs)) +
  geom_line(aes(y = pdf_inf_1), color = "blue", linetype = "solid", size = 1) +
  geom_line(aes(y = pdf_inf_2), color = "red", linetype = "solid", size = 1) +
  labs(x = "Probability", y = "Density", title = "Informative Priors") +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal()

# Plot weakly-informative priors
ggplot(df_weak, aes(x = probs)) +
  geom_line(aes(y = pdf_weak_1), color = "green", linetype = "solid", size = 1) +
  geom_line(aes(y = pdf_weak_2), color = "purple", linetype = "solid", size = 1) +
  labs(x = "Probability", y = "Density", title = "Weakly-Informative Priors") +
  scale_color_manual(values = c("green", "purple")) +
  theme_minimal()





# Load required packages
library(ggplot2)

# Define the parameter values for the Beta distributions
alpha <- c(2:25)  # Additional alpha values
beta <- c(2)      # Additional beta values

# Generate a sequence of student marks between 0 and 100
marks <- seq(0, 100, length.out = 100)

# Initialize an empty data frame to store the results
df <- expand.grid(alpha = alpha, beta = beta, marks = marks)

# Calculate the probability density function (PDF) for each Beta distribution
df$PDF <- dbeta(df$marks / 100, shape1 = df$alpha, shape2 = df$beta)  # Divide marks by 100 to scale to [0, 1]

# Plot the prior distributions for student marks
ggplot(df, aes(x = marks, y = PDF, color = as.factor(alpha))) +
  geom_line() +
  ggtitle("Prior Distributions for PS947 Grades") +
  labs(x = "Student Marks", y = "Probability density", color = "Alpha") +
  theme_minimal()


##Last part
# Load required packages
library(ggplot2)

# Define additional parameter values for the Beta distributions
alpha <- c(2:25)  # Additional alpha values
beta <- c(2)      # Additional beta values

# Generate a sequence of student marks between 0 and 100
marks <- seq(0, 100, length.out = 100)

# Initialize an empty data frame to store the results
df <- expand.grid(alpha = alpha, beta = beta, marks = marks)

# Calculate the probability density function (PDF) for each Beta distribution
df$PDF <- dbeta(df$marks / 100, shape1 = df$alpha, shape2 = df$beta)  # Divide marks by 100 to scale to [0, 1]

# Plot the prior distributions for student marks
ggplot(df, aes(x = marks, y = PDF, color = as.factor(alpha))) +
  geom_line() +
  ggtitle("Prior Distributions for PS947 Grades") +
  labs(x = "Student Marks", y = "Probability density", color = "Alpha") +
  theme_minimal()


