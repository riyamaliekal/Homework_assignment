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
  
  
  
  
  
  # Final Answer script
  
  
  
  The Effect of Recommendation on Choice

2.1 Fitting the model and explanation.

We need a model to explain the effect of mode(auditory, visual) and other variables in following recommendations.

A logistic regression is used because the response variable is a binary output that is; whether a recommendation is followed or not (1,0).

The response variable here is recommendation followed and this is a binary data (yes/no)

In the first model, all the variables are used to see how much each of them explains the response variable and their corresponding levels of significance.

We use the composite intellect for the second model as it is a composite measure of perceived intellect using the ratings of competence, intelligence and thoughtfulness, this avoids the risk of multicollinearity in the model. The other variable is the mode (visual,auditory)

The second model is used to see how much of the variance of the model is explained by the significant variables from the previous model. In our model it is, the mode and the composite intellect.

{r}
data=read.csv("C:/Users/ASUS/Documents/GitHub/Homework_assignment/recommendations.csv")

Fit the logistic regression model 1.

{r setup, warning=FALSE, message=FALSE}
library(tidyverse)
model_1=glm(RecommendationFollowed ~ Mode + Competent + Intelligent + Thoughtful + CompositeIntellect, data = data, family=binomial)
summary(model_1)

Here, the mode is the only variable that has significance.

Fit the logistic regression model 2.

{r}
data$Mode1<- as.factor(data$Mode)
model_2 <- glm(RecommendationFollowed ~ Mode1  + CompositeIntellect, data = data, family = binomial)
summary(model_2)

Here the mode and the composite intellect are significant.

Here the intercept -1.96 represents log-odds of following recommendations when all other predictor variables are held constant at zero.

The coefficient of Mode1Visual, which is approximately -0.73, signifies that while holding all other predictor variables constant, receiving the recommendation visually is associated with a decrease in the log-odds of following the recommendation by approximately 0.73 units compared to receiving it through auditory mode.

The coefficient of Composite Intellect, which is approximately 0.39, indicates that while holding all other predictor variables constant, for every one-unit increase in composite intellect, there is an expected increase of approximately 0.39 units in the log-odds of following the recommendation.

This implies that higher levels of composite intellect are associated with higher odds of following the recommendation, assuming all other factors remain constant.

2.2 Assessment of whether there is evidence for recommendation mode affecting choice

To understand the probability of following recommendation after switching from auditory to visual mode.

We use marginal effects to know the probability of following recommendation with respect to the modes.

Please note: The marginal effects is running for R script but there is an error in quarto file. The codes used are as follows:

install.packages('margins')

library(margins) summary(margins(model_1)) for (i in (1:9)) { print(summary(margins(model_1, variables = "CompositeIntellect", at=list(CompositeIntellect=i)))) }

The marginal effect of 'Mode' being visual (compared to auditory) on the probability of "following the recommendation" is approximately -0.16.

While keeping all the other variables constant, people who received recommendation through visual mode is expected to have a 16 percentage decrease in the probability of following the recommendation when compared to the auditory mode.

The composite intellect is higher in the range of 5-7, indicating that for both visual and auditory mode the composite intellect was taken into account when following recommendation. That is, the composite intellect of auditory is more than the visual. It also increases with probability of following recommendation.

Higher the recommender's composite intellect, higher the probability for the recommendation made to be followed.

2.3 Distinction Material

The table clearly shows that recommendations are followed more when the mode of recommendation is auditory and the recommendation are less likely to be followed when the mode of recommendation is visual.

The previous analysis also provides us with the data that the mode of recommendation is not the best predictor of the recommendations being followed.

As the composite intellect is the composite measure of perceived intellect using the ratings of competence, intelligence and thoughtfulness, this can represent the combination of these datas.

Individually the variables;competence, intelligence and thoughtfulness are weak. But when composite intellect is taken into account, it shows more significance.

There is a stronger relationship between the Composite intellect and recommendation followed.

There is an increase in recommendation followed with every unit increase in composite intellect.

This suggests that composite intellect plays an important role in predicting the following of recommendation.

As the figure represents, the auditory mode is higher than the visual mode, for recommendation being followed.

With this evidence, we can conclude that the composite intellect with auditory mode has more probability of following recommendation when compared to visual mode with composite intellect.

{r}
table(data$RecommendationFollowed,data$Mode)
table_data <- table(data$RecommendationFollowed, data$Mode)
table_df <- as.data.frame.table(table_data)

{r}
library(ggplot2)
ggplot(table_df, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Recommendation Followed by Different Modalities",
       x = "Recommendation not followed(0) v/s Recommendation followed(1)",
       y = "Count") +
  scale_fill_manual(values = c("lightblue", "lightgreen")) +
  theme_minimal()

Selecting appropriate priors

3.1 Beta Distribution with alpha and beta

The parameters used in a beta distribution are alpha and beta.

The alpha parameter is the shape parameter that controls the skewness of a beta distribution.

The beta parameter is the shape parameter that controls the spread of a beta distribution.

The changes that needs to be made to the dependent variables are,

to standardise the data, so that mean=0 and SD=1 ; This is a common preprocessing step to ensure that the data is on a consistent scale.

convert the parameters into alpha and beta using the following equation:

alpha <- (mu^2 * (1 - mu) / sigma^2 - mu) / (mu - 1)

beta <- alpha * (1 - mu) / mu

where, mu=mean, sigma^2= standard deviation

3.2 Example of a beta distribution plot with alpha=10 beta=8

The approximate range of the scores is 40%-60%.

The plot has a peak near the range of 60.

{r}
library(ggplot2)
alpha <- 10
beta <- 8
marks <- seq(0, 100, length.out = 100)
pdf <- dbeta(marks / 100, shape1 = alpha, shape2 = beta)
df <- data.frame(marks = marks, pdf = pdf)
ggplot(df, aes(x = marks, y = pdf)) +
  geom_line(color = "blue") +
  ggtitle("Example Beta Distribution of Student Marks") +
  labs(x = "Student Marks", y = "Density") +
  theme_minimal()

3.3 Setting Informative and Weakly Informative Priors

To create a set of informative and a set of weakly-informative priors for the parameters in our model.

The informative priors taken are alpha (5,10) and beta(2,8). These priors give an approximate estimation of possible probability distribution of marks.

The weakly informative priors taken are alpha (1,0.5), beta(1,0.5). These priors are not representing the possible outcomes.

{r}
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
  geom_line(aes(y = pdf_inf_1), color = "blue", linetype = "solid", linewidth = 1) +
  geom_line(aes(y = pdf_inf_2), color = "red", linetype = "solid", linewidth = 1) +
  labs(x = "Probability", y = "Density", title = "Informative Priors") +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal()

# Plot weakly-informative priors
ggplot(df_weak, aes(x = probs)) +
  geom_line(aes(y = pdf_weak_1), color = "green", linetype = "solid", linewidth = 1) +
  geom_line(aes(y = pdf_weak_2), color = "purple", linetype = "solid", linewidth = 1) +
  labs(x = "Probability", y = "Density", title = "Weakly-Informative Priors") +
  scale_color_manual(values = c("green", "purple")) +
  theme_minimal()

3.4 Creating some prior predictions

Here the prior predictions are made with 23 alpha values and 1 fixed beta value.

{r}
# Load required packages
library(ggplot2)

# Define additional parameter values for the Beta distributions
alpha <- c(2:25)  
beta <- c(2)      

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

