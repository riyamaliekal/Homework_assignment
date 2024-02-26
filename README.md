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
  geom_point(data = data, aes(x = Mode, y = RecommendationFollowed)) +
  labs(x = "Recommendation Mode", y = "Probability of Following Recommendation", title = "Effect of Recommendation Mode on Whether a Recommendation is Followed") +
  theme_minimal()