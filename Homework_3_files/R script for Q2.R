# The Effect of Recommendation on Choice
  
##Fitting the model and explaination. 
#We need a model to explain the effect of mode(auditory, visual) and other variables in following recommendations. 
#A logistic regression is used because the response variable is a binary output that is; whether a recommendation is followed or not (1,0).  . 
#The response variable here is recommendation followed and this is a binary data (yes/no)
#In the first model, all the variables are used to see how much each of them explains the response variable and their corresponding levels of significance.
#We use the composite intellect for the second model as it is a composite measure of perceived intellect using the ratings of competence, intelligence and thoughtfulness, this avoids the risk of multicollinearity in the model. The other variable is the mode (visual,auditory)
#The second model is used to see how much of the variance of the model is explained by the significant variables from the previous model. In our model it is, the mode and the composite intellect. 

library(ggplot2)
library(dplyr)

data <- read.csv("C:/Users/ASUS/Documents/GitHub/Homework_assignment/recommendations.csv")
summary(data)

#Fit the logistic regression model 1. 
model <- glm(RecommendationFollowed ~ Mode, data = data, family = binomial)
summary(model)

data$Mode1<- as.factor(data$Mode)
model_1 <- glm(RecommendationFollowed ~ Mode1  + CompositeIntellect, data = data, family = binomial)
summary(model_1)

#Here, the mode is the only variable that has significance. 

#Fit the logistic regression model 2. 

data$Mode1<- as.factor(data$Mode)
model_2 <- glm(RecommendationFollowed ~ Mode1  + CompositeIntellect, data = data, family = binomial)
summary(model_2)

#Here the mode and the composite intellect are significant. 
#Here the intercept -1.96 represents log-odds of following recommendations when all other predictor variables are held constant at zero.  
# The coefficient of Mode1Visual, which is approximately -0.73, signifies that while holding all other predictor variables constant, receiving the recommendation visually is associated with a decrease in the log-odds of following the recommendation by approximately 0.73 units compared to receiving it through auditory mode.
# The coefficient of Composite Intellect, which is approximately 0.39, indicates that while holding all other predictor variables constant, for every one-unit increase in composite intellect, there is an expected increase of approximately 0.39 units in the log-odds of following the recommendation.
# This implies that higher levels of composite intellect are associated with higher odds of following the recommendation, assuming all other factors remain constant.

#Assessment of whether there is evidence for recommendation mode affecting choice

#To understand the probability of following recommendation after switching from auditory to visual mode.
#We use marginal effects to know the probability of following recommendation

install.packages('margins')

library(margins)

# Compute marginal effects for categorical_variable when it equals 1
summary(margins(model_1))

for (i in (1:9)) {
  print(summary(margins(model_1, variables = "CompositeIntellect", at=list(CompositeIntellect=i))))
}
summary(data$CompositeIntellect)

table(data$RecommendationFollowed,data$Mode)

table_data <- table(data$RecommendationFollowed, data$Mode)

# Convert table to data frame
table_df <- as.data.frame.table(table_data)

#Plotting the graph 

ggplot(table_df, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Recommendation Followed by Mode",
       x = "Recommendation Followed",
       y = "Count") +
  scale_fill_manual(values = c("lightblue", "lightgreen")) +
  theme_minimal()
