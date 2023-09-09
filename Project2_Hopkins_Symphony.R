#############################################
#                                           #
# Author:     Symphony Hopkins              #
# Date:       03/20/2023                    #
# Subject:    Project 1                     #
# Class:      DSCI 512                      #
# Section:    01W                           #         
# Instructor: Juan David Munoz              #
# File Name:  Project2_Hopkins_Symphony.R   #
#                                           #
#############################################

#0.0. Importing Dataset.

library(readxl)
mtcars <- read_excel("Documents/Maryville_University/DSCI_512/Week_1/mtcars.xlsx")
View(mtcars)

#1.1. Use the lm() function to perform a simple linear 
#     regression with the response mpg and the predictor hp.
#     Answer: See code.

lm_1 <- lm(mpg ~ hp, data=mtcars)
summary(lm_1)

#2.1. Is there a relationship between the target mpg and 
#     predictor hp?
#     Answer: Our p-value, 2.723e-09, is less than our significance
#     level (α = 0.05), which means there is a relationship between
#     the target mpg and predictor hp.

summary(lm_1)

#3.1. How strong is the relationship between the response 
#     and predictor?
#     Answer: With r-squared equal to 0.6006001, we can say that the
#     relationship between the response (mpg) and predictor (hp) is
#     moderate. The r-squared value is higher than 0.5, but not high
#     enough to say that the relationship is strong.

summary(lm_1)$r.squared

#4.1. Is the relationship between mpg and hp positive 
#     or negative?
#     Answer: The estimate number for hp is -0.064548, which means the
#     relationship is negative.

summary(lm_1)

#5.1. What is the predicted mpg associated with a horsepower (hp) 
#     of 100? 
#     Answer: The predicted mpg is 22.84317.

hp_100_pred <- predict(lm_1, newdata=data.frame(hp=c(100)), interval="confidence",
                       level=0.95)
print(hp_100_pred)

#5.2. What’s the 95% confidence interval for the predicted 
#     mpg?
#     Answer: The confidence interval is [21.5279, 24.15844] at 95%
#     confidence.

print(hp_100_pred)

#6.1. Plot the response and the predictor and add the regression 
#     line using abline().
#     Answer: See code.

library(ggplot2)
ggplot(mtcars,aes(x=hp,y= mpg)) + geom_point()+
  geom_smooth(method=lm)

#7.1. Perform a multiple linear regression with mpg as 
#     the response and the predictors cyl, disp, hp, wt, 
#     vs, and gear. 
#     Answer: See code.

lm_2 <- lm(mpg ~ cyl + disp + hp + wt + vs + gear, data=mtcars)

#7.2. Print out the results using summary() function.
#     Answer: See code.

summary(lm_2)

#8.1. Is there a relationship between the predictors 
#     and the response?
#     Answer: Our p-value, 2.568e-12, is less than our significance
#     level (α = 0.05), which means there is a relationship between
#     the predictors and response.

summary(lm_2)

#9.1. Which predictors appears to have a statistically 
#     significant relationship to the response? 
#     Answer: cyl, hp, and wt

summary(lm_2)


#10.1.Use * symbols to fit linear regression models with 
#     interaction effects between hp and wt. 
#     Answer:

lm_3 <- lm(mpg ~ hp*wt, data=mtcars)
summary(lm_3)

#10.2.Does this interaction appear to be statistically 
#     significant?
#     Answer: Our p-value, 0.000362, is less than our significance
#     level (α = 0.05), which means there is a statistically significant
#     interaction between hp and wt.

summary(lm_3)


#End Assignment



