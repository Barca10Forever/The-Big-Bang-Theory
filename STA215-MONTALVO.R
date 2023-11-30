## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2023_11_16
# Who:       Zachary D. Kline



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")



##################################################################################
############### STEP 1: Table 1    ####################   
##################################################################################
mean(data$Scientific_Words)
sd(data$Scientific_Words)
table(data$Scientific_Words)
describe(data$Scientific_Words)
summary(data$Scientific_Words)

mean(data$Humor_Laughter)
sd(data$Humor_Laughter)
table(data$Humor_Laughter)
describe(data$Humor_Laughter)
summary(data$Humor_Laughter)

mean(data$Character_Development)
sd(data$Character_Development)
table(data$Character_Development)
describe(data$Character_Development)
summary(data$Character_Development)

mean(data$Scientific_Accuracy)
sd(data$Scientific_Accuracy)
table(data$Scientific_Accuracy)
describe(data$Scientific_Accuracy)
summary(data$Scientific_Accuracy)
##################################################################################
####################  STEP 2: Table  2             ####################   
##################################################################################
table(data$Character_Development)
table(data$Scientific_Accuracy)

##################################################################################
####################   STEP 3: Chi squared test             ####################   
##################################################################################
chisq.test(data$Character_Development,data$Scientific_Accuracy)

##################################################################################
####################  STEP 4: ANOVA                ####################   
##################################################################################
anova <- aov( Scientific_Words ~ Character_Development, data = data)
summary(anova)
anova <- aov( Humor_Laughter ~ Scientific_Accuracy, data = data)
summary(anova)
##################################################################################
####################  STEP 5: Correlation                ####################   
##################################################################################
cor(data$Scientific_Words,data$Humor_Laughter)

##################################################################################
####################  STEP 6: Linear Regression                ####################   
##################################################################################
linear_relationship <- lm(Humor_Laughter ~ Scientific_Words, data = data)
summary(linear_relationship)
##################################################################################
####################  STEP 7: Figure 1                                ####################   
##################################################################################
x_mean <- mean(data$Scientific_Words)
y_mean <- mean(data$Humor_Laughter)

plot(data$Scientific_Words, data$Humor_Laughter)
abline(linear_relationship, col= "red")

abline(v = x_mean, col = "red")
abline(h = y_mean, col = "red")
##################################################################################
####################  STEP 8: Examine residuals                     ####################   
##################################################################################
plot(data$Scientific_Words, residuals(linear_relationship))
abline(h = 0, col = "red")