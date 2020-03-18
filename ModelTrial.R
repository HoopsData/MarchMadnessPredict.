#Project to create a model that will rank this season's March Madness Competitors
#Libraries
library(ggplot2)
library(rpart)
library(ggcorrplot)
library(purrr)
library(tidyverse)
library(caret)
library(leaps)
library(dplyr)
library(caret)
library(MASS)
#Load Data
df <- read.csv("df2.csv", header = TRUE)
#Remove NIT
df <- df[-c(which(df$Tourney.Wins==99)),]
df[36, 1] = 1
#Remove useless stats
df <- df[,-c(2,3,4)]
#Make the entire df numeric
df <- mutate_all(df, function(x) as.numeric(as.character(x)))
#Make a Correlation Matrix
corr <- round(cor(df, method = "spearman"), 3)
head(corr[])
ggcorrplot(corr)
#Creating a step-wise regression
full.model <- lm(df$Tourney.Wins ~., data = df)
step.model <- stepAIC(full.model, direction = "both", trace = FALSE)
#Cross Validate
train_control <- trainControl(method="cv", number=10)
model <- train(Tourney.Wins~ AdjO+AdjD+AdjEM.1+Luck+OppO+X.5+OppD+AdjEM.2,
               data=df, trControl=train_control, method="lm")
print(model)
#Evaluating the model by applying it to the 2018 bracket...
#Load and fix 2018 real quick
df.2018 <- read.csv("2018.csv", header = TRUE)
df.2018 <- df.2018[,-c(2,3,4)]
df.2018 <- df.2018[-c(which(df.2018$Rk==99)),]
df.2018 <- df.2018[,-c(1)]
df.2018 <- mutate_all(df.2018, function(x) as.numeric(as.character(x)))
df.read.names <- read.csv("2018.csv", header = TRUE) #just for my con.
df.read.names <- df.read.names[-c(which(df.read.names$Rk==99)),]
#Make the prediction
Pred.2018 <- predict(model, df.2018, interval="confidence")
#Applied for this season
df.2019 <- read.csv("df3.csv", header = TRUE)
Pred.2019 <- predict(model, df.2019, interval="confidence")
df.2019$Rk <- Pred.2019
#Check on last season
df.known <- read.csv("df2.csv", header = TRUE)
Pred.known <- predict(model, df.known, interval="confidence")
df.known$Conf <- Pred.known
