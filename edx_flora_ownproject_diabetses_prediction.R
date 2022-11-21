library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)
library(corrplot)
library(tidyr)
library(rpart)
library(naivebayes)
# here i'm going to set my working directory where my dataset will be loaded
setwd("C:/Users/Flora/OneDrive/Desktop/RStudio Projects/diabetes/data")

# after setting working directory, I'm going to read my data set 
diabetes_dataset <- read.csv("diabetes.csv")

#now after reading my dataset I start data exploratory and analysis of my dataset

#I use head to return my observation of data set
head(diabetes_dataset)

#here I'm going to get statistical analysis of my dataset 

summary(diabetes_dataset)

#I'm going to represent my data in visual representation to check if they have reasonable distribution
# age histogram 
diabetes_dataset %>%
  ggplot(aes(x=Age))+ggtitle("Age")+
  geom_histogram(aes(y= 100*(..count..)/sum(..count..)),binwidth = 2, color="black",fill="green")+ylab("Percentage")

#DiabetesPedigreeFunction histogram
diabetes_dataset %>%
  ggplot(aes(x=DiabetesPedigreeFunction))+ggtitle("Diabetes Pedigree Function")+
  geom_histogram(aes(y= 100*(..count..)/sum(..count..)),binwidth = 2, color="black",fill="blue")+ylab("Percentage")
#Pregnancies diagram

diabetes_dataset %>%
  ggplot(aes(x=Pregnancies))+ggtitle("Pregnancies")+
  geom_histogram(aes(y= 100*(..count..)/sum(..count..)),binwidth = 2, color="black",fill="pink")+ylab("Percentage")

#insulin diagram
diabetes_dataset %>%
  ggplot(aes(x=Insulin))+ggtitle("Insulin")+
  geom_histogram(aes(y= 100*(..count..)/sum(..count..)),binwidth = 2, color="black",fill="black")+ylab("Percentage")

#Body Mass Index diagram
diabetes_dataset %>%
  ggplot(aes(x=BMI))+ggtitle("Body Mass Index")+
  geom_histogram(aes(y= 100*(..count..)/sum(..count..)),binwidth = 2, color="black",fill="blue")+ylab("Percentage")

#skin thickness
diabetes_dataset %>%
  ggplot(aes(x=SkinThickness))+ggtitle("SkinThickness")+
  geom_histogram(aes(y= 100*(..count..)/sum(..count..)),binwidth = 2, color="black",fill="blue")+ylab("Percentage")

#Blood Pressure
diabetes_dataset %>%
  ggplot(aes(x=BloodPressure))+ggtitle("Blood Pressure")+
  geom_histogram(aes(y= 100*(..count..)/sum(..count..)),binwidth = 2, color="black",fill="blue")+ylab("Percentage")

#Glucose diagram
diabetes_dataset %>%
  ggplot(aes(x=Glucose))+ggtitle("Glucose")+
  geom_histogram(aes(y= 100*(..count..)/sum(..count..)),binwidth = 2, color="black",fill="blue")+ylab("Percentage")

# as all numeric variables have reasonable distribution, I WILL USE THEM for regression
#Correlation between numerical variables
numeric.var <-sapply(diabetes_dataset,is.numeric)
corr.matrix <- cor(diabetes_dataset[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", order = "hclust", tl.col = "black", tl.srt=45, tl.cex=0.6, cl.cex=0.6)
box(which = "outer", lty = "solid")

#now we check correlation between numerical variables and outcome

attach(diabetes_dataset)
par(mfrow=c(2,4))
boxplot(Pregnancies~Outcome, main="No. of Pregnancies vs. Diabetes", 
        xlab="Outcome", ylab="Pregnancies",col="purple")
boxplot(Glucose~Outcome, main="Glucose vs. Diabetes", 
        xlab="Outcome", ylab="Glucose",col="pink")
boxplot(BloodPressure~Outcome, main="Blood Pressure vs. Diabetes", 
        xlab="Outcome", ylab="Blood Pressure",col="blue")
boxplot(SkinThickness~Outcome, main="Skin Thickness vs. Diabetes", 
        xlab="Outcome", ylab="Skin Thickness",col="green")
boxplot(Insulin~Outcome, main="Insulin vs. Diabetes", 
        xlab="Outcome", ylab="Insulin",col="yellow")
boxplot(BMI~Outcome, main="BMI vs. Diabetes", 
        xlab="Outcome", ylab="BMI",col="red")
boxplot(DiabetesPedigreeFunction~Outcome, main="Diabetes Pedigree Function vs. Diabetes", xlab="Outcome", ylab="DiabetesPedigreeFunction",col="lightgreen")
boxplot(Age~Outcome, main="Age vs. Diabetes", 
        xlab="Outcome", ylab="Age",col="lightblue")
box(which = "outer", lty = "solid")

# Blood pressure and skin thickness show little variation with diabetes so they will be escarded
#now we will train our set with regression model
diabetes_dataset$BloodPressure <- NULL
diabetes_dataset$SkinThickness <- NULL
train <- diabetes_dataset[1:540,]
test <- diabetes_dataset[541:768,]
model <-glm(Outcome ~.,family=binomial(link='logit'),data=train)
summary(model)

# From above analysis model we see that the most relevant feauters are Pregnancies, Glucose and BMI because they have low p-value
#Age and Insulin are rejected because their p-value are not statistically significant
anova(model,test = "Chisq")

#Cross validation model

fitted.results <- predict(model,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Outcome)
print(paste('Accuracy',1-misClasificError))
(conf_matrix_logi <-table(fitted.results,test$Outcome))
#decision tree
library(rpart)
model2 <- rpart(Outcome ~ Pregnancies + Glucose + BMI + DiabetesPedigreeFunction, data=train,method="class")
plot(model2, uniform=TRUE, 
     main="Classification Tree for Diabetes")
text(model2, use.n=TRUE, all=TRUE, cex=.8)
box(which = "outer", lty = "solid")


#if the BMI is less than 45.4 and diabetes pedigree functions is less than 0.8745, the person is likely to have diabetes

#confusion table and accuracy

tree_prediction <- predict(model2,test, type = 'class')
(conf_matrix_tree <-table(tree_prediction,test$Outcome))

mean(tree_prediction == test$Outcome)

#naive bayes model
