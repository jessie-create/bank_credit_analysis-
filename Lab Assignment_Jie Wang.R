# import dataset
library(readr)
Credit <- read_csv("~/Desktop/ALY6060/Credit.csv")
View(Credit)
summary(Credit)
hist(Credit$Balance)
install.packages("DataExplorer")
library(DataExplorer)
plot_bar(Credit)
plot_histogram(Credit)
plot_qq(Credit)
install.packages("igraph")
library(igraph)
# linear regression of income and balance
lm_Income<- lm(Balance~Income,data = Credit)
summary(lm_Income)
confint(lm_Income)
plot(Balance~Income,data = Credit, main = 'Balance with Income')
abline(lm_Income, col = "red")
#linear regression of account limit and balance
lm_Limit<- lm(Balance~Limit,data = Credit)
summary(lm_Limit)
confint(lm_Limit)
plot(Balance~Limit,data = Credit, main = 'Balance with Account Limit')
abline(lm_Limit, col = "red")
#linear regression of credit rating and balance
lm_Rating<- lm(Balance~Rating,data = Credit)
summary(lm_Rating)
confint(lm_Rating)
plot(Balance~Rating,data = Credit, main = 'Balance with Credit Rating')
abline(lm_Rating, col = "red")
#linear regression of age and balance
lm_Age<- lm(Balance~Age,data = Credit)
summary(lm_Age)
confint(lm_Age)
plot(Balance~Age,data = Credit, main = 'Balance with Age')
abline(lm_Age, col = "red")
#linear regression of education and balance
lm_Education<- lm(Balance~Education,data = Credit)
summary(lm_Education)
confint(lm_Education)
plot(Balance~Education,data = Credit, main = 'Balance with Education')
abline(lm_Education, col = "red")
#linear regression of whether student and balance
lm_Student<- lm(Balance~Student,data = Credit)
summary(lm_Student)
confint(lm_Student)
#linear regression of whether married and balance
lm_Married<- lm(Balance~Married,data = Credit)
summary(lm_Married)
confint(lm_Married)
#full model
lm_full<- lm(Balance~Income+Limit+Rating+Age+Education+Student+Married
                ,data = Credit)
summary(lm_full)
confint(lm_full)
# list the covariate with association 
library(tidyr)
library(tibble)
library(tidyverse)
data <- Credit[c(1:5,8)]
view(data)
cor(data)


