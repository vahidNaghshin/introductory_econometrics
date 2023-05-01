library(tidyverse)
library('wooldridge')
#Problem 2
gpa <- c(2.8, 3.4, 3.0, 3.5, 3.6, 3.0, 2.7, 3.7)
act <- c(21, 24, 26, 27, 29, 25, 25, 30)

#i
act_mean <- mean(act)
gpa_mean <- mean(gpa)

#derive it via applying derived formula
beta_1 <- sum((gpa - gpa_mean) * (act-act_mean))/sum((act-act_mean)^2)
beta_0 <- gpa_mean - (beta_1 * act_mean)

# SLR using R function
model <- lm(gpa ~ act)
model

#ii
gpa_hat <- beta_0 + (beta_1 * act)
gpa_hat

res <- gpa - gpa_hat
res
mean(res)

##iii
gpa_20 <- beta_0 + (beta_1 * 20)
gpa_20

###iv
sst <- sum((gpa - gpa_mean)^2)
sse <- sum((gpa_hat - gpa_mean)^2)
ssr <- sum(res^2)

R_sq <- sse/sst
R_sq

#Also can be calcualted via squared correlation between gpa_hat and gpa
cor(gpa_hat, gpa)^2

#Problem 4
df <- bwght
model <- lm(df$bwght ~ df$cigs)
model
summary(model)

beta_0 <- 119.77190
beta_1 <- 0.51377

#i
#for cig=0, it is bwght_hat = beta_0 = 119.77190
#for cigar=20
(beta_0 - (beta_1 * 20))

#ii
#if we look at the correlation between cigar and other factors, we see non-zero
#correlation between them
cor(df)

#another way is looking at residual and check xero conditional mean assumption
#E(u|x) = 0 (which is different for three level of cigs)

x <- filter(df, df$cigs==1)
res <- x$bwght - (beta_0 + (beta_1 * x$cigs))
mean(res)

x <- filter(df, df$cigs==5)
res <- x$bwght - (beta_0 + (beta_1 * x$cigs))
mean(res)

x <- filter(df, df$cigs==10)
res <- x$bwght - (beta_0 + (beta_1 * x$cigs))
mean(res)

#iii
(125 - beta_0)/beta_1

#Problem 5
beta_0 <- -124.84
beta_1 <- 0.853

#ii
beta_0 + (30000 * beta_1)

#iii
inc <- seq(10000, 30000, 1000)
cons <- beta_0 + (beta_1 * inc)
mpc <- beta_1 #marginal propensity to consume (MPC)
apc <- beta_1 + (beta_0/inc) # average propensity to consume (APC) 
plot(inc, apc)

#problem 6
#i
#one percent (1%) increase in distance leads to 0.3% increase in price sell

#problem 7
#i
#E(u|inc) = E(sqrt(inc)*e) = sqrt(inc) * E(e) = 0
#ii
#Var(u|inc) = Var(sqrt(inc)*e) = inc * Var(e)
#iii

#problem 8
# The proof can be easily driven in a similar way to beta_1
# key takeaway:
# the beta_1^~ is rigor estimator with high bias but less variance while the beta_1 
# is low bias but high variance estimator

#problem C1
df <- k401k

#i
prate_mean <- mean(df$prate)
mrate_mean <- mean(df$mrate)
prate <- df$prate
mrate <- df$mrate

#ii
beta_1 <- sum((prate - prate_mean) * (mrate - mrate_mean))/sum((mrate-mrate_mean)^2)
beta_0 <- prate_mean - (beta_1 * mrate_mean)
beta_1
beta_0

model <- lm(prate ~ mrate, data=df)
summary(model)

#R-squared
cor(df$prate, predict(model, newdata=df))^2

prate_hat <- beta_0 + (beta_1 * df$mrate)
sst <- sum((prate - prate_mean)^2)
sse <- sum((prate_hat - prate_mean)^2)

R_sq <- sse/sst
R_sq

#iii
(beta_0 + (beta_1 * 2))

#problem C2
#i
df <- ceosal2
mean(df$lsalary)
mean(df$ceoten)
nrow(df)
#ii
nrow(filter(df, ceoten==0))
max(df$ceoten)
#iii
linear_model <- lm(lsalary~ceoten, data = df)
linear_model
# Creating a data frame
variable_ceoten <- data.frame(ceoten = c(4))
# Predict
predict(linear_model, newdata = variable_ceoten)

# Clear data
rm(list = ls())  # Removes all objects from environment
# Clear console
cat("\014")  # Mimics ctrl+L

#problem C3
df <- sleep75
#i
linear_model <- lm(sleep~totwrk, data = df)
linear_model
#number of observation
nrow(df)
#R-squared
cor(df$sleep, predict(linear_model, newdata=df))^2
#change in sleep by increasing totwork by 2 hrs
2*120*(-0.1507)

# Clear data
rm(list = ls())  # Removes all objects from environment
# Clear console
cat("\014")  # Mimics ctrl+L

#problem C4
df <- wage2
#i
mean(df$wage)
mean(df$IQ)
sd(df$IQ)
#ii
linear_model <- lm(wage~IQ, data = df)
beta_1 <- 8.303
15 * beta_1
#R-squared
cor(df$wage, predict(linear_model, newdata=df))^2
#iii
linear_model <- lm(lwage~IQ, data = df)
linear_model 
beta_1 <- 0.008807
100 * 15 * beta_1

# Clear data
rm(list = ls())  # Removes all objects from environment
# Clear console
cat("\014")  # Mimics ctrl+L

#problem C5
df <- rdchem
linear_model <- lm(lsales~lrd, data = df)
linear_model
# the elasticit is beta_1=0.8458
# one percent increase in rd leads to 0.8458% increase in sale

# Clear data
rm(list = ls())  # Removes all objects from environment
# Clear console
cat("\014")  # Mimics ctrl+L

#problem C6
df <- meap93
linear_model <- lm(math10~lexpend, data = df)
linear_model
#R-squared
math10_hat <- predict(linear_model, newdata = df)
cor(df$math10, math10_hat)^2

# Clear data
rm(list = ls())  # Removes all objects from environment
# Clear console
cat("\014")  # Mimics ctrl+L

#problem C7
df <- charity
#i
#mean of gift
mean(df$gift)
#ratio of people with no gift
nrow(filter(df, gift==0))/nrow(df)
#ii
mean(df$mailsyear)
max(df$mailsyear)
min(df$mailsyear)
#iii
linear_model <- lm(gift ~ mailsyear, data=df)
linear_model
#R-squared
cor(df$gift, predict(linear_model, data=df))^2

#iv
min(predict(linear_model, data=df))

# Clear data
rm(list = ls())  # Removes all objects from environment
# Clear console
cat("\014")  # Mimics ctrl+L


#problem C8
#i
x <- runif(500)*10
mean(x)
sd(x)
#ii
u <- rnorm(500) * 6
mean(u)
sd(u)
#iii
y <- 1 + (2 * x) + u
lm(y~x) 
#iv
y_hat <- predict(lm(y~x), data=x)
res <- y - y_hat
mean(res)
mean(x*res)
#v
mean(u)
mean(u*x)

# Clear data
rm(list = ls())  # Removes all objects from environment
# Clear console
cat("\014")  # Mimics ctrl+L

#Problem C9
df <- countymurders
#use only for year 1996
df <- filter(df, year==1996)
#how many county with zero murder
nrow(filter(df, murders==0))
#how many county with at least one execution
nrow(filter(df, execs > 0))
max(df$execs)
#ii
linear_model <- lm(murders~execs, data=df)
linear_model
#R-squared
cor(df$murders, predict(linear_model, newdata = df))^2
#iv
res <- predict(linear_model, newdata = df) - df$murders
mean(res)
mean(df$execs*res)

# Clear data
rm(list = ls())  # Removes all objects from environment
# Clear console
cat("\014")  # Mimics ctrl+L

#Problem C10
df <- catholic

#i
#number of students
nrow(df)
#mean & std of math12 and read12
mean(df$math12)
mean(df$read12)
sd(df$math12)
sd(df$read12)

#ii
linear_model <- lm(math12~read12, data=df)
linear_model
nrow(df)
#R-squared
cor(predict(linear_model, newdata = df), df$math12)^2

# Clear data
rm(list = ls())  # Removes all objects from environment
# Clear console
cat("\014")  # Mimics ctrl+L

#Problem C11
df <- gpa1
#i
nrow(df)
#mean and max 
mean(df$colGPA)
max(df$colGPA)
#ii
sum(df$PC)
#iii
linear_model <- lm(colGPA~PC, data=df)
linear_model

beta_0 <- mean(filter(df, PC==0)$colGPA)
beta_1 <- mean(filter(df, PC==1)$colGPA) - mean(filter(df, PC==0)$colGPA)
beta_0
beta_1
#iii
#R-squared
cor(predict(linear_model, newdata=df), df$colGPA)^2

#iv
# in order to derive the casual infrence the other factors should be almost the 
#same for two categories.
sd(filter(df, PC==0)$colGPA)
sd(filter(df, PC==1)$colGPA)
summary(linear_model)

# for example number of mothers graduated from college is higher for the people
#with PC than the ones without
mean(filter(df, PC==0)$mothcoll)
mean(filter(df, PC==1)$mothcoll)


