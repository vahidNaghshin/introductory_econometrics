library(tidyverse)
library('wooldridge')
#Problem C1
#i
# Positive as the higher income leads to higher nutritions and hence higher baby weight
#ii
# it is believed that cigs is correlated with income and guess it is negatively correlated
##iii
#considering faminc as another factor
df <- bwght
linear_model <- lm(bwght~cigs+faminc, data=df)
summary(linear_model)
#R-squared as squared of predicted and truth values
cor(predict(linear_model, newdata=df), df$bwght)^2

#consider simple regression on cigar only
linear_model <- lm(bwght~cigs, data=df)
summary(linear_model)
#R-squared as squared of predicted and truth values
cor(predict(linear_model, newdata=df), df$bwght)^2


# the amount of bias from omitting faminc 
linear_model <- lm(faminc~cigs, data=df)
summary(linear_model)

(bias <- 0.09276 * -0.5429 )
bias

#C=Problem C2
df <- hprice1
help(hprice1)

#i
linear_model <- lm(price~sqrft+bdrms, data=df)
summary(linear_model)

#ii
#15.19819x1 = 15k increase in price

#iii
#define new observation
new <- data.frame(bdrms=c(5), sqrft=c(140))
#use fitted model to predict the response value for the new observation
predict(linear_model, newdata=new)

#define new observation
new <- data.frame(bdrms=c(6), sqrft=c(140))
#use fitted model to predict the response value for the new observation
predict(linear_model, newdata=new)

#iv
#define new observation
new <- data.frame(bdrms=c(4), sqrft=c(2438))
#use fitted model to predict the response value for the new observation
predict(linear_model, newdata=new)

#Problem C3
df <- ceosal2
help(ceosal2)
#i
linear_model <- lm(lsalary~lsales+lmktval, data=df)
summary(linear_model)
#ii
linear_model <- lm(lsalary~lsales+lmktval+profits, data=df)
summary(linear_model)

#iii
#One year increase in ceoten leads to 1.16% increase in salary
linear_model <- lm(lsalary~lsales+lmktval+profits+ceoten, data=df)
summary(linear_model)

#iv
cor(df$lmktval, df$profits)


#Problem C4
df <- attend
help(attend)

#i
min(df$atndrte);max(df$atndrte); mean(df$atndrte)
min(df$priGPA);max(df$priGPA); mean(df$priGPA)
min(df$ACT);max(df$ACT); mean(df$ACT)

#ii

linear_model <- lm(atndrte ~ priGPA + ACT, data=df)
summary(linear_model)

#iii
new <- data.frame(priGPA=c(3.65), ACT=c(20))
predict(linear_model, newdata=new)

#iv
new1 <- data.frame(priGPA=c(3.1), ACT=c(21))
new2 <- data.frame(priGPA=c(2.1), ACT=c(26))

a1 <- predict(linear_model, newdata=new1)
a2 <- predict(linear_model, newdata=new2)
a1 - a2

#Problem C5
df <- wage1
help(wage1)

linear_model <- lm(educ ~ exper + tenure, data=df)
summary(linear_model)

r1_hat <- df$educ - predict(linear_model, newdata = df)

linear_model <- lm(lwage ~ r1_hat, data=df)
summary(linear_model)

# regress lwage on educ, exper, tenure
linear_model <- lm(lwage ~ educ + exper + tenure, data=df)
summary(linear_model)

#problem C6
df <- wage2
help(wage2)

linear_model <- lm(IQ ~ educ, data=df)
summary(linear_model)

delta_1 <- 3.5338

linear_model <- lm(lwage ~ educ, data=df)
summary(linear_model)

beta_tilda <- 0.059839

linear_model <- lm(lwage ~ educ + IQ, data=df)
summary(linear_model)

beta_hat <- 0.0391199
beta_2_hat <- 0.0058631

#as they are equal
beta_tilda; beta_hat + (beta_2_hat*delta_1)


#Problem C7
df <- meap93
help(meap93)

#i
linear_model <- lm(math10 ~ lexpend + lnchprg, data=df)
summary(linear_model)

#iii
linear_model <- lm(math10 ~ lexpend, data=df)
summary(linear_model)

#iv
# it makes sense since the correlation is negative and beta_2 is also negative
#which makes the bias positive
cor(df$lexpend, df$lnchprg)

#Problem C8
df <- discrim
help(discrim)

#i
mean(df$prpblck,na.rm=TRUE);sd(df$prpblck,na.rm=TRUE) 
mean(df$income,na.rm=TRUE);sd(df$income,na.rm=TRUE) 

#ii
linear_model <- lm(psoda~prpblck+income, data=df)
summary(linear_model)

##iii
linear_model <- lm(psoda~prpblck, data=df)
summary(linear_model)

#iv
linear_model <- lm(lpsoda~prpblck+lincome, data=df)
summary(linear_model)

#2.xx <- 2.43, so xx=43

#v
linear_model <- lm(lpsoda~prpblck+lincome+prppov, data=df)
summary(linear_model)

#vi
df <- na.omit(df)
cor(df$lincome, df$prppov)


#Problem C9
df <- charity
help(charity)
#i
linear_model <- lm(gift ~ mailsyear+giftlast+propresp, data=df)
summary(linear_model)

#SLR on 
linear_model <- lm(gift ~ mailsyear, data=df)
summary(linear_model)

#iv
linear_model <- lm(gift ~ mailsyear + giftlast + propresp + avggift, data=df)
summary(linear_model)

#Problem C10
df <- htv
help(htv)

#i
min(df$educ); max(df$educ)
nrow(filter(df, educ==12)) / nrow(df)
mean(df$educ); mean(df$motheduc); mean(df$fatheduc)

#ii
linear_model <- lm(educ ~ motheduc + fatheduc, data = df)
summary(linear_model)

#iii
linear_model <- lm(educ ~ motheduc + fatheduc + abil, data = df)
summary(linear_model)

#when controlling for parent's education
linear_model <- lm(educ ~ abil, data = df)
summary(linear_model)

#iv
linear_model <- lm(educ ~ motheduc + fatheduc + abil + I(abil^2), data=df)
summary(linear_model)


#iv
new <- data.frame(motheduc=c(12.18), fatheduc=c(12.48), abil=seq(-6, 6, 1))
educ_hat <- predict(linear_model, newdata=new)
plot(seq(-6, 6, 1), educ_hat)


#Problem C11
df <- meapsingle
help("meapsingle")

#i
linear_model <- lm(math4 ~ pctsgle, data=df)
summary(linear_model)

#ii
linear_model <- lm(math4 ~ pctsgle + lmedinc + free, data=df)
summary(linear_model)

#iii
cor(df$lmedinc, df$free)

#iv
#load the car library
install.packages("car")
library('car')

linear_model <- lm(math4 ~ pctsgle + lmedinc + free, data=df)
summary(linear_model)
vif(linear_model)

#Problem C12
df <- econmath
help("econmath")

#i
nrow(filter(df, score==100)); mean(df$score)
mean(df$actmth, na.rm=TRUE); sd(df$actmth, na.rm=TRUE)
mean(df$acteng, na.rm=TRUE); sd(df$acteng, na.rm=TRUE)

#ii
linear_model <- lm(score ~ colgpa + actmth + acteng, data=df)
summary(linear_model)

#Problem C.12
df <- gpa1
help(gpa1)

#i
linear_model <- lm(colGPA ~ PC, data=df)
summary(linear_model)

#ii
linear_model <- lm(colGPA ~ PC + hsGPA + ACT, data=df)
summary(linear_model)

#iii
#Having PC

#iv
linear_model <- lm(colGPA ~ PC + hsGPA + ACT + fathcoll + mothcoll, data=df)
summary(linear_model)

#v
cor(df$hsGPA, df$ACT)

# Clear data
rm(list = ls())  # Removes all objects from environment
# Clear console
cat("\014")  # Mimics ctrl+L

