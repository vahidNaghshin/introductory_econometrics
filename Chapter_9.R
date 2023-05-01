library(tidyverse)
library('wooldridge')
library(ggplot2)
#for robust t-test
library("lmtest")
library("sandwich")

install.packages('L1pack')
library("L1pack")
install.packages('MASS')
library(MASS)

#C1

df <- ceosal1
help("ceosal1")

df$rosneg[df$ros < 0] <- 1 
df$rosneg[df$ros > 0] <- 0

#i 
linear_model <- lm(lsalary ~ lsales + roe + rosneg, data=df)
(out <- summary(linear_model))

df$y_hat <- fitted(linear_model)

#Applying RESET

linear_model_ur <- lm(lsalary ~ lsales + roe + rosneg + I(y_hat^2) + I(y_hat^3), data=df)
(out_ur <- summary(linear_model_ur))

linear_model_r <- lm(lsalary ~ lsales + roe + rosneg, data=df)
(out_r <- summary(linear_model_r))

#since F is large enough, it is OK to reject the hypothesis testing and
#both variables are jointly significant
(F_statistcs <- ((out_ur$r.squared - out_r$r.squared)/(1-out_ur$r.squared) *(203/2)))

#for f-stat of 1.33 with 2 and 203 dof, the p-value would be .266
#so we cannot reject the null hypothesis and the original equation does not suffer from functional
#error

#ii
# Robust t test
coeftest(linear_model_ur, vcov = vcovHC(linear_model_ur, type = "HC0"))

#C2
df <- wage2
help('wage2')

#i
linear_model <- lm(lwage~educ+exper+tenure+married+south+urban+black+KWW+I(educ*KWW), data = df)
(out <- summary(linear_model))
#ii
linear_model <- lm(lwage~educ+exper+tenure+married+south+urban+black+KWW+I(educ*KWW)+IQ+I(IQ*educ), data = df)
(out <- summary(linear_model))

#iii
#KWW is SS while IQ is not
#joint significant

linear_model_ur <- lm(lwage~educ+exper+tenure+married+south+urban+black+KWW+I(educ*KWW)+IQ+I(IQ*educ), data=df)
(out_ur <- summary(linear_model_ur))

linear_model_r <- lm(lwage~educ+exper+tenure+married+south+urban+black, data=df)
(out_r <- summary(linear_model_r))

#since F is large enough, it is OK to reject the hypothesis testing and
#both variables are jointly significant
(F_statistcs <- ((out_ur$r.squared - out_r$r.squared)/(1-out_ur$r.squared) *(203/2)))


#C3
df <- jtrain
help("jtrain")

#ii
df <- filter(df, d88==1)
(nrow(df));min(df$year);max(df$year)

linear_model <- lm(lscrap~grant, data=df)
(out <- summary(linear_model))

#iii
linear_model <- lm(lscrap~grant+lscrap_1, data=df)
(out <- summary(linear_model))

#iv
(t_stat <- (0.83116-1)/0.04444)
#the p-value is very small so we can reject the null hypothesis

#v
# Robust t test
coeftest(linear_model, vcov = vcovHC(linear_model, type = "HC0"))

#C4
df <- infmrt
help("infmrt")

#i
linear_model <- lm(infmort~lpcinc+lphysic+lpopul+DC, data=df)
(out <- summary(linear_model))

#ii
linear_model <- lm(infmort~lpcinc+lphysic+lpopul, data=df)
(out <- summary(linear_model))

#C5
df <- rdchem
help('rdchem')

#i
df$sales <- df$sales/1000
#with all firms
linear_model <- lm(rdintens~sales+I(sales^2)+profmarg, data=df)
(out <- summary(linear_model))
#without 
df <- filter(df, sales <= 30)
linear_model <- lm(rdintens~sales+I(sales^2)+profmarg, data=df)
(out <- summary(linear_model))

#ii
#with all firms
linear_model <- lad(rdintens~sales+I(sales^2)+profmarg, data=df)
(out <- summary(linear_model))
#without 
df <- filter(df, sales <= 30)
linear_model <- lad(rdintens~sales+I(sales^2)+profmarg, data=df)
(out <- summary(linear_model))

#C7
df <- loanapp
help("loanapp")

#i
nrow(filter(df, obrat>40))

#ii
linear_model <- lm(approve~white, data=df)
(out <- summary(linear_model))

#iii
linear_model <- lm(approve~white, data=filter(df, obrat<40))
(out <- summary(linear_model))

#C8
df <- twoyear
help("twoyear")

#i
mean(df$stotal); sd(df$stotal)

#ii
linear_model <- lm(stotal~jc+univ, data=df)
(out <- summary(linear_model))

#iii
linear_model <- lm(lwage ~ jc + I(jc + univ) + exper + stotal, data=df)
(out <- summary(linear_model))
#since the t-stat is small, we failed to reject the null hypothesis

#iv
linear_model <- lm(lwage ~ jc + I(jc + univ) + exper + stotal + I(stotal^2), data=df)
(out <- summary(linear_model))

#v
linear_model_ur <- lm(lwage ~ jc + I(jc + univ) + exper + stotal 
                      + I(stotal*univ) + I(stotal*jc), data=df)
(out_ur <- summary(linear_model_ur))

linear_model_r <- lm(lwage ~ jc + I(jc + univ) + exper + stotal, data=df)
(out_r <- summary(linear_model_r))

#since F is large enough, it is OK to reject the hypothesis testing and
#both variables are jointly significant
(F_statistcs <- ((out_ur$r.squared - out_r$r.squared)/(1-out_ur$r.squared) *(6758/2)))
#since the p-value is 0.14, w failed to reject the null hypothesis

#C9
df <- k401ksubs
help("k401ksubs")

#i
linear_model <- lm(nettfa~inc+I(inc^2)+age+I(age^2)+male+e401k, data=df)
(out <- summary(linear_model))

#ii
df$res <- resid(linear_model)
linear_model <- lm(I(res^2)~inc+I(inc^2)+age+I(age^2)+male+e401k, data=df)
(out <- summary(linear_model))

#via Breush-Pagan Test the heteroskedascity is confirmed.

#iii
linear_model <- lad(nettfa~inc+I(inc^2)+age+I(age^2)+male+e401k, data=df)
(out <- summary(linear_model))

#C10
df1 <- jtrain2
df2 <- jtrain3
help('jtrain2')
#i
nrow(filter(df1, train==1))/nrow(df1);nrow(filter(df2, train==1))/nrow(df2)

#ii
linear_model <- lm(re78~train, data=df1)
(out <- summary(linear_model))

#iii
linear_model <- lm(re78~train + re74 + re75 + educ + age + black + hisp, data=df1)
(out<-summary(linear_model))

#iv
linear_model <- lm(re78~train, data=df2)
(out <- summary(linear_model))

linear_model <- lm(re78~train + re74 + re75 + educ + age + black + hisp, data=df2)
(out<-summary(linear_model))

#v
df1$avg <- (df1$re74 + df1$re75)/2; df2$avg <- (df2$re74 + df2$re75)/2
mean(df1$avg); mean(df2$avg)
sd(df1$avg); sd(df2$avg)
min(df1$avg); min(df2$avg)
max(df1$avg); max(df2$avg)

#vi
linear_model <- lm(re78~train + re74 + re75 + educ + age + black + hisp, data=filter(df1, avg<10))
(out<-summary(linear_model))
linear_model <- lm(re78~train + re74 + re75 + educ + age + black + hisp, data=filter(df2, avg<10))
(out<-summary(linear_model))

#vii
linear_model <- lm(re78~train, data=filter(df1, (unem74==1)&(unem75==1)))
(out<-summary(linear_model))

#C11
df <- murder
help("murder")

#i
linear_model <- lm(mrdrte~exec+unem, data=filter(df, year==93))
(out <- summary(linear_model))

#ii
sum(filter(df, state=="TX")$exec)
(df %>% 
  group_by(state) %>% 
  summarise(exec2 = sum(exec)))

df$is_tx <- ifelse(df$state == 'TX', 1, 0)
linear_model <- lm(mrdrte~exec+unem+is_tx, data=filter(df, year==93))
(out <- summary(linear_model))

#iii
linear_model <- lm(mrdrte~exec+unem+cexec_1, data=filter(df, year==93))
(out <- summary(linear_model))

#iv
linear_model <- lm(mrdrte~exec+unem+cexec_1, data=filter(df, (year==93)&(state != 'TX')))
(out <- summary(linear_model))


#C12
df <- elem94_95
help("elem94_95")

#i
linear_model <- lm(lavgsal~bs+lenrol+lstaff+lunch, data=df)
(out <- summary(linear_model))
coeftest(linear_model, vcov = vcovHC(linear_model, type = "HC0"))

#ii
linear_model <- lm(lavgsal~bs+lenrol+lstaff+lunch, data=filter(df, bs > 0.5))
(out <- summary(linear_model))
coeftest(linear_model, vcov = vcovHC(linear_model, type = "HC0"))


#iii
df$d68 <- ifelse(df$distid == 9030, 1, 0)
df$d1127 <- ifelse(df$distid == 63160, 1, 0)
df$d1508 <- ifelse(df$distid == 82010, 1, 0)
df$d1670 <- ifelse(df$distid == 82040, 1, 0)

linear_model <- lm(lavgsal~bs+lenrol+lstaff+lunch+d68+d1127+d1508+d1670, data=df)
(out <- summary(linear_model))
coeftest(linear_model, vcov = vcovHC(linear_model, type = "HC0"))

#vi
linear_model <- lad(lavgsal~bs+lenrol+lstaff+lunch, data=df)
(out <- summary(linear_model))

#C13
df <- ceosal2
help("ceosal2")

#i 
linear_model <- lm(lsalary ~ lsales + lmktval + ceoten + ceotensq, data=df)
(out <- summary(linear_model))

#ii
#calculate studentized residuals
(df$stud_resids <- abs(studres(linear_model)))
filter(df, stud_resids>1.96)

#iii
linear_model <- lm(lsalary ~ lsales + lmktval + ceoten + ceotensq, data=filter(df, stud_resids<1.96))
(out <- summary(linear_model))

#iv
linear_model <- lad(lsalary ~ lsales + lmktval + ceoten + ceotensq, data=df)
(out <- summary(linear_model))


#C14
df <- econmath
help("econmath")

#i
sum(is.na(df$act)); sum(is.na(df$act))/nrow(df)
df$actmiss <- ifelse(is.na(df$act), 1, 0)

#ii
df$act0 <- ifelse(is.na(df$act), 0, df$act)
mean(df$act0);mean(df$act,  na.rm=TRUE)

#iii
linear_model <- lm(score~act, data=df)
(out <- summary(linear_model))

#iv
linear_model <- lm(score~act0, data=df)
(out <- summary(linear_model))

#v
linear_model <- lm(score~act0+actmiss, data=df)
(out <- summary(linear_model))

#vi
linear_model <- lm(score~act+colgpa, data=df)
(out <- summary(linear_model))

linear_model <- lm(score~act0+actmiss+colgpa, data=df)
(out <- summary(linear_model))


#Clear plots
graphics.off()  # Clears plots, closes all graphics devices
# Clear console
cat("\014")  # Mimics ctrl+L
# Clear data
rm(list = ls())  # Removes all objects from environment

