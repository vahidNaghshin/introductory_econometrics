library(tidyverse)
library('wooldridge')

# C1
df <- vote1
help("vote1")

#i
#if expendA increases by 1%, then the voteA increases beta_1/100 point

#ii
#the null hypothesis is: H0: beta_1 = -beta_2

#iii & iv
linear_model <- lm(voteA ~ lexpendA + lexpendB + prtystrA, data=df)
summary(linear_model)

#in order to test the null hypothesis we introduce new variable theta = beta_1 + beta_2
# and test the null hypothesis of theta=0

linear_model <- lm(voteA ~ lexpendA + I(lexpendB - lexpendA) + prtystrA, data=df)
summary(linear_model)
#since p-value is around 0.32, then we failed to reject that beta_1 = -beta2

#C2
df <- lawsch85
help("lawsch85")

#i
linear_model <- lm(lsalary ~ LSAT + GPA + llibvol + lcost + rank, data=df)
summary(linear_model)
#since the p-value is very low <2e-16, then we reject the null hypothesis and the rank
#has ceteris paribus effect of the median starting salary

#ii
#LSAT is not individually significant but GPA is. 

#for joint significance of GPA and LSAT
#df$lsalary[!is.na(df$lsalary)]
#df <- df[complete.cases(df$LSAT, df$GPA, df$lsalary),]
df <- df[complete.cases(df), ] 

linear_model_unrestricted <- lm(lsalary ~ LSAT + GPA + llibvol + lcost + rank, data=df)
summary(linear_model_unrestricted)

linear_model_restricted <- lm(lsalary ~ llibvol + lcost + rank, data=df)
summary(linear_model_restricted)

ssr_r <- sum((fitted(linear_model_restricted) - df$lsalary)^2)
ssr_ur <- sum((fitted(linear_model_unrestricted) - df$lsalary)^2)

(F <- ((ssr_r - ssr_ur)/ssr_ur)*(86/2))
#since F is greater than 3.05, then we reject the null hypothesis and GPA and
#LSAT are jointly significant

#iii
linear_model <- lm(lsalary ~ LSAT + GPA + llibvol + lcost + rank + clsize + faculty, data=df)
summary(linear_model)

#C3
df <- hprice1
help(hprice1)

#i
linear_model <- lm(lprice ~ I(sqrft - 150) + bdrms, data=df)
out <- summary(linear_model)

sd <- out$coefficients[3 , 2]
theta_1 <- out$coefficients[3 , 1]
#confidence interval for 95% 
conf_int <- c(theta_1 - (2*sd), theta_1 + (2*sd))

#C4
df <- bwght
help('bwght')

#i
linear_model_restricted <- lm(bwght~cigs+parity+faminc, data=df)
summary(linear_model_restricted)

#C5
df <- mlb1
help('mlb1')

#i
linear_model <- lm(lsalary ~ years+gamesyr+bavg+hrunsyr+rbisyr, data=df)
summary(linear_model)

linear_model_restricted <- lm(lsalary ~ years+gamesyr+bavg+hrunsyr, data=df)
summary(linear_model_restricted)

#ii
#only runsyr is statistically significant
linear_model <- lm(lsalary ~ years+gamesyr+bavg+hrunsyr+rbisyr+runsyr+fldperc+sbasesyr, data=df)
out_ur <- summary(linear_model)

#iii
linear_model_restricted <- lm(lsalary ~ years+gamesyr+hrunsyr+rbisyr+runsyr, data=df)
out_r <-summary(linear_model_restricted)

(F_statistics <- ((out_ur$r.squared - out_r$r.squared)/(1-out_ur$r.squared))*(347/3))

#C6
df <- wage2
help('wage2')

#i
#theta = beta_2 - beta_3
linear_model <- lm(lwage ~ educ + exper + I(exper+tenure), data=df)
out <- summary(linear_model)

sd <- out$coefficients[3 , 2]
theta <- out$coefficients[3 , 1]
#confidence interval for 95% 
(conf_int <- c(theta - (2*sd), theta + (2*sd)))

#C7
df <- twoyear
help('twoyear')
#i
min(df$phsrank);max(df$phsrank);mean(df$phsrank)

#ii
linear_model <- lm(lwage ~ jc+totcoll+exper+phsrank, data=df)
summary(linear_model)

#iii
#adding phsrank does not affect two and four year college
linear_model <- lm(lwage ~ jc+totcoll+exper, data=df)
summary(linear_model)

#iv
#id is irrelevant variable
linear_model <- lm(lwage ~ id+jc+totcoll+exper+phsrank, data=df)
summary(linear_model)

#C8
df <- k401ksubs
help('k401ksubs')

#i
nrow(filter(df, fsize==1))

##ii
df <-filter(df, fsize==1)

linear_model <- lm(nettfa~inc+age, data=df)
out <- summary(linear_model)

#iv
(t_statitics <- (out$coefficients[3 , 1] - 1)/out$coefficients[3 , 2])
#since t_statistics is not greater than 2.8 (the critical value for 1%),
#we failed to reject the null hypothesis

#v
#does not differ much
linear_model <- lm(nettfa ~ inc, data=df)
(out <- summary(linear_model))

#C9
df <- discrim
help('discrim')
#i
linear_model <- lm(lpsoda ~ prpblck + lincome + prppov, data=df)
(out <- summary(linear_model))
# the beta_1 is statistically significant at 5% level but not at 1% level

#ii
mat <- cbind(df$lincome, df$prppov)
cor(mat, use = "pairwise.complete.obs")
#both variables are statistically significant at 1% level

#iii
linear_model <- lm(lpsoda ~ prpblck + lincome + prppov + lhseval, data=df)
(out <- summary(linear_model))
# the p-value is 2.67e-11

#iv
linear_model_unrestricted <- lm(lpsoda ~ prpblck + lincome + prppov + lhseval, data=df)
(out_ur <- summary(linear_model_unrestricted))

linear_model_restricted <- lm(lpsoda ~ prpblck + lhseval, data=df)
(out_r <- summary(linear_model_restricted))

#since F is above 3.5 we can reject the null hypothesis, so lincome and prpov
#are jointly significant
(F_statistcs <- ((out_ur$r.squared - out_r$r.squared)/(1-out_ur$r.squared) *(398/2)))

#C10
df <- elem94_95
help('elem94_95')
#i
linear_model <- lm(lavgsal ~ bs, data=df)
(out <- summary(linear_model))
# it is statistically different from zero
# for testing against -1
((out$coefficients[2,1]+1)/out$coefficients[2,2])
#since it is not greater than 2 (the critical value for 5% level) then it is not
#significantly different than -1

#ii
linear_model <- lm(lavgsal ~ bs+lenrol+lstaff, data=df)
(out <- summary(linear_model))

#iv
linear_model <- lm(lavgsal ~ bs+lenrol+lstaff+lunch, data=df)
(out <- summary(linear_model))

#C11
#i
df <- htv
help('htv')
linear_model <- lm(educ ~ motheduc+fatheduc+abil+I(abil^2), data=df)
(out <- summary(linear_model))
#ii
#since for motheduc the p-value is not below 0.05, then it is not significantly
#statistic, so, we failed to reject that beta_1=beta_2
linear_model <- lm(educ ~ motheduc+I(motheduc+fatheduc)+abil+I(abil^2), data=df)
(out <- summary(linear_model))

#iii
linear_model_unrestricted <- lm(educ ~ motheduc+fatheduc+abil+I(abil^2)+tuit17+tuit18, data=df)
(out_ur <- summary(linear_model_unrestricted))

linear_model_restricted <- lm(educ ~ motheduc+fatheduc+abil+I(abil^2), data=df)
(out_r <- summary(linear_model_restricted))
(F_statistcs <- ((out_ur$r.squared - out_r$r.squared)/(1-out_ur$r.squared) *(1225/2)))
#since the F value is not large enough, both variables are jointly insignificant

#iv
cor(df$tuit17, df$tuit18)
df$avgtuit <- (df$tuit17 + df$tuit18)/2
linear_model <- lm(educ ~ motheduc+fatheduc+abil+I(abil^2) + avgtuit, data=df)
summary(linear_model)


#C12
df <- econmath
help('econmath')

#i
linear_model <- lm(colgpa ~ hsgpa+actmth+acteng, data=df)
summary(linear_model)

#iii
#theta=beta_2-beta_3
linear_model <- lm(colgpa ~ hsgpa+actmth+I(actmth+acteng), data=df)
summary(linear_model)
#since the coefficent for actmth is not statistically significant then 
#we failed to reject that both actmath and acteng have different effect

#C13
df <- gpa1
help('gpa1')
#i
linear_model <- lm(colGPA ~ PC + hsGPA + ACT , data=df)
out <- summary(linear_model)
#ii
(sd <- out$coefficients[2 , 2])
(theta_1 <- out$coefficients[2 , 1])
#confidence interval for 95% 
(conf_int <- c(theta_1 - (2*sd), theta_1 + (2*sd)))
#iii
#hsGPA is statistically significant while the ACT is not
#based on magnitude and significance of hsGPA, ACT, we can conclude that 
#hsGPA is more important predictor than ACT

#C14
df <- jtrain98
help('jtrain98')
#i
linear_model <- lm(earn98 ~ train + educ + earn96 + age + married + unem96, data=df )
(out <- summary(linear_model))
#iii
cor(df$earn96, df$unem96)
#iv
#high correlation does not mean we should drop the related variabels. Only perfect correlation
#is the reason for dropping one of them

# Clear plots
graphics.off()  # Clears plots, closes all graphics devices
# Clear console
cat("\014")  # Mimics ctrl+L
# Clear data
rm(list = ls())  # Removes all objects from environment
