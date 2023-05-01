library(tidyverse)
library('wooldridge')
library(ggplot2)

#C1
df <- gpa1
help("gpa1")

#i
linear_model <- lm(colGPA ~ PC + hsGPA + ACT + mothcoll + fathcoll, data=df)
(out <- summary(linear_model))
#The PC factor is statistically significant at 5% level

#ii
linear_model_ur <- lm(colGPA ~ PC + hsGPA + ACT + mothcoll + fathcoll, data=df)
(out_ur <- summary(linear_model_ur))

linear_model_r <- lm(colGPA ~ PC + hsGPA + ACT , data=df)
(out_r <- summary(linear_model_r))

(F_statistcs <- ((out_ur$r.squared - out_r$r.squared)/(1-out_ur$r.squared) *(135/2)))

# the p-value would be 0.78340 which is a strong evidence against H0. So, 
# two factors are not jointly significant

#iii
linear_model <- lm(colGPA ~ PC + hsGPA + I(hsGPA^2) + ACT + mothcoll + fathcoll, data=df)
(out <- summary(linear_model))
#since the hsGPA^2 is not statistically significant it is not need

#C2
df <- wage2
help('wage2')

#i
linear_model <- lm(lwage ~ educ + tenure + exper 
                   + married + black + south + urban, data=df)
(out <- summary(linear_model))

#being black with assumption that all other factors are fixed, reduce the monthly
#salary by 18%

#ii
linear_model_ur <- lm(lwage ~ educ + tenure + I(tenure^2) + exper + I(exper^2) 
                      + married + black + south + urban, data=df)
(out_ur <- summary(linear_model_ur))

linear_model_r <- lm(lwage ~ educ + tenure + exper +  
                     + married + black + south + urban , data=df)
(out_r <- summary(linear_model_r))

(F_statistcs <- ((out_ur$r.squared - out_r$r.squared)/(1-out_ur$r.squared) *(925/2)))
#the p-value is 0.22596 which shows that they are not significant even in 
# 20% interval

#iii
linear_model <- lm(lwage ~ educ + I(educ*black) + tenure + exper 
                   + married + black + south + urban, data=df)
(out <- summary(linear_model))
#the interaction term is not statistically significant

#iv
#approach one
linear_model <- lm(lwage ~ educ + tenure + exper 
                   + married + black + I(married*black)+south + urban, data=df)
(out <- summary(linear_model))

#approach two
df$single_non_black = (1 - df$black) * (1 - df$married)
df$single_black = (df$black) * (1 - df$married)
df$married_non_black = (1 - df$black) * (df$married)
df$married_black = (df$black) * (df$married)

linear_model <- lm(lwage ~ educ + tenure + exper 
                   + single_black + married_non_black + married_black +south + urban, data=df)
(out <- summary(linear_model))

#C3
df <- mlb1
help("mlb1")
#i
linear_model <- lm(lsalary ~ years + gamesyr + bavg + hrunsyr
                   + rbisyr + runsyr + fldperc + allstar
                   + frstbase + scndbase + thrdbase + shrtstop
                   + catcher, data=df)
(out <- summary(linear_model))
#since the catcher coefficient is statistically significant at 5% level
#so, the salary for catchers and outfielders are not the same

#ii
#testing whether first, second, third bases coeffs are not jointly significant

linear_model_ur <- lm(lsalary ~ years + gamesyr + bavg + hrunsyr
                      + rbisyr + runsyr + fldperc + allstar
                      + frstbase + scndbase + thrdbase + shrtstop
                      + catcher, data=df)
(out_ur <- summary(linear_model_ur))

linear_model_r <- lm( lsalary ~ years + gamesyr + bavg + hrunsyr
                     + rbisyr + runsyr + fldperc + allstar
                    + shrtstop
                     + catcher , data=df)
(out_r <- summary(linear_model_r))

(F_statistcs <- ((out_ur$r.squared - out_r$r.squared)/(1-out_ur$r.squared) *(339/3)))
#the p-value for given f-statistics is 0.51998, so we failed to reject that poisitons are important

#C4
df <- gpa2
help("gpa2")

#ii
linear_model <- lm(colgpa ~ hsize + I(hsize^2) + hsperc + sat
                   + female + athlete, data=df)
(out <- summary(linear_model))

#iii
linear_model <- lm(colgpa ~ hsize + I(hsize^2) + hsperc
                   + female + athlete, data=df)
(out <- summary(linear_model))

cor(df$athlete, df$sat)
car::vif(linear_model)

#iv
linear_model <- lm(colgpa ~ hsize + I(hsize^2) + hsperc + sat
                   + female + athlete + I(female*athlete), data=df)
(out <- summary(linear_model))
#the p-value is almost one, so strongly there is no difference between 

#v
linear_model <- lm(colgpa ~ hsize + I(hsize^2) + hsperc + sat + I(sat*female)
                   + female + athlete , data=df)
(out <- summary(linear_model))
#the p-value is almost one, so strongly there is no difference between 

#C5
df <- ceosal1
help("ceosal1")

df$rosneg[df$ros < 0] <- 1 
df$rosneg[df$ros > 0] <- 0

linear_model <- lm(lsalary ~ lsales + roe + rosneg, data=df)
(out <- summary(linear_model))

#C6
df <- sleep75
help("sleep75")

#i
df_male <- filter(df, male==1)
df_female <- filter(df, male==0)

linear_model_m <- lm(sleep ~ totwrk + educ + age + I(age^2) + yngkid, data=df_male)
(out_m <- summary(linear_model_m))

linear_model_f <- lm(sleep ~ totwrk + educ + age + I(age^2) + yngkid, data=df_female)
(out_f <- summary(linear_model_f))

linear_model_p <- lm(sleep ~ totwrk + educ + age + I(age^2) + yngkid, data=df)
(out_p <- summary(linear_model_p))

srr_12 <- sum(out_m$residuals^2) + sum(out_f$residuals^2)

(Chow_stat <- ((sum(out_p$residuals^2) - srr_12)/srr_12) * (706-(2*(5+1)))/(5+1))
#The p-value is  0.04949 which causes us to fail teh null hypothesis

#ii
linear_model_m <- lm(sleep ~ totwrk + educ + age + I(age^2) + yngkid, data=df_male)
(out_m <- summary(linear_model_m))

linear_model_f <- lm(sleep ~ totwrk + educ + age + I(age^2) + yngkid, data=df_female)
(out_f <- summary(linear_model_f))

linear_model_p <- lm(sleep ~ male + totwrk + educ + age + I(age^2) + yngkid, data=df)
(out_p <- summary(linear_model_p))

srr_12 <- sum(out_m$residuals^2) + sum(out_f$residuals^2)

(Chow_stat <- ((sum(out_p$residuals^2) - srr_12)/srr_12) * (706-(2*(5+1)))/(5))
#now the p-value is 0.28135 which is not a strong evidence against null hypothesis

#iii
#comparing the results from i and ii, we see that only the intercept difference should be in equation

#C7
df <- wage1
help("wage1")

#i
linear_model <- lm(lwage ~ female+educ+I(female*educ)+exper+I(exper^2)+tenure+I(tenure^2), data=df)
(out <- summary(linear_model))

linear_model <- lm(lwage ~ female+educ+I(female*(educ-12.5))+exper+I(exper^2)+tenure+I(tenure^2), data=df)
(out <- summary(linear_model))

#The female is statically significant around the education 12.5. 
boxplot(df$educ, main="education", 
        xlab="educ")

#C8
df <- loanapp
help("loanapp")
#i
linear_model <- lm(approve~white, data=df)
(out <- summary(linear_model))

#ii
linear_model <- lm(approve~ white+hrat+obrat+loanprc+unem 
                   +male+married+dep+sch+cosign 
                   +chist+ pubrec+ mortlat1+ mortlat2+vr, data = df)
(out <- summary(linear_model))

#iv
linear_model <- lm(approve~ white+I(white*obrat)+hrat+obrat+loanprc+unem 
                   +male+married+dep+sch+cosign 
                   +chist+ pubrec+ mortlat1+ mortlat2+vr, data = df)
(out <- summary(linear_model))

#v
linear_model <- lm(approve~ white+I(white*(obrat-32))+hrat+obrat+loanprc+unem 
                   +male+married+dep+sch+cosign 
                   +chist+ pubrec+ mortlat1+ mortlat2+vr, data = df)
(out <- summary(linear_model))

(conf_int <- c(out$coefficients[2, 1] - out$coefficients[2,2]*2,
              out$coefficients[2, 1] + out$coefficients[2,2]*2))

#C9
df <- k401ksubs 
help("k401ksubs")

#i
nrow(filter(df, e401k==1))/nrow(df)

#ii
linear_model <- lm(e401k~inc+age+male+I(inc^2)+I(age^2), data=df)
(out <- summary(linear_model))

#iii
#it seems it is independent of age but related to income and age

#iv
df$fitted <- predict(linear_model, data=df)

#v & vi
df$fitted[df$fitted>0.5] <- 1
df$fitted[df$fitted<0.5] <- 0
nrow(filter(df, fitted==1))/nrow(df)

#vii
linear_model <- lm(e401k~inc+age+male+I(inc^2)+I(age^2)+pira, data=df)
(out <- summary(linear_model))

#C10
df <- nbasal
help("nbasal")

#i
linear_model <- lm(points ~ exper + I(exper^2) + forward + guard, data=df)
(out <- summary(linear_model))

#ii
#To prevent the multicolinearity trap

#iii
#yes since the coefficient of guard is positive and SS

#iv
linear_model <- lm(points ~ exper + I(exper^2) + forward + guard + marr, data=df)
(out <- summary(linear_model))
#it is not SS

#v
linear_model <- lm(points ~ exper + I(exper^2) + I(marr*exper) + I(marr*exper^2)
                   + forward + guard + marr, data=df)
(out <- summary(linear_model))

#vi
linear_model <- lm(assists ~ exper + I(exper^2) + forward + guard + marr, data=df)
(out <- summary(linear_model))

#C11
df <- k401ksubs
help("k401ksubs")

#i
mean(df$nettfa); sd(df$nettfa);max(df$nettfa);min(df$nettfa)

#ii
linear_model <- lm(nettfa ~ e401k, data=df)
(out <- summary(linear_model))

#iii
linear_model <- lm(nettfa ~ e401k + inc + I(inc^2) + age + I(age^2), data=df)
(out <- summary(linear_model))

#iv
linear_model <- lm(nettfa ~ e401k + inc + I(inc^2) + age + I(age^2) + I(e401k*(age-41))
                   +I(e401k*(age-41)^2), data=df)
(out <- summary(linear_model))

#v
df$fsize5 <- ifelse(df$fsize >= 5,1,0)
df$fsize4 <- ifelse(df$fsize >= 4,1,0)
df$fsize3 <- ifelse(df$fsize >= 3,1,0)
df$fsize2 <- ifelse(df$fsize >= 2,1,0)
df$fsize1 <- ifelse(df$fsize >= 1,1,0)
linear_model <- lm(nettfa ~ e401k + inc + I(inc^2) + age + I(age^2) + 
                     fsize5 + fsize4 + fsize3 + fsize2, data=df)
(out <- summary(linear_model))

#check for joint significance of dummy family size variables
linear_model_r <- lm(nettfa ~ e401k + inc + I(inc^2) + age + I(age^2), data=df)
(out_r <- summary(linear_model_r))

linear_model_r <- lm( nettfa ~ e401k + inc + I(inc^2) + age + I(age^2), data=df)
(out_r <- summary(linear_model_r))

(F_statistcs <- ((out_ur$r.squared - out_r$r.squared)/(1-out_ur$r.squared) *(9265/4)))
#since the p-value is 0.00023 then it is jointly significant

#vi
linear_model_r <- lm(nettfa ~ e401k + inc + I(inc^2) + age + I(age^2)+
                       fsize5 + fsize4 + fsize3 + fsize2, data=df)
(out_r <- summary(linear_model_r))
(SSR_r <- sum(out_r$residuals^2) )


df_5<- filter(df, fsize5==1)
linear_model_5 <- lm(nettfa ~ e401k + inc + I(inc^2) + age + I(age^2), data=df_5)
(out_5 <- summary(linear_model_5))
(SSR_5 <- sum(out_5$residuals^2))

df_4<- filter(df, fsize4==1)
linear_model_4 <- lm(nettfa ~ e401k + inc + I(inc^2) + age + I(age^2), data=df_4)
(out_4 <- summary(linear_model_4))
(SSR_4 <- sum(out_4$residuals^2))

df_3<- filter(df, fsize3==1)
linear_model_3 <- lm(nettfa ~ e401k + inc + I(inc^2) + age + I(age^2), data=df_3)
(out_3 <- summary(linear_model_3))
(SSR_3 <- sum(out_3$residuals^2))

df_2<- filter(df, fsize2==1)
linear_model_2 <- lm(nettfa ~ e401k + inc + I(inc^2) + age + I(age^2), data=df_2)
(out_2 <- summary(linear_model_2))
(SSR_2 <- sum(out_2$residuals^2))

(SSR_ur <- SSR_5+SSR_4+SSR_3+SSR_2)

(Chow_stat <- ((SSR_r - SSR_ur)/SSR_ur) * (9245)/(20))

#C12

df <- beauty
help("beauty")

#i
mean_beauty <- mean(df$looks)
nrow(filter(df, female==1 & looks > mean_beauty))/nrow(df)
nrow(filter(df, female==0 & looks > mean_beauty))/nrow(df)

#ii
linear_model <- lm(abvavg~female, data=df)
(out <- summary(linear_model))

#iii
linear_model_m <- lm(lwage ~ belavg + abvavg, data=filter(df, female==0))
(out_m <- summary(linear_model_m))

linear_model_f <- lm(lwage ~ belavg + abvavg, data=filter(df, female==1))
(out_f <- summary(linear_model_f))

#the one-tailed p-value is the probability observing larger statistics than the realized one, here
#-3.314
p_value_one_tail_male <-  0.00049

#iv
#since the belavg is only significant at 10% level it seems there is not much difference
#between belavg and abvavg

linear_model_m <- lm(lwage ~ belavg + abvavg+educ+ exper+ I(exper^2)+ union+ goodhlth+ black+
                       married+ south+ bigcity+ smllcity+ service, data=filter(df, female==0))
(out_m <- summary(linear_model_m))

linear_model_f <- lm(lwage ~ belavg + abvavg+educ+ exper+ I(exper^2)+ union+ goodhlth+ black+
                       married+ south+ bigcity+ smllcity+ service, data=filter(df, female==1))
(out_f <- summary(linear_model_f))

#C12
linear_model_r <- lm(lwage ~ female + belavg + abvavg+educ+ exper+ I(exper^2)
                     + union + goodhlth + black +
                       married + south + bigcity + smllcity+ service, data=df)
(out_r <- summary(linear_model_r))
(SSR_r <- sum(out_r$residuals^2))

linear_model_m <- lm(lwage ~ belavg + abvavg+educ+ exper+ I(exper^2)+ union+ goodhlth+ black+
                       married+ south+ bigcity+ smllcity+ service, data=filter(df, female==0))
(out_m <- summary(linear_model_m))
(SSR_m <- sum(out_m$residuals^2))
linear_model_f <- lm(lwage ~ belavg + abvavg+educ+ exper+ I(exper^2)+ union+ goodhlth+ black+
                       married+ south+ bigcity+ smllcity+ service, data=filter(df, female==1))
(out_f <- summary(linear_model_f))
(SSR_f <- sum(out_f$residuals^2))

(SSR_ur <- SSR_m + SSR_f)
(Chow_stat <- ((SSR_r - SSR_ur)/SSR_ur) * (1260-14)/(14))

#C13
df <- apple
help("apple")

#i
df$ecobuy <- ifelse(df$ecolbs> 0, 1, 0)
nrow(filter(df, ecobuy==1))/nrow(df)

#ii
linear_model <- lm(ecobuy~ecoprc+regprc+hhsize+educ+faminc+age, data=df)
(out<- summary(linear_model))

#iii
linear_model_ur <- lm(ecobuy~ecoprc+regprc+hhsize+educ+faminc+age, data=df)
(out_ur<- summary(linear_model_ur))

linear_model_r <- lm(ecobuy~ecoprc+regprc, data=df)
(out_r<- summary(linear_model_r))

(F_statistcs <- ((out_ur$r.squared - out_r$r.squared)/(1-out_ur$r.squared) *(653/4)))

#iv
linear_model <- lm(ecobuy~ecoprc+regprc+hhsize+educ+I(log(faminc))+age, data=df)
(out<- summary(linear_model))

#v
df$fitted <- predict(linear_model, df)
nrow(filter(df, fitted>1))/nrow(df)
nrow(filter(df, fitted<0))/nrow(df)

#vi
df$predcited <- ifelse(df$fitted>=0.5, 1, 0)
nrow(filter(df, predcited==ecobuy))/nrow(df)

#C14
df <- charity
help("charity")

#i
linear_model <- lm(respond ~ resplast+avggift, data=df)
(out <- summary(linear_model))

#ii
#Not really

#iii
linear_model <- lm(respond ~ resplast+avggift+propresp, data=df)
(out <- summary(linear_model))

#v
linear_model <- lm(respond ~ resplast+avggift+propresp+mailsyear, data=df)
(out <- summary(linear_model))

#C15
df <- fertil2
help("fertil2")

#i
min(df$children); max(df$children); mean(df$children)

#ii
nrow(filter(df, electric==1))/nrow(df)

#iii
mean(filter(df, electric==1)$children); mean(filter(df, electric==0)$children)

linear_model <- lm(children~electric, data=df)
(out <- summary(linear_model))

#iv
#since the coeff is SS, we can say yes

#v
linear_model <- lm(children~electric+educ+age+I(age^2)+ urban+ spirit + protest, data=df)
(out <- summary(linear_model))

#vi
linear_model <- lm(children~electric+educ+ I(electric*educ)+age+I(age^2)+ urban+ spirit + protest, data=df)
(out <- summary(linear_model))

#vii
linear_model <- lm(children~electric+educ+ I(electric*(educ-7))+age+I(age^2)+ urban+ spirit + protest, data=df)
(out <- summary(linear_model))


#C16
df <- catholic
help("catholic")

#i
nrow(filter(df, cathhs==1))/nrow(df)
mean(df$math12)
#ii
linear_model <- lm(math12~cathhs, data=df)
(out <- summary(linear_model))

#iii
linear_model <- lm(math12~cathhs+lfaminc+motheduc+fatheduc, data=df)
(out <- summary(linear_model))

#iv
colSums(is.na(df))

#v
linear_model_ur <- lm(math12~cathhs + lfaminc + motheduc + fatheduc
                   + I(cathhs*lfaminc) + I(cathhs*motheduc) + I(cathhs*fatheduc), data=df)
(out_ur <- summary(linear_model_ur))

linear_model_r <- lm(math12~cathhs + lfaminc + motheduc + fatheduc
                      , data=df)
(out_r <- summary(linear_model_r))


(F_statistcs <- ((out_ur$r.squared - out_r$r.squared)/(1-out_ur$r.squared) *(7422/3)))
#the p_value would be 0.30374 which means they are not jointly significant

#vi
mean(df$lfaminc);mean(df$motheduc);mean(df$fatheduc)

#C17
df <- jtrain98
help("jtrain98")

#i
nrow(filter(df, train ==1 & unem98==1))/nrow(df)

#ii
linear_model <- lm(unem98~train, data=df)
(out <- summary(linear_model))

#iii
linear_model <- lm(unem98~train+earn96+educ+age+married, data=df)
(out <- summary(linear_model))

cor(df$train, df$earn96)
cor(df$train, df$educ)
cor(df$train, df$age)
cor(df$train, df$married)
car::vif(linear_model)

#iv
mean_earn96 = mean(df$earn96)
mean_educ = mean(df$educ)
mean_age = mean(df$age)
mean_married = mean(df$married)

linear_model <- lm(unem98~train+earn96+educ+age+married
                   +I(train*(earn96-mean_earn96))
                   +I(train*(educ-mean_educ))
                      +I(train*(age-mean_age))
                      +I(train*(married-mean_married)), data=df)
(out <- summary(linear_model))

#v
linear_model_ur <- lm(unem98~train+earn96+educ+age+married
                      +I(train*(earn96-mean_earn96))
                      +I(train*(educ-mean_educ))
                      +I(train*(age-mean_age))
                      +I(train*(married-mean_married)), data=df)
(out_ur <- summary(linear_model_ur))

linear_model_r <- lm(unem98~train+earn96+educ+age+married
                     , data=df)
(out_r <- summary(linear_model_r))


(F_statistcs <- ((out_ur$r.squared - out_r$r.squared)/(1-out_ur$r.squared) *(1120/4)))
#the p_value would be 0.78 which means they are not jointly significant

#vi

linear_model_c <- lm(unem98~earn96+educ+age+married
                     +I(train*(earn96-mean_earn96))
                     +I(train*(educ-mean_educ))
                     +I(train*(age-mean_age))
                     +I(train*(married-mean_married)), data=filter(df, train==0))
linear_model_t <- lm(unem98~earn96+educ+age+married
                     +I(train*(earn96-mean_earn96))
                     +I(train*(educ-mean_educ))
                     +I(train*(age-mean_age))
                     +I(train*(married-mean_married)), data=filter(df, train==1))
unem98_0 <- predict(linear_model_c, newdata=df)
unem98_1 <- predict(linear_model_t, newdata=df)

mean(unem98_1-unem98_0)


#Clear plots
graphics.off()  # Clears plots, closes all graphics devices
# Clear console
cat("\014")  # Mimics ctrl+L
# Clear data
rm(list = ls())  # Removes all objects from environment

