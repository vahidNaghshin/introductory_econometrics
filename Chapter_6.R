library(tidyverse)
library('wooldridge')
library(ggplot2)

#C1
df <- kielmc
help('kielmc')

#i
df <- filter(df, year==1981)
linear_model <- lm(lprice~ldist, data=df)
(out <- summary(linear_model))

#ii
linear_model <- lm(lprice~ldist+lintst+larea+lland+rooms+baths+age, data=df)
(out <- summary(linear_model))

#iii
linear_model <- lm(lprice~ldist+lintst+I(lintst^2)+larea+lland+rooms+baths+age, data=df)
(out <- summary(linear_model))

#when there is a big change in estimating the coefficient when new variabels 
#are added it might indicate the multicolinearity
cor(df$ldist, df$lintst)
cor(df$ldist, df$larea)
cor(df$ldist, df$lland)
cor(df$ldist, df$rooms)
cor(df$ldist, df$baths)
cor(df$ldist, df$age)
car::vif(linear_model)

#C2
df <- wage1
help("wage1")
#i
linear_model <- lm(lwage~ educ+exper+I(exper^2), data=df)
(out <- summary(linear_model))
#ii
#yes it is 
#iii
(delta <- 100*(out$coefficients[3,1]+2*out$coefficients[4,1]*5))
(delta <- 100*(out$coefficients[3,1]+2*out$coefficients[4,1]*20))
#iv
(turning_point <- abs((out$coefficients[3,1]/(2*out$coefficients[4,1]))))
#number of samples with over 29 years of experience
(nrow(filter(df, exper> 29)))

#C3
df <- wage1
#i
linear_model <- lm(lwage~ educ+exper+I(educ*exper), data=df)
(out <- summary(linear_model))

#iii
linear_model <- lm(lwage~ educ+exper+I(educ*exper), data=df)
(out <- summary(linear_model))

#iv
(theta <- out$coefficients[2,1] + (10 * out$coefficients[4,1]))

linear_model <- lm(lwage ~ educ + exper + I((educ*exper) - (10*educ)), data=df)
(out <- summary(linear_model))

#confidence interval
(conf_int <- c(out$coefficients[2, 1] - (2 * out$coefficients[2, 2]), 
              out$coefficients[2, 1] + (2 * out$coefficients[2, 2])))

#C4
#i
df <- gpa2
help('gpa2')
linear_model <- lm(sat~hsize+I(hsize^2), data=df)
(out <- summary(linear_model))
#yes it is statistically significant

#ii
(opt_size <- abs(out$coefficients[2,1]/(2*out$coefficients[3,1])))

#iv
df$lsat = log(df$sat)
linear_model <- lm(lsat~hsize+I(hsize^2), data=df)
(out <- summary(linear_model))
(opt_size <- abs(out$coefficients[2,1]/(2*out$coefficients[3,1])))

#C5
df <- hprice1
help("hprice1")

#i
linear_model <- lm(lprice~llotsize+lsqrft+bdrms, data=df)
(out <- summary(linear_model))

#ii
res <- linear_model$residuals
new <- data.frame(llotsize=c(log(20000)), lsqrft=c(log(2500)), bdrms=c(4))
logy <- predict(linear_model, newdata = new)

(y_hat <- mean(exp(res))*exp(logy))

#iii
linear_model <- lm(lprice ~ llotsize + lsqrft + bdrms, data=df)
(out <- summary(linear_model))
(out$r.squared)

linear_model <- lm(price ~ lotsize + sqrft + bdrms, data=df)
(out <- summary(linear_model))
(out$r.squared)
#since the linear scale has the highest R-squared then it is better off
#using the linear scale

#C6
df <- vote1 
help("vote1")

#i & ii
linear_model <- lm(voteA ~ prtystrA + expendA + expendB + I(expendA*expendB), 
                   data=df)
(out <- summary(linear_model))
#partial effect of expendB = -3.172e-02 + -6.629e-06 * expendA
#the interaction term is not statistically significant

#iii
(mean_expendA <- mean(df$expendA))
((100*out$coefficients[4,1]) + (100*out$coefficients[5,1]*100))

#iv
((100*out$coefficients[3,1]) + (100*out$coefficients[5,1]*100))

#v
linear_model <- lm(voteA ~ prtystrA + expendA + expendB + shareA, 
                   data=df)
(out <- summary(linear_model))

#C7
df <- attend
help('attend')
#i
linear_model <- lm(stndfnl ~ attend+priGPA + I(priGPA^2) + ACT + I(ACT^2)
                   + I(priGPA*attend), data=df)
(out<- summary(linear_model))

(partial_effect <- (out$coefficients[3,1]+2*out$coefficients[4,1]*2.59
                     + out$coefficients[7,1]*82))

#ii
linear_model <- lm(stndfnl ~ attend + priGPA + ACT + I((priGPA-2.59)^2)
                   + I(ACT^2)+I(priGPA*(attend-82)), data=df)
(out<- summary(linear_model))

#iii
linear_model <- lm(stndfnl ~ attend + priGPA + ACT + I((priGPA-2.59)^2)
                   + I(ACT^2)+I((priGPA-2.59)*(attend-82)), data=df)
(out<- summary(linear_model))


#C8
df <- hprice1
help("hprice1")

#i
linear_model <- lm(price~lotsize+sqrft+bdrms, data=df)
(out <- summary(linear_model))

new <- data.frame(lotsize=c(10000), sqrft=c(2300), bdrms=c(4))
(round(predict(linear_model, newdata = new)))

#ii
linear_model <- lm(price ~ I(lotsize - 10000) + 
                     I(sqrft - 2300) + 
                     I(bdrms - 4), data=df)
(out1 <- summary(linear_model))

#the confidence interval is derived based on intercept standard error
(conf_int <- c(out$coefficients[1,1] - (1.96 * out$coefficients[1,2]),
               out$coefficients[1,1] + (1.96 * out$coefficients[1,2])))

#iii
se_predicted <- (sqrt(var(out$residuals) + (out1$coefficients[1,2])^2))

(conf_int <- c(out1$coefficients[1,1] - (1.96 * se_predicted),
               out1$coefficients[1,1] + (1.96 * se_predicted)))

#C9
df <- nbasal
help('nbasal')

#i
linear_model <- lm(points ~ exper + I(exper^2)+ age + coll, data=df)
(out <- summary(linear_model))

#ii
(opt_size <- abs(out$coefficients[2,1]/(2*out$coefficients[3,1])))

#iv
#it seems if we include quadratic form of age, after age 37 the salary increases
#not the sign of age and age squared. it seems adding age is not necessary
linear_model <- lm(points ~ exper + I(exper^2)+ age + I(age^2) + coll, data=df)
(out <- summary(linear_model))
(opt_size <- abs(out$coefficients[4,1]/(2*out$coefficients[5,1])))

#v
linear_model <- lm(lwage ~ exper + I(exper^2)+ age + coll, data=df)
(out <- summary(linear_model))

#vi
linear_model_ur <- lm(lwage ~ exper + I(exper^2)+ age + coll, data=df)
(out_ur <- summary(linear_model_ur))

linear_model_r <- lm(lwage ~ exper + I(exper^2), data=df)
(out_r <- summary(linear_model_r))

#since F is large enough, it is OK to reject the hypothesis testing and
#both variables are jointly significant
(F_statistcs <- ((out_ur$r.squared - out_r$r.squared)/(1-out_ur$r.squared) *(264/2)))

#C10
df <- bwght2
help('bwght2')
#i
linear_model <- lm(lbwght ~ npvis + I(npvis^2), data=df)
(out <- summary(linear_model))

#ii
nrow(filter(df, npvis >= 22))

#iii
linear_model <- lm(lbwght ~ npvis + I(npvis^2) + mage + I(mage^2), data=df)
(out <- summary(linear_model))

(opt_size <- abs(out$coefficients[4,1] / (2*out$coefficients[5,1])))
nrow(filter(df, mage >= 30))

#iv
linear_model <- lm(lbwght ~ npvis + I(npvis^2) + mage + I(mage^2), data=df)
(out_l <- summary(linear_model))
linear_model <- lm(bwght ~ npvis + I(npvis^2) + mage + I(mage^2), data=df)
(out <- summary(linear_model))


#C11
df <- apple
help("apple")

#i
linear_model <- lm(ecolbs~regprc+ecoprc, data=df)
(out <- summary(linear_model))

#ii
#Yes, they are significant
fitted_value <- predict(linear_model, new=df)
min(fitted_value); max(fitted_value). #range of fitted values
nrow(filter(df, ecolbs==0))

#iv
#the price variables explain only 3% of variation

#v

linear_model_ur <- lm(ecolbs ~ regprc + ecoprc + faminc + educ + hhsize + age, data=df)
(out_ur <- summary(linear_model_ur))

linear_model_r <- lm(ecolbs ~ regprc + ecoprc, data=df)
(out_r <- summary(linear_model_r))

#since F is large enough, it is OK to reject the hypothesis testing and
#both variables are jointly significant
(F_statistcs <- ((out_ur$r.squared - out_r$r.squared)/(1-out_ur$r.squared) *(653/4)))
#from table we can see the p-value is
p_value <- 0.81210

#vi
linear_model_reg <- lm(ecolbs ~ regprc , data=df)
(out_reg <- summary(linear_model_reg))

linear_model_eco <- lm(ecolbs ~ ecoprc, data=df)
(out_eco <- summary(linear_model_eco))

linear_model <- lm(ecolbs ~ regprc + ecoprc, data=df)
(out <- summary(linear_model))
car::vif(linear_model)

cor(df$regprc, df$ecoprc)

#vii
linear_model <- lm(ecolbs ~ regprc + ecoprc + faminc + reglbs, data=df)
(out <- summary(linear_model))

#C12
df <- k401ksubs
help('k401ksubs')

df <- filter(df, fisze==1)

#i
nrow(filter(df, age==25))

#ii & iii
linear_model <- lm(nettfa ~ inc + age + I(age^2), data=df)
(out <- summary(linear_model))

#iv
linear_model <- lm(nettfa ~ inc + age + I((age - 25)^2), data=df)
(out <- summary(linear_model))

#iv
linear_model <- lm(nettfa ~ inc + I((age - 25)^2), data=df)
(out <- summary(linear_model))

#v
x <- seq(25,70,1)
plot(x, 30 + I((x - 25)^2), ylab='30 + (age-25)^2', xlab='Age')

#C13
df <- meap00_01
help('meap00_01')

#i
linear_model <- lm(math4~ lexppp+ lenroll + lunch, data=df )
(out <- summary(linear_model))

#ii
fitted_value <- predict(linear_model, new=df)
min(fitted_value); max(fitted_value)
min(df$math4); max(df$math4)

#iii
idx_max <- which.max(out$residuals)
bcode_max_residual <- df[idx_max, ][, 2]

#iv
linear_model <- lm(math4~ lexppp+ lenroll + lunch, data=df )
(out <- summary(linear_model))

#v
linear_model_ur <- lm(math4~ lexppp + I(lexppp^2) + lenroll + I(lenroll^2) + lunch + I(lunch^2), data=df)
(out_ur <- summary(linear_model_ur))

linear_model_r <- lm(math4~ lexppp+ lenroll + lunch, data=df)
(out_r <- summary(linear_model_r))

#since F is small, it is failed to reject the hypothesis testing and
#both variables are jointly insignificant
(F_statistcs <- ((out_ur$r.squared - out_r$r.squared)/(1-out_ur$r.squared) *(653/4)))

#v
linear_model <- lm(math4~ I(lexppp/sd(lexppp)) + I(lenroll/sd(lenroll))
                   + I(lunch/sd(lunch)), data=df )
(out <- summary(linear_model))

#C14
df <- benefits
help("benefits")

#i
linear_model <- lm(lavgsal ~ bs, data=df)
(out <- summary(linear_model))
#We are failed to reject the bs=0
p_value <- 0.00248

#for beta_bs=-1
(t_statistics <- (-0.50346+1)/0.16615)

#probability that t-statistics is greater than zero
p_value <- (1- 0.99857743)
#since the p_value is low enough then we can reject that it is statistically signifincant

#ii
sd(log(df$bs)); min(log(df$bs));max(log(df$bs))
sd(df$bs); min(df$bs);max(df$bs)

#iii
linear_model <- lm(lavgsal ~ log(bs), data=df)
(out <- summary(linear_model))

#iv
linear_model <- lm(lavgsal ~ bs + lenroll + lstaff + lunch, data=df)
(out <- summary(linear_model))

#vi
linear_model <- lm(lavgsal ~ bs + lenroll + lstaff + lunch + I(lunch^2), data=df)
(out <- summary(linear_model))
(turning_point <- abs((out$coefficients[5,1]/(2*out$coefficients[6,1]))))

min(df$lunch); max(df$lunch);
nrow(filter(df, lunch>=56))

#vii
#Since the turning point is 56, then after 56 the level of poverty (lunch)
#has positive effect on the teacher salary


#Clear plots
graphics.off()  # Clears plots, closes all graphics devices
# Clear console
cat("\014")  # Mimics ctrl+L
# Clear data
rm(list = ls())  # Removes all objects from environment

