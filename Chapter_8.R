library(tidyverse)
library('wooldridge')
library(ggplot2)

#for robust t-test
library("lmtest")
library("sandwich")


#C1

#i
# As one option, the variance of u can be exp(a0+a1) for male 
#and exp(a0) for female

#ii
df <- sleep75
help("sleep75")

linear_model <- lm(sleep ~ totwrk + educ + age + I(age^2) + yngkid + male, data=df)
(out <- summary(linear_model))

df$residual <- out$residuals
#the variance is different across males and females
(var(filter(df, male==1)$residual)); (var(filter(df, male==0)$residual))

df$lu2 <- log(df$residual^2)

linear_model <- lm(lu2~male, data=df)
(out <- summary(linear_model))


#iii
#it is not statistically different

#C2
df <- hprice1
help("hprice1")

#i
linear_model <- lm(price~lotsize+sqrft+bdrms, data=df)
(out <- summary(linear_model))

#check heteroskesdastic visually
#create residual vs. fitted plot
plot(fitted(linear_model), resid(linear_model))
#add a horizontal line at y=0 
abline(0,0)

#calculating the heteroskedastic-robust variance. for lotsize
linear_model_lotsize <- lm(lotsize~sqrft+bdrms, data=df)
(out_lotsize <- summary(linear_model_lotsize))

(std_het_robust <- sqrt(sum(resid(linear_model_lotsize)^2*resid(out)^2))/sum(resid(linear_model_lotsize)^2))

# Robust t test
coeftest(linear_model, vcov = vcovHC(linear_model, type = "HC0"))

#ii
linear_model <- lm(lprice~llotsize+lsqrft+bdrms, data=df)
(out <- summary(linear_model))

#check heteroskesdastic visually
#create residual vs. fitted plot
plot(fitted(linear_model), resid(linear_model))
#add a horizontal line at y=0 
abline(0,0)
# Robust t test
coeftest(linear_model, vcov = vcovHC(linear_model, type = "HC0"))

#iii
#log transformation reduces the heteroskedascity

#C3
df <- hprice1
linear_model <- lm(lprice~llotsize+lsqrft+bdrms, data=df)
(out <- summary(linear_model))

df$res_2 <- resid(linear_model)^2
df$fitted <- fitted(linear_model)

linear_model_u_hat_2 <- lm(res_2~fitted+I(fitted^2), data=df)
(out_res <- summary(linear_model_u_hat_2))

(LM <- nrow(df)*out_res$r.squared)

# for p-value we calculate the P(X> LM) where X is distributed according to chi(2)
p_value <- 0.18
#since the p-value is not small enough, we failed to reject the null hypothsis and 
#the homoskedasticity

#C4
df <- vote1
help("vote1")

#i
linear_model <- lm(voteA ~ prtystrA+democA+lexpendA+lexpendB, data=df)
(out <- summary(linear_model))
df$res <- resid(linear_model)

linear_model_u_hat <- lm(res ~ prtystrA+democA+lexpendA+lexpendB, data=df)
(out_u_hat <- summary(linear_model_u_hat))
#since based on OLS estimation, the residual is conditionally uncorrelated of independent variables

#ii
df$res_2 <- resid(linear_model)^2
linear_model_u_hat <- lm(res_2 ~ prtystrA + democA + lexpendA + lexpendB, data=df)
(out_u_hat <- summary(linear_model_u_hat))

r_u_2 <- out_u_hat$r.squared

(F_stat <- (r_u_2/(1-r_u_2))*((nrow(df)-4-1)/4))

#p-value is P(X> F) where X is distributed ~ F(4, 168)
p_value <- 0.05806

#iii
df$fitt_ed_value <- fitted(linear_model)
linear_model_u_hat <- lm(res_2 ~ fitt_ed_value + I(fitt_ed_value^2), data=df)
(out_u_hat <- summary(linear_model_u_hat))
r_u_2 <- out_u_hat$r.squared
(F_stat <- (r_u_2/(1-r_u_2))*((nrow(df)-2-1)/2))

#p-value is P(X> F) where X is distributed ~ F(2, 170)
p_value <- 0.06450

#C5
df <- pntsprd
help('pntsprd')

#i
linear_model <- lm(sprdcvr~1, data=df)
(out <- summary(linear_model))

(t_stats<- (out$coefficients[1,1]-0.5)/out$coefficients[1,2])
#p-value is P(X>t_stats) where X is t-distributed 
p_value <- 0.23512
#so it is failed to reject the null hypothesis at 10%

#ii
nrow(filter(df, neutral==1))/nrow(df)

#iii
linear_model <- lm(sprdcvr~favhome+neutral+fav25+und25, data=df)
(out <- summary(linear_model))

# Robust t test
coeftest(linear_model, vcov = vcovHC(linear_model, type = "HC0"))

#v
df$res_2 <- resid(linear_model)^2

linear_model_u_hat <- lm(res_2~favhome+neutral+fav25+und25, data=df)
(out <- summary(linear_model_u_hat))

#the F-stats as mentioned is 2.762
p_value <- 0.02704
#so we reject the null hypothesis

#C6
df <- crime1
help("crime1")

#i 
df$arr86 <- ifelse(df$narr86 > 0, 1, 0)

linear_model <- lm(arr86~pcnv+avgsen+tottime+ptime86+qemp86, data=df)
(out <- summary(linear_model))

df$fitted_value <- fitted(linear_model)
nrow((df$fitted_value > 1) & (df$fitted_value < 0))
min(df$fitted_value); max(df$fitted_value)
#ii
df$h_hat <- df$fitted_value * (1- df$fitted_value)

wls_model <- lm(arr86~pcnv+avgsen+tottime+ptime86+qemp86, data=df, weights=I(1/h_hat))
(out_wls <- summary(wls_model))

#iii
wls_model_ur <- lm(arr86~pcnv+avgsen+tottime+ptime86+qemp86, data=df, weights=I(1/h_hat))
(out_wls_ur <- summary(wls_model_ur))

wls_model_r <- lm(arr86~pcnv+ptime86+qemp86, data=df, weights=I(1/h_hat))
(out_wls_r <- summary(wls_model_r))

(F_statistcs <- ((out_wls_ur$r.squared - out_wls_r$r.squared)/(1-out_wls_ur$r.squared) *(2719/2)))

#since p-value is not small enough, we failed to reject the null hypothesis
p_value <- 0.41286

#C7
df <- loanapp
help("loanapp")

#i
linear_model <- lm(approve~ white+hrat+obrat+loanprc+unem 
                   +male+married+dep+sch+cosign 
                   +chist+ pubrec+ mortlat1+ mortlat2+vr, data = df)
(out <- summary(linear_model))

# Robust t test
rbst_sd_error <- coeftest(linear_model, vcov = vcovHC(linear_model, type = "HC0"))

#non-robust confidence interval
confint(linear_model, "white")

#robust confidence interval 
(confint <- c(rbst_sd_error[2,1]-2*rbst_sd_error[2,2], rbst_sd_error[2,1]+2*rbst_sd_error[2,2]))

#ii
nrow((fitted(linear_model) > 1) & (fitted(linear_model) < 0))

#C8
df <- gpa1
help("gpa1")

#i
linear_model <- df %>% lm(colGPA ~ hsGPA + ACT + skipped + PC, data=.)
(out <- summary(linear_model))

df$res <- resid(linear_model)
df$fitted_value <- fitted(linear_model)
#ii
linear_model_res <- df %>% lm(I(res^2) ~ fitted_value+I(fitted_value^2), data=.)
df$fitted_res <- fitted(linear_model_res)

#iii
#fitted values are strictly positive
df %>% filter(fitted_res<0)

wls_model <- lm(colGPA ~ hsGPA + ACT + skipped + PC, data=df, weights=I(1/fitted_res))
(out_wls <- summary(wls_model))

# Robust t test
(rbst_sd_error <- coeftest(wls_model, vcov = vcovHC(wls_model, type = "HC0")))

#C9
df <- smoke
help("smoke")

#i
linear_model <- lm(cigs ~ lincome + lcigpric + educ + 
                     age + I(age^2) + restaurn, data=df)
(out <- summary(linear_model))

#ii
df$res <- resid(linear_model)
df$fitted_value <- fitted(linear_model)

linear_model_res <- lm(I(log(res^2)) ~ fitted_value + I(fitted_value^2), data=df)

df$h_i <- exp(fitted(linear_model_res))

wls_model <- lm(cigs ~ lincome + lcigpric + educ + 
                  age + I(age^2) + restaurn, data=df, weights=I(1/I(h_i)))
(out_wls <- summary(wls_model))

#iii
df$res_weighted <- df$res/sqrt(df$h_i)
df$rfitted_value_weighted <- df$fitted_value/sqrt(df$h_i)

linear_model_res <- lm(I(res_weighted^2)~rfitted_value_weighted+I(rfitted_value_weighted^2), data=df)
(out_u_hat <- summary(linear_model_res))
r_u_2 <- out_u_hat$r.squared

(F_stat <- (r_u_2/(1-r_u_2))*((nrow(df)-2-1)/2))

#the p-value
(p_value <- 1-pf(F_stat, df1 = 2, df2 = nrow(df)-2-1))

#v
# Robust t test
(rbst_sd_error <- coeftest(wls_model, vcov = vcovHC(wls_model, type = "HC0")))

#C10
df <- k401ksubs
help('k401ksubs')

#i
linear_model <- lm(e401k~inc+I(inc^2)+age+I(age^2)+male, data=df)
(out <- summary(linear_model))

# Robust t test
(rbst_sd_error <- coeftest(linear_model, vcov = vcovHC(linear_model, type = "HC0")))

#iii
df$res <- resid(linear_model)
df$fit <- fitted(linear_model)
linear_model_res <- lm(I(res^2)~fit + I(fit^2), data=df)
(out <- summary(linear_model_res))

#iv
sum(fitted(linear_model)<0); sum(fitted(linear_model)>1)

df$h_i <- fitted(linear_model_res)

linear_model <- lm(e401k~inc+I(inc^2)+age+I(age^2)+male, data=df, weights = I(1/h_i))
(out <- summary(linear_model))

#C11
df <- k401ksubs %>% filter(fsize==1)

#i
linear_model <- lm(nettfa~inc+I((age-25)^2)+male+e401k+I(e401k*inc), data=df)
(out <- summary(linear_model))
# Robust t test
(rbst_sd_error <- coeftest(linear_model, vcov = vcovHC(linear_model, type = "HC0")))

#ii
linear_model <- lm(nettfa~inc+I((age-25)^2)+male+e401k+I(e401k*inc), data=df, weights = I(1/inc))
(out <- summary(linear_model))
# Robust t test
(rbst_sd_error <- coeftest(linear_model, vcov = vcovHC(linear_model, type = "HC0")))

#iv
linear_model <- lm(nettfa~inc+I((age-25)^2)+male+e401k+I(e401k*(inc-30)), data=df, weights = I(1/inc))
(out <- summary(linear_model))

#C12
df <- meap00_01
help("meap00_01")

#i
linear_model <- lm(math4~lunch+lexppp+lenroll, data=df)
(out <- summary(linear_model))
# Robust t test
(rbst_sd_error <- coeftest(linear_model, vcov = vcovHC(linear_model, type = "HC0")))

#ii
df$res <- resid(linear_model)
df$y_hat <- fitted(linear_model)

linear_model_res <- lm(I(res^2) ~ y_hat + I(y_hat^2), data=df)
(out_res <- summary(linear_model_res))

#perform Breusch-Pagan Test
bptest(linear_model)

#iii
linear_model_res <- lm(I(log(res^2)) ~ y_hat + I(y_hat^2), data=df)
(out_res <- summary(linear_model_res))

df$h_i <- exp(fitted(linear_model_res))
linear_model <- lm(math4~lunch+lexppp+lenroll, data=df, weights = I(1/h_i))
(out <- summary(linear_model))
# Robust t test
(rbst_sd_error <- coeftest(linear_model, vcov = vcovHC(linear_model, type = "HC0")))

#C13
df <- fertil2
help("fertil2")

#i
linear_model <- lm(children ~ age + I(age^2) + educ + urban + electric, data=df)
(out <- summary(linear_model))
# Robust t test
(rbst_sd_error <- coeftest(linear_model, vcov = vcovHC(linear_model, type = "HC0")))

#ii
model_ur <- lm(children ~ age + I(age^2) + educ + urban + electric
                   +spirit+protest+catholic, data=df)
(out_wls_ur <- summary(model_ur))

model_r <- lm(children ~ age + I(age^2) + educ + urban + electric, data=df)
(out_wls_r <- summary(model_r))

(F_statistcs <- ((out_wls_ur$r.squared - out_wls_r$r.squared)/(1-out_wls_ur$r.squared) *(2719/2)))

#the p-value
(p_value <- 1-pf(F_statistcs, df1 = 3, df2 = nrow(df)-3-1))

#iii
res <- resid(linear_model)
fit <- fitted(linear_model)

dfd <- data.frame(res, fit)

linear_model_u_hat <- lm(I(res^2)~fit+I(fit^2), data=dfd)
(summary(linear_model_u_hat))

#C14
df <- beauty
help("beauty")
#i
linear_model <- lm(lwage~belavg+abvavg+female+educ+exper+I(exper^2), data=df)
(out <- summary(linear_model))
# Robust t test
(rbst_sd_error <- coeftest(linear_model, vcov = vcovHC(linear_model, type = "HC0")))

#ii
model_ur <- lm(lwage~belavg+abvavg+female+educ+exper+I(exper^2)
               +I(educ*female)+I(belavg*female)+I(abvavg*female)+I(exper*female)
               +I(exper^2*female), data=df)
(out_wls_ur <- summary(model_ur))

model_r <- lm(lwage~belavg+abvavg+female+educ+exper+I(exper^2), data=df)
(out_wls_r <- summary(model_r))

(F_statistcs <- ((out_wls_ur$r.squared - out_wls_r$r.squared)/(1-out_wls_ur$r.squared) *(1248/5)))
#the p-value
(p_value <- 1-pf(F_statistcs, df1 = 5, df2 = 1248))

#iii
model_ur <- lm(lwage~belavg+abvavg+female+educ+exper+I(exper^2)
               +I(educ*female)+I(belavg*female)+I(abvavg*female)+I(exper*female)
               +I(exper^2*female), data=df)
(out_wls_ur <- summary(model_ur))

model_r <- lm(lwage~belavg+abvavg+female+educ+exper+I(exper^2)
              +I(educ*female)+I(exper*female)
              +I(exper^2*female), data=df)
(out_wls_r <- summary(model_r))

(F_statistcs <- ((out_wls_ur$r.squared - out_wls_r$r.squared)/(1-out_wls_ur$r.squared) *(1258/2)))
#the p-value
(p_value <- 1-pf(F_statistcs, df1 = 2, df2 = 1258))



#Clear plots
graphics.off()  # Clears plots, closes all graphics devices
# Clear console
cat("\014")  # Mimics ctrl+L
# Clear data
rm(list = ls())  # Removes all objects from environment

