library(tidyverse)
library('wooldridge')
library(ggplot2)


# Install and load the "sandwich" package
install.packages("sandwich")
library(sandwich)

# Perform iterated Cochrane-Orcutt estimation
library(orcutt)
# Install and load the plm package
install.packages("plm")
library(plm)

# Install and load the prais package
install.packages("prais")
library(prais)


#C1
df <- fertil3
help("fertil3")

linear_model <- lm(cgfr ~ cpe + cpe_1 + cpe_2, data=df)
(out <- summary(linear_model))

df$res <- c(NA, NA, NA, resid(linear_model))
df$res_1 <- c(NA, df$res[-nrow(df)])

linear_model_res <- lm(res ~ res_1, data=df)
(out <- summary(linear_model_res))

# Calculate HAC standard errors
(hac_se <- sqrt(diag(vcovHAC(linear_model))))

#C2
df <- wageprc
help("wageprc")

#i
linear_model <- lm(gprice ~ gwage + gwage_1 + gwage_2 + 
                            gwage_3 + gwage_4 + gwage_5 + 
                            gwage_6 + gwage_7 + gwage_8 +
                            gwage_9 + gwage_10 + gwage_11 + 
                            gwage_12, data=df)
(out <- summary(linear_model))

#ii
df$res <- c(rep(c(NA), times = 13), resid(linear_model))
df$res_1 <- c(NA, df$res[-nrow(df)])

linear_model_res <- lm(res~res_1, data=df)
(out <- summary(linear_model_res))

(modelco<-cochrane.orcutt(linear_model, convergence = 5, max.iter=1000))
# Calculate HAC standard errors
(hac_se <- sqrt(diag(vcovHAC(linear_model))))

#iii
linear_model <- lm(gprice ~ I(gwage + gwage_1 + gwage_2 + 
                                    gwage_3 + gwage_4 + gwage_5 + 
                                    gwage_6 + gwage_7 + gwage_8 +
                                    gwage_9 + gwage_10 + gwage_11 + 
                                    gwage_12) + 
                            I(-(gwage_1 + gwage_2 + 
                                gwage_3 + gwage_4 + gwage_5 + 
                                gwage_6 + gwage_7 + gwage_8 +
                                gwage_9 + gwage_10 + gwage_11 + 
                                gwage_12)) + 
                     I(-(gwage + gwage_2 + 
                           gwage_3 + gwage_4 + gwage_5 + 
                           gwage_6 + gwage_7 + gwage_8 +
                           gwage_9 + gwage_10 + gwage_11 + 
                           gwage_12)) +
                     I(-(gwage + gwage_1 + 
                           gwage_3 + gwage_4 + gwage_5 + 
                           gwage_6 + gwage_7 + gwage_8 +
                           gwage_9 + gwage_10 + gwage_11 + 
                           gwage_12)) +
                     I(-(gwage + gwage_1 + 
                           gwage_2 + gwage_4 + gwage_5 + 
                           gwage_6 + gwage_7 + gwage_8 +
                           gwage_9 + gwage_10 + gwage_11 + 
                           gwage_12)) +
                     I(-(gwage + gwage_1 + 
                           gwage_3 + gwage_2 + gwage_5 + 
                           gwage_6 + gwage_7 + gwage_8 +
                           gwage_9 + gwage_10 + gwage_11 + 
                           gwage_12)) +
                     I(-(gwage + gwage_1 + 
                           gwage_3 + gwage_4 + gwage_2 + 
                           gwage_6 + gwage_7 + gwage_8 +
                           gwage_9 + gwage_10 + gwage_11 + 
                           gwage_12)) +
                     I(-(gwage + gwage_1 + 
                           gwage_3 + gwage_4 + gwage_5 + 
                           gwage_2 + gwage_7 + gwage_8 +
                           gwage_9 + gwage_10 + gwage_11 + 
                           gwage_12)) +
                     I(-(gwage + gwage_1 + 
                           gwage_3 + gwage_4 + gwage_5 + 
                           gwage_6 + gwage_2 + gwage_8 +
                           gwage_9 + gwage_10 + gwage_11 + 
                           gwage_12)) +
                     I(-(gwage + gwage_1 + 
                           gwage_3 + gwage_4 + gwage_5 + 
                           gwage_6 + gwage_7 + gwage_2 +
                           gwage_9 + gwage_10 + gwage_11 + 
                           gwage_12)) +
                     I(-(gwage + gwage_1 + 
                           gwage_3 + gwage_4 + gwage_5 + 
                           gwage_6 + gwage_7 + gwage_8 +
                           gwage_2 + gwage_10 + gwage_11 + 
                           gwage_12)) +
                     I(-(gwage + gwage_1 + 
                           gwage_3 + gwage_4 + gwage_5 + 
                           gwage_6 + gwage_7 + gwage_8 +
                           gwage_9 + gwage_2 + gwage_11 + 
                           gwage_12)) +
                     I(-(gwage + gwage_1 + 
                           gwage_3 + gwage_4 + gwage_5 + 
                           gwage_6 + gwage_7 + gwage_8 +
                           gwage_9 + gwage_10 + gwage_2 + 
                           gwage_12)) +
                     I(-(gwage + gwage_1 + 
                           gwage_3 + gwage_4 + gwage_5 + 
                           gwage_6 + gwage_7 + gwage_8 +
                           gwage_9 + gwage_10 + gwage_11 + 
                           gwage_2)), data=df)
(out <- summary(linear_model))

# Calculate HAC standard errors
(hac_se <- sqrt(diag(vcovHAC(linear_model))))

#C3
df <- inven
help("inven")

#i
linear_model <- lm(cinven ~ cgdp, data=df)
(out <- summary(linear_model))

df$res <- c(rep(c(NA), times = 1), resid(linear_model))
df$res_1 <- c(NA, df$res[-nrow(df)])

linear_model_res <- lm(res ~ res_1, data=df)
(out <- summary(linear_model_res))

(modelco<-cochrane.orcutt(linear_model, convergence = 5, max.iter=1000))

#ii
(modelco<-cochrane.orcutt(linear_model, convergence = 5, max.iter=1000))

#C4
df <- nyse
help('nyse')

#i
linear_model <- lm(return ~ return_1, data=df)
(out <- summary(linear_model_res))

df$res_sq <- c(rep(c(NA), times = 2), resid(linear_model)^2)

linear_model_res <- lm(res_sq ~ return_1, data=df)
(out <- summary(linear_model_res))

sum(fitted(linear_model_res)<0)

#ii
linear_model_res <- lm(res_sq ~ return_1 + I(return_1^2), data=df)
(out <- summary(linear_model_res))

sum(fitted(linear_model_res)<0)

#iii
df$fitted_var <- c(rep(c(NA), times = 2), fitted(linear_model_res))

wls_model <- lm(return ~ return_1, data=df, weights = 1/fitted_var)
(out <- summary(wls_model))

#iv
df$res_sq_1 <- c(NA, df$res_sq[-nrow(df)])
linear_model_res <- lm(res_sq ~ res_sq_1, data=df)
(out <- summary(linear_model_res))

df$fitted_var <- c(rep(c(NA), times = 3), fitted(linear_model_res))

wls_model <- lm(return ~ return_1, data=df, weights = 1/fitted_var)
(out <- summary(wls_model))


#C5
df <- fair
help("fair")

#i 
df <- filter(df, year<1992)
linear_model <- lm(demwins ~ I + DPER + I(I*n) + I(p15*I), data=df)
(out <- summary(linear_model))

#ii
sum(fitted(linear_model)>1); sum(fitted(linear_model)<0)

#iii
df$predicted <- ifelse(fitted(linear_model) > 0.5, 1, 0)
df$truth <- ifelse(df$V > 0.5, 1, 0)
#number of correct classified
sum(df$predicted==df$truth)

#iv

# Create an unseen data point
new_data <- filter(df, year==1996)

# Predict the value for the unseen data
(predicted <- predict(linear_model, newdata = new_data))

#v
# Calculate HAC standard errors
(hac_se <- sqrt(diag(vcovHAC(linear_model))))

# Compute heteroskedasticity-robust t-test
(robust_test <- coeftest(linear_model, vcov = vcovHC(linear_model)))

#C6
#i
df <- consump
help("consump")

linear_mode <- lm(gc~gy, data=df)
(out <- summary(linear_mode))

df$res <- c(NA, resid(linear_mode))
df$res_1 <- c(NA, df$res[-nrow(df)])

linear_model_res <- lm(res ~ res_1, data=df)
(out <- summary(linear_model_res))

#ii
linear_model <- lm(gc~gc_1, data=df)
(out <- summary(linear_model))

df$res_sq <- c(NA, resid(linear_mode)^2)

linear_model_res <- lm(res_sq ~ gc_1 + I(gc_1^2), data=df)
(out <- summary(linear_model_res))


#C7
#i
df <- barium
help("barium")

#ii
linear_model <- lm(lchnimp ~ lchempi + lgas + lrtwex + befile6 + affile6 + afdec6, data=barium)
(out <- summary(linear_model))


# Load the package
library(prais)
pw <- prais_winsten(lchnimp ~ lchempi + lgas + lrtwex + befile6 + affile6 + afdec6,
                    data = barium, index = "t")
summary(pw)

library(orcutt)
(modelco<-cochrane.orcutt(linear_model, convergence = 5, max.iter=1000))

#C8
df <- traffic2
help("traffic2")

#i
linear_model <- lm(prcrfat ~ t + 
                     feb + mar + apr + may + jun + jul +
                     aug + sep + oct + nov + dec +
                     wkends + unem + spdlaw + beltlaw, data=traffic2)
(out <- summary(linear_model))

df$res <- resid(linear_model)
df$res_1 <- c(NA, df$res[-nrow(df)])

linear_model_res <- lm(res ~ res_1, data=df)
(out <- summary(linear_model_res))

#ii

# Calculate Newey-West standard errors
(nw_se <- sqrt(diag(NeweyWest(linear_model, lag = 4, adjust = TRUE))))

# Print the coefficients and Newey-West standard errors
coeftest(linear_model, vcov = nw_se)

#iii
# Load the package
library(prais)
pw <- prais_winsten(prcrfat ~ t + 
                      feb + mar + apr + may + jun + jul +
                      aug + sep + oct + nov + dec +
                      wkends + unem + spdlaw + beltlaw, data=traffic2, index = "t")
summary(pw)


#C9
df <- fish
help("fish")

#i
linear_model <- lm(lavgprc ~ mon + tues + wed + thurs + t, data=df)
(out <- summary(linear_model))

#ii
linear_model <- lm(lavgprc ~ mon + tues + wed + thurs + t + wave2 + wave3, data=df)
(out <- summary(linear_model))

#v
df$res <- resid(linear_model)
df$res_1 <- c(NA, df$res[-nrow(df)])

linear_model_res <- lm(res ~ res_1, data=df)
(out <- summary(linear_model_res))

#vi
# Calculate Newey-West standard errors
(nw_se <- sqrt(diag(NeweyWest(linear_model, lag = 4, adjust = TRUE))))

#vii
pw <- prais_winsten(lavgprc ~ mon + tues + wed + thurs + t + wave2 + wave3
                    , data=df, index = "t")
summary(pw)

#C10
df <- phillips
help("phillips")

#i
linear_model <- lm(inf~unem, data=df)
summary(linear_model)


#if we conclude that the error is AR(1) process
linear_model <- lm(inf~unem+unem_1+inf_1, data=df)
summary(linear_model)




#ii
df$res <- resid(linear_model)
df$res_1 <- c(NA, df$res[-nrow(df)])

linear_model_res <- lm(res ~ res_1, data=df)
(out <- summary(linear_model_res))

#iii

pw <- prais_winsten(inf~unem, data=df, index = "year")
summary(pw)

#iv
(modelco<-cochrane.orcutt(linear_model, convergence = 5, max.iter=1000))


#C10
df <- nyse
help("nyse")

#i
linear_model <- lm(return ~ return_1, data=df)
summary(linear_model)

df$res_sq <- c(rep(c(NA), times = 2), resid(linear_model)^2)

min(df$res_sq,  na.rm = TRUE); max(df$res_sq,  na.rm = TRUE); 
mean(df$res_sq,  na.rm = TRUE); median(df$res_sq,  na.rm = TRUE);

#ii
linear_model <- lm(res_sq ~ return_1 + I(return_1^2), data=df)
summary(linear_model)

#iii
# Create a plot
plot(df$return_1, df$res_sq,  type = "p", col = "blue", xlab = "return-1", ylab = "res^2", main = "Scatter Plot")

#iv
sum(predict(linear_model, data=df)<0)

#v
df$res_sq_1 <- c(NA, df$res_sq[-nrow(df)])
linear_model <- lm(res_sq ~ res_sq_1, data=df)
summary(linear_model)

#vi
df$res_sq_2 <- c(NA, df$res_sq_1[-nrow(df)])
linear_model <- lm(res_sq ~ res_sq_1 +res_sq_2, data=df)
summary(linear_model)

#C12
df <- inven
help("inven")

#i
linear_model <- lm(cinven~cgdp, data=df)
(out <- summary(linear_model))

df$res_sq <- c(rep(c(NA), times = 1), resid(linear_model))
df$res_sq_1 <- c(NA, df$res_sq[-nrow(df)])
linear_model <- lm(res_sq ~ res_sq_1, data=df)
summary(linear_model)

#ii
(modelco<-cochrane.orcutt(linear_model, convergence = 5, max.iter=1000))

#C13
df <- okun
help("okun")

#i
linear_model <- lm(pcrgdp~cunem, data=df)
(out <- summary(linear_model))

df$res_sq <- c(rep(c(NA), times = 1), resid(linear_model))
df$res_sq_1 <- c(NA, df$res_sq[-nrow(df)])
linear_model <- lm(res_sq ~ res_sq_1, data=df)
summary(linear_model)

#ii
linear_model <- lm(I(res_sq^2)~cunem, data=df)
(out <- summary(linear_model))
#iv
(robust_se <- sqrt(diag(vcovHC(linear_model, type = "HC1"))))

#C14
df <- minwage
help("minwage")

#i
linear_model <- lm(gwage232 ~ gmwage + gcpi, data=df)
summary(linear_model)

df$res_sq <- c(rep(c(NA), times = 1), resid(linear_model))
df$res_sq_1 <- c(NA, df$res_sq[-nrow(df)])
linear_model <- lm(res_sq ~ res_sq_1, data=df)
summary(linear_model)

#ii
# Calculate Newey-West standard errors
(nw_se <- sqrt(diag(NeweyWest(linear_model, lag = 12, adjust = TRUE))))

#iii
(robust_se <- sqrt(diag(vcovHC(linear_model, type = "HC1"))))

#iv
linear_model <- lm(gwage232 ~ gmwage + gcpi, data=df)
summary(linear_model)

df$res_sq <- c(rep(c(NA), times = 1), resid(linear_model)^2)
linear_model <- lm(res_sq ~ gmwage + gcpi, data=df)
summary(linear_model)

#v
linear_model <- lm(gwage232 ~ gmwage + gmwage_1 + gmwage_2 + gmwage_3 + gmwage_4 +
                                     + gmwage_5 + gmwage_6 + gmwage_7 + gmwage_8 +
                                     + gmwage_9 + gmwage_10 + gmwage_11 + 
                     gmwage_12 + gcpi, data=df)
summary(linear_model)

#v, vi, vii
#for running joint test using heteroskedasticity-robust test or Newey-west
# approach you should rewrite the equation for including all variables as united
# variable

#C15
df <- barium
help("barium")

#i
linear_model <- lm(lchnimp ~ lchempi + lgas + lrtwex + befile6 + affile6 + afdec6, data=barium)
(out <- summary(linear_model))

# Calculate Newey-West standard errors
(nw_se <- sqrt(diag(NeweyWest(linear_model, lag = 4, adjust = TRUE))))

#vii
install.packages("plm")
library(plm)

model <- plm(lchnimp ~ lchempi + lgas + lrtwex + befile6 + affile6 + afdec6, 
             data=df,  model = "pooling")
summary(model)
# Compute Prais-Winsten standard errors

linear_model <- lm(lchnimp ~ lchempi + lgas + lrtwex + befile6 + affile6 + afdec6, data=barium)
(out <- summary(linear_model))

# Calculate Newey-West standard errors
(nw_se <- sqrt(diag(NeweyWest(linear_model, lag = 12, adjust = TRUE))))




#C16
df <- approval
help("approval")

#i
df$dapprove <- c(rep(c(NA), times = 1),diff(df$approve))
df$dlcpifood <- c(rep(c(NA), times = 1),diff(df$lcpifood))
df$dlrgasprice <- c(rep(c(NA), times = 1),diff(df$lrgasprice))
df$dunemploy <- c(rep(c(NA), times = 1),diff(df$unemploy))

linear_model <- lm(dapprove ~ dlcpifood + dlrgasprice + dunemploy + X11.Sep + iraqinvade, 
                   data=df)
summary(linear_model)

df$res_sq <- c(rep(c(NA), times = 1), resid(linear_model))
df$res_sq_1 <- c(NA, df$res_sq[-nrow(df)])
linear_model <- lm(res_sq ~ res_sq_1, data=df)
summary(linear_model)

#ii
model <- plm(dapprove ~ dlcpifood + dlrgasprice + dunemploy + X11.Sep + iraqinvade, 
             data=df,  model = "pooling")
summary(model)

#iii
# Calculate Newey-West standard errors
(nw_se <- sqrt(diag(NeweyWest(linear_model, lag = 1, adjust = TRUE))))
(nw_se <- sqrt(diag(NeweyWest(linear_model, lag = 4, adjust = TRUE))))
(nw_se <- sqrt(diag(NeweyWest(linear_model, lag = 8, adjust = TRUE))))


#Clear plots
graphics.off()  # Clears plots, closes all graphics devices
# Clear console
cat("\014")  # Mimics ctrl+L
# Clear data
rm(list = ls())  # Removes all objects from environment
