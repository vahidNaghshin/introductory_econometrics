library(tidyverse)
library('wooldridge')
install.packages("AER")
# Load the required package
library(AER)

#C1
df <- wage2
help('wage2')

#i
linear_model <- lm(lwage~sibs, data=df)
(out <- summary(linear_model))

# Specify the formula for the linear regression model
formula <- formula(lwage ~ educ | sibs)

# Fit the instrumental variable regression model
iv_model <- ivreg(formula, data = df)

# Print the summary of the instrumental variable regression results
summary(iv_model)

#ii
linear_model <- lm(educ~brthord, data=df)
(out <- summary(linear_model))

#iii
formula <- formula(lwage ~ educ | brthord)
iv_model <- ivreg(formula, data = df)
summary(iv_model)

#iv
#so we see that the null hypothesis is rejected and hence we can use brthord
# as IV for educ
linear_model <- lm(educ~sibs+brthord, data=df)
summary(linear_model)

#v
# specify the ivreg formula as y ~ x1 + x2 + w1 | w1 + z1 + z2 + z3
# where x_i are endogenous regressors and w_i are exogenous regressor
formula <- formula(lwage ~ educ + sibs | sibs + brthord)
iv_model <- ivreg(formula, data = df)
coeftest(iv_model, vcov = vcovHC, type = "HC1")

#vi
linear_model <- lm(educ~sibs+brthord, data=df)

# Calculate correlation
correlation <- cor(linear_model$fitted.values, df$sibs)

#C2
df <- fertil2
help("fertil2")

#i
linear_model <- lm(children ~ educ + age + I(age^2), data=df)
(out <- summary(linear_model))

#ii
linear_model <- lm(educ ~ frsthalf, data=df)
(out <- summary(linear_model))

#iii
formula <- formula(children ~ educ + age + I(age^2) | age + I(age^2) + frsthalf)
iv_model <- ivreg(formula, data = df)
coeftest(iv_model, vcov = vcovHC, type = "HC1")

#iv

formula <- formula(children ~ educ + age + I(age^2)
                   + tv + electric + bicycle | age + I(age^2) +
                     tv + electric + bicycle + frsthalf)
iv_model <- ivreg(formula, data = df)
coeftest(iv_model, vcov = vcovHC, type = "HC1")

#C3
df <- card
help('card')

#ii
linear_model <- lm(IQ~nearc4, data=df)





#Clear plots
graphics.off()  # Clears plots, closes all graphics devices
# Clear console
cat("\014")  # Mimics ctrl+L
# Clear data
rm(list = ls())  # Removes all objects from environment
