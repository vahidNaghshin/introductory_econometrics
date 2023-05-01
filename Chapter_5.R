library(tidyverse)
library('wooldridge')
#load ggplot2
library(ggplot2)

#C1
df <- wage1
help('wage1')

#i
linear_model <- lm(wage ~ educ + exper + tenure, data=df)
(out_l <- summary(linear_model))
residual <- resid(linear_model)
#create histogram of residuals
ggplot(data = df, aes(x = linear_model$residuals)) +
  geom_histogram(fill = 'steelblue', color = 'black',bins=30) +
  labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency')

#ii
linear_model <- lm(lwage ~ educ + exper + tenure, data=df)
#create histogram of residuals
ggplot(data = df, aes(x = linear_model$residuals)) +
  geom_histogram(fill = 'steelblue', color = 'black',bins=30) +
  labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency')

#C2
df <- gpa2
help('gpa2')
#i
linear_model <- lm(colgpa ~ hsperc + sat, data=df)
(out1 <- summary(linear_model))
#ii
linear_model <- lm(colgpa ~ hsperc + sat, data=df[1:2070,])
(out2 <- summary(linear_model))

#it is pretty close to sqrt of number of samples ratio
(out2$coefficients[2,2]/out1$coefficients[2,2])
(sqrt(nrow(df)/2070))

#C3
df <- bwght

#remove nans 
df <- df %>% drop_na(c(cigs, parity, faminc, motheduc, fatheduc)) 

linear_model <- lm(bwght ~ cigs+parity+faminc, data=df)
res <- residuals(linear_model)


linear_model_res <- lm(res ~ df$cigs+df$parity+df$faminc+df$motheduc+df$fatheduc)
out <- summary(linear_model_res)
#The 10% critical value for chi-squared distribution with two degrees of freedom is 4.61
#so as the LM statistics is not larger than 4.61, it 
(out$r.squared*nrow(df))

#C4

#i
df <- k401ksubs
help("k401ksubs")
df <- filter(df, fsize==1)
mean(df$inc^3); mean(log(df$inc)^3)
#ii
df <- bwght2
mean(df$bwght^3); mean(log(df$bwght)^3)

#C5
df <- htv
help('htv')
linear_model <- lm(educ ~ motheduc+fatheduc+abil+I(abil^2), data=df)
(out <- summary(linear_model))

#i
plot(df$educ)
unique(df$educ)

#ii
ggplot(data = df, aes(x = educ)) +
  geom_histogram(fill = 'steelblue', color = 'black',bins=30) +
  labs(title = 'Histogram of educ', x = 'educ', y = 'Frequency')

#C6
#i
df <- econmath
min(df$score);max(df$score)

#ii
ggplot(data = df, aes(x = score)) +
  geom_histogram(fill = 'steelblue', color = 'black',bins=30) +
  labs(title = 'Histogram of score', x = 'score', y = 'Frequency')

#iii
linear_model <- lm(score ~ colgpa + actmath + acteng, data=df)
(out <- summary(linear_model))



# Clear plots
graphics.off()  # Clears plots, closes all graphics devices
# Clear console
cat("\014")  # Mimics ctrl+L
# Clear data
rm(list = ls())  # Removes all objects from environment
