library(tidyverse)
library('wooldridge')
# Load the sandwich package
library(sandwich)



#C1
df <- fertil1
help('fertil1')

#i
linear_model_ur <- lm(kids ~ educ + age + I(age^2) + black + 
                             east + northcen + west + farm + othrural + 
                             town + smcity + y74 + y76 + y78 + 
                             y80 + y82 + y84, data=fertil1)
(out_ur <- summary(linear_model_ur))

linear_model_r <- lm(kids ~ educ + age + I(age^2) + black +
                       y74 + y76 + y78 + 
                       y80 + y82 + y84, data=fertil1)
(out_r <- summary(linear_model_r))

#both variables are jointly significant
(F_statistcs <- ((out_ur$r.squared - out_r$r.squared)/(1-out_ur$r.squared) *(1122/7)))
#based on F-stat the p-value and is 
(p_value <- 1 - pf(F_statistcs, 7, 1122))

#ii
linear_model_ur <- lm(kids ~ educ + age + I(age^2) + black + 
                        east + northcen + west + farm + othrural + 
                        town + smcity + y74 + y76 + y78 + 
                        y80 + y82 + y84, data=fertil1)
(out_ur <- summary(linear_model_ur))

linear_model_r <- lm(kids ~ educ + age + I(age^2) + black + 
                       farm + othrural + 
                       town + smcity + y74 + y76 + y78 + 
                       y80 + y82 + y84, data=fertil1)
(out_r <- summary(linear_model_r))

#both variables are NOT jointly significant
(F_statistcs <- ((out_ur$r.squared - out_r$r.squared)/(1-out_ur$r.squared) *(1122/7)))
#based on F-stat the p-value and is 
(p_value <- 1 - pf(F_statistcs, 7, 1122))

#iii
df$resid_sq <- resid(linear_model_ur)^2

linear_model <- lm(resid_sq ~ y74 + y76 + y78 + 
                       y80 + y82 + y84, data=df)
(out <- summary(linear_model))

#iv
linear_model_ur <- lm(kids ~ educ + age + I(age^2) + black + 
                        east + northcen + west + farm + othrural + 
                        town + smcity + y74 + y76 + y78 + 
                        y80 + y82 + y84 + 
                        I(y74*educ) + I(y76*educ) + I(y78*educ) + 
                        I(y80*educ) + I(y82*educ) + I(y84*educ), data=fertil1)
(out_ur <- summary(linear_model_ur))

linear_model_r <- lm(kids ~ educ + age + I(age^2) + black + 
                       east + northcen + west + farm + othrural + 
                       town + smcity + y74 + y76 + y78 + 
                       y80 + y82 + y84, data=fertil1)
(out_r <- summary(linear_model_r))

#both variables are NOT jointly significant
(F_statistcs <- ((out_ur$r.squared - out_r$r.squared)/(1-out_ur$r.squared) *(1122/6)))
#based on F-stat the p-value is 
(p_value <- 1 - pf(F_statistcs, 7, 1122))

#C2
df <- cps78_85
help("cps78_85")

#ii
linear_model <- lm(lwage ~ y85 + educ + I(y85*(educ-12)) + I(exper) +
                     I(exper^2) + union + female + I(y85*female), data=df)
(out <- summary(linear_model))

(conf_int <- c(out$coefficients[2,1]-2*out$coefficients[2,2], 
               out$coefficients[2,1]+2*out$coefficients[2,2]))

#iii
df$lrwage <- ifelse(df$year==78, df$lwage, df$lwage-log(1.65))

linear_model <- lm(lwage ~ y85 + educ + I(y85*educ) + I(exper) +
                     I(exper^2) + union + female + I(y85*female), data=df)
(out <- summary(linear_model))

linear_model <- lm(lrwage ~ y85 + educ + I(y85*educ) + I(exper) +
                     I(exper^2) + union + female + I(y85*female), data=df)
(out <- summary(linear_model))

#v
linear_model <- lm(lwage ~ y85 + educ + I(y85*educ) + I(exper) +
                     I(exper^2) + union + y85union + female + I(y85*female), data=df)
(out <- summary(linear_model))

#vi
#no confliction is observed

#C3
df <- kielmc
help("kielmc")

#i
#sign of delta_1 should be negative if building incinerator has a determintal effect
#on price of home. When the sing of beta_1 is positive, it means that the price of
#home is increasing as homes get farther away from incinerator

#ii
linear_model <- lm(lprice ~ y81 + ldist + y81ldist, data=df)
(out <- summary(linear_model))

#iii
linear_model <- lm(lprice ~ y81 + ldist + y81ldist + age + agesq + 
                     rooms + baths + lintst + lland + larea, data=df)
(out <- summary(linear_model))

#C4
df <- injury
help("injury")

#i
linear_model <- lm(ldurat ~ afchnge + highearn + I(afchnge*highearn)
                   + male + married + indust + injtype, data=filter(df, ky==1))
(out <- summary(linear_model))

#iii
#i
linear_model <- lm(ldurat ~ afchnge + highearn + I(afchnge*highearn)
                   + male + married + indust + injtype, data=filter(df, mi==1))
(out <- summary(linear_model))

#C5
df <- rental
help("rental")

#i
linear_model <- lm(lrent ~ y90 + lpop + lavginc + pctstu, data=df)
(out <- summary(linear_model))

#ii
linear_model <- lm(clrent ~ clpop + clavginc + cpctstu, data=df)
(out <- summary(linear_model))

#iii
# Compute heteroskedasticity-robust standard errors
(robust_se <- sqrt(diag(vcovHC(linear_model, type = "HC1"))))

#C6
df <- crime3
help('crime3')

#i
linear_model <- lm(lcrime ~ d78 + clrprc1 + I(clrprc1+clrprc2), data=df)
(out <- summary(linear_model))

#iii

linear_model <- lm(clcrime ~  cavgclr, data=df)
(out <- summary(linear_model))

#C7
df <- gpa3
help("gpa3")

#i
linear_model <- lm(trmgpa ~  spring + sat + hsperc + female + black + white
                   + frstsem + tothrs + crsgpa + season, data=df)
(out <- summary(linear_model))

#iii
linear_model <- lm(ctrmgpa ~  ctothrs + ccrsgpa + cseason, data=df)
(out <- summary(linear_model))

#C8
df <- vote2
help("vote2")

#i
linear_model <- lm(cvote ~ clinexp + clchexp + cincshr, data=df)
(out <- summary(linear_model))

#ii
linear_model_ur <- lm(cvote ~ clinexp + clchexp + cincshr, data=df)
(out_ur <- summary(linear_model_ur))

linear_model_r <- lm(cvote ~ cincshr, data=df)
(out_r <- summary(linear_model_r))

#both variables are NOT jointly significant
(F_statistcs <- ((out_ur$r.squared - out_r$r.squared)/(1-out_ur$r.squared) *(153/2)))
#based on F-stat the p-value is 
(p_value <- 1 - pf(F_statistcs, 2, 153))

#iii
linear_model <- lm(cvote ~ cincshr, data=df)
(out <- summary(linear_model))

#iv
linear_model <- lm(cvote ~ cincshr, data=filter(df, rptchall==1))
(out <- summary(linear_model))

#C9
df <- crime4
help("crime4")

#i
linear_model <- lm(lcrmrte ~ d83 + d84 + d85 + d86 + d87 
                   + clprbarr + clprbpri + clprbcon + clavgsen + clpolpc
                   + wcon + wtuc + wtrd + wfir + wser + wmfg + wfed
                   + wsta + wloc + mix, data=df)
(out <- summary(linear_model))

#ii
linear_model_ur <- lm(lcrmrte ~ d83 + d84 + d85 + d86 + d87 
                      + clprbarr + clprbpri + clprbcon + clavgsen + clpolpc
                      + wcon + wtuc + wtrd + wfir + wser + wmfg + wfed
                      + wsta + wloc + mix, data=df)
(out_ur <- summary(linear_model_ur))

linear_model_r <- lm(lcrmrte ~ d83 + d84 + d85 + d86 + d87 
                     + clprbarr + clprbpri + clprbcon + clavgsen + clpolpc
                     , data=df)
(out_r <- summary(linear_model_r))

#both variables are NOT jointly significant
(F_statistcs <- ((out_ur$r.squared - out_r$r.squared)/(1-out_ur$r.squared) *(519/10)))
#based on F-stat the p-value is 
(p_value <- 1 - pf(F_statistcs, 10, 519))

#C10
df <- jtrain
help("jtrain")

#i

linear_model <- lm(chrsemp~d88+d89+cgrant+clemploy, data=df)
(out <- summary(linear_model))

#C11
df <- mathpnl
help("mathpnl")

#ii
linear_model <- lm(cmath4~y93+y94+y95+y96+y97+y98+lrexpp+genrol+clunch, data=df)
(out <- summary(linear_model))

#iii
linear_model <- lm(cmath4~y93+y94+y95+y96+y97+y98+lrexpp+lrexpp_1+genrol+clunch, data=df)
(out <- summary(linear_model))

#iv
# Compute heteroskedasticity-robust standard errors
(robust_se <- sqrt(diag(vcovHC(linear_model, type = "HC1"))))

#v
# Calculate HAC standard errors
(hac_se <- sqrt(diag(vcovHAC(linear_model))))

#vi

# Get the index of rows with non-NA values
non_na_rows <- complete.cases(df[, c('y93', 'y94','y95','y96','y97','y98',
                                     'lrexpp','lrexpp_1','genrol','clunch')])


df[non_na_rows, ]$res <- resid(linear_model)
df[non_na_rows, ]$res_1 <- c(NA, df[non_na_rows, ]$res[-nrow(df[non_na_rows, ])])

linear_model <- lm(res~res_1, data=df)

#C12
df <- murder
help("murder")

#i
linear_model <- lm(mrdrte~d93+exec+unem, data=filter(df, year!=87))
(out <- summary(linear_model))

#ii
linear_model <- lm(cmrdrte ~ cexec + cunem, data=filter(df, year==93))
(out <- summary(linear_model))

#iii
#run PG test for heteroskedasticity
df_93 <- filter(df, year==93)
df_93$res_sq <- resid(linear_model)^2

linear_model <- lm(res_sq ~ cexec + cunem, data=df_93)
(out <- summary(linear_model))
#based on F-stat it is not SS, so we failed to reject the homoskedasticy

#iv
linear_model <- lm(cmrdrte ~ cexec + cunem, data=filter(df, year==93))
(out <- summary(linear_model))
(robust_se <- sqrt(diag(vcovHC(linear_model, type = "HC1"))))

#C13
df <- wagepan
help("wagepan")

#ii
# Calculate the differenced values for each n-row chunk independently

df$d81_educ <- df$d81 * df$educ
df$d82_educ <- df$d82 * df$educ
df$d83_educ <- df$d83 * df$educ
df$d84_educ <- df$d84 * df$educ
df$d85_educ <- df$d85 * df$educ
df$d86_educ <- df$d86 * df$educ
df$d87_educ <- df$d87 * df$educ


n <- 8
df_diff <- df %>%
  mutate(Group = ceiling(row_number() / n)) %>%
  group_by(Group) %>%
  mutate(clwage = lwage - lag(lwage, default = first(lwage)), 
         cd81 = d81 - lag(d81, default = first(d81)),
         cd82 = d82 - lag(d82, default = first(d82)),
         cd83 = d83 - lag(d83, default = first(d83)),
         cd84 = d84 - lag(d84, default = first(d84)),
         cd85 = d85 - lag(d85, default = first(d85)),
         cd86 = d86 - lag(d86, default = first(d86)),
         cd87 = d87 - lag(d87, default = first(d87)),
         cd81_educ = d81_educ - lag(d81_educ, default = first(d81_educ)),
         cd82_educ = d82_educ - lag(d82_educ, default = first(d82_educ)),
         cd83_educ = d83_educ - lag(d83_educ, default = first(d83_educ)),
         cd84_educ = d84_educ - lag(d84_educ, default = first(d84_educ)),
         cd85_educ = d85_educ - lag(d85_educ, default = first(d85_educ)),
         cd86_educ = d86_educ - lag(d86_educ, default = first(d86_educ)),
         cd87_educ = d87_educ - lag(d87_educ, default = first(d87_educ)),
         cunion = union - lag(union, default = first(union)),) %>%
  ungroup()

linear_model_ur <- lm(clwage ~ cd81+cd82+cd83+cd84+cd85+cd86+cd87+
                        cd81_educ+cd82_educ+cd83_educ+cd84_educ+cd85_educ
                      +cd86_educ+cd87_educ, data=df_diff)
(out_ur <- summary(linear_model_ur))

linear_model_r <- lm(clwage ~ cd81+cd82+cd83+cd84+cd85+cd86+cd87, data=df_diff)
(out_r <- summary(linear_model_r))

#both variables are NOT jointly significant
(F_statistcs <- ((out_ur$r.squared - out_r$r.squared)/(1-out_ur$r.squared) *(4345/7)))
#based on F-stat the p-value is 
(p_value <- 1 - pf(F_statistcs, 7, 4345))

#C14
df <- jtrain3
help("jtrain3")

#i
linear_model <- lm(re78~train, data=df)
(out <- summary(linear_model))

#ii
df$cre <- df$re78 - df$re75
linear_model <- lm(cre~train, data=df)
(out <- summary(linear_model))

#iii
(conf_int <- c(out$coefficients[2,1]-2*out$coefficients[2,2], 
               out$coefficients[2,1]+2*out$coefficients[2,2]))


#C15
df<- happiness
help("happiness")

#i
(group_counts <- df %>% count(year))
(max_row_index <- which.max(group_counts$n))
(min_row_index <- which.min(group_counts$n))
group_counts[max_row_index,]
group_counts[min_row_index,]

(nrow(filter(df, vhappy==1))/nrow(df))

#ii
linear_model <- lm(vhappy~y96+y98+y00+y02+y04+y06, data=df)
(out <- summary(linear_model))

library(sandwich)
library(lmtest)
library(car)

# Fit your model and obtain the F-statistic
linear_model <- lm(vhappy~y96+y98+y00+y02+y04+y06, data=df)
(out <- summary(linear_model))


myH0 <- c("y96", "y98", "y00", "y02", "y04", "y06")
(f_statistic <- linearHypothesis(linear_model, myH0))

# Calculate the Heteroskedasticity-Robust F-statistic
(robust_f_statistic <- linearHypothesis(linear_model, myH0, vcoc=hccm(linear_model, type = 'hc0')))

#iii
linear_model <- lm(vhappy~y96+y98+y00+y02+y04+y06+occattend+regattend, data=df)
(out <- summary(linear_model))

#iv
df$highinc <- ifelse(df$income == '$25000 or more', 1, 0)
linear_model <- lm(vhappy~y96+y98+y00+y02+y04+y06+occattend+regattend
                   +unem10+ educ+teens, data=df)
(out <- summary(linear_model))


#C16
df <- countymurders
help("countymurders")

#i
mean(df$murdrate); sd(df$murdrate); 
mean(df$murdrate == 0) * 100

#ii
sum(df$execs==0)
max(df$execs)

#iii
df$y80 <- ifelse(df$year==1980, 1, 0)
df$y81 <- ifelse(df$year==1981, 1, 0)
df$y82 <- ifelse(df$year==1982, 1, 0)
df$y83 <- ifelse(df$year==1983, 1, 0)
df$y84 <- ifelse(df$year==1984, 1, 0)
df$y85 <- ifelse(df$year==1985, 1, 0)
df$y86 <- ifelse(df$year==1986, 1, 0)
df$y87 <- ifelse(df$year==1987, 1, 0)
df$y88 <- ifelse(df$year==1988, 1, 0)
df$y89 <- ifelse(df$year==1989, 1, 0)
df$y90 <- ifelse(df$year==1990, 1, 0)
df$y91 <- ifelse(df$year==1991, 1, 0)
df$y92 <- ifelse(df$year==1992, 1, 0)
df$y93 <- ifelse(df$year==1993, 1, 0)
df$y94 <- ifelse(df$year==1994, 1, 0)
df$y95 <- ifelse(df$year==1995, 1, 0)
df$y96 <- ifelse(df$year==1996, 1, 0)


n <- 17
df_diff <- df %>%
  mutate(Group = ceiling(row_number() / n)) %>%
  group_by(Group) %>%
  mutate(execs_1 = lag(execs, default = first(execs)),) %>%
  ungroup()

linear_model <- lm(murdrate~y80+y81+y82+y83+y84+y85+y86+y87
                   +y88+y89+y90+y91+y92+y93+y94+y95+y96
                   +execs+execs_1+percblack+percmale+perc1019+perc2029, data=df_diff)

(out <-summary(linear_model))

#vi
df_diff <- df %>%
  mutate(Group = ceiling(row_number() / n)) %>%
  group_by(Group) %>%
  mutate(cexecs = execs - lag(execs, default = first(execs)),
         cmurdrate = murdrate - lag(murdrate, default = first(murdrate))) %>%
  ungroup()

linear_model <- lm(cmurdrate~y80+y81+y82+y83+y84+y85+y86+y87
                   +y88+y89+y90+y91+y92+y93+y94+y95+y96
                   +cexecs+percblack+percmale+perc1019+perc2029, data=df_diff)

(out <-summary(linear_model))


#Clear plots
graphics.off()  # Clears plots, closes all graphics devices
# Clear console
cat("\014")  # Mimics ctrl+L
# Clear data
rm(list = ls())  # Removes all objects from environment
