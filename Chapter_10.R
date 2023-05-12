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
df <- intdef
help("intdef")

df$y79 <- ifelse(df$year > 1979, 1, 0)
linear_model <- lm(i3 ~ inf + def + y79, data=df)
(out <- summary(linear_model))

#C2
df <- barium
help("barium")
#i
#No other variables, other than the trend, is statistically significant
linear_model <- lm(lchnimp~lchempi+lgas+lrtwex+befile6+affile6+afdec6+t, data=df)
(out <- summary(linear_model))

#ii
model_ur <- lm(lchnimp~lchempi+lgas+lrtwex+befile6+affile6+afdec6+t, data=df)
(out_ur <- summary(model_ur))

model_r <- lm(lchnimp~lchempi+lgas+lrtwex+befile6+affile6+afdec6, data=df)
(out_r <- summary(model_r))

#based on f-stat, the other variables are SS
(F_statistcs <- ((out_ur$r.squared - out_r$r.squared)/(1-out_ur$r.squared) *(123/1)))

#iii
linear_model <- lm(lchnimp~lchempi+lgas+lrtwex+befile6+affile6+afdec6+t
                   +feb+mar+apr+may+jun+jul+aug+sep+oct+nov+dec, data=df)
(out <- summary(linear_model))


#C3
df <- prminwge
help("prminwge")

linear_model <- lm(lprepop~lmincov+lusgnp+t+prgnp, data=df)
(out <- summary(linear_model))

#C4
df <- fertil3
help("fertil3")
linear_mode <- lm(gfr~pe+I(pe_1-pe)+I(pe_2-pe)+ww2+pill, data=df)
(out <- summary(linear_mode))


#C5
df <- ezanders
help("ezanders")

#i
linear_mode <- lm(luclms~year+feb+mar+apr+may+jun+jul+aug+sep+oct+nov+dec, data=df)
(out <- summary(linear_mode))

#ii
linear_mode <- lm(luclms~ez+year+feb+mar+apr+may+jun+jul+aug+sep+oct+nov+dec, data=df)
(out <- summary(linear_mode))

#C6
df <- fertil3
help("fertil3")

#i
linear_mode <- lm(gfr~t+tsq, data=df)
df$res <- resid(linear_mode)

#ii
linear_mode <- lm(res~pe+ww2+pill+t+tsq, data=df)
(out <- summary(linear_mode))

#iii
linear_mode <- lm(res~pe+ww2+pill+t+tsq+tcu, data=df)
(out <- summary(linear_mode))

#C7
df <- consump
help("consump")

#i
linear_mode <- lm(gc~gy, data=df)
(out <- summary(linear_mode))

#ii
linear_mode <- lm(gc~gy+gc_1, data=df)
(out <- summary(linear_mode))

#iii
linear_mode <- lm(gc~gy+gc_1+i3, data=df)
(out <- summary(linear_mode))

#C8
df <- fertil3
help("fertil3")

#i

model_ur <- lm(gfr~pe+pe_1+pe_2+ww2+pill+pe_3+pe_4, data=df)
(out_ur <- summary(model_ur))

model_r <- lm(gfr~pe+pe_1+pe_2+ww2+pill, data=df)
(out_r <- summary(model_r))

#based on f-stat, two lags are not SS
(F_statistcs <- ((out_ur$r.squared - out_r$r.squared)/(1-out_ur$r.squared) *(60/2)))

#ii
linear_mode <- lm(gfr~ww2+pe+I(pe_1-pe)+I(pe_2-pe)+pill+I(pe_3-pe)+I(pe_4-pe), data=df)
(out_ur <- summary(model_ur))

#C9
df <- volat
help("volat")
#i

linear_mode <- lm(rsp500~pcip+i3, data=df)
(out <- summary(linear_mode))

#C10
df <- intdef
help('intdef')

#i
(cor(df$inf, df$def))

#ii
linear_mode <- lm(i3~inf+def+inf_1+def_1, data=df)
(out<- summary(linear_mode))

#iii
LRP <- 0.3426+0.3820

#iv

model_ur <- lm(i3~inf+def+inf_1+def_1, data=df)
(out_ur <- summary(model_ur))

model_r <- lm(i3~inf+def, data=df)
(out_r <- summary(model_r))

#based on f-stat, two lags are SS (p-value=0.00290)
(F_statistcs <- ((out_ur$r.squared - out_r$r.squared)/(1-out_ur$r.squared) *(50/2)))

#C11
df <- traffic2
help("traffic2")
#i
df[which(df$beltlaw == 1)[1],]; df[which(df$spdlaw == 1)[1],]

#ii
linear_model <- lm(ltotacc~t+feb+mar+apr+may+jun+jul+aug+sep+oct+nov+dec, data=df)
(out <- summary(linear_model))

#iii

linear_model <- lm(ltotacc~t+feb+mar+apr+may+jun+jul+aug+sep+oct+nov+dec
                   + wkends+unem+spdlaw+beltlaw, data=df)
(out <- summary(linear_model))

#v
mean(df$prcfat)

#vi
linear_model <- lm(prcfat~t+feb+mar+apr+may+jun+jul+aug+sep+oct+nov+dec
                   + wkends+unem+spdlaw+beltlaw, data=df)
(out <- summary(linear_model))

#C12
df <- phillips
help("phillips")
(nrow(df))
#i
linear_mode <- lm(inf~unem, data=df)
(out <- summary(linear_mode))

#C13
df <- minwage
help("minwage")

#i
linear_mode <- lm(gwage232~gmwage+gcpi, data=df)
(out <- summary(linear_mode))

#ii
linear_mode <- lm(gwage232~gmwage+gcpi+
                    gmwage_1+gmwage_2+gmwage_3+gmwage_4+
                    gmwage_5+gmwage_6+gmwage_7+gmwage_8+
                    gmwage_9+gmwage_10+gmwage_11+gmwage_12, data=df)
(out <- summary(linear_mode))

#iii
linear_mode <- lm(gemp232~gmwage+gcpi, data=df)
(out <- summary(linear_mode))

#iv
linear_mode <- lm(gemp232~gmwage+gcpi+
                    gmwage_1+gmwage_2+gmwage_3+gmwage_4+
                    gmwage_5+gmwage_6+gmwage_7+gmwage_8+
                    gmwage_9+gmwage_10+gmwage_11+gmwage_12, data=df)
(out <- summary(linear_mode))

#C14
df <- approval
help("approval")

#i
min(df$approve); max(df$approve); mean(df$approve)

#ii
linear_mode <- lm(approve~lcpifood+lrgasprice+unemploy, data=df)
(out <- summary(linear_mode))

#iv
linear_mode <- lm(approve~lcpifood+lrgasprice+unemploy+iraqinvade+sep11, data=df)
(out <- summary(linear_mode))

#vi
linear_mode <- lm(approve~lcpifood+lrgasprice+unemploy+iraqinvade+sep11+sp500, data=df)
(out <- summary(linear_mode))


#Clear plots
graphics.off()  # Clears plots, closes all graphics devices
# Clear console
cat("\014")  # Mimics ctrl+L
# Clear data
rm(list = ls())  # Removes all objects from environment
