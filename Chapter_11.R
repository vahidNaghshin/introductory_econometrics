library(tidyverse)
library('wooldridge')
library(ggplot2)

#C1
df <- hseinv
help("hseinv")

#i
(cor(tail(df, 41)$linvpc, tail(df, 41)$linvpc_1))

linear_model <- lm(linvpc~t, data=df)
linear_model_1 <- lm(linvpc_1~t, data=df)

(cor(resid(linear_model)[2:42], resid(linear_model_1)))


(cor(tail(df, 41)$lprice, tail(df, 41)$lprice_1))

linear_model <- lm(lprice~t, data=df)
linear_model_1 <- lm(lprice_1~t, data=df)

(cor(resid(linear_model)[2:42], resid(linear_model_1)))

#since the first order autocorrelation for lprice is above 0.8, it most likely 
#belong to unit root process

#ii
linear_model <- lm(linvpc~gprice+t, data=df)
(out <- summary(linear_model))

#iii
linear_model <- lm(linvpc~t, data=df)
df$dtrended_linvpc <- resid(linear_model)
linear_model <- lm(dtrended_linvpc~gprice, data=df)
(out <- summary(linear_model))

#iv
linear_model <- lm(gprice~t, data=df)
(out <- summary(linear_model))

#C2
df <- earns
help('earns')

#i
linear_model <- lm(ghrwage~goutphr+goutph_1, data=df)
(out <- summary(linear_model))

#ii
linear_model <- lm(ghrwage~goutphr+I(goutphr+goutph_1), data=df)
(out <- summary(linear_model))
#as can be seen it is not SS

#iii
linear_model <- lm(ghrwage ~ goutphr + goutph_1 + goutph_2, data=df)
(out <- summary(linear_model))

#C3
df <- nyse
help('nyse')

#i
linear_model <- lm(return~return_1+I(return_1^2), data = df)
(out <- summary(linear_model))

#ii
#by looking at the f-stats we can see that both coefficients are not SS

#iii
library(data.table)
df$return_2 <- shift(df$return_1, n=1, fill=NA, type="lag")

linear_model <- lm(return~return_1+I(return_1*return_2), data = df)
(out <- summary(linear_model))

#Based on efficient market, return at time t should be predictable by lagged return
#based on estimation it is not SS at 5% (but SS at 10%)

#C4
df <- phillips
help("phillips")

#i
linear_model <- lm(cinf~cunem, data=df)
(out <- summary(linear_model))

#C5
df <- fertil3
help("fertil3")

#i
linear_model <- lm(cgfr~cpe+cpe_1+cpe_2+t, data=df)
(out<-summary(linear_model))

#ii
model_ur <- lm(cgfr~cpe+cpe_1+cpe_2+ww2+pill, data=df)
(out_ur <- summary(model_ur))

model_r <- lm(cgfr~cpe+cpe_1+cpe_2, data=df)
(out_r <- summary(model_r))

#based on f-stat, two lags are not SS
(F_statistcs <- ((out_ur$r.squared - out_r$r.squared)/(1-out_ur$r.squared) *(63/2)))

#iii
linear_model <- lm(cgfr~cpe+cpe_1+cpe_2+ww2+pill+t, data=df)
(out<-summary(linear_model))

#iv
linear_model <- lm(cgfr~cpe+I(cpe_1-cpe)+I(cpe_2-cpe)+ww2+pill, data=df)
(out<-summary(linear_model))

#C6
df <- inven
help("inven")

#i
linear_model <- lm(cinven~cgdp, data=df)
(out <- summary(linear_model))

#ii
linear_model <- lm(cinven~cgdp+r3, data=df)
(out <- summary(linear_model))

#iii
#it seems r3 wroks better than cr3 as its p-value is lower
linear_model <- lm(cinven~cgdp+cr3, data=df)
(out <- summary(linear_model))

#C7
df <- consump
help("consump")

#i
linear_model <- lm(gc~gc_1, data=df)
(out <- summary(linear_model))

#ii

df$i3_1 <- shift(df$i3, n=1, fill=NA, type="lag")
df$inf_1 <- shift(df$inf, n=1, fill=NA, type="lag")

model_ur <- lm(gc~gc_1+gy_1+i3_1+inf_1, data=df)
(out_ur <- summary(model_ur))

model_r <- lm(gc~gc_1, data=df)
(out_r <- summary(model_r))

#based on f-stat, they are not SS
(F_statistcs <- ((out_ur$r.squared - out_r$r.squared)/(1-out_ur$r.squared) *(33/3)))

#C8
df <- phillips
help('phillips')

#i
linear_model <- lm(unem~unem_1, data=df)
(out <- summary(linear_model))

#ii
linear_model <- lm(unem~unem_1+inf_1, data=df)
(out <- summary(linear_model))

#C9

df <- traffic2
help('traffic2')

#i
cor(df$prcfat[2:nrow(df)], df$prcfat_1[2:nrow(df)])
df$unem_1 <- shift(df$unem, n=1, fill=NA, type="lag")
cor(df$unem[2:nrow(df)], df$unem_1[2:nrow(df)])

#ii
df$cunem <- df$unem - df$unem_1
df$cprcfat <- df$prcfat - df$prcfat_1

linear_model <- lm(cprcfat~t+feb+mar+apr+may+jun+jul+aug+sep+oct+nov+dec
                   + wkends+cunem+spdlaw+beltlaw, data = df)
(out <- summary(linear_model))

#C10
df <- phillips
help("phillips")

#i
linear_model <- lm(cinf~unem, data = df)
(out <- summary(linear_model))

#ii
(new_natural_rate <- 2.8282/0.5176)

#iii
cor(df$unem[2:nrow(df)], df$unem_1[2:nrow(df)])

#iv
linear_model <- lm(cinf~cunem, data = df)
(out <- summary(linear_model))

#C11
df <- okun
help("okun")

#i
linear_model <- lm(pcrgdp~cunem, data=df)
(out<-summary(linear_model))

#ii
(t_stat <- (-1.8909+2)/0.1820)

#iii
(t_stat <- (3.3444-3)/0.1627)

#iv
#we failed to reject the null hypothesis
linear_model <- lm(I(pcrgdp - 3 + 2*cunem)~cunem, data=df)
(out<-summary(linear_model))

#C12
df <- minwage
help("minwage")

#i
df$gwage232_1 <- shift(df$gwage232, n=1, fill=NA, type="lag")
(cor(df$gwage232[3:nrow(df)], df$gwage232_1[3:nrow(df)]))

#ii
linear_model <- lm(gwage232~gwage232_1+gmwage+gcpi, data=df)
(out <- summary(linear_model))

#iii
df$gemp232_1 <- shift(df$gemp232, n=1, fill=NA, type="lag")
linear_model <- lm(gwage232~gwage232_1+gmwage+gcpi+gemp232_1, data=df)
(out <- summary(linear_model))

#iv
linear_model <- lm(gwage232~gmwage+gcpi, data=df)
(out <- summary(linear_model))

#v
linear_model <- lm(gmwage~gwage232_1+gemp232_1, data=df)
(out <- summary(linear_model))

#C13
df<-beveridge
help("beveridge")
#i
(cor(df$urate[2:nrow(df)], df$urate_1[2:nrow(df)]))

#ii
(cor(df$vrate[2:nrow(df)], df$vrate_1[2:nrow(df)]))

#iii
linear_model <- lm(urate~vrate, data = df)
(out <- summary(linear_model))

#iv
linear_model <- lm(I(urate-urate_1)~I(vrate-vrate_1), data = df)
(out <- summary(linear_model))


#C14
df <- approval
help("approval")

#i
df$lrgasprice_1 <-  shift(df$lrgasprice, n=1, fill=NA, type="lag")
(cor(df$lrgasprice_1[2:nrow(df)], df$lrgasprice[2:nrow(df)]))

df$approve_1 <-  shift(df$approve, n=1, fill=NA, type="lag")
(cor(df$approve_1[2:nrow(df)], df$approve[2:nrow(df)]))

#ii
linear_model <- lm(approve~lcpifood+lrgasprice+unemploy+X11.Sep+iraqinvade,data=df)
(out <- summary(linear_model))

#iii
df$approve_1 <- shift(df$approve, n=1, fill=NA, type="lag")
df$lcpifood_1 <- shift(df$lcpifood, n=1, fill=NA, type="lag")
df$lrgasprice_1 <- shift(df$lrgasprice, n=1, fill=NA, type="lag")
df$unemploy_1 <- shift(df$unemploy, n=1, fill=NA, type="lag")
df$X11.Sep_1 <- shift(df$X11.Sep, n=1, fill=NA, type="lag")
df$iraqinvade_1 <- shift(df$iraqinvade, n=1, fill=NA, type="lag")

linear_model <- lm(I(approve-approve_1)~I(lcpifood-lcpifood_1)
                   +I(lrgasprice-lrgasprice_1)
                   +I(unemploy-unemploy_1)
                   +I(X11.Sep-X11.Sep_1)
                   +I(iraqinvade-iraqinvade_1),data=df)
(out <- summary(linear_model))

#v
df$lsp500_1 <- shift(df$lsp500, n=1, fill=NA, type="lag")
linear_model <- lm(approve~lcpifood+lrgasprice+unemploy
                   +X11.Sep+iraqinvade+I(lsp500-lsp500),data=df)
(out <- summary(linear_model))


#Clear plots
graphics.off()  # Clears plots, closes all graphics devices
# Clear console
cat("\014")  # Mimics ctrl+L
# Clear data
rm(list = ls())  # Removes all objects from environment
