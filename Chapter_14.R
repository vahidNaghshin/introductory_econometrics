library(tidyverse)
library('wooldridge')

#C1
df <- rental
help("rental")
#i
linear_model <- lm(lrent ~ y90+lpop+lavginc+pctstu, data=df)
(out <- summary(linear_model))
#ii
#no as we have not taken constant effect ai into account
#iii
linear_model <- lm(clrent ~ clpop+clavginc+cpctstu, data=df)
(out <- summary(linear_model))
#iv
install.packages("plm")
library(plm)
df$individual_id <- df$city
df$time <- df$year
fixed_model <- plm(lrent ~ y90+lpop+lavginc+pctstu, data = df, model = "within")
(out <- summary(fixed_model))

#C2
df <- crime4
help("crime4")
#i
df$individual_id <- df$county
df$time <- df$year
fixed_model <- plm(lcrmrte ~ d83+d84+d85+d86+d87+lprbarr+lprbconv+
                     lprbpris+lavgsen+lpolpc
                   , data = df, model = "within")
(out <- summary(fixed_model))

#ii
df$time <- df$year
fixed_model <- plm(lcrmrte ~ d83+d84+d85+d86+d87+lprbarr+lprbconv+
                     lprbpris+lavgsen+lpolpc+
                     lwcon+lwtuc+lwtrd+lwfir+lwser+lwmfg+lwfed+lwsta+lwloc
                   , data = df, model = "within")
(out <- summary(fixed_model))

#iii
model_ur <- plm(lcrmrte ~ d83+d84+d85+d86+d87+lprbarr+lprbconv+
                     lprbpris+lavgsen+lpolpc+
                     lwcon+lwtuc+lwtrd+lwfir+lwser+lwmfg+lwfed+lwsta+lwloc
                   , data = df, model = "within")
(out_ur <- summary(model_ur))

model_r <- plm(lcrmrte ~ d83+d84+d85+d86+d87+lprbarr+lprbconv+
                  lprbpris+lavgsen+lpolpc
                , data = df, model = "within")
(out_r <- summary(model_r))
#based on f-stat, two lags are  SS
(F_statistcs <- ((out_ur$r.squared - out_r$r.squared)/(1-out_ur$r.squared) *(530/9)))
#based on F-stat the p-value and is 
(p_value <- 1 - pf(F_statistcs[1], 9, 530))

#C3
df <- jtrain
help("jtrain")

#i
# Remove rows with null values in "column1"
df <- df[complete.cases(df$column1), ]
# Group by 'group' and count the elements
(result <- df[complete.cases(df$grant, df$grant_1, df$lemploy), ] %>%
  group_by(fcode) %>%
  summarize(count = n()))
#number of firms
nrow(filter(result, count>=2))
# Get the number of unique values in "fcode"
(unique_count <- length(unique(df$fcode)))

#fixed effect model
df$individual_id <- df$fcode
df$time <- df$year
fixed_model <- plm(hrsemp~d88+d89+grant+grant_1+lemploy
                   , data = df, model = "within")
(out <- summary(fixed_model))

#C4
df <- ezanders
help("ezanders")

#ii
# Calculate the difference between consecutive values in "column1"
df$cez <- c(NA, diff(df$ez))
linear_model <- lm(luclms~cez, data=df)
(out <- summary(linear_model))

#iii
df$cy81 <- c(NA, diff(df$y81))
df$cy82 <- c(NA, diff(df$y82))
df$cy83 <- c(NA, diff(df$y83))
df$cy84 <- c(NA, diff(df$y84))
df$cy85 <- c(NA, diff(df$y85))
df$cy86 <- c(NA, diff(df$y86))
df$cy87 <- c(NA, diff(df$y87))
df$cy88 <- c(NA, diff(df$y88))
linear_model <- lm(luclms~cez+
                     cy81+cy82+cy83+cy84+cy85+cy86+cy87+cy88, data=df)
(out <- summary(linear_model))

#C5
df <- wagepan
help("wagepan")
#i
df$individual_id <- df$nr
df$time <- df$year
fixed_model <- plm(lwage~exper+I(exper^2)+union+
                     occ2+occ3+occ4+occ5+occ6+occ7+occ8+occ9
                   , data = df, model = "within")
(out <- summary(fixed_model))

#C6
df$t_union <- df$union * df$year
fixed_model <- plm(lwage~exper+I(exper^2)+union+
                     occ2+occ3+occ4+occ5+occ6+occ7+occ8+occ9+t_union
                   , data = df, model = "within")
(out <- summary(fixed_model))

random_eff_model <- plm(lwage~exper+I(exper^2)+union+
                     occ2+occ3+occ4+occ5+occ6+occ7+occ8+occ9+t_union
                   , data = df, model = "random")
(out <- summary(random_eff_model))

#C7
df <- murder
help("murder")

#ii
linear_model <- lm(mrdrte~ d93+exec+unem, data=filter(df, (year==90) | (year==93)))
(out <- summary(linear_model))

#iii
linear_model <- lm(cmrdrte ~ cexec + cunem, data=filter(df, (year==90) | (year==93)))
(out <- summary(linear_model))

#iv
# Compute heteroskedasticity-robust standard errors
(robust_se <- sqrt(diag(vcovHC(linear_model, type = "HC1"))))

#v
# Subset the data frame for the year 1993
subset_df <- subset(df, year == 93)

# Find the row index with the maximum execution variable
max_index <- which.max(df$exec)

# Get the state with the maximum execution variable
state_with_max_execution <- df[max_index, "state"]

# Print the result
print(state_with_max_execution)

#for the next highest execution state
# Subset the data frame to include only the required columns
subset_df <- df[, c("state", "exec")]

# Sort the data frame by the execution variable in descending order
sorted_df <- df[order(df$exec, decreasing = TRUE), ]

# Get the state with the second-highest execution variable
state_with_second_highest_execution <- df[2, "state"]

# Print the result
print(state_with_second_highest_execution)

#how much higher
first_highest_exec = filter(df, state==state_with_max_execution & year==93)[, 'exec']
sec_highest_exec = filter(df, state==state_with_second_highest_execution & year==93)[, 'exec']
print(first_highest_exec-sec_highest_exec)

#vi
# Drop the rows with the specified state from the data frame
updated_df <- subset(df, state != 'TX')
linear_model <- lm(cmrdrte ~ cexec + cunem, data=filter(updated_df, (year==90) | (year==93)))
(out <- summary(linear_model))

#vii
# Compute the differencing after grouping by "state"
differenced_df <- df %>%
  group_by(state) %>%
  mutate(cd90 = c(rep(c(NA), times = 1),diff(d90))) %>%
  ungroup()
df$c90 <- differenced_df$cd90

differenced_df <- df %>%
  group_by(state) %>%
  mutate(cd93 = c(rep(c(NA), times = 1), diff(d93))) %>%
  ungroup()
df$c93 <- differenced_df$cd93


linear_model <- lm(cmrdrte ~ c90 + c93 + cexec + cunem, 
                   data=df)
(out <- summary(linear_model))

#C8











