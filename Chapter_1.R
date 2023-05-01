library('wooldridge')
library(help='wooldridge')
# problem 1
df <- wage1
# i
print(mean(df$educ))
print(median(df$educ))
print(max(df$educ))
print(min(df$educ))
# ii
print(mean(df$wage))
# v
print(sum(df$female)). #number of females
print(nrow(df) - sum(df$female)) #number of males

#problem 2
df <- bwght
#i
print(nrow(df))
print(sum(df$cigs > 0))

#ii
print(mean(df$cigs))
#iii
df_smoke = filter(df, cigs > 0)
print(mean(df_smoke$cigs))
#iv
print(mean(df_smoke$fatheduc))
df_fath = filter(df, !is.na(df$fatheduc))
print(mean(df_fath$fatheduc))
#v
print(mean(df_smoke$faminc))
print(sd(df_smoke$faminc))

#problem 3
df <- meap01
#i
print(min(df$math4))
print(max(df$math4))
#ii
df_pass = filter(df, df$math4==100)
nrow(df_pass)
nrow(df_pass)/nrow(df)
#iii
df_50pass = filter(df, df$math4==50)
nrow(df_50pass)
#iv
print(mean(df$math4))
print(mean(df$read4))
#v
cor(df$math4, df$read4)
#vi
print(mean(df$exppp))
print(sd(df$exppp))
#vii
print((6000-5500)*100/6000)
print(log(6000)-log(5500))


#problem 4
#i
df <- jtrain2
print(sum(df$train))
#ii
df_not_train = filter(df, df$train==0)
df_train = filter(df, df$train==1)
print(mean(df_train$re78))
print(mean(df_not_train$re78))
#iii
print(sum(df_train$unem78)/nrow(df_train))
#iv
print(sum(df_not_train$unem78)/nrow(df_not_train))

#problem 5
df <- fertil2
#i
print(mean(df$children))
print(max(df$children))
print(min(df$children))
#ii
print(nrow(filter(df, df$electric==1))/nrow(df))
#iii
df %>% filter(df$electric==1) %>% pull(children) %>% mean()
df %>% filter(df$electric==0) %>% pull(children) %>% mean()

#problem 6
df <- countymurders
df <- filter(df, df$year == 1996)
#i
lengths(list(unique(df$countyid)))

df %>% 
  filter(df$murders==0) %>% 
  pull(countyid) %>%
  unique() %>% 
  list() %>% 
  lengths()

zero_exec = df %>% 
  filter(df$execs==0) %>% 
  pull(countyid) %>%
  unique() %>% 
  list() %>% 
  lengths()

zero_exec / lengths(list(unique(df$countyid)))

#ii
max(df$murders)
min(df$murders)

max(df$execs)
min(df$execs)
mean(df$execs)

#iii
cor(df$execs, df$murders)

#problem 7
#i
df <- alcohol
abuse_num = df %>% filter(df$abuse==1) %>% nrow()
abuse_num / nrow(df)
unemp_num = df %>% filter(df$employ==1) %>% nrow()
unemp_num / nrow(df)
#ii
df_abuse = df %>% filter(df$abuse==1) 
sum(df_abuse$employ) / nrow(df_abuse)
#iii
df_no_abuse = df %>% filter(df$abuse==0) 
sum(df_no_abuse$employ) / nrow(df_no_abuse)

#problem 8
df <- econmath
nrow(df)
df %>% filter(df$econhs==1) %>% nrow()

df %>% filter(df$econhs==1) %>% pull(score) %>% mean()
df %>% filter(df$econhs==0) %>% pull(score) %>% mean()
