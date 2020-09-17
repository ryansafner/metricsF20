# 3 -------------------

# a ------------------

median(c(83,92,72,81))

# b -----------------

mean(c(83,92,72,81))

# c -----------------

sd(c(83,92,72,81))

# d -----------------

# load tidyverse (for tibble and ggplot2)
library(tidyverse)

# make a dataframe of our data, 
# called df
# one variable in it, called quiz

df <- tibble(quiz = c(83,92,72,81))

# use this as our data for plot
ggplot(data = df)+
  aes(x = quiz)+
  geom_histogram(breaks=seq(0,100,10), # make bins of size 10 between 0 and 100
                 color = "white", # color is for borders
                 fill = "blue")+ # fill is for area
  scale_x_continuous(breaks=seq(0,100,10))+ # have x axix ticks same as breaks 
  theme_classic()

# e -----------------

mean(c(83,92,0,81))
median(c(83,92,0,81))

# make new tibble called df_2
df_2 <- tibble(quiz = c(83,92,0,81)) # replace 72 with 0

# use this as our data for plot
ggplot(data = df_2)+
  aes(x = quiz)+
  geom_histogram(breaks=seq(0,100,10), # make bins of size 10 between 0 and 100
                 color = "white", # color is for borders
                 fill = "blue")+ # fill is for area
  geom_vline(aes(xintercept = median(quiz)), size = 1, color = "green", linetype = "dashed")+ # green dashed line is median
  geom_label(aes(x = median(quiz), y = 1), label = "Median", color = "green")+ # label median line
  geom_vline(aes(xintercept = mean(quiz)), size = 1, color = "red", linetype = "dotted")+ # green dashed line is mean
  geom_label(aes(x = mean(quiz), y = 1), label = "Mean", color = "red")+ # label mean line
  scale_x_continuous(breaks=seq(0,100,10))+ # have x axix ticks same as breaks 
  theme_classic()

# 4 -----------------

# c -----------------

# make a dataframe called "amazon" of # of books and associated probabilities
amazon<-tibble(books = c(0,1,2),
               prob = c(0.2,0.4,0.4))

# look at it 
amazon

# find expected value
amazon %>%
  summarize(exp_value = sum(books*prob)) 

# it's 1.2, let's save exp_value

exp_value <- 1.2

# make new columns: devs, devs_sq, p_weight_devs_sq
# save to new tibble
amazon_table<-amazon %>%
  mutate(devs = books - exp_value,
         devs_sq = devs^2,
         p_weight_devs_sq = prob*devs^2)

# look at the tibble
amazon_table

# now let's take these and summarize 
amazon_table %>%
  summarize(var = sum(p_weight_devs_sq), # variance
            sd = sqrt(var)) # sqrt to get sd, confirm its same!

# 5 -----------------

# made optional pdf graphs for each

# a -----------------

# see class 2.3 notes on how to graph and shade stats graphs

# it helps to first figure out where the x-axis ticks should be
# show about 4 standard deviations above and below the mean (mu +/- 4*sd)
# then have ticks in intervals of one sd

# in this case, with mean 500 and sd 100, it should be seq(100,900,100)

s_plot<-ggplot(data = tibble(scores=seq(from = 100,
                                        to = 900,
                                        by = 100)))+
  aes(x = scores)+
  stat_function(fun = dnorm,
                args = list(mean = 500, sd = 100),
                size = 2, color = "blue")+
  labs(x = "SAT Scores (out of 1600)",
       y = "Probability")+
  scale_x_continuous(breaks=seq(from  = 100,
                                to = 900,
                                by = 100))+
  theme_classic(base_family = "Fira Sans Condensed",
                base_size=20)

s_plot+stat_function(fun = dnorm,
                     args = list(mean = 500, sd = 100),
                     geom = "area",
                     xlim = c(400,600),
                     size = 2, fill = "blue", alpha = 0.5)

Z<-ggplot(data = tibble(Z=seq(from = -4,
                              to = 4,
                              by = 1)))+
  aes(x = Z)+
  stat_function(fun = dnorm,
                size = 2, color = "blue")+
  labs(x = "Z-Scores",
       y = "Probability")+
  scale_x_continuous(breaks=seq(from  = -4,
                                to = 4,
                                by = 1))+
  theme_classic(base_family = "Fira Sans Condensed",
                base_size=20)

Z+stat_function(fun = dnorm,
                geom = "area",
                xlim = c(-1,1),
                size = 2, fill = "blue", alpha = 0.5)

# b -----------------

s_plot+stat_function(fun = dnorm, args = list(mean = 500, sd = 100), geom = "area", xlim = c(300,700), size = 2, fill = "blue", alpha = 0.5)

Z+stat_function(fun = dnorm, geom = "area", xlim = c(-2,2), size = 2, fill = "blue", alpha = 0.5)

# c -----------------

s_plot+stat_function(fun = dnorm, args = list(mean = 500, sd = 100), geom = "area", xlim = c(700,900), size = 2, fill = "blue", alpha = 0.5)

Z+stat_function(fun = dnorm, geom = "area", xlim = c(2,4), size = 2, fill = "blue", alpha = 0.5)

# d -----------------

s_plot+stat_function(fun = dnorm, args = list(mean = 500, sd = 100), geom = "area", xlim = c(100,700), size = 2, fill = "blue", alpha = 0.5)

Z+stat_function(fun = dnorm, geom = "area", xlim = c(-4,2), size = 2, fill = "blue", alpha = 0.5)

# 6 -----------------

# a -----------------

pnorm(600, mean = 500, sd = 100, lower.tail = TRUE) - pnorm(400, mean = 500, sd = 100, lower.tail = TRUE)

# b -----------------

pnorm(700, mean = 500, sd = 100, lower.tail = TRUE) - pnorm(300, mean = 500, sd = 100, lower.tail = TRUE)

# c -----------------

1- pnorm(700, mean = 500, sd = 100, lower.tail = TRUE)

# d -----------------

pnorm(700, mean = 500, sd = 100, lower.tail = TRUE)
