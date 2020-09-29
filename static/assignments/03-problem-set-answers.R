# 9 -------------------

# first load tidyverse 
library(tidyverse)

# import data
mlb<-read_csv("../data/MLBAttend.csv")

# a ------------------

# make home attendance variable in millions
mlb<-mlb %>%
  mutate(home_attend_mil=home_attend/1000000)

# b ------------------

# summarize and get correlation
mlb %>%
  summarize(Correlation = cor(runs_scored, home_attend_mil))

# c ------------------

# create scatterplot with regression line 
scatter<-ggplot(data = mlb)+
  aes(x = home_attend_mil,
      y = runs_scored)+
  geom_point()+
  geom_smooth(method="lm")
scatter

# d ------------------

# run regression, save as reg
reg<-lm(runs_scored ~ home_attend_mil, data = mlb)

# get summary of reg
summary(reg)

# OPTIONAL

# Here I'm going to save beta 0 hat and beta 1 hat
# as objects to call up in the text of the markdown document
# We'll need broom and tidy() first
library(broom)
reg_tidy<-tidy(reg)

reg_tidy

# first beta 0 hat 

beta_0_hat<-reg_tidy %>%
  filter(term=="(Intercept)") %>% # look at intercept row
  pull(estimate) %>% # extract beta 0 hat
  round(., 2) # round to 2 decimal places

beta_0_hat # check 

# now beta 1 hat 

beta_1_hat<-reg_tidy %>%
  filter(term=="home_attend_mil") %>% # look at X-variable row
  pull(estimate) %>% # extract beta 1 hat
  round(., 2) # round to 2 decimal places

beta_1_hat

# e ------------------

# OPTIONAL

# if you are using markdown, try out the equatiomatic package
#install.packages("equatiomatic")
library(equatiomatic)
extract_eq(reg, # the regression
           use_coefs = TRUE, # use the estimated numbers
           coef_digits = 3, # how many digits to show
           fix_signs = TRUE) # fix negatives

# f ------------------

# load huxtable
library(huxtable)
huxreg(reg, # this is sufficient, the rest is decoration
       coefs = c("Constant" = "(Intercept)",
                 "Home Attendance (Millions)" = "home_attend_mil"),
       statistics = c("N" = "nobs",
                      "R-Squared" = "r.squared",
                      "SER" = "sigma"),
       number_format = 3)

# g ------------------

# here we need broom's augment() command to add residuals to the data

# load broom
library(broom)
# augment the regression, save as reg_aug
reg_aug<-reg %>%
  augment()

# now we use this as the data in our histogram plot in ggplot, (x is .resid)
ggplot(data = reg_aug)+
  aes(x = .resid)+
  geom_histogram(color="white")

# h ------------------

# this is another plot from reg_aug, where x is home attendance and y is .fitted
ggplot(data = reg_aug)+
  aes(x = home_attend_mil,
      y = .resid)+
  geom_point()+
  geom_hline(yintercept=0, color="red")

# i ------------------

# this requires the lmtest package for the bptest() command

# install.packages("lmtest")
# load lmtest
library(lmtest)
bptest(reg)

# j ------------------

# this requires the car package for the outlierTest() command

# install.packages("car")
# load car

library(car)
outlierTest(reg)

# This test detected one outlier, which is observation (row) number 816. Let's look it up:

mlb %>%
  slice(816)

# OPTIONAL, point it out on scatterplot

outlier<-mlb %>%
  slice(816)

library(ggrepel)
scatter+ # our scatterplot saved from part c
  geom_point(data = outlier,
             aes(x = home_attend_mil,
                 y = runs_scored),
             color = "red")+
  geom_text_repel(data = outlier,
                  aes(x = home_attend_mil,
                      y = runs_scored),
                  label = "1981 Blue Jays",
                  color = "red")

# l ------------------

# save slope from our regression as sample_slope

sample_slope<-reg_tidy %>%
  filter(term=="home_attend_mil") %>%
  pull(estimate)

# double check it worked
sample_slope
# note I could also just use the beta_1_hat I optionally made in Part D

# make 1000 simulations of sample slopes under null hypothesis that slope = 0

#install.packages(infer)
library(infer) # load infer
slope_simulations<-mlb %>% # save as a tibble
  specify(runs_scored ~ home_attend_mil) %>% # our lm model
  hypothesize(null = "independence") %>% # null hypothesis, slope = 0 (X and Y independent)
  generate(reps = 1000, type = "permute") %>% # make 1000 permutations
  calculate(stat = "slope") # calculate the sample slope of each permutation

# make sure it worked
slope_simulations %>%
  head(., n = 10) # there are a LOT! I only print the first 10 for space

slope_simulations %>%
  get_p_value(obs_stat = sample_slope,
              direction = "both")

# m ------------------

# now make a histogram of this
slope_simulations %>%
  visualize(obs_stat = sample_slope)+
  shade_p_value(obs_stat = sample_slope, # set the obs_stat equal to our saved slope from above
                direction = "both") # two-sided test, shade both sides

# OPTIONAL

# infer no longer shades the p-value, so I'll do it manually

slope_simulations %>%
  ggplot(data = .)+
  aes(x = stat)+ # slope from simulations
  geom_histogram(color = "white", # the histogram
                 fill = "indianred")+
  # add vertical line for our sample slope
  geom_vline(xintercept = sample_slope,
             color = "red",
             size = 2)+
  # add label
  geom_label(x = sample_slope,
             y = 200,
             color = "red",
             label = expression(paste("Our ", hat(beta[1]))))+
  # add "shading" for p-value's two sides
  # right side
  geom_rect(xmin=68.798,
            xmax=100,
            ymin=0,
            ymax=300,
            color = "red",
            alpha=0.5)+
  # left side
  geom_rect(xmin=-100,
            xmax=-68.798,
            ymin=0,
            ymax=300,
            color = "red",
            alpha=0.5)+
  scale_x_continuous(seq(-75,75,25))+
  labs(x = expression(paste("Distribution of ", hat(beta[1]), " under ", H[0], " that ", beta[1]==0)),
       y = "Samples")+
  theme_classic(base_family = "Fira Sans Condensed",
                base_size=20)

# OPTIONAL - rescale it to t-values

slope_simulations %>%
  summarize(mean = mean(stat), # get the mean slope of the simulated distribution of slopes
            se = (sd(stat))) # get the standard error of the slope 
```

```{r}
# standardize slopes to t-statistics
tstatistics<-slope_simulations %>%
  mutate(tscores = ((stat - mean(stat))/sd(stat)))

our_t<-((sample_slope-mean(slope_simulations$stat))/sd(slope_simulations$stat))

# what is our t-statistic?
our_t

# now plot t-statistics %>%
ggplot(data = tstatistics)+
  aes(x = tscores)+ # slope from simulations
  geom_histogram(color = "white", # the histogram
                 fill = "indianred")+
  # add vertical line for our sample slope's t-statistic
  geom_vline(xintercept = our_t,
             color = "red",
             size = 2)+
  #add label
  geom_label(x = our_t,
             y = 50,
             color = "red",
             label = "Our t")

# n ------------------

# save as a tibble called "ci_values"
ci_values<-slope_simulations %>%
  get_confidence_interval(level = 0.95,
                          type = "se",
                          point_estimate = sample_slope)

# see what we made
ci_values

# we'll use this for the endpoints in the shade_ci() command 

slope_simulations %>%
  visualize(obs_stat = sample_slope)+
  shade_confidence_interval(endpoints = ci_values)
