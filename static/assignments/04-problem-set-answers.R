# 9 -------------------

## A -------------
# install.packages("wooldridge")
library(wooldridge)
bwght<-wooldridge::bwght

## B -------------
bwght %>%
  select(bwght, cigs, motheduc, cigprice, faminc) %>%
  cor(use = "pairwise.complete.obs")

## C -------------

### Implication 1
lm(bwght~cigprice+cigs+faminc, data = bwght) %>% summary()

### Implication 2
lm(bwght~motheduc+cigs+faminc, data = bwght) %>% summary()

### Implication 3
bwght %>%
  select(faminc, cigprice) %>%
  cor()

### Implication 4
bwght %>%
  select(cigprice, motheduc) %>%
  cor(use="pairwise.complete.obs")

## F -------------
lm(bwght~cigs+faminc, data = bwght) %>% summary()

## I -------------
lm(bwght~cigprice+cigs+faminc+motheduc, data = bwght) %>% summary()

## L -------------
lm(bwght~cigs+faminc+motheduc, data = bwght) %>% summary()

## M -------------
library(ggdag)
dagify(bwght~cigs+inc+educ,
       cigs~price+educ+inc,
       inc~educ+u1,
       price~u1,
       exposure = "cigs",
       outcome = "bwght") %>% 
  ggdag_status()+
  theme_dag_blank()+
  theme(legend.position = "none")


# 10 -------------------

## A -------------
library(tidyverse)

# load data
heights<-read_csv("https://metricsf20.classes.ryansafner.com/Data/heightwages.csv")

# make scatterplot
ggplot(data=heights, aes(x=height85, y=wage96))+
  geom_jitter(color="blue")+
  geom_smooth(method="lm",color="red")+
  labs(x = "Adult Height in 1985 (inches)",
       y = "Hourly Wage in 1996 ($)")+
  theme_classic(base_family = "Fira Sans Condensed",
                base_size=20)
```

## B -------------

reg1<-lm(wage96~height85, data=heights)
summary(reg1)


## C -------------

# If you want to calculate it with R
library(broom)
reg1_tidy<-tidy(reg1)

beta_0<-reg1_tidy %>%
  filter(term == "(Intercept)") %>%
  pull(estimate)

beta_1<-reg1_tidy %>%
  filter(term == "height85") %>%
  pull(estimate)

beta_0+beta_1*70 # some rounding error in my calculation above

## D -------------

heights %>%
  select(wage96, height81, height85) %>%
  cor(use = "pairwise.complete.obs")

## E -------------

reg2<-lm(wage96~height85+height81, data=heights)
summary(reg2)

## F -------------

# If you want to calculate it with R
library(broom)
reg2_tidy<-tidy(reg2)

multi_beta_0<-reg2_tidy %>%
  filter(term == "(Intercept)") %>%
  pull(estimate)

multi_beta_1<-reg2_tidy %>%
  filter(term == "height85") %>%
  pull(estimate)

multi_beta_2<-reg2_tidy %>%
  filter(term == "height81") %>%
  pull(estimate)

multi_beta_0+multi_beta_1*70+multi_beta_2*58 # some rounding error in my calculation above

## H -------------

ggplot(data=heights, aes(x=height81, y=height85))+
  geom_jitter(color="blue")+
  geom_smooth(method="lm",color="red")+
  labs(x = "Adult Height in 1985 (inches)",
       y = "Adolescent Height in 1981 (inches)")+
  theme_classic(base_family = "Fira Sans Condensed",
                base_size=20)

## I -------------

#install.packages("car") # install if you don't have 
library("car") # load the car library for the vif() command

vif(reg2) # run vif 

## J -------------

aux_reg <- heights %>%
  filter(!is.na(wage96)) %>% # use only for which we have wages
  lm(data = ., height85~height81) # run regression

summary(aux_reg) # look for R-squared

# in R: 

# extract r.squared using broom
aux_r_sq<-glance(aux_reg) %>%
  pull(r.squared)

# vif formula
1/(1-aux_r_sq)

## K -------------

# in R: 

# extract r.squared using broom
aux_r_sq<-glance(aux_reg) %>%
  pull(r.squared)

# vif formula
1/(1-aux_r_sq)