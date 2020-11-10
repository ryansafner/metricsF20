# 9 -------------------

## A -------------
# load tidyverse
library("tidyverse")

# read in data
lead<-read_csv("http://metricsf20.classes.ryansafner.com/data/leadmortality.csv")

# mean of infrate for cities with lead
mean_lead<-lead %>%
  filter(lead==1) %>%
  summarize(mean(infrate)) %>%
  pull() # to save as number

# mean of infrate for cities with no lead
mean_no_lead<-lead %>%
  filter(lead==0) %>%
  summarize(mean(infrate)) %>%
  pull() # to save as number

mean_lead-mean_no_lead

# run t-test of difference
t.test(infrate ~ lead, data = lead)

## B -------------

lead_reg1<-lm(infrate ~ lead, data = lead)
summary(lead_reg1)


## C -------------

lead_reg2<-lm(infrate ~ lead + ph, data = lead)
summary(lead_reg2)

## D -------------

lead_reg3<-lm(infrate ~ lead + ph + lead:ph, data = lead)
summary(lead_reg3)

## E -------------

lead_scatter<-ggplot(data = lead)+
  aes(x = ph,
      y = infrate)+
  geom_point(aes(color = as.factor(lead)))+ # making it a factot makes color discrete rather than continuous!
  
  # now I'm just making it pretty
  # changing color
  scale_color_viridis_d("Pipes",
                        labels=c("0"="Not Lead", "1"="Lead"))+ # changing labels for colors
  labs(x = "pH",
       y = "Infant Mortality Rate")+
  theme_classic(base_family = "Fira Sans Condensed",
                base_size=20)
lead_scatter

lead_scatter+facet_wrap(~lead, 
                        labeller=labeller(lead=c("0"="Not Lead", "1"="Lead")))+ # change facet titles
  guides(color = F) # hide other legend

## H -------------

# regression for no lead

lead %>%
  filter(lead==0) %>%
  lm(data = ., infrate ~ lead + ph + lead:ph) %>%
  summary()

# regression for lead

lead %>%
  filter(lead==1) %>%
  lm(data = ., infrate ~ lead + ph + lead:ph) %>%
  summary()

## I -------------

library(huxtable)
huxreg(lead_reg1,
       lead_reg2,
       lead_reg3,
       coefs = c("Constant" = "(Intercept)",
                 "Lead Pipes" = "lead",
                 "pH" = "ph",
                 "Lead * pH" = "lead:ph"),
       statistics = c("N" = "nobs",
                      "R-Squared" = "r.squared",
                      "SER" = "sigma"),
       number_format = 2)

# 10 --------------------------

## A -------------

# load data
freedom<-read_csv("https://metricsf20.classes.ryansafner.com/data/freedom.csv")

# scatterplot

freedom_plot<-ggplot(data = freedom)+
  aes(x = econ_freedom,
      y = gdp_pc)+
  geom_point(aes(color = continent))+
  scale_y_continuous(labels=scales::dollar)+
  labs(x = "Economic Freedom Score (0-100)",
       y = "GDP per Capita")+
  theme_classic(base_family = "Fira Sans Condensed",
                base_size=20)
freedom_plot

## B -------------

freedom_reg1<-lm(gdp_pc ~ econ_freedom, data = freedom)

summary(freedom_reg1)


## C -------------

freedom_reg2<-lm(gdp_pc ~ econ_freedom + I(econ_freedom^2), data = freedom)

summary(freedom_reg2)

## D -------------

freedom_plot+geom_smooth(method = "lm", formula = "y~x+I(x^2)", color = "green")

## E -------------

# if you want to calculate it in R
library(broom)

freedom_reg2_tidy<-tidy(freedom_reg2)

freedom_beta_1<-freedom_reg2_tidy %>%
  filter(term == "econ_freedom") %>%
  pull(estimate)

freedom_beta_2<-freedom_reg2_tidy %>%
  filter(term == "I(econ_freedom^2)") %>%
  pull(estimate)

# freedom_beta_1+2*freedom_beta_2* # number

## F -------------

# to calculate in R

0.5*(freedom_beta_1/freedom_beta_2)

# let's visualize on the scatterplot

freedom_plot+geom_smooth(method = "lm", formula = "y~x+I(x^2)", color = "green")+
  geom_vline(xintercept = min, linetype = "dashed", size = 1)+
  geom_label(x = min, y = 75000, label = round(min,2))

## G -------------

freedom_reg3<-lm(gdp_pc ~ econ_freedom + I(econ_freedom^2)+ I(econ_freedom^3), data = freedom)
summary(freedom_reg3)

# let's visualize it on the scatterplot
freedom_plot+
  geom_smooth(method = "lm", formula = "y~x+I(x^2)", color = "green")+
  geom_smooth(method = "lm", formula = "y~x+I(x^2)+I(x^3)", color = "orange")

## H -------------

# run F test
library(car)
linearHypothesis(freedom_reg3, c("I(econ_freedom^2)", "I(econ_freedom^3)"))

## I -------------

# log linear model 
freedom_reg4<-lm(log(gdp_pc) ~ econ_freedom, data = freedom)
summary(freedom_reg4)

## J -------------

log_freedom_plot<-ggplot(data = freedom)+
  aes(x = econ_freedom,
      y = log(gdp_pc))+
  geom_point(aes(color = continent))+
  geom_smooth(method = "lm")+
  labs(x = "Economic Freedom Score (0-100)",
       y = "Log GDP per Capita")+
  theme_classic(base_family = "Fira Sans Condensed",
                base_size=20)
log_freedom_plot

## K -------------

huxreg("GDP per Capita" = freedom_reg1,
       "GDP per Capita" = freedom_reg2,
       "Log(GDP per Capita)" = freedom_reg3,
       coefs = c("Constant" = "(Intercept)",
                 "Economic Freedom Score (0-10)" = "econ_freedom",
                 "Economic Freedom Squared" = "I(econ_freedom^2)",
                 "Economic Freedom Cubed" = "I(econ_freedom^3)"),
       statistics = c("N" = "nobs",
                      "R-Squared" = "r.squared",
                      "SER" = "sigma"),
       number_format = 2)