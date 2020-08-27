# 1 -------------------
# first time only
# install.packages("gapminder")

# load gapminder
library(gapminder)

# get help
?gapminder

# 2 ------------------

# a 

str(gapminder)

# b 

# - country: a factor
# - continent: a factor
# - year: an integer
# - lifeExp: a number
# - gdpPercap: a number

# c

head(gapminder)

# d 

summary(gapminder)

# 3 -----------------

hist(gapminder$gdpPercap)

# 4 -----------------

boxplot(gapminder$gdpPercap)

# 5 -----------------

boxplot(gapminder$gdpPercap~gapminder$continent)

# alternate method
# boxplot(gdpPercap~continent, data = gapminder)

# 6 ----------------

plot(gapminder$lifeExp~gapminder$gdpPercap)

# alternate method
# boxplot(lifeExp~gdpPercap, data = gapminder)

# 7 ----------------

# install if you don't have
# install.packages("ggplot2")

# load ggplot2 
library(ggplot2)

# 8 ---------------

ggplot(data = gapminder,
       aes(x = continent))+
  geom_bar()

# 9 ---------------

ggplot(data = gapminder,
       aes(x = gdpPercap))+
  geom_histogram()

# 10 --------------

ggplot(data = gapminder,
       aes(x = gdpPercap,
           fill = continent))+
  geom_histogram()

# 11 --------------

ggplot(data = gapminder,
       aes(x = gdpPercap,
           fill = continent))+
  geom_density(alpha=0.4)

# 12 --------------

ggplot(data = gapminder,
       aes(x = lifeExp,
           fill = continent))+
  geom_density(alpha=0.4)

# 13 --------------

ggplot(data = gapminder,
       aes(x = gdpPercap,
           y = lifeExp))+
  geom_point()

# 14 --------------

ggplot(data = gapminder,
       aes(x = gdpPercap,
           y = lifeExp,
           color = continent))+
  geom_point()

# 15 -------------

ggplot(data = gapminder,
       aes(x = gdpPercap,
           y = lifeExp,
           color = continent))+
  geom_point()+
  geom_smooth()

# 16 -------------

ggplot(data = gapminder,
       aes(x = gdpPercap,
           y = lifeExp))+
  geom_point(aes(color = continent))+
  geom_smooth()

# 17 -------------

ggplot(data = gapminder,
       aes(x = gdpPercap,
           y = lifeExp))+
  geom_point(aes(color = continent, 
                 size = pop))+
  geom_smooth()

# 18 -------------

ggplot(data = gapminder,
       aes(x = gdpPercap,
           y = lifeExp))+
  geom_point(aes(color = continent, 
                 size = pop))+
  geom_smooth(aes(color = "black"))

# putting it inside aesthetics tries to map color to something
# in the da ta called "black", since R can't find "black", 
# it will produce some random color

ggplot(data = gapminder,
       aes(x = gdpPercap,
           y = lifeExp))+
  geom_point(aes(color = continent, 
                 size = pop))+
  geom_smooth(color = "black")

# putting it outside aesthetics (correctly) sets color to black

# 19 ----------------

ggplot(data = gapminder,
       aes(x = gdpPercap,
           y = lifeExp))+
  geom_point(aes(color = continent, 
                 size = pop))+
  geom_smooth(color = "black")+
  facet_wrap(~continent)

# 20 ---------------

ggplot(data = gapminder,
       aes(x = gdpPercap,
           y = lifeExp))+
  geom_point(aes(color = continent, 
                 size = pop))+
  geom_smooth(color="black")+
  scale_x_log10()

# 21 ----------------

ggplot(data = gapminder,
       aes(x = gdpPercap,
           y = lifeExp))+
  geom_point(aes(color = continent, 
                 size = pop))+
  geom_smooth(color="black")+
  scale_x_log10()+
  labs(x = "GDP per Capita",
       y = "Life Expectancy",
       color = "Continent",
       size = "Population")

# 22 ---------------

america<-gapminder[gapminder$continent=="Americas",]

# verify this worked
america

ggplot(data = america,
       aes(x = gdpPercap,
           y = lifeExp))+
  geom_point(aes(color = continent, 
                 size = pop))+
  geom_smooth()

# 23 --------------

gap_2002<-gapminder[gapminder$year==2002,]

# verify this worked
gap_2002

ggplot(data = gap_2002,
       aes(x = gdpPercap,
           y = lifeExp))+
  geom_point(aes(color = continent, 
                 size = pop))+
  geom_smooth()


