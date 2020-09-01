# 1 -------------------
library(tidyverse)
library(gapminder)
# select(gapminder, c(year, lifeExp, country))

# using the pipe

gapminder %>%
  select(c(year, lifeExp, country))

# 2 ------------------

# select(gapminder, -pop)

# using the pipe

gapminder %>%
  select(-pop)

# 3 -----------------

# rename(gapminder, cont=continent)

# using the pipe

gapminder %>%
  rename(cont=continent)

# 4 -----------------

# arrange(gapminder, year)

# with the pipe
gapminder %>%
  arrange(year)

# 5 -----------------

# arrange(gapminder, desc(year))

# with the pipe
gapminder %>%
  arrange(desc(year))

# 6 ----------------

# arrange(gapminder, year, lifeExp)

# with the pipe

gapminder %>%
  arrange(year, lifeExp)

# 7 ----------------

# arrange(filter, pop>1000000000)

# with the pipe

gapminder %>%
  filter(pop>1000000000)

# 8 ---------------

gapminder %>%
  filter(pop>1000000000) %>%
  filter(country=="India")

# 9 ---------------

gapminder %>%
  select(year, gdpPercap, country) %>%
  filter(year==1997,
         gdpPercap>20000) %>%
  arrange(country)

# 10 --------------

gapminder %>%
  mutate(GDP = gdpPercap * pop)

# 11 --------------

gapminder %>%
  mutate(popm = pop/1000000)

# 12 --------------

gapminder %>%
  summarize(mean(gdpPercap))

# 13 --------------

gapminder %>%
  summarize(Obs = n(),
            Average = mean(gdpPercap),
            Minimum = min(gdpPercap),
            Maximum = max(gdpPercap),
            SD = sd(gdpPercap))

# 14 --------------

gapminder %>%
  group_by(year) %>%
  summarize(Average_GDP = mean(gdpPercap))

# 15 -------------

gapminder %>%
  group_by(continent) %>%
  summarize(Average_GDP = mean(gdpPercap))

# 16 -------------

gdp <- gapminder %>%
  group_by(year, continent) %>%
  summarize(Average_GDP = mean(gdpPercap))

ggplot(data = gdp)+
  aes(x = year,
      y = Average_GDP,
      color = continent)+
  geom_line()

# 17 -------------

gapminder %>%
  group_by(year, continent) %>%
  summarize(Average_GDP = mean(gdpPercap)) %>%
  ggplot(data = .)+
  aes(x = year,
      y = Average_GDP,
      color = continent)+
  geom_line()

# 18 -------------

majors<-read_csv("../data/recent-grads.csv") # note my location on my computer is different than yours!

# 19 ----------------

glimpse(majors)

# 20 ---------------

majors %>%
  distinct(Major)

majors %>%
  summarize(n_distinct(Major))

# 21 ----------------

majors %>%
  arrange(Unemployment_rate) %>%
  select(Major, Unemployment_rate)

# 22 ---------------

majors %>%
  arrange(desc(ShareWomen)) %>%
  select(Major, ShareWomen) %>%
  slice(1:3) # head(., n=3) also works

# 23 --------------

ggplot(data = majors)+
  aes(x = Major_category,
      y = Median,
      fill = Major_category)+
  geom_boxplot()+
  theme(axis.text.x=element_text(angle=45, hjust=1), legend.position = "")

# 24 ---------------

majors %>%
  group_by(Major_category) %>%
  count() %>%
  arrange(n)

# 25 ---------------

stem_categories <- c("Biology & Life Science",
                     "Computers & Mathematics",
                     "Engineering",
                     "Physical Sciences")

majors <- majors %>%
  mutate(stem = ifelse(Major_category %in% stem_categories,
                       "stem",
                       "not_stem")) %>%
  select(stem, everything()) # just so we can see it up front to verify

majors %>%
  group_by(stem) %>%
  summarize(mean(Median))