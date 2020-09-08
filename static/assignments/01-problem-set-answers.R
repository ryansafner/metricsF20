# 1 -------------------
library(tidyverse)

# install for first use
# install.packages("babynames")

# load package 
library(babynames)

# explore help
# ?babynames

# a ------------------

# save as a new tibble
top_5_boys_2017 <- babynames %>% # take data
  filter(sex=="M", # filter by males
         year==2017) %>% # and for 2007
  arrange(desc(n)) %>% # arrange in largest-to-smallest order of n (number)
  slice(1:5) %>% # optional, look only at first 5 rows; head(., n=5) also works
  mutate(percent = round(prop*100, 2)) # also optional, make a percent variable rounded to 2 decimals

# look at our new tibble
top_5_boys_2017

# b -----------------

# save as a new tibble
top_5_girls_2017 <- babynames %>% # take data
  filter(sex=="F", # filter by females
         year==2017) %>% # and for 2007
  arrange(desc(n)) %>% # arrange in largest-to-smallest order of n (number)
  slice(1:5) %>% # optional, look only at first 5 rows; head(., n=5) also works
  mutate(percent = round(prop*100, 2)) # also optional, make a percent variable rounded to 2 decimals

# look at our new tibble
top_5_girls_2017

# 2 ------------------

# for boys
ggplot(data = top_5_boys_2017)+
  aes(x = reorder(name, n), #note this reorders the x variable from small to large n
      y = percent, # you can use prop if you didn't make a percent variable
      fill = name)+ # optional color!
  geom_col()+
  
  # now I'm just making it pretty
  scale_y_continuous(labels=function(x)paste(x,"%",sep=""))+ # optional, add percent signs
  labs(x = "Name",
       y = "Percent of All Babies With Name",
       title = "Most Popular Boys Names Since 1880",
       fill = "Boy's Name",
       caption = "Source: SSA")+
  theme_classic(base_family = "Fira Sans Condensed", base_size=16)+
  coord_flip()+ # rotate axes!
  theme(legend.position = "") # hide legend

# for girls 
ggplot(data = top_5_girls_2017)+
  aes(x = reorder(name, n), #note this reorders the x variable from small to large n
      y = percent, # you can use prop if you didn't make a percent variable
      fill = name)+ # optional color!
  geom_col()+
  # now I'm just making it pretty
  scale_y_continuous(labels=function(x)paste(x,"%",sep=""))+ # optional, add percent signs
  labs(x = "Name",
       y = "Percent of All Girls With Name",
       title = "Most Popular Girls Names Since 1880",
       fill = "Girl's Name",
       caption = "Source: SSA")+
  theme_classic(base_family = "Fira Sans Condensed", base_size=16)+
  coord_flip()+ # rotate axes!
  theme(legend.position = "") # hide legend

# 3 -----------------

babynames %>%
  filter(name == "Ryan") %>%
  count(sex, wt=n) %>%
  mutate(percent = round((n/sum(n)*100),2))

# 4 -----------------

# note here I'm going to wrangle the data and then pipe it directly into ggplot
# you can wrangle the data and save it as a different tibble, then use THAT tibble
# for your (data = ...) command in ggplot

# first wrangle data
babynames %>%
  filter(name == "Ryan") %>%
  
  # now we pipe into ggplot
  ggplot(data = .)+ # the "." is a placeholder for the stuff above!
  aes(x = year,
      y = n,
      color = sex)+
  geom_line(size=1)+
  labs(x = "Year",
       y = "Number of Babies",
       title = "Popularity of Babies Named 'Ryan'",
       color = "Sex",
       caption = "Source: SSA")+
  theme_classic(base_family = "Fira Sans Condensed", base_size=16)

# 5 -----------------

# a -----------------

babynames %>%
  group_by(year) %>% # we want one observation per year
  filter(sex == "M",
         year>1979) %>% # or >==1980
  arrange(desc(n))%>% # start with largest n first
  slice(1) # take first row only

# b -----------------

babynames %>%
  group_by(year) %>% # we want one observation per year
  filter(sex == "F",
         year>1979) %>% # or >==1980
  arrange(desc(n))%>% # start with largest n first
  slice(1) # take first row only


# 6 ----------------

# a -----------------

babynames %>%
  group_by(name) %>% # we want one row per name
  filter(sex=="M") %>%
  summarize(total=sum(n)) %>% # add upp all of the n's for all years for each name
  arrange(desc(total)) %>% # list largest total first
  slice(1:5) 

# make a vector of the names (we'll need this for our graph below)
top_boys_names<-c("James", "John", "Robert", "Michael", "William")

# you could alternatively add a command, 
# %>% pull(name) to the first chunk of code, 
# and it would do the same thing, but we'd want to save it, 
# for example:

babynames %>%
  group_by(name) %>% # we want one row per name
  filter(sex=="M") %>%
  summarize(total=sum(n)) %>% # add upp all of the n's for all years for each name
  arrange(desc(total)) %>% # list largest total first
  slice(1:5) %>%
  pull(name)

# for girls

babynames %>%
  group_by(name) %>% # we want one row per name
  filter(sex=="F") %>%
  summarize(total=sum(n)) %>% # add upp all of the n's for all years for each name
  arrange(desc(total)) %>% # list largest total first
  slice(1:5)

# make a vector of the names (we'll need this for our graph below)
top_girls_names<-c("Mary", "Elizabeth", "Patricia", "Jennifer", "Linda")

# you could alternatively add a command, %>% pull(name) to the first chunk of code, and it would do the same thing, but we'd want to save it

# b -----------------

# for boys

babynames %>%
  group_by(year) %>%
  filter(sex == "M",
         name %in% top_boys_names) %>%
  ggplot(data = .,
         aes(x = year,
             y = prop,
             color = name))+
  geom_line()+
  labs(x = "Year",
       y = "Proportion of Babies with Name",
       title = "Most Popular Boys Names Since 1880",
       color = "Boy's Name",
       caption = "Source: SSA")+
  theme_classic(base_family = "Fira Sans Condensed", base_size=16)

# for girls

babynames %>%
  group_by(year) %>%
  filter(sex == "F",
         name %in% top_girls_names) %>%
  ggplot(data = .,
         aes(x = year,
             y = prop,
             color = name))+
  geom_line()+
  labs(x = "Year",
       y = "Proportion of Babies with Name",
       title = "Most Popular Girls Names Since 1880",
       color = "Girl's Name",
       caption = "Source: SSA")+
  theme_classic(base_family = "Fira Sans Condensed", base_size=16)

# 7 (BONUS) ----------------

# there's a whole lot to this one, I suggest you read the full answer key

babynames %>%
  mutate(male = ifelse(sex == "M", n, 0),
         female = ifelse(sex == "F", n, 0)) %>%
  group_by(name) %>%
  summarize(Male = sum(male),
            Female = sum(female))%>%
  mutate(perc_male = (Male/(Male+Female)*100)) %>%
  arrange(perc_male) %>%
  filter(perc_male > 48,
         perc_male < 52) %>%
  mutate(total = Male+Female) %>%
  arrange(desc(total)) %>%
  slice(1:10)

# 8 ---------------

# import data with read_csv from readr

# note these file paths will be different for you
polfreedom<-read_csv("../data/freedomhouse2018.csv")
econfreedom<-read_csv("../data/econfreedom.csv")

# look at each dataframe
polfreedom
econfreedom

# 9 ---------------

polfreedom<-polfreedom %>%
  select(`Country/Territory`, Total) %>%
  rename(Country=`Country/Territory`)

# 10 --------------

freedom<-left_join(econfreedom, polfreedom, by="Country")

# 11 --------------

ggplot(data=freedom, aes(x=ef,y=Total))+
  geom_point(aes(color=continent))+
  xlab("Economic Freedom Score")+ylab("Political Freedom Score")+theme_bw()+labs(caption="Sources: Frasier Institute, Freedom House")+
  theme_classic(base_family = "Fira Sans Condensed", base_size=16)

# 12 --------------

# install.packages("ggrepel") install for first use 
library(ggrepel) # load 

interest<-filter(freedom, ISO %in% c("CHN", "NOR", "USA"))

ggplot(data=freedom, aes(x=ef,y=Total))+
  geom_point(aes(color=continent))+
  geom_label_repel(data=interest, aes(ef, Total, label=ISO,color=continent),alpha=0.6)+
  xlab("Economic Freedom Score")+ylab("Political Freedom Score")+theme_bw()+labs(caption="Sources: Frasier Institute, Freedom House")+
  theme_classic(base_family = "Fira Sans Condensed", base_size=16)

# 13 --------------

ggplot(data=freedom, aes(x=ef,y=gdp))+
  geom_point(aes(color=continent))+
  geom_smooth(data=freedom)+
  geom_label_repel(data=interest, aes(ef, Total, label=ISO,color=continent),alpha=0.6)+
  xlab("Economic Freedom Score")+ylab("Political Freedom Score")+theme_bw()+labs(caption="Sources: Frasier Institute, Freedom House")+
  theme_classic(base_family = "Fira Sans Condensed", base_size=16)
