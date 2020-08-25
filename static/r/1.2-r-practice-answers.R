# 1 -------------------

## a 
me <- c("Ryan", "Safner")

## b
me

## c 
class(me)

# 2 ----------------

?paste() # or help(paste)
# paste is a function that combines (concatenates) multiple string objects into a single object
paste("Ryan", "Safner")

# note you can choose how to separate string objects with the "sep" argument
# for example
paste("Ryan", "Safner", sep="") # no separation
paste("Ryan", "Safner", sep=" ") # separate with a space " " (the default)
paste("Ryan", "Safner", sep="_") # separate with underscore

# 3 ---------------

my_vector <- c(2,4,6,8,10)

# verify it worked
my_vector
# alternatively, you can use the sequence function, seq()
# see the Class page for more about this function
my_vector <- seq(from = 2, # starting integer
                 to = 10, # ending integer
                 by = 2) # by 2's

# you can shorten it by not including the names of the arguments:
my_vector <- seq(2,10,2)

# verify it worked
my_vector

# 4 -------------------

mean(my_vector)


# 5 -------------------

# create a sequence of integers by 1 with starting_number:ending_number
# see Class 3 page for more

# you can do this all at once without making an object
mean(18:763)

# alternatively you can save this as a vector and run the mean on it
vec1 <- 18:763

mean(vec1)

# 6 ------------------

install.packages("ggplot2") # note the s and the quotes

# 7 ------------------

library("ggplot2") # quotes not necessary, but can be used

# 8 ------------------

str(diamonds)

# We have
# - carat: a number
# - cut: an ordered factor
# - color: an ordered factor
# - clarity: an ordered factor
# - depth: a number
# - table: a number
# - price: an integer
# - x: a number
# - y: a number
# - z: a number

# 9 ------------------

summary(diamonds$carat)
summary(diamonds$depth)
summary(diamonds$table)
summary(diamonds$price)

# 10 ------------------

table(diamonds$color)
table(diamonds$cut)
table(diamonds$clarity)

# 11 ------------------

summary(diamonds)

# 12 ------------------

# remember, dataframes are indexed by: df[row#s, column#s]
diamonds[1:4,] # select first through fourth rows, all columns

# alternatively
diamonds[c(1,2,3,4),] # using a vector-approach

# 13 ------------------

diamonds[c(3,7),] # select 3rd and 7th row, all columns

# 14 ------------------
diamonds[,2] # select all rows, 2nd column

# 15 ------------------

# second column is called "cut"
# diamonds$cut dont' run this, it'll print 53,000 rows!

# 16 -------------------

# use the [square brackets] to subset, 
# first argument (rows) are chosen by conditional: 
# - choose diamonds based on their carat, and only carats >= 1
diamonds[diamonds$carat >= 1,] # select rows on condition, and all columns

# 17 -------------------

# we are testing for equality, so we need two ==
# we are selecting based on clarity, a character/factor, so we need quotes
diamonds[diamonds$clarity=="VVS1",] # select rows on condition, and all columns

# 18 -------------------

# same idea as last problem, except now we want one of any of these 4 colors

# first (tedious) way, a series of checking equality and using "OR"s (|) 
diamonds[diamonds$color=="E" | diamonds$color=="F" | diamonds$color=="I" | diamonds$color=="J",] # select rows on condition, and all columns

# second (better) way, using group membership operator (%in%) and list the elements as a vector
diamonds[diamonds$color %in% c("E","F","I","J"),] # select rows on condition, and all columns 

# 19 -------------------

# testing for two conditions (AND)
diamonds[diamonds$carat>=1 & diamonds$clarity=="VVS1",] # select rows on condition, and all columns 

# 20 -------------------

# use command from last question as the argument to the mean function
## but be sure that you look at the price, specifically
mean(diamonds$price[diamonds$carat>=1 & diamonds$color=="D" & diamonds$clarity=="VVS1"])

# 21 -------------------

max(diamonds$price[diamonds$carat>=1 & diamonds$color=="D" & diamonds$clarity=="VVS1"])
