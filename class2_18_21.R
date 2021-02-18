library(tidyverse)
install.packages("palmerpenguins")
library(palmerpenguins)

#tells you if data comes with any packages
data(package = 'palmerpenguins')

head(penguins)
#tables in tidyverse are called tibles
#each columns has the data type listed


obj <- c(1:6)
assign("x", c(1:6))


#----------------------------------
#loading CSV files
cars <- read.csv("cars.csv")
head(cars)

#read in tidyverse way
cars_tidy <-read_csv("cars.csv")
head(cars_tidy)

#------------------------------------
#removing object
rm(x)


#----------------------------------
# Data types

#Vector: single list/col of one data type
#Matrix: multi cols/rows but ONE data type
#Data frame: multi rows/cols, but can be MIDEX types

m <- matrix(1:9, ncol = 3)
m

#rows: [r,]
#cols: [,c]

#can apply math functions to data types
m+2

#positional indexing
m[2,3]


data = data.frame( x = c(11, 12, 14),
                   y = c("a", "b", "c"),
                   z = c(T, F, T))
data
view(data)  #cleaner viewing format but pops in new window


#let's add data
x <- c(1,2,3)
y <- c(4,5,6)
z <- cbind(x,y)
z   #made two cols
w <- rbind(x,y)
w   #made two rows
 

# Object Oriented (OO) classes----------------
# Basically putting together several objs, but 
#objs in this case are called assays
# need to share one column
# relation data base


#----------Reading R code--------------------
#Base R
#Tidyverse
#Formula based packages
#all have their own way of writing things but they can only do the same thing