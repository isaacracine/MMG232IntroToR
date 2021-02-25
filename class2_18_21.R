library(tidyverse)
#install.packages("palmerpenguins")
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


#------------------ Day 2----------------
install.packages("janitor")
library(janitor)
head(penguins)

#make names into camel case
clean_names(penguins, "small_camel")

#renaming single col
colnames(penguins)[8] <- "Year"
colnames(penguins)

#Removing missing data
table(is.na(penguins))
dim(penguins)

#removing Nas
#removes entire row w NA
penguins2 <-na.omit(penguins)
table(is.na(penguins2))
dim(penguins2) #DIMENSIONS CHANGED


##FILTERING DATA
df <-data.frame("SN" = 1:5, "Age" = c(21,15,80,45,27),
                "Name" = c("John", "Dora", "Mike",
                           "Lucas", "Ashley"),
                stringsAsFactors = FALSE)

str(df) #returns structure
glimpse(df) #TIDYVERSE version of structure


#positional indexing
df[3,2]
ages <-df[,2] #pass entire col
df[1:3,]  #multiple consecutive entries
df[c(1,4,5),] #pick specific rows to view

#grepping
df$SN   #prints entire col, only for dataframes


#-----------Filtering with pipes-----
#pipe =  %>%
adelie <- penguins %>% filter(species == "Adelie")
adelie
#filters out just adelie species
#filtering numeric uses just one =, while char and
#string data must have double ==

dim(adelie)

other_species <- penguins %>% filter(species != "Adelie")
other_species
dim(other_species)


#another way to remove entire col
no_island <-penguins %>% select(-island)
colnames(no_island) #removed island col
colnames(penguins)

#---------------Layering Pipes-------------
penguins %>%
  filter(species == "Gentoo") %>%
  filter(sex == "female") %>%
  filter(bill_length_mm >= 30)
#order of pipe is important bc can exclude data
#start with broad and work to small

p_kg <-penguins %>%
  filter(species == "Gentoo") %>%
  filter(sex == "female") %>%
  filter(bill_length_mm >= 30) %>%
  mutate(body_mass_kg = body_mass_g/1000)

knitr::kable(head(p_kg))


#----------------Summary stats---------
summary(penguins)
summary(penguins$bill_depth_mm) #single col

penguins %>%
  group_by(species) %>%
  summarise_at(c("bill_length_mm", "body_mass_g"), mean, na.rm = TRUE)


#-------------joinging data---------------
#use kableExtra package
#when joinging only length of first obj used
#so if second obj larger the vals dropped
# install.packages("kableExtra")
# library(kableExtra)
# annotations <- read_tsv("gene_annotations.tsv")
# seqs <- read_tsv("genes_and_seqs.tsv")
# 
# joined_annotations <- annotations %>% 
#   left_join(seqs, by = "gene_ID")
# 
# head(joined_annotations) %>%
#   kbl() %>%
#   kable_styling()


#-------------Challenge Deconstrunctiong----
url <- "http://varianceexplained.org/files/Brauer2008_DataSet1.tds"
unclean  <- read_delim(url, delim = "\t")

#description on R walk through
cleaned_data <- read_delim(url, delim = "\t") %>%
  separate(NAME, c("name", "BP", "MF", "systematic_name", "number"), sep = "\\|\\|") %>%
  mutate_at(vars(name:systematic_name), funs(trimws)) %>%
  select(-number, -GID, -YORF, -GWEIGHT) %>%
  gather(sample, expression, G0.05:U0.3) %>%
  separate(sample, c("nutrient", "rate"), sep = 1, convert = TRUE) %>%
  filter(!is.na(expression), systematic_name != "")
