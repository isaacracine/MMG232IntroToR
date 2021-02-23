#Lecture 2: Data Visulaization
#Isaac Racine
# 2.23.21

#-----------------------------------------------
library(tidyverse)
library(ggplot2)
library(palmerpenguins)

data(package = "palmerpenguins")

ggplot(data = penguins, aes(x = flipper_length_mm, 
                            y = body_mass_g)) +
  geom_point()
