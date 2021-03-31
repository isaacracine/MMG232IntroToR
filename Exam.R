# Isaac Racine
# Take home exam 
# 12 Mar 2021
#------------------------------------------------
cars <- data("mtcars")
library(ggplot2)
head(cars)
library(tidyverse)

#-------------- Q17 ---------------
data <- mtcars[1:3, 1:4]
print(mtcars)
print(data)

#-------------- Q18 ---------------
c1 <- c(2,3,4)
c2 <- c(5,6,7)
c_new <- cbind(c1,c2)
print(c_new)
typeof(c_new)

#-------------- Q19 ---------------
dim(c_new)

#-------------- Q21 ---------------
new_d <- mtcars %>%
  filter(mpg > 30) %>%
  filter(gear > 4)
print(new_d)

#-------------- Q22 ---------------
line_plot <- ggplot(data = mtcars, aes(x = mpg, y= gear)) + 
  geom_line() + theme_minimal() + labs(x = "mpg", y = "gear", title = "Line Graph of mpg vs gear")
print(line_plot)

#save it and upload

#-------------- Q23 ---------------
library(tidyHeatmap) 



mtcars_tidy <- mtcars %>% as_tibble(rownames = "Car name") %>% 
  # Scale
  mutate_at(vars(-`Car name`, -mpg, -gear), scale) %>% 
  # tidyfy
  pivot_longer(cols = -c(`Car name`, mpg, gear), names_to = "Property", 
               values_to = "Value")

mtcars_tidy

#NOW can make heatmap with additional features
#need to normalize cols differently (scaling)
mtcars_heatmap <- mtcars_tidy %>% group_by(gear) %>% 
  heatmap(`Car name`,Property, Value) %>% add_tile(mpg)

mtcars_heatmap

#-------------- Q24 ---------------

#---------------------PCA-------------------------


dim(mtcars)

#run PCA
car.pca <- prcomp (~ mpg + gear + hp + cyl,
                    data=mtcars,
                    scale = TRUE)

car.pca

#transfer to new obj to cleanly plot
pca_car <- as.data.frame(car.pca$x)
pca_car$mpg <- mtcars$mpg
head(pca_car)
ggplot(pca_car, aes(x = PC1, y = PC2, color = mpg)) + 
  geom_point() + ggtitle("MPG PCA Plot")

#now repeat the pca but with gears
pca1_car <-as.data.frame(car.pca$x)
pca1_car$gear <- mtcars$gear
head(pca1_car)
ggplot(pca1_car, aes(x = PC1, y = PC2, color = gear)) +
  geom_point() + ggtitle("Gear PCA Plot")
