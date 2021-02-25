#Lecture 2: Data Visulaization
#Isaac Racine
# 2.23.21

#-----------------------------------------------
library(tidyverse)
library(ggplot2)
library(palmerpenguins)

data(package = "palmerpenguins")

#addingg labels
ggplot(data = penguins, aes(x = flipper_length_mm, 
                            y = body_mass_g)) +
  geom_point() + xlab("Flipper Length (mm)") +
  ylab("Body Mass (g)") + 
  ggtitle("Scatterplot of Flipper Length by Body Mass")

#Another way to add labels
ggplot(data = penguins, aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point() + 
  labs(x = "Flipper Length (mm)",
       y = "Body Mass (g)", 
       title = "Scatterplot of Flipper Length by Body Mass")

#Changing color and shape of points by species
ggplot(data = penguins, aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point(aes(color = species, shape = species), size = 1.5) + 
  labs(x = "Flipper Length (mm)", y = "Body Mass (g)", title = "Scatterplot of Flipper Length by Body Mass")

#Can manually change the colors used as well
#cAn be HTML style # followed by numbers or
#specific color names
#Make sure to use scale_color NOT scale_fill
ggplot(data = penguins, aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point(aes(color = species, shape = species), size = 2) + 
  labs(x = "Flipper Length (mm)", y = "Body Mass (g)", title = "Scatterplot of Flipper Length by Body Mass") + 
  scale_color_manual(values = c("#E69F00", "#009E73", "#0072B2"))


#Change the legend details
#can also remove legend for several plots on one, thus
#having only one legend
    #theme(legend.title = element_blank())
ggplot(data = penguins, aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point(aes(color = species, shape = species), size = 2) + 
  labs(x = "Flipper Length (mm)", y = "Body Mass (g)", title = "Scatterplot of Flipper Length by Body Mass", 
       colour = "Species", shape = "Species") + scale_color_manual(values = c("#E69F00", 
                                                                              "#009E73", "#0072B2")) + theme(legend.position = "bottom")


#PLot themes
#change background
#THE LAST THEME IS THE SEED
#See how the the legend is positioned to bottom
#but appears on side, to make work add to last theme instead
ggplot(data = penguins, aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point(aes(color = species, shape = species), size = 2) + 
  labs(x = "Flipper Length (mm)", y = "Body Mass (g)", title = "Scatterplot of Flipper Length by Body Mass", 
       colour = "Species", shape = "Species") + scale_color_manual(values = c("#E69F00", 
                                                                              "#009E73", "#0072B2")) + theme(legend.position = "bottom") + 
  theme_minimal()


#install load gg packages
install.packages('ggthemes')
install.packages('ggsci') #has themes for specific journals
library(ggthemes)
library(ggsci)

#practice
ggplot(data = penguins, aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point(aes(color = species, shape = species), size = 2) + 
  labs(x = "Flipper Length (mm)", y = "Body Mass (g)", title = "Scatterplot of Flipper Length by Body Mass", 
       colour = "Species", shape = "Species") + scale_color_npg() + 
  theme_clean() + theme(legend.position = "bottom")


#Plots can be exported in several ways:
#Using the export button in the Plots pane of Q4
    #SVG option best for photoshop

#With code from our script/console:
png("scatter_penguins.png", width = 5, height = 5, units = "in", 
    res = 200, pointsize = 12)
ggplot(data = penguins, aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point(aes(color = species, shape = species), size = 2) + 
  labs(x = "Flipper Length (mm)", y = "Body Mass (g)", title = "Scatterplot of Flipper Length by Body Mass", 
       colour = "Species", shape = "Species") + scale_color_npg() + 
  theme_clean() + theme(legend.position = "bottom")
dev.off()
#DONT forget to run dev.off!!!

#------------------------Line Graphs-----------------
ggplot(penguins, aes(x = body_mass_g, y = flipper_length_mm)) + 
  geom_line() + theme_minimal()


#Smoothing 
ggplot(penguins, aes(x = body_mass_g, y = flipper_length_mm)) + 
  geom_line() + theme_minimal() + geom_smooth(method = "auto", 
                                              se = TRUE, fullrange = FALSE, level = 0.95)

#Regression
ggplot(penguins, aes(x = body_mass_g, y = flipper_length_mm)) + 
  geom_line() + theme_minimal() + geom_smooth(method = "lm", 
                                              se = TRUE, fullrange = FALSE, level = 0.95)

#Histogram
ggplot(data = penguins, aes(x = flipper_length_mm)) + geom_histogram(aes(fill = species))
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
#Can be used to change bin size

#changes the transperency
#also does not stack  but layers count by species
ggplot(data = penguins, aes(x = flipper_length_mm)) + geom_histogram(aes(fill = species), 
                                                                     alpha = 0.7, position = "identity")


#----------------SAVINNG PLots as Objects-------------
flipper_hist <- ggplot(data = penguins, aes(x = flipper_length_mm)) + 
  geom_histogram(aes(fill = species), alpha = 0.5, position = "identity")

#can then add arguments to OBJ
flipper_hist + ggtitle("Histogram of Flipper Length by Species")

#CAN ALSO SAVE PLOT as R Data Obj to reduce time loading

#------------------------Boxplot---------------
ggplot(data = penguins, aes(x = species, y = flipper_length_mm)) + 
  geom_boxplot(aes(color = species), width = 0.3, show.legend = FALSE)

#JITTER: let's us see our points on distribution
#done b/c moves points a little bit so not stacked
ggplot(data = penguins, aes(x = species, y = flipper_length_mm)) + 
  geom_boxplot(aes(color = species), width = 0.3, show.legend = FALSE) + 
  geom_jitter(aes(color = species), alpha = 0.5, show.legend = FALSE, 
              position = position_jitter(width = 0.2, seed = 0))

#--------------------- FACETS --------------------
ggplot(data = penguins, aes(x = species, y = flipper_length_mm)) + 
  geom_boxplot(aes(color = species), width = 0.3, show.legend = FALSE) + 
  geom_jitter(aes(color = species), alpha = 0.5, show.legend = FALSE, 
              position = position_jitter(width = 0.2, seed = 0)) + 
  geom_boxplot(aes(color = species)) + facet_wrap(~sex)

#CAN REMOVE NAs
ggplot(data = subset(penguins, !is.na(sex)), aes(x = species, 
                                                 y = flipper_length_mm)) + geom_boxplot(aes(color = species), 
                                                                                        width = 0.3, show.legend = FALSE) + geom_jitter(aes(color = species), 
                                                                                                                                        alpha = 0.5, show.legend = FALSE, position = position_jitter(width = 0.2, 
                                                                                                                                                                                                     seed = 0)) + geom_boxplot(aes(color = species)) + facet_wrap(~sex)

#---------------- VIOLIN PLOT ----------------------
ggplot(data = subset(penguins, !is.na(sex)), aes(x = species, 
                                                 y = flipper_length_mm)) + geom_violin(aes(color = species), 
                                                                                       draw_quantiles = 0.5) + geom_jitter(aes(color = species), 
                                                                                                                           alpha = 0.5, show.legend = FALSE, position = position_jitter(width = 0.2, 
                                                                                                                                                                                        seed = 0)) + facet_wrap(~sex)

#----------------------- PRACTICE -------------
depth <- read_csv("depths.csv")

ggplot(data = depth, aes(x =date, y = meter)) + 
  geom_line() + geom_point() + theme_minimal() #+
  labs(x = "Date", y = "Meter", title = "Line Plot of Depth CSV")

view(depth)
#repeat dates 2018-05-01
which(depth$date == "2018-05-01")
#remove first entry
depth <- depth[-30,]

#plot again
ggplot(data = depth, aes(x =date, y = meter)) + 
  geom_line() + geom_point() + theme_minimal() +
  labs(x = "Date", y = "Meter", title = "Line Plot of Depth CSV")

#EXTRAS
depth %>% ggplot(aes(x = date, y = meter, group = 1)) + geom_line(color = "grey") + 
  geom_point(shape = 21, color = "black", fill = "#69b3a2", 
             size = 2) + theme_clean() + theme(axis.text.x = element_text(angle = 90))


#---------------------PCA-------------------------
peng <- penguins %>% rename(bill_length = bill_length_mm, bill_depth = bill_depth_mm, 
                            flipper_length = flipper_length_mm, body_mass = body_mass_g) %>% 
  mutate(species = as.factor(species), island = as.factor(island), 
         sex = as.factor(substr(sex, 1, 1))) %>% filter(!is.na(bill_depth))

#compare new dims
dim(penguins)
dim(peng)

#run PCA
peng.pca <- prcomp (~ bill_length + bill_depth + flipper_length + body_mass,
                    data=peng,
                    na.action=na.omit,  # not actually necessary: we removed NA
                    scale. = TRUE)

peng.pca

#transfer to new obj to cleanly plot
peng.pca_out <- as.data.frame(peng.pca$x)
peng.pca_out$species <- peng$species
head(peng.pca_out)
ggplot(peng.pca_out, aes(x = PC1, y = PC2, color = species)) + 
  geom_point()

#------------------ HEATMAP -----------------------
mat <- as.matrix(mtcars)
heatmap(mat)

#1) cluter rows and cols
#2) ftrs of intest are in rows

heatmap(mat, Colv = NA, scale = "column")

##MORE COMPLEX
install.packages("modeltools")
library(modeltools)
install.packages("ComplexHeatmap")
library(ComplexHeatmap)
install.packages("tidyHeatmap", repos="http://cran.rstudio.com/", dependencies=TRUE)
library(tidyHeatmap) #GIVES AN ERROR????????

mtcars_tidy <- mtcars %>% as_tibble(rownames = "Car name") %>% 
   # Scale
  mutate_at(vars(-`Car name`, -hp, -cyl), scale) %>% 
  # tidyfy
  pivot_longer(cols = -c(`Car name`, hp, cyl), names_to = "Property", 
               values_to = "Value")

mtcars_tidy

#NOW can make heatmap with additional features
mtcars_heatmap <- mtcars_tidy %>% group_by(cyl) %>% 
  heatmap(`Car name`,Property, Value) %>% add_tile(hp)

mtcars_heatmap

#-------------------Correlation Plots--------------
install.packages("rstatix")
library(rstatix)

corr_matrix <- cor(mat)

cor_plot(corr_matrix, method = 'number', type = 'lower')
