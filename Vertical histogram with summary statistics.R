######################################################################
##                                                                  ##
##  Vertical histograms with summary statistics (mean +- error)     ##
##                                                                  ##
##  Author: Dr. Maja Ilic                                           ##
##          School of Biological Sciences                           ##
##          Queen's University Belfast                              ##
##          maja.ilic.bio@gmail.com                                 ##
##                                                                  ##
##  Date: 27.09.2020                                                ##
##                                                                  ##
##  Original idea: Prof. William Sutherland                         ##
##                 Department of Zoology                            ##
##                 University of Cambridge                          ##
##                                                                  ##
##  How it all started:                                             ##
##  https://twitter.com/Bill_Sutherland/status/1309771803661930496  ##
##                                                                  ##
######################################################################

##===============================================
## Packages ----

library(ggplot2)
library(ggridges)
library(dplyr)

##===============================================
## Data ----

data(iris)
head(iris)

##===============================================
## First step: generate a plot using ggplot() with vertical histograms ----

# Define a nice lable for x-axis
# Note: \u00B1 is generating the +- sign

xlab <- "Sepal length (cm), mean \u00B1 s.d"

## Simple plot

# Use geom_density_ridges() to generate histograms for each Iris species
# You can play around with the number of "bins", here, 20 seemed to be suitable
# You can choose if you want to draw the baseline, if not, set draw_baseline = FALSE
# alpha = 0.5 is used for transparency
# scale = 0.95 is used to make all histograms visible without overlapping

g1 <- ggplot() + 
  geom_density_ridges(data = iris, aes(x = Sepal.Length, y = Species, 
                                       fill = Species, color = Species, height = stat(density)),
                      stat = "binline", bins = 20, draw_baseline = TRUE, alpha = 0.5, scale = 0.95) 

g1

# To make the plot nicer, you can choose another theme (e.g. theme_classic())
# and/or add specific changes to your theme (bold titles etc).
# Change to default colors using scale_fill and scale_color
# Use coord_flip() to flip the coordinates and get vertical histograms
# Note that the legend here is redundant and can be removed with legend.position = "none"
# within theme()

g1 <- g1 +
  theme_classic() +
  theme(axis.title.y = element_text(size = 14, face = "bold", margin = margin(0,15,0,0)),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(10,0,0,0)),
        legend.title = element_text(size = 14, face = "bold")) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  labs(x = xlab) +
  coord_flip()

g1

##===============================================
## Second step: calculate summary statistics ----

# Note: there are ways of calculating the summary statistics
# (such as mean) directly within the ggplot(), however,
# I personally prefer having a separate dataframe with means,
# errors and other summary statistics

# For exercise purposes, I choose to work with "Sepal.Length" 
# and have only calculated mean and standard deviation for this one variable

summary.df <- iris %>% 
  group_by(Species) %>% 
  summarize(Mean_SL = mean(Sepal.Length, na.rm = T),
            SD_SL = sd(Sepal.Length, na.rm =T))

summary.df

# Note: using na.rm = T is reduntant in this specific case, as there are no NAs present,
# but it is in general a good practise and is therefore included in the code

##===============================================
## Third step: add summary statistics to the plot ----

# In order to add the mean values to the plot, all you have to do 
# is to add them to the existing plot using geom_point()
# Make sure you now use your dataframe with summary statistics
# Note that I chose the diamond shape here (shape = 23)

g1 <- g1 +   
  geom_point(data = summary.df, 
             aes(x = Mean_SL, y = Species, 
                 fill = Species, color = Species),
             size = 4, shape = 23)

g1

# To add the error bar (here: standard deviation), you can use geom_segment()
# Values that you will need to specify here are x, xend, y and yend
# Although it is obvious that the positions of the histograms are 1, 2 and 3,
# you can check this by getting a table from your existing ggplot() with all 
# information on positions, colors and similar

gt <- ggplot_build(g1)
gt

# The easiest way to find the positions here is to look at the second data layer,
# which in this case are the mean values we've plotted (see column "y")

gt$data[[2]]

# You can add the position to the dataframe with summary statistics

summary.df$Pos <- gt$data[[2]]$y
summary.df

# Finally, you can add the standard deviation to the existing plot
# I have used "size" to make the "error bar" a bit thicker

g1 <- g1 +   
  geom_segment(data = summary.df, 
               aes(x = Mean_SL - SD_SL, xend = Mean_SL + SD_SL, 
                   y = Pos, yend = Pos, 
                   color = Species),
               size = 2)

g1

# Save the plot using ggsave()

ggsave("Vertical histogram with summary statistics for Iris.png", g1, width = 8, height = 6)

# Done!
# As always, I am sure there are other ways of coding this, 
# but for me, this seems very easy and intuitive.
# And most importantly: it works and looks nice!