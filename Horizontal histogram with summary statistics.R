######################################################################
##                                                                  ##
##  Horizontal histograms with summary statistics (mean +- error)   ##
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
# When specifying the histogram bins, there are multiple different options
# "bins": sets a number of bins (with arbitrary center/boundaries)
# "binwidth" sets the with of each bin (with arbitrary center/boundaries)
# "center": specifies the center of each bin, e.g. center = 0 and width = 1 centeres on integers;
# same can be achieved with boundary = 0.5 and width = 1
# Alternativelly, you can specify the breaks, which in my opinion gives you the most "control"
# You can choose if you want to draw the baseline, if not, set draw_baseline = FALSE
# alpha = 0.5 is used for transparency
# scale = 0.95 is used to make all histograms visible without overlapping

# Generate breaks for the histogram bins

hist.bins <- seq(from = floor(min(iris$Sepal.Length, na.rm = T)),
                 to = ceiling(max(iris$Sepal.Length, na.rm = T)),
                 by = 0.2)

# Note: the above code can be written much simpler (e.g. without na.rm = T etc.),
# but for exercises purposes, I wanted to keep it as general as possible, 
# so that it can be easily applied to other data
# by = 0.2 seemed reasonable here (each bin is 0.2 cm wide)

g1 <- ggplot() + 
  geom_density_ridges2(data = iris, aes(x = Sepal.Length, y = Species, 
                                        fill = Species, color = Species, height = stat(density)),
                       stat = "binline", breaks = hist.bins, draw_baseline = FALSE, alpha = 0.5, scale = 0.95) 

g1

# To make the plot nicer, you can choose another theme (e.g. theme_classic())
# and/or add specific changes to your theme (bold titles etc).
# Change to default colors using scale_fill and scale_color
# Alternative: Use coord_flip() to flip the coordinates and get vertical histograms
# Note that the legend here is redundant and can be removed with legend.position = "none"
# within theme()
# For a "very scientific" figure, I will also adjust the labels on the y-axis and add the sample size
# Note that this is done manually, although there might also be a way of doing it more general

spec.labs <- c(expression(paste(italic("Iris setosa")~"("*italic("n")~"= 50)")),
               expression(paste(italic("Iris versicolor")~"("*italic("n")~"= 50)")),
               expression(paste(italic("Iris virginica")~"("*italic("n")~"= 50)")))

g1 <- g1 +
  theme_classic() +
  theme(axis.title.y = element_text(size = 14, face = "bold", margin = margin(0,15,0,0)),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(10,0,0,0)),
        legend.position = "none") +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  scale_y_discrete(breaks = levels(iris$Species),
                   labels = spec.labs) +
  labs(x = xlab)

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

##===============================================
## Fourt step: add counts to each bin ----

# This is a bit tricky, so take your time while you're doing this and make a few checks
# The idea is to extract the counts from the ggplot object
# The are stored in the column "counts" in gt$data[[1]]

hist.counts <- gt$data[[1]]

# Next, exclude all counts which are NA or zero

hist.counts <- hist.counts %>% filter(!is.na(count) & count != 0)

# Add the Species column (necessary for Species-specific color of the text)

hist.counts[which(hist.counts$y == 1), "Species"] <- levels(iris$Species)[1]
hist.counts[which(hist.counts$y == 2), "Species"] <- levels(iris$Species)[2]
hist.counts[which(hist.counts$y == 3), "Species"] <- levels(iris$Species)[3]

# Check if the number of counts reflects the sample size (which is 50 for all three species)
# Note: all bins are represented twice in hist.counts, so the sum of all counts per species should be 100

hist.counts %>% group_by(Species) %>% 
  summarize(all.counts = sum(count))

# Add the counts to the plot
# Note: I chose xmin + 0.1 as the position for the numbers, as this is exactly the middle 
# of each bar in your histogram (as bin width was chosen to be 0.2)
# The y-position of the text is something you might need to play with,
# using ymax from the hist.counts dataframe, and adjusting with +- height
# With "size", you can set the size of the text
# You can either specify one common color (e.g. white or black)
# or you a grouping variable (e.g. Species) for group-/Species-specific color

g1 <- g1 +  
  geom_text(data = hist.counts, 
            aes(x = xmin + 0.1, y = ymax - 0.04,
                label = count, color = Species),
            size = 2.5)

g1

# And finally, so that the figure can be understood with any additional text/legend,
# add the bin width to the plot:

g1 <- g1 + 
  annotate("text", x = 4, y = 3.75, hjust = 0.5, label = "Bin width = 0.2 cm", size = 3)

g1

# Save the plot using ggsave()

ggsave("Horizontal histogram with summary statistics for Iris.png", g1, width = 8, height = 6)

# Done!
# As always, I am sure there are other ways of coding this, 
# but for me, this seems very easy and intuitive.
# And most importantly: it works and looks nice!
