## Packages

library(ggplot2)
library(reshape2)
library(ggthemes)
library(cowplot)
library(grid)
library(gridExtra)


## Add your working directory here where you stored the original script for the flat violin plot
#setwd("...")
source("geom_flat_violin.R")

## Use iris dataset for practice

data(iris)
str(iris)

## ggplot with a whole boxplot + flat violinplot
## You will probably have to adjust position_nudge and width a bit...

g <- ggplot(iris,aes(x=Species,y=Petal.Length))
g <- g + geom_flat_violin(aes(fill=Species),color="black",position=position_nudge(0.15),scale="count",trim=FALSE)  ## or TRUE
g <- g + geom_boxplot(aes(color=Species),width=0.5,position=position_nudge(-0.15))
g

##------------------------------------------------------------------------##

## ggplot with a half boxplot + flat violinplot
## Adding the boxplot is somewhat tricky and a lot of it has to be done "manually"
## Perhaps there is a better way, but this works for me.
## You can also check this out:
## https://stackoverflow.com/questions/49003863/how-to-plot-a-hybrid-boxplot-half-boxplot-with-jitter-points-on-the-other-half
## I first calculated the median, the quantiles etc.
## Note that I calculated min and max instead of lower and upper whiskers

mydata <- iris %>% group_by(Species) %>% 
  summarize(median = median(Petal.Length),
            q25 = quantile(Petal.Length,0.25),
            q75 = quantile(Petal.Length,0.75),
            min = min(Petal.Length),
            max = max(Petal.Length))
mydata

## Alternatively, you can also get the necessary data this way:

mybox <- boxplot(iris$Petal.Length ~ iris$Species)
mybox$stats

## [,1] = setosa
## [,2] = versicolor
## [,3] = virginica

## [1,] = lower whisker
## [2,] = lower hinge (25% quartile)
## [3,] = median
## [4,] = upper hinge (75% quartile)
## [5,] = upper whisker

## I will use this second option and reorganize the data to use them for the ggplot
mydata2 <- as.data.frame(mybox$stats)
names(mydata2) <- levels(iris$Species)
rownames(mydata2) <- c("lower_whisker","q25","median","q75","upper_whisker")
mydata2 <- t(mydata2)
mydata2 <- as.data.frame(mydata2)
mydata2$Species <- as.factor(rownames(mydata2))
mydata2

## To get the outliers, you can do the following:
mybox$out; mybox$group

## This means, outlier 1 belongs to setosa, and outlier 3 to versicolor

myout <- data.frame(outlier= c(mybox$out,NA),Species=c("setosa","versicolor","virginica"))
myout

## Note that ggplot and base boxplot calculate the outliers etc. a bit differently
## I will however not go into detail here, but just be aware of the data you are plotting

## Plot

## Start with the violin plot first (notice it is different than above!)
## I will already add some extra features (plot and panel background color etc.)

## Plot appearance inspired by great plots from Cédric Scherer
## https://github.com/Z3tt/TidyTuesday/blob/master/plots/2019_28/2019_28_FIFA_WWCs.png
## https://github.com/Z3tt/TidyTuesday/blob/master/plots/2019_27/2019_27_FranchiseRevenue.png

# Colors ("cold version")
mycolors <- c(rgb(211,241,167,max=255),
              rgb(100,183,137,max=255),
              rgb(22,79,99,max=255))

# Colors ("warm version")
# mycolors <- c(rgb(215,136,8,max=255),
#               rgb(112,0,0,max=255),
#               rgb(157,89,49,max=255))

## Image of different iris species
## Iris setosa: https://www.plant-world-seeds.com/store/view_seed_item/6010
## Iris versicolor: https://www.plant-world-seeds.com/store/view_seed_item/3664
## Iris virginica: https://www.fs.fed.us/wildflowers/beauty/iris/Blue_Flag/iris_virginica.shtml

img_iris_set <- png::readPNG("./Iris setosa.png") 
iris_set <- grid::rasterGrob(img_iris_set, interpolate = T)

img_iris_vers <- png::readPNG("./Iris versicolor.png") 
iris_vers <- grid::rasterGrob(img_iris_vers, interpolate = T)

img_iris_virg <- png::readPNG("./Iris virginica.png") 
iris_virg <- grid::rasterGrob(img_iris_virg, interpolate = T)

##

g <- ggplot()
g <- g + geom_flat_violin(data=iris,aes(x=Species,y=Petal.Length,fill=Species),
                          position=position_nudge(0.05),
                          color="white",scale="count",trim=FALSE)  ## or TRUE
g <- g + theme_bw()
g <- g + theme(plot.background = element_rect(fill = rgb(51,51,51,max=255)),
               panel.background = element_rect(fill = rgb(51,51,51,max=255), colour = "white",
                                               size = 2, linetype = "solid"),
               panel.grid = element_blank(),
               axis.title.y = element_text(size=30,color="white",face="bold",margin=margin(0,15,0,0)),
               axis.text.y = element_text(size=26,color="white"),
               axis.text.x = element_text(size=26,color="white",face="italic"),
               axis.ticks = element_line(color="white"),
               legend.position = "none")
g <- g + labs(x="",y="Petal length (cm)")
g <- g + scale_x_discrete(breaks=c("setosa","versicolor","virginica"),
                          labels=c("Iris setosa","Iris versicolor","Iris virginica"))
g <- g + scale_y_continuous(limits=c(0,10),exp=c(0,0))
g <- g + scale_fill_manual(values=mycolors)
g <- g + scale_color_manual(values=mycolors)
g <- g + annotation_custom(iris_set, xmin = 0.8, xmax = 1.2, ymin = 2.75, ymax = 3.9) 
g <- g + annotation_custom(iris_vers, xmin = 1.8, xmax = 2.2, ymin = 6.2, ymax = 7.35)
g <- g + annotation_custom(iris_virg, xmin = 2.75, xmax = 3.25, ymin = 8.25, ymax = 9.5) 
g

## Add the boxplot to your existing flat violinplot:

## Box

g <- g + geom_rect(data = mydata2, stat = "identity",
                      aes(xmin = as.numeric(Species) - 0.25,
                          ymin = q25,
                          xmax = as.numeric(Species) - 0.05,
                          ymax = q75,
                          fill = Species),
                   color = "white")

g

## Median

g <- g + geom_segment(data = mydata2, stat = "identity",
                      aes(x = as.numeric(Species) - 0.25,
                          y = median,
                          xend = as.numeric(Species) - 0.05,
                          yend = median),
                      color = "white",
                      size = 1)

g

## Vertical segment
g <- g + geom_segment(data = mydata2, stat = "identity",
                      aes(x = as.numeric(Species) - 0.05,
                          y = lower_whisker,
                          xend = as.numeric(Species) - 0.05,
                          yend = upper_whisker),
                      color = "white")

g

## Bottom horizontal segment

g <- g + geom_segment(data = mydata2, stat = "identity",
                      aes(x = as.numeric(Species) - 0.1,
                          y = lower_whisker,
                          xend = as.numeric(Species) - 0.05,
                          yend = lower_whisker),
                      color = "white")

g

## Top horizontal segment

g <- g + geom_segment(data = mydata2, stat = "identity",
                      aes(x = as.numeric(Species) - 0.1,
                          y = upper_whisker,
                          xend = as.numeric(Species) - 0.05,
                          yend = upper_whisker),
                      color = "white")

g

## Outliers
## The example here is pretty easy, as there are not many outliers

g <- g + geom_point(data = myout, stat = "identity",
                    aes(x = as.numeric(Species) - 0.05,
                        y = outlier,
                        fill = Species),
                    color = "white",
                    shape = 21,
                    size = 1.5)
g

## Save the plot

png("Half boxplot and flat violin plot cold.png", width = 13, height = 10, units = "cm", res = 300)
print(g)
dev.off()

# png("Half boxplot and flat violin plot warm.png", width = 13, height = 10, units = "cm", res = 300)
# print(g)
# dev.off()

