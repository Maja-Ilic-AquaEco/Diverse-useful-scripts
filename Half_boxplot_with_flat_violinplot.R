## Packages

library(ggplot2)
library(reshape2)
library(ggthemes)
library(cowplot)
library(grid)
library(gridExtra)


## Add your working directory here where you stored the original script for the flat violin plot
setwd("D:/Maja Ilic/Doktorarbeit Köln/R - Anwendung in der Ökologie/My functions")
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

g <- ggplot()
g <- g + geom_flat_violin(data=iris,aes(x=Species,y=Petal.Length,fill=Species),
                          position=position_nudge(0.05),
                          color="black",scale="count",trim=FALSE)  ## or TRUE
g

## Add the boxplot to your existing flat violinplot:

## Box

g <- g + geom_rect(data = mydata2, stat = "identity",
                      aes(xmin = as.numeric(Species) - 0.25,
                          ymin = q25,
                          xmax = as.numeric(Species) - 0.05,
                          ymax = q75,
                          color = Species),
                      fill = "white")

g

## Median

g <- g + geom_segment(data = mydata2, stat = "identity",
                      aes(x = as.numeric(Species) - 0.25,
                          y = median,
                          xend = as.numeric(Species) - 0.05,
                          yend = median,
                          color = Species),
                      size = 1)

g

## Vertical segment
g <- g + geom_segment(data = mydata2, stat = "identity",
                      aes(x = as.numeric(Species) - 0.05,
                          y = lower_whisker,
                          xend = as.numeric(Species) - 0.05,
                          yend = upper_whisker),
                      color = "black")

g

## Bottom horizontal segment

g <- g + geom_segment(data = mydata2, stat = "identity",
                      aes(x = as.numeric(Species) - 0.1,
                          y = lower_whisker,
                          xend = as.numeric(Species) - 0.05,
                          yend = lower_whisker),
                      color = "black")

g

## Top horizontal segment

g <- g + geom_segment(data = mydata2, stat = "identity",
                      aes(x = as.numeric(Species) - 0.1,
                          y = upper_whisker,
                          xend = as.numeric(Species) - 0.05,
                          yend = upper_whisker),
                      color = "black")

g

## Outliers
## The example here is pretty easy, as there are not many outliers

g <- g + geom_point(data = myout, stat = "identity",
                    aes(x = as.numeric(Species) - 0.05,
                        y = outlier,
                        fill = Species),
                    color = "black",
                    shape = 21,
                    size = 2)
g

## Save the plot

png("Half boxplot and flat violin plot.png", width = 15, height = 12, units = "cm", res = 600)
print(g)
dev.off()

