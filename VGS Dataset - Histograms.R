library(tidyverse)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(quantreg)

#histogram of Critic_Score
ggplot(vgdf_del, aes(x = Critic_Score)) +
       geom_histogram(
                      aes(y = ..count..), 
                      binwidth = 1
                      ) +
       scale_x_continuous(name = "MetaCritic average (out of 100)") +
       scale_y_continuous(name = "Count")

#line histogram of Critic_Score
ggplot(vgdf_del, aes(x = Critic_Score)) +
       geom_freqpoly(
                aes(y = ..count..), 
                binwidth = 1
                ) +
       scale_x_continuous(name = "MetaCritic average (out of 100)") +
       scale_y_continuous(name = "Count")

#histogram with line borders of Critic_Score
ggplot(vgdf_del, aes(x = Critic_Score)) +
       stat_count(
                  aes(y = ..count..), 
                  binwidth = 1
                  ) +
       scale_x_continuous(name = "MetaCritic average (out of 100)") +
       scale_y_continuous(name = "Count")

#histogram of User_Score  
ggplot(vgdf, aes(x = as.numeric(User_Score)) ) +
       geom_histogram(
                      aes(y = ..count..), 
                      binwidth = 0.1
                      ) +
       scale_x_continuous(name = "MetaCritic average user score (out of 10)") +
       scale_y_continuous(name = "Count")

#deleted value histogram with normal curve and density gradient for Critic_Score
gd <- ggplot(vgdf_del, aes(x=Critic_Score))
gd <- gd + geom_histogram(binwidth=2, colour="black", 
                          aes(y=..density.., fill=..count..))
gd <- gd + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
gd <- gd + stat_function(fun=dnorm,
                         color="red",
                         args=list(mean=mean(vgdf_del$Critic_Score), 
                                   sd=sd(vgdf_del$Critic_Score)))

gd

#mean value histogram with normal curve and density gradient for Critic_Score
#I'm having trouble with the mean score replacing the other values. This messes up the histogram!
gm <- ggplot(vgdf_mean, aes(x=Critic_Score))
gm <- gm + geom_histogram(binwidth=2, colour="black", 
                          aes(y=..density.., fill=..count..))
gm <- gm + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
gm <- gm + stat_function(fun=dnorm,
                         color="red",
                         args=list(mean=mean(vgdf_mean$Critic_Score), 
                                   sd=sd(vgdf_mean$Critic_Score)))

gm

#histogram with normal curve and density gradient for User_Score (too chunky)
gu <- ggplot(vgdf_mean, aes(x=as.numeric(User_Score)))
gu <- gu + geom_histogram(binwidth=2, colour="black", 
                          aes(y=..density.., fill=..count..))
gu <- gu + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
gu <- gu + stat_function(fun=dnorm,
                         color="red",
                         args=list(mean=mean(as.numeric(vgdf_del$User_Score)), 
                                   sd=sd(as.numeric(vgdf_del$User_Score))))

gu

#histogram for Global_Sales with density curve
#This histogram doesn't work because most games don't sell anywhere near 
#where the highest games do. I'd need to compress the visualization or 
#remove outliers. Same with NA_Sales
gs <- ggplot(vgdf_del, aes(x=Global_Sales))
gs <- gs + geom_histogram(binwidth=2, colour="black", 
                          aes(y=..density.., fill=..count..))
gs <- gs + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
gs <- gs + stat_function(fun=dnorm,
                         color="red",
                         args=list(mean=mean(vgdf_del$Global_Sales), 
                                   sd=sd(vgdf_del$Global_Sales)))

gs

#Same as above, but replace Global_Sales with log of Global_Sales.
#This looks much closer to a normal distribution!
gl <- ggplot(vgdf_del, aes(x=log(Global_Sales)))
gl <- gl + geom_histogram(binwidth=2, colour="black", 
                          aes(y=..density.., fill=..count..))
gl <- gl + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
gl <- gl + stat_function(fun=dnorm,
                         color="red",
                         args=list(mean=mean(vgdf_del$Global_Sales), 
                                   sd=sd(vgdf_del$Global_Sales)))

gl