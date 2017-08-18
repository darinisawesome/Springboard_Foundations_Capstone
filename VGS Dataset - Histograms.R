library(tidyverse)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(quantreg)

#histogram of Critic_Score
ggplot(vgdf, aes(x = Critic_Score)) +
       geom_histogram(
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