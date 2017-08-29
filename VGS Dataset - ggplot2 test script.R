library(tidyverse)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(quantreg)

#histogram of Critic_Score
ggplot(vgdf_del) +
  geom_histogram(
                 aes(x = Critic_Score), 
                 binwidth = 1
                 )

#histogram of User_Score  
ggplot(vgdf_del) +
  geom_histogram(
                 aes(x = as.numeric(User_Score)), 
                 binwidth = 0.1
  )

#Jitter plot of Critic_Score against Global_Sales
ggplot(vgdf_del, aes(Critic_Score, Global_Sales)) +
  geom_jitter(alpha = 0.05)

#Refined Jitter
ggplot(vgdf_del, aes(Critic_Score, Global_Sales), 
  size=2, position = position_jitter(x = 2, y = 2)) +
  geom_jitter(colour = alpha("black", 0.15))

#Point and density - The density contour doesn't help at all
ggplot(vgdf_del, aes(Critic_Score, Global_Sales)) +
  geom_point(size=1) + geom_density2d()

#Scatter Plot
ggplot(vgdf_del, aes(Critic_Score, Global_Sales)) +
  geom_point()

#Line
ggplot(vgdf_del, aes(Critic_Score, Global_Sales)) +
  geom_line()

#Point + Smooth - This shows the correlation really well! There's not much!
ggplot(vgdf_del, aes(Critic_Score, Global_Sales)) +
  geom_point() +
  geom_smooth(method = "lm")
       
#Box Plot - I need to zoom in or remove outliers
ggplot(vgdf_del, aes(Year_of_Release, Critic_Score)) +
  geom_boxplot(aes(group = Year_of_Release))