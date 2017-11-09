#########################
### Data Preparation  ###
#########################

#load necessary libraries
library(tidyverse)
library(dplyr)
library(readr)
library(tidyr)

#load video game sales dataset
vgdf <- read_csv("~/Github/Springboard_Foundations_Capstone/Video_Games_Sales_as_at_22_Dec_2016_original.csv")

#Are there NA values in each column, and if so, how many?

vgdf %>% 
  select_if(function(x) any(is.na(x))) %>% 
  summarise_all(funs(sum(is.na(.)))) -> vgdf_NA_count

#According to vgdf_NA_count, there are two games with NA titles.

#When these games are inspected with vgdf %>% filter(is.na(Name)), 
#you can see that these games also have NA values for Critic_Score and Critic_Count

#That said, I'll remove these two rows from the dataset

vgdf <- vgdf %>%
  filter(!is.na(Name))

vgdf %>% 
  select_if(function(x) any(is.na(x))) %>% 
  summarise_all(funs(sum(is.na(.)))) -> vgdf_NA_count

#When I run the vgdf_NA_count function again, I see that 
#removing these two rows removed a few more NA values from the NA count

str(vgdf) 

#User_Count and User_Score are chr, so I'll convert them to numeric 
vgdf$User_Count<-as.numeric(as.character(vgdf$User_Count))
vgdf$User_Score<-as.numeric(as.character(vgdf$User_Score))

#Next, I create a dataframe with only games that have NA values for Critic_Score

NA_Critic_Score_df <- vgdf %>% 
  filter(is.na(Critic_Score))

#Reviewing this dataframe shows that these games have sales data (NA_Sales, EU_Sales), 
#but no critic score (Critic_Score) data, which makes them somewhat useless for the task at hand.
#Some of these games have user scores (User_Score), so...

#Remove rows without critic or user scores or counts

vgdf <- vgdf %>% 
  filter(!is.na(Critic_Score) & !is.na(User_Score) & !is.na(Critic_Count) & !is.na(User_Count))

#This has removed nearly all the NA values!

vgdf %>% 
  select_if(function(x) any(is.na(x))) %>% 
  summarise_all(funs(sum(is.na(.)))) -> vgdf_NA_count

#Running the NA count function again, we can see there are still
# 4 NAs under the "Developer" column and
# 70 NAs under the "Rating" column.
#I'll replace them just for completion's sake.

vgdf <- vgdf %>%
  mutate(Developer = replace(Developer, is.na(Developer), "Unknown")) %>%
  mutate(Rating = replace(Rating, is.na(Rating), "Unrated"))

vgdf %>% 
  select_if(function(x) any(is.na(x))) %>% 
  summarise_all(funs(sum(is.na(.)))) -> vgdf_NA_count

#After all this, no NA values remain!
#As proof, vgdf_NA_count has 7017 rows with no variables.

#Output to clean csv file
write_csv(vgdf, "Video_Games_Sales_as_at_22_Dec_2016_clean-delete.csv")

######################################### 
### Data Visualization & Exploration  ###
#########################################
library(ggplot2)
library(quantreg)

# Below are loaded datasets cleaned in the same way as above except...
# vgdf_mean, when possible, has the NA values replaced with a mean of all other values.
# vgdf_mean is useful for machine learning algorithms, though it doesn't display...
# as well when a histogram is made as can be seen in the visualizations below.

vgdf_del <- read_csv("~/Github/Springboard_Foundations_Capstone/Video_Games_Sales_as_at_22_Dec_2016_clean-delete.csv")
vgdf_mean <- read_csv("~/Github/Springboard_Foundations_Capstone/Video_Games_Sales_as_at_22_Dec_2016_clean-mean.csv")

#histogram of Critic_Score
ggplot(vgdf_del, aes(x = Critic_Score)) +
  geom_histogram(
    aes(y = ..count..), 
    binwidth = 1
  ) +
  scale_x_continuous(name = "MetaCritic average (out of 100)") +
  scale_y_continuous(name = "Count")

#histogram with normal curve and density gradient for Critic_Score
nchg <- ggplot(vgdf_del, aes(x=Critic_Score))
nchg <- nchg + geom_histogram(binwidth=2, colour="black", 
                          aes(y=..density.., fill=..count..))
nchg <- nchg + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
nchg <- nchg + stat_function(fun=dnorm,
                         color="red",
                         args=list(mean=mean(vgdf_del$Critic_Score), 
                                   sd=sd(vgdf_del$Critic_Score)))

nchg

#scaling of User_Score to match Critic_Score
vgdf_del$User_Score_num = as.numeric(as.character(vgdf_del$User_Score)) *10

#histogram of User_Score_num (a scaled value for User_Score)
ggplot(vgdf_del, aes(x = User_Score_num)) +
  geom_histogram(
    aes(y = ..count..), 
    binwidth = 1
  ) +
  scale_x_continuous(name = "MetaCritic average user score (*10)") +
  scale_y_continuous(name = "Count")

#Point + Smooth - This shows the correlation really well! There's not much!
ggplot(vgdf_del, aes(Critic_Score, Global_Sales)) +
  geom_point() +
  geom_smooth(method = "lm")

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

#Box Plot - I need to zoom in or remove outliers
ggplot(vgdf_del, aes(Year_of_Release, Critic_Score)) +
  geom_boxplot(aes(group = Year_of_Release))

###################
### Predictions ###
###################

# I'll use vgdf_mean here in order to have more data for training my model

vglm <- lm(Global_Sales ~ Critic_Score + User_Score + Critic_Count + User_Count, data = vgdf_mean)

vgpoly <- lm(Global_Sales ~ Critic_Score + I(Critic_Score^2) + I(Critic_Score^3), data = vgdf_mean)

