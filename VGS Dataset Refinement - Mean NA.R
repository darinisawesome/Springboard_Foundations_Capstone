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

#When I run the vgdf_NA_count function again, I see that 
#removing these two rows removed a few more NA values from the NA count

str(vgdf) 

#User_Count and User_Score are chr, so I'll convert them to numeric 
vgdf$User_Count<-as.numeric(as.character(vgdf$User_Count))
vgdf$User_Score<-as.numeric(as.character(vgdf$User_Score))

#Next, I create a dataframe with only games that have NA values for Critic_Score

NA_Critic_Score_df <- vgdf %>% 
  filter(is.na(Critic_Score))

#Reviewing this dataframe shows that these games almost universally
#have sales data (NA_Sales, EU_Sales), but no critic score (Critic_Score) data.

#In order to preserve data, I'm going to replace NA values 
#with a mean of all the other numerical values:

#This replaces all the NA values in Critic_Score with the mean of all the other values.
vgdf <- vgdf %>%
  mutate(Critic_Score = replace(Critic_Score, is.na(Critic_Score), mean(Critic_Score, na.rm=TRUE)))

#This one is a little trickier, but it coerces string values ie("tbd", etc.) to NA, then replaces all
#NA values with the mean of the other values.  
vgdf <- vgdf %>%
  mutate(User_Score = replace(User_Score, is.na(User_Score), mean(as.numeric(User_Score), na.rm=TRUE)))

#This replaces Critic_Count NA values with 0 as there are either no critic reviews or 
#there aren't enough for a Critic_Score to have been created. 
#A mean replacement wouldn't make sense here.
vgdf <- vgdf %>%
  mutate(Critic_Count = replace(Critic_Count, is.na(Critic_Count), 0))

#Same as above but for User_Count.
vgdf <- vgdf %>%
  mutate(User_Count = replace(User_Count, is.na(User_Count), 0))

#After this, only Developer and Rating have NA values which shouldn't affect our main purposes here,
#but let's replace them with something anyway just to full clean up the set

vgdf <- vgdf %>%
  mutate(Developer = replace(Developer, is.na(Developer), "Unknown")) %>%
  mutate(Rating = replace(Rating, is.na(Rating), "Unrated"))

#After all this, no NA values remain!

#Output to clean csv file
write_csv(vgdf, "Video_Games_Sales_as_at_22_Dec_2016_clean-mean.csv")
