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
