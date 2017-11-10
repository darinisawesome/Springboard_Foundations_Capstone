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

#histogram of Critic_Score
ggplot(vgdf, aes(x = Critic_Score)) +
  geom_histogram(
    aes(y = ..count..), 
    binwidth = 1
  ) +
  scale_x_continuous(name = "MetaCritic average (out of 100)") +
  scale_y_continuous(name = "Count")

#histogram with normal curve and density gradient for Critic_Score
nchg <- ggplot(vgdf, aes(x=Critic_Score))
nchg <- nchg + geom_histogram(binwidth=2, colour="black", 
                          aes(y=..density.., fill=..count..))
nchg <- nchg + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
nchg <- nchg + stat_function(fun=dnorm,
                         color="red",
                         args=list(mean=mean(vgdf$Critic_Score), 
                                   sd=sd(vgdf$Critic_Score)))

nchg

#scaling of User_Score to match Critic_Score
vgdf$User_Score_num = as.numeric(as.character(vgdf$User_Score)) *10

#histogram of User_Score_num (a scaled value for User_Score)
ggplot(vgdf, aes(x = User_Score_num)) +
  geom_histogram(
    aes(y = ..count..), 
    binwidth = 1
  ) +
  scale_x_continuous(name = "MetaCritic average user score (*10)") +
  scale_y_continuous(name = "Count")

#Point + Smooth - This shows the correlation really well! There's not much!
ggplot(vgdf, aes(Critic_Score, Global_Sales)) +
  geom_point() +
  geom_smooth(method = "lm")

#Refined Jitter
ggplot(vgdf, aes(Critic_Score, Global_Sales), 
       size=2, position = position_jitter(x = 2, y = 2)) +
  geom_jitter(colour = alpha("black", 0.15))

#Point and density - The density contour doesn't help at all
ggplot(vgdf, aes(Critic_Score, Global_Sales)) +
  geom_point(size=1) + geom_density2d()

#Scatter Plot
ggplot(vgdf, aes(Critic_Score, Global_Sales)) +
  geom_point()

#Line
ggplot(vgdf, aes(Critic_Score, Global_Sales)) +
  geom_line()

#Box Plot - I need to zoom in or remove outliers
ggplot(vgdf, aes(Year_of_Release, Critic_Score)) +
  geom_boxplot(aes(group = Year_of_Release))

###################
### Predictions ###
###################

vglm <- lm(Global_Sales ~ Critic_Score + User_Score + Critic_Count + User_Count, data = vgdf)
# very poor coeffecients. I'm not sure what to do here.

vgpoly <- lm(Global_Sales ~ Critic_Score + I(Critic_Score^2) + I(Critic_Score^3), data = vgdf)
# not familiar with this style (polynomial?) of prediction, but possibly better predictions?

### My testing below ###

summary(vgdf)
#doesn't work because of character strings

# summary of Global_Sales and Critic_Score columns, all rows
sales.critic <- subset(vgdf, select = c("Critic_Score", "Global_Sales"))
summary(sales.critic)

# correlation between Global_Sales and Critic_Score (0.2369535)
cor(sales.critic)

#plot the data
plot(sales.critic)

# Fit our regression model
sales.mod <- lm(Global_Sales ~ Critic_Score, # regression formula
              data=sales.critic) # data set

# Summarize and print the results (high level of significance!)
summary(sales.mod) # show regression coefficients table

sales.mod2 <- lm(Global_Sales ~ Critic_Score + User_Score, data = vgdf)
summary(sales.mod2)
# Both are ***!

confint(sales.mod)
confint(sales.mod2)
# Not sure what to make of these...
# Maybe Critic_Score is a better predictor than User_Score?

plot(sales.mod)
# lots of info here...

plot(sales.mod2)
# same. Again, not sure what to make of this info.

anova(sales.mod, sales.mod2)
# Model with more data seems better here.

sales.mod3 <- lm(Global_Sales ~ Critic_Score + User_Score + Critic_Count + User_Count, data = vgdf)
summary(sales.mod3)
# All data is useful, but User_Score is least useful

anova(sales.mod, sales.mod2, sales.mod3)
# Still, 2 and 3 are the best models

# Now, let's add some categorical variables to the model

str(vgdf)

vgdf_mod <- vgdf

vgdf_mod$Platform <- factor(vgdf$Platform)
vgdf_mod$Year_of_Release <- factor(vgdf$Year_of_Release)
vgdf_mod$Genre <- factor(vgdf$Genre)
vgdf_mod$Publisher <- factor(vgdf$Publisher)

str(vgdf_mod)
# Now, I can use these factors for predictions!

sales.mod4 <- lm(Global_Sales ~ Critic_Score + User_Score + Critic_Count + 
                   User_Count + Platform + Year_of_Release + Genre + Publisher, 
                 data = vgdf_mod)
summary(sales.mod4)
# Lots of data here. Some categories more important than others.

anova(sales.mod, sales.mod2, sales.mod3, sales.mod4)
# Models 3 and 4 seem like our best bets, but 3 is much simpler. Hm.

########################
### Model Evaluation ###
########################

library(mlbench)
library(caret)
library(boot)

set.seed(7)

attach(vgdf)

MSE_LOOCV <- cv.glm(vgdf, sales.mod3)
MSE_LOOCV
# delta = NaN?
# if this works later, make a for loop

MSE_10_FOLD_CV = NULL

for (i in 1:10){
  model = glm(Global_Sales ~ poly(Critic_Score + User_Score + Critic_Count + User_Count, i), data = vgdf)
  MSE_10_FOLD_CV[i] <- cv.glm(vgdf, model, K = 10)$delta[1]
}
MSE_10_FOLD_CV
# the MSE are all pretty similar. The model isn't stellar, but maybe it's okay?

