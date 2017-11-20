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

no_outlier <- vgdf %>%
  filter(Name != "Wii Sports")

#Point + Smooth again without Wii Sports (an obvious outlier)
ggplot(no_outlier, aes(Critic_Score, Global_Sales)) +
  geom_point() +
  geom_smooth(method = "lm")

#Refined Jitter
ggplot(no_outlier, aes(Critic_Score, Global_Sales), 
       size=2, position = position_jitter(x = 2, y = 2)) +
  geom_jitter(colour = alpha("black", 0.15))

#Point and density - The density contour doesn't help at all
ggplot(no_outlier, aes(Critic_Score, Global_Sales)) +
  geom_point(size=1) + geom_density2d()

#Scatter Plot
ggplot(no_outlier, aes(Critic_Score, Global_Sales)) +
  geom_point()

#Line
ggplot(no_outlier, aes(Critic_Score, Global_Sales)) +
  geom_line()

#Box Plot - I need to zoom in or remove outliers
ggplot(no_outlier, aes(Year_of_Release, Critic_Score)) +
  geom_boxplot(aes(group = Year_of_Release))


###############
### P-tests ###
###############

xbar = mean(vgdf$Critic_Score) # mean metascore
xbar
mu0 = 70 # I estimate that the average review score is 70
sigma = sd(vgdf$Critic_Score) # standard deviation of metascores
n = 7017  # sample size 
z <- (xbar - mu0)/(sigma/sqrt(n))
z   # test statistic 

alpha = .05 
z.half.alpha = qnorm(1-alpha/2) 
c(-z.half.alpha, z.half.alpha) 

pval = 2 * pnorm(z)    # lower tail 
pval                   # two−tailed p−value 

# Value must be greater than 0.05
# 1.507636 is between -1.959964 and 1.959964, so we accept the hypothesis!

xbar = mean(vgdf$User_Score) # mean user score
xbar
mu0 = 7 # I estimate that the average review score is 7
sigma = sd(vgdf$User_Score) # standard deviation of metascores
n = 7017  # sample size 
z <- (xbar - mu0)/(sigma/sqrt(n))
z   # test statistic 

alpha = .05 
z.half.alpha = qnorm(1-alpha/2) 
c(-z.half.alpha, z.half.alpha) 

pval = 2 * pnorm(z)    # lower tail 
pval                   # two−tailed p−value 

# Value must be greater than 0.05
# 10.60307 is not between -1.959964 and 1.959964, so we deny the hypothesis!

xbar = mean(vgdf$User_Score) # mean user score
xbar
mu0 = 7.2 # I estimate that the average review score is 7.2
sigma = sd(vgdf$User_Score) # standard deviation of metascores
n = 7017  # sample size 
z <- (xbar - mu0)/(sigma/sqrt(n))
z   # test statistic 

alpha = .05 
z.half.alpha = qnorm(1-alpha/2) 
c(-z.half.alpha, z.half.alpha) 

pval = 2 * pnorm(z)    # lower tail 
pval                   # two−tailed p−value 

# Value must be greater than 0.05
# -1.021294 is between -1.959964 and 1.959964, so we accept the hypothesis!

#...possibly add more. These are waaaay basic tests.



##################
### Clustering ###
##################

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

#create dataset with only numbers
vgdf_nums <- vgdf
vgdf_nums$Name = NULL
vgdf_nums$Platform = NULL
vgdf_nums$Year_of_Release = NULL
vgdf_nums$Genre = NULL
vgdf_nums$Publisher = NULL
vgdf_nums$Developer = NULL
vgdf_nums$Rating = NULL

vgdf_clust <- scale(vgdf_nums)
wssplot(vgdf_clust)

#3 clusters seem like the right choice here...

library(NbClust)
set.seed(1234)
nc <- NbClust(vgdf_nums, min.nc=2, max.nc=15, method="kmeans")
# This took about 15 minutes or so to process. To save time, it output as:
#
# [1] "Frey index : No clustering structure in this data set"
#*** : The Hubert index is a graphical method of determining the number of clusters.
#In the plot of Hubert index, we seek a significant knee that corresponds to a 
#significant increase of the value of the measure i.e the significant peak in Hubert
#index second differences plot. 
#
#*** : The D index is a graphical method of determining the number of clusters. 
#In the plot of D index, we seek a significant knee (the significant peak in Dindex
#second differences plot) that corresponds to a significant increase of the value of
#the measure. 

#******************************************************************* 
#  * Among all indices:                                                
#  * 5 proposed 2 as the best number of clusters 
#* 8 proposed 3 as the best number of clusters 
#* 2 proposed 4 as the best number of clusters 
#* 2 proposed 8 as the best number of clusters 
#* 1 proposed 9 as the best number of clusters 
#* 1 proposed 10 as the best number of clusters 
#* 1 proposed 14 as the best number of clusters 
#* 3 proposed 15 as the best number of clusters 
#
#***** Conclusion *****                            
#  
#  * According to the majority rule, the best number of clusters is  3 
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")
# Seems like 3 clusters would be best. :)

# To simplify, let's cluster just the Critic_Score column.

Critic_Score <- vgdf$Critic_Score
CS <- scale(Critic_Score)
wssplot(CS) #again, it looks like 3 clusters might be best.
CS_clust <- NbClust(CS, min.nc=2, max.nc=15, method="kmeans")
# This also took about 25 minutes. Still looks like 3 clusters is best.
# Results are below.
#
# * Among all indices:                                                
#* 1 proposed 2 as the best number of clusters 
#* 2 proposed 3 as the best number of clusters 
#* 1 proposed 7 as the best number of clusters 
#* 1 proposed 11 as the best number of clusters 
#* 1 proposed 15 as the best number of clusters 
#***** Conclusion *****                            
#* According to the majority rule, the best number of clusters is  3

# Let's cluster!

fit.km.vgdf <- kmeans(vgdf_nums, 3)

cs.km <- table(vgdf$Critic_Score, fit.km.vgdf$cluster)
cs.km

library(cluster)
clusplot(vgdf_nums, vgdf_nums$Critic_Score, color = TRUE, shade = TRUE, plotchar = FALSE)

# The clustering here almost looks random. I'm not sure if this is helpful or not.
# Maybe I messed something up.


###################
### Predictions ###
###################

vglm <- lm(Global_Sales ~ Critic_Score + User_Score + Critic_Count + User_Count, data = no_outlier)
# Very low p-value, but I'm getting 2-3 * significance codes. Hm.

vgpoly <- lm(Global_Sales ~ Critic_Score + I(Critic_Score^2) + I(Critic_Score^3), data = no_outlier)
# not familiar with this style (polynomial?) of prediction, but possibly better predictions?

### My testing below ###

summary(vgdf)
#doesn't work because of character strings

# summary of Global_Sales and Critic_Score columns, all rows
sales.critic <- subset(no_outlier, select = c("Critic_Score", "Global_Sales"))
summary(sales.critic)

# correlation between Global_Sales and Critic_Score (0.2369535)
cor(sales.critic)

#plot the data
plot(sales.critic)

# Fit our regression model
sales.mod <- lm(Global_Sales ~ Critic_Score, # regression formula
              data=no_outlier) # data set

sales.modw <- lm(Global_Sales ~ Critic_Score,
                 data=no_outlier,
                 weights = Critic_Count) # added weight

# Summarize and print the results (high level of significance!)
summary(sales.mod) # show regression coefficients table

sales.mod2 <- lm(Global_Sales ~ Critic_Score + User_Score, data = no_outlier)
summary(sales.mod2)
# Both are ***!

sales.modw2 <- lm(Global_Sales ~ Critic_Score + User_Score,
                 data=no_outlier,
                 weights = Critic_Count + User_Count) # added weight

confint(sales.mod)
confint(sales.mod2)
confint(sales.modw)
confint(sales.modw2)
# Not sure what to make of these...
# Maybe Critic_Score is a better predictor than User_Score?

plot(sales.mod)
# lots of info here...

plot(sales.mod2)
# same. Again, not sure what to make of this info.

anova(sales.mod, sales.mod2)
# Model with more data seems better here.

sales.mod3 <- lm(Global_Sales ~ Critic_Score + User_Score + Critic_Count + User_Count, data = no_outlier)
summary(sales.mod3)
# All data is useful, but User_Score is least useful

anova(sales.mod, sales.mod2, sales.mod3)
# Still, 2 and 3 are the best models

# Now, let's add some categorical variables to the model

str(vgdf)

vgdf_mod <- no_outlier

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

anova(sales.mod, sales.mod2, sales.mod3, sales.mod4, sales.modw, sales.modw2)
# Models 3 and 4 seem like our best bets, but 3 is much simpler. Hm.
# I later added models with weights taken from the amount of reviewers and users...
# Model 2 appears best in this case with models 3-5 also showing significance.

########################
### Model Evaluation ###
########################

library(mlbench)
library(caret)
library(boot)

set.seed(7)

attach(no_outlier)

MSE_LOOCV <- cv.glm(vgdf_nums, sales.mod2)
MSE_LOOCV
# delta = NaN?
# if this works later, make a for loop

MSE_10_FOLD_CV = NULL

for (i in 1:10){
  model = glm(Global_Sales ~ poly(Critic_Score + User_Score, i), data = vgdf_nums)
  MSE_10_FOLD_CV[i] <- cv.glm(vgdf, model, K = 10)$delta[1]
}
MSE_10_FOLD_CV
# the MSE are all pretty similar. The model isn't stellar, but maybe it's okay?

MSE_5_FOLD_CV = NULL

for (i in 1:10){
  model = glm(Global_Sales ~ poly(Critic_Score + User_Score, i), data = vgdf_nums)
  MSE_5_FOLD_CV[i] <- cv.glm(vgdf, model, K = 5)$delta[1]
}
MSE_5_FOLD_CV
# If K = 5 or 10, the results are very close to each other. This is a good indication of a reliable model.

