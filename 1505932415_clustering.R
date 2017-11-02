# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

install.packages(c("cluster", "rattle.data","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle.data")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
df <- scale(wine[-1])

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(df) 
# 3 clusters?

# Exercise 2:
#   * How many clusters does this method suggest?
#### 3 clusters (a bend appears at 3 clusters on the X axis)
#   * Why does this method work? What's the intuition behind it?
#### After 3 clusters, the decrease of within groups sum of squares drops off.
#### The way k-means clustering works would suggest this decrease drop off as... 
#### a good way to tell what a likely number of clusters might be.
#   * Look at the code for wssplot() and figure out how it works
#### I don't think I quite get it.
#### A step-by-step run-through of the function would be helpful.

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?
#### This method seems to highly suggest 3 clusters once more.

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(df, 3)

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
ct.km <- table(wine$Type, fit.km$cluster)
ct.km

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
#### This wouldn't work properly for me
# * Would you consider this a good clustering?
#### I can't tell on my own. The library(flexclust) command doesn't work like in the example.

library(cluster)
clusplot() #I'm not sure what to put in here. 
# The documentation is confusing me. No data or values I have so far fit properly.
# "Error in as.vector(clus) : argument "clus" is missing, with no default"
# ???
