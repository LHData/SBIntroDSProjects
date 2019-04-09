# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

install.packages(c("cluster", "rattle.data","NbClust"))

# Now load the data and look at the first few rows
library(cluster)
library(rattle.data)
library(NbClust)
data(wine, package="rattle.data")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
wine[1] = NULL
wineScaled = scale(wine)


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

wssplot(wineScaled)

# Exercise 2:
#   * How many clusters does this method suggest?
#   * Why does this method work? What's the intuition behind it?
#   * Look at the code for wssplot() and figure out how it works
#My answers: 
#   * This method suggests using three clusters. 
#   * This method works because the graph bends at the point when the sum of squares for the previous point 
#      is very different compared to sum of squares for the next point, meaning that the 
#      difference between clusters flattens out beyond that point
#   * The wssplot function runs the kmeans function for 2 through 15 clusters and 
#     plots the output of the within cluster sum of squares by the number of clusters 

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(wineScaled, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Number of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?
# The output of this function says "According to the majority rule, the best number of clusters is also 3"
#   Also, the bar chart shows that the "number of criteria" graph peaks at 3 clusters

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(wineScaled, centers = 3)

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
#Add Type back into the wine data set
data(wine, package="rattle.data")
#look at the table
table(wine$Type, fit.km$cluster)
#I think this is a pretty good model. It looks like it miscegorized only 6 of 178 rows.

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

clusplot(wine, fit.km$cluster)
# I think this is a good model, but it's not perfect. There two  points on the border of the
#   cluster in the upper right that look like they might belong better in the bottom cluster. 
#   Otherwise, it looks really good. 