# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

# install.packages(c("cluster", "rattle.data","NbClust"))
library("cluster")
library("NbClust")
library("rattle.data")
# Now load the data and look at the first few rows
data(wine, package="rattle.data")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
df <- wine[, c(2:13)]
df <- scale(df, center = TRUE, scale = TRUE)

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

# Exercise 2:
#   * How many clusters does this method suggest?
##      The biggest "bend" in the chart comes at 3 clusters
#   * Why does this method work? What's the intuition behind it?
##      The biggest "bend" indicates the point at which the rate of change in the sum of squares decreases the
##      most and therefore where the improvement becomes less marked.
#   * Look at the code for wssplot() and figure out how it works

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Number of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?
##    Again the method points towards 3 clusters (although 2 clusters also has a high representation)

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(df, centers = 3, iter.max = 150, nstart = 5 )

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

table(wine$Type, fit.km$cluster)

##  Clearly here there is a misalignment of naming - Cluster 2 should relate to wine$Type 1.  Once this is 
##  taken into account we see that there are 13 wrong predictions out of 178 items which seems a pretty good
##  start.

clean_cat <- as.data.frame(matrix(data = c(wine$Type, fit.km$cluster), nrow = 178, ncol = 2))
names(clean_cat) <- c("Wine.Type","Cluster")
clean_cat$Cluster[clean_cat$Cluster == 1] <- 4
clean_cat$Cluster[clean_cat$Cluster == 2] <- 1
clean_cat$Cluster[clean_cat$Cluster == 4] <- 2

table(wine$Type, clean_cat$Cluster)

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

clusplot(df, clean_cat$Cluster, main = "2D Representation of the cluster data", color = TRUE, shade = TRUE, 
         lines = 0)

##  Ideally I would like to see distinct clusters with no overlapping.  However here there are a couple of
##  intersections implying that some points may be more ambiguous with regard to their clustering.

