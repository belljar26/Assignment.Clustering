# Clustering Assignment
library(e1071)
library(ggplot2)
house_votes_Dem <- read_csv("house_votes_Dem.csv")
View(house_votes_Dem)
# Subset columns for clustering
clust_Data_dem <- house_votes_Dem[,c(3:5)]
View(clust_Data_dem)
#set seed and choose 2 centers
set.seed(1)
kmeans_obj_Dem <- kmeans(clust_Data_dem, centers=2)
kmeans_obj_Dem
head(kmeans_obj_Dem)
party_clusters_Dem <- as.factor(kmeans_obj_Dem$cluster)
View(party_clusters_Dem)
ggplot(house_votes_Dem, aes(x = aye, 
                            y = nay,
                            shape = party_clusters_Dem)) + 
  geom_point(size = 6) +
  ggtitle("Aye vs. Nay votes for Democrat-introduced bills") +
  xlab("Number of Aye Votes") +
  ylab("Number of Nay Votes") +
  scale_shape_manual(name = "Cluster", 
                     labels = c("Cluster 1", "Cluster 2"),
                     values = c("1", "2")) +
  theme_light()

# Evaluating clustering
# betweenss
num_Dem <- kmeans_obj_Dem$betweenss
num_Dem 
# totss
denom <- kmeans_obj_Dem$totss
denom
#calculate variability
var.exp.dem <- num_Dem/denom
var.exp.dem
# .795-- 79.5 variance explained
#Elbow Method--how many clusters should we use? try 3
set.seed(1)
kmeans_obj_Dem2 <-kmeans(clust_Data_dem, centers=3)
num.Dem3 <- kmeans_obj_Dem2$betweenss
denom.Dem3 <-kmeans_obj_Dem2$totss
var.exp.dem3 <-num.Dem3/denom.Dem3
var.exp.dem3
# increased to 84.6% with 3 
#create function to automate
explained_var <- function(data_in, k) {
  set.seed(1)
  kmeansobj <- kmeans(data_in, centers=k)
  var.exp <- kmeansobj$betweenss/kmeansobj$totss
}
clust_Data_dem <- house_votes_Dem[, c("aye", "nay", "other")]

explained_var_Dem = sapply(1:10, explained_var, data_in = clust_Data_dem)
View(explained_var_Dem)
elbow_data_Dem = data.frame(k = 1:10, explained_var_Dem)
View(elbow_data_Dem)
# plot elbow data to view bend
ggplot(elbow_data_Dem, aes(x=k, y=explained_var_Dem)) +
  geom_point(size=4)+
  geom_line(size= 1) +
  xlab("K") +
  ylab("InterclusterVar/TotalVar") +
  theme_light()
# elbow shows at 2 clusters

# Now try with NB Clust
library(NbClust)
nbclust_obj_Dem = NbClust(data = clust_Data_dem, method = "kmeans")
nbclust_obj_Dem
View(nbclust_obj_Dem$Best.nc)
# recommended clusters = 2
