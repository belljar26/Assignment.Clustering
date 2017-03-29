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


