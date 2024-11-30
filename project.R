rm(list = ls())
library(cluster)
file_path <- "/Users/mac/Documents/FALL 2024/CAP5765-Computational Data Analysis/Final Project/R/updated.csv"
file_path1<-"/Users/mac/Documents/FALL 2024/CAP5765-Computational Data Analysis/Final Project/R/newdff.csv"
file_path2= "/Users/mac/Documents/FALL 2024/CAP5765-Computational Data Analysis/Final Project/R/catdata.csv"
new_data= read.csv(file_path1)
data <- read.csv(file_path)
catdata<- read.csv(file_path2)
head(data)

data1= scale(data)

## KMeans Clustering

#Elbow Plot
# Function to compute WCSS for k clusters
wcss <- function(k) {
  kmeans(data1, centers = k, nstart = 25)$tot.withinss
}

# Compute WCSS for k = 1 to 10
k_values <- 1:10
wcss_values <- sapply(k_values, wcss)

# Create the elbow plot
plot(k_values, wcss_values, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Squares (WCSS)",
     main = "Elbow Method for Optimal k")

#Silhouette Plot for Optimal K
fviz_nbclust(data1, kmeans, method = "silhouette") +
  labs(subtitle = "Optimal Number of Clusters Using Silhouette Method")

#Silhouette score at k=4
kmeans_result <- kmeans(data1, centers = 4, nstart = 25)
# Compute silhouette values
sil <- silhouette(kmeans_result$cluster, dist(data1))

#Fitting the KMeans Clustering with k=4
kmeans4 <-kmeans(data1,centers=4)
kmeans4$cluster
kmeans4$tot.withinss

#Adding kmeans labels to a new dataframe
new_dataf<-cbind(new_data, kmeansCluster = kmeans4$cluster)

#Plotting the Kmeans clustered data
plot(data[,1],data[,2],col=(kmeans4$cluster+1),
     main = "K-Means Clustering Results with K=4",
     xlab = "Claim Amount", ylab = "Close Amount",pch=20,cex=2)


#KPrototype Clustering
library(clustMixType)

#Fitting KPrototype with 4 clusters
catdata$Claim.Type <- as.factor(catdata$Claim.Type)
catdata$Claim.Site <- as.factor(catdata$Claim.Site)
catdata$Disposition <- as.factor(catdata$Disposition)

kproto_result <- kproto(catdata, k = 4)

# Clustering cost
kproto_result$tot.withinss

# Visualize profiles
clprofiles(kproto_result, catdata)

#Plotting the KProto clustered data
plot(data[,1],data[,2],col=(kproto_result$cluster+1),
     main = "K-Prototype Clustering Results with K=4",
     xlab = "Claim Amount", ylab = "Close Amount",pch=20,cex=2)

#Adding kproto labels to a new dataframe
new_dataf$kproto_labels<- kproto_result$cluster

# Calculate dissimilarity matrix
dissimilarity_matrix <- dissimilarity(catdata, kproto_result$centers)
sil <- silhouette(kproto_result$cluster, dist(dissimilarity_matrix))


#DBSCAN
library(dbscan)
library(inflection)
library(factoextra)

#Determining the optimal eps using kNN distance plot
k <- 5 
kNN_distances <- kNNdist(data1, k)
kNNdistplot(data1, k, mai)
title(main = "k-NN Distance Plot to Find Optimal eps")
abline(h = 0.5, col = "red", lty = 2) 

#Fitting the DBSCAN with eps=0.5 and minpts=5
db <- dbscan(data1, eps = 0.5, minPts = k)

#Visualizing the clustering results
plot(data[,1],data[,2],col=(db$cluster+1),
     main = "DBSCAN Clustering Results",
     xlab = "Claim Amount", ylab = "Close Amount",pch=20,cex=2)

#Function to compute total within-cluster sum of squares
calculate_tot_withinss <- function(data, clusters) {
  cluster_ids <- unique(clusters)
  cluster_ids <- cluster_ids[cluster_ids != 0]  # Exclude noise (cluster ID = 0)
  
  tot_withinss <- 0
  for (cluster_id in cluster_ids) {
    cluster_points <- data[clusters == cluster_id, ]
    cluster_center <- colMeans(cluster_points)  # Compute cluster centroid
    withinss <- sum(rowSums((cluster_points - cluster_center)^2))  # Sum of squared distances
    tot_withinss <- tot_withinss + withinss
  }
  return(tot_withinss)
}

# Calculate total within-cluster sum of squares
tot_withinss <- calculate_tot_withinss(data1, db$cluster)
print(paste("Total Within-Cluster Sum of Squares:", tot_withinss))


#Excluding noise points (cluster 0) for silhouette calculation
valid_clusters <- db$cluster[db$cluster > 0]
valid_data <- data[db$cluster > 0, ]

# Compute silhouette scores
sil <- silhouette(valid_clusters, dist_matrix)


#Clusters Analysis
par(mfrow=c(2,2))
plot(data[,1],data[,2],col=(kmeans4$cluster+1),
     main = "K-Means Clustering Results with K=4",
     xlab = "Claim Amount", ylab = "Close Amount",pch=20,cex=2)
plot(data[,1],data[,2],col=(kproto_result$cluster+1),
     main = "K-Prototype Clustering Results with K=4",
     xlab = "Claim Amount", ylab = "Close Amount",pch=20,cex=2)
plot(data[,1],data[,2],col=(db$cluster+1),
     main = "DBSCAN Clustering Results",
     xlab = "Claim Amount", ylab = "Close Amount",pch=20,cex=2)
par(mfrow=c(1,1))


#Plotting model results
library(ggplot2)
library(reshape2)
library(patchwork)
model_results=data.frame(Algorithm=c("KMeans","KPrototype","DBSCAN","BIRCH"), Sil_score=c(0.875,0.452,0.843,0.865), Inertia=c(161138.5,582464416687,606223.61,4820300.90))
model_results_melted <- melt(model_results, id.vars = "Algorithm")

#Silhouette Plot
sil_plot <- ggplot(subset(model_results_melted, variable == "Sil_score"), 
                   aes(x = Algorithm, y = value, group = 1, color = variable)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Silhouette Scores of Clustering Models", 
       x = "Algorithm", 
       y = "Silhouette Score") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_manual(values = "blue")

# Line plot for Inertia
inertia_plot <- ggplot(subset(model_results_melted, variable == "Inertia"), 
                       aes(x = Algorithm, y = value, group = 1, color = variable)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Inertia Values of Clustering Models", 
       x = "Algorithm", 
       y = "Inertia (Log Scale)") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_manual(values = "red") +
  scale_y_continuous(trans = "log10", 
                     breaks = c(1e5, 1e6, 1e9), 
                     labels = scales::comma)

# Displaying the plots
print(sil_plot)
print(inertia_plot)

combined_plot <- sil_plot / inertia_plot + 
  plot_annotation(title = "Comparison of Clustering Model Performance", 
                  subtitle = "Silhouette Scores and Inertia")
print(combined_plot)


#Descriptive Analysis of each cluster
# Counting data points per cluster
cluster_summary <- table(new_dataf$kmeansCluster)
cluster_percentage <- prop.table(cluster_summary) * 100

# Print results
print(data.frame(Cluster = names(cluster_summary), 
                 Count = as.numeric(cluster_summary),
                 Percentage = cluster_percentage))

library(dplyr)

# Average Claim and Close Amount per cluster
cluster_analysis <- new_dataf %>%
  group_by(kmeansCluster) %>%
  summarise(
    Avg_Claim_Amount = mean(Claim.Amount, na.rm = TRUE),
    Avg_Close_Amount = mean(Close.Amount, na.rm = TRUE),
    Total_Claims = n()
  )

print(cluster_analysis)

plot1<- ggplot(new_dataf, aes(x = as.factor(kmeansCluster), y = Claim.Amount)) +
  geom_boxplot() +
  labs(title = "Claim Amount Distribution Across Clusters",
       x = "Cluster",
       y = "Claim Amount") +
  theme_minimal()

plot2<-ggplot(new_dataf, aes(x = as.factor(kmeansCluster), y = Close.Amount)) +
  geom_boxplot() +
  labs(title = "Close Amount Distribution Across Clusters",
       x = "Cluster",
       y = "Close Amount") +
  theme_minimal()
plot1 + plot2


# Analyzing distribution of Claim Type within clusters
claim_type_distribution <- new_dataf %>%
  group_by(kmeansCluster, Claim.Type) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

print(claim_type_distribution, n=Inf)

# Analyzing distribution of Claim Site within clusters
claim_site_distribution <- new_dataf %>%
  group_by(kmeansCluster, Claim.Site) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

print(claim_site_distribution, n=Inf)

# Analyzing distribution of Claim Disposition within clusters
claim_disposition_distribution <- new_dataf %>%
  group_by(kmeansCluster, Disposition) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

print(claim_disposition_distribution, n=Inf)


library(ggplot2)
# Example: Plotting claim counts by Incident Year for each cluster
ggplot(new_dataf, aes(x = Incident.Year, fill = as.factor(kmeansCluster))) +
  geom_bar(position = "dodge") +
  labs(title = "Claims by Year and Cluster", x = "Year", y = "Number of Claims") +
  scale_fill_discrete(name = "Cluster") +
  theme_minimal()

ggplot(new_dataf, aes(x = Incident.Month.Num, fill = as.factor(kmeansCluster))) +
  geom_bar(position = "dodge") +
  labs(title = "Claims by Month and Cluster", x = "Month", y = "Number of Claims") +
  scale_fill_discrete(name = "Cluster") +
  theme_minimal()

# Identifying high-risk clusters based on claim patterns
high_risk_clusters <- cluster_analysis %>%
  filter(Avg_Claim_Amount > quantile(new_dataf$Claim.Amount, 0.75))  # Top 25% for claim amounts

print(high_risk_clusters)


# Analyzing distribution of Incident Month within clusters
claim_in_month_distribution <- new_dataf %>%
  group_by(kmeansCluster, Incident.Month) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

print(claim_in_month_distribution, n=Inf)

#Median Claim AND Close amounts
cluster_analysis_median <- new_dataf %>%
  group_by(kmeansCluster) %>%
  summarise(
    Med_Claim_Amount = median(Claim.Amount, na.rm = TRUE),
    Med_Close_Amount = median(Close.Amount, na.rm = TRUE),
    Total_Claims = n()
  )
print(cluster_analysis_median)

# Bar plot for Claim Types per cluster
ggplot(claim_type_distribution, aes(x = Claim.Type, y = Percentage, fill = as.factor(kmeansCluster))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Claim Type Distribution by Cluster", x = "Claim Type", y = "Percentage") +
  scale_fill_discrete(name = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Cluster summary visualization
ggplot(cluster_analysis, aes(x = as.factor(kmeansCluster), y = Avg_Claim_Amount, fill = as.factor(kmeansCluster))) +
  geom_bar(stat = "identity") +
  labs(title = "Average Claim Amount by Cluster", x = "Cluster", y = "Average Claim Amount") +
  scale_fill_discrete(name = "Cluster") +
  theme_minimal()


# Bar plot for Claim Disposition per cluster
ggplot(claim_disposition_distribution, aes(x = Disposition, y = Percentage, fill = as.factor(kmeansCluster))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Claim Disposition Distribution by Cluster", x = "Claim Disposition", y = "Percentage") +
  scale_fill_discrete(name = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar plot for Claim Site per cluster
ggplot(claim_site_distribution, aes(x = Claim.Site, y = Percentage, fill = as.factor(kmeansCluster))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Claim Site Distribution by Cluster", x = "Claim Site", y = "Percentage") +
  scale_fill_discrete(name = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Stacked bar chart for incident month
ggplot(claim_in_month_distribution, aes(x = factor(kmeansCluster), y = Percentage, fill = factor(Incident.Month))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Distribution of Incident Month within Clusters",
    x = "Cluster",
    y = "Percentage of Claims",
    fill = "Incident Month"
  ) +
  theme_minimal()


# Rearranging Incident.Month as an ordered factor
claim_in_month_distribution$Incident.Month <- factor(
  claim_in_month_distribution$Incident.Month,
  levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
             "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
)
ggplot(claim_in_month_distribution, aes(x = factor(Incident.Month), y = factor(kmeansCluster), fill = Percentage)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Percentage") +
  labs(
    title = "Heatmap of Incident Month Distribution by Cluster",
    x = "Incident Month",
    y = "Cluster"
  ) +
  theme_minimal()

#ANOVA test for Claim Amount across clusters
anova_result_claim <- aov(Claim.Amount ~ as.factor(kmeansCluster), data = new_dataf)
summary(anova_result_claim)
TukeyHSD(anova_result_claim)

#ANOVA test for Close Amount across clusters
anova_result_close <- aov(Close.Amount ~ as.factor(kmeansCluster), data = new_dataf)
summary(anova_result_close)

#Kruskal-Wallis test
kruskal_test_claim <- kruskal.test(Claim.Amount ~ as.factor(kmeansCluster), data = new_dataf)
print(kruskal_test_claim)

kruskal_test_close <- kruskal.test(Close.Amount ~ as.factor(kmeansCluster), data = new_dataf)
print(kruskal_test_close)
