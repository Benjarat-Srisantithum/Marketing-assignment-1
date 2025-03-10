install.packages("dplyr")
install.packages("dendextend")
# Load necessary libraries
library(readxl)
library(ggplot2)
library(cluster)
library(factoextra)
library(NbClust)
library(dplyr)
library(dendextend)

df <- read_excel("C:/Users/Admin/Desktop/SmartWatch Data File.xlsx")

str(df)
summary(df)

df_scaled <- scale(df[, c("ConstCom", "TimelyInf", "TaskMgm", "DeviceSt", "Wellness", "Athlete", "Style", "AmznP", "Female", "Degree", "Income", "Age")])

# Elbow Method
elbow_plot <- fviz_nbclust(df_scaled, kmeans, method = "wss") +
  ggtitle("Elbow Method: Optimal Cluster Number")
ggsave("Elbow_Method.png", plot = elbow_plot, width = 7, height = 5)

set.seed(123)
kmeans_result <- kmeans(df_scaled, centers = 4, nstart = 25)

df$Cluster <- as.factor(kmeans_result$cluster)

kmeans_plot <- fviz_cluster(kmeans_result, data = df_scaled) +
  ggtitle("K-means Clustering Results")
ggsave("KMeans_Clusters.png", plot = kmeans_plot, width = 7, height = 5)

dist_matrix <- dist(df_scaled, method = "euclidean")

# Perform Hierarchical clustering using Ward's method
hc <- hclust(dist_matrix, method = "ward.D2")

# Dendrogram for 3 to 5 clusters
png("Dendrogram_3-5_Clusters.png", width = 800, height = 600)
plot(hc, labels = FALSE, main = "Dendrogram for 3-5 Clusters")
rect.hclust(hc, k = 3, border = "blue")
rect.hclust(hc, k = 4, border = "red")
rect.hclust(hc, k = 5, border = "green")
dev.off()

# Cut tree into 4 clusters (final decision)
df$HCluster <- cutree(hc, k = 4)

# Final 4-cluster model Dendrogram
png("Dendrogram_4_Clusters.png", width = 800, height = 600)
plot(hc, labels = FALSE, main = "Final 4-Cluster Model Dendrogram")
rect.hclust(hc, k = 4, border = "red")
dev.off()

# --- Step 4: Comparing Mean Values between 3-cluster and 4-cluster Models ---

# Generate clusters for 3-cluster model
df$HCluster3 <- cutree(hc, k = 3)

# Summary for 3-cluster and 4-cluster models
mean_comparison <- df %>%
  group_by(HCluster3) %>%
  summarise_all(mean) %>%
  mutate(Model = "3 Clusters") %>%
  bind_rows(
    df %>%
      group_by(HCluster) %>%
      summarise_all(mean) %>%
      mutate(Model = "4 Clusters")
  )


df$Business_Strength <- (df$ConstCom + df$TimelyInf + df$TaskMgm) / 3
df$Market_Attractiveness <- (df$Wellness + df$Athlete + df$Style) / 3

features <- c("ConstCom", "TimelyInf", "TaskMgm", "DeviceSt", "Wellness", "Athlete", "Style", "AmznP", "Female", "Degree", "Income", "Age")
df_cluster <- df[, features]

df_scaled <- scale(df_cluster)

# Compute the distance matrix using Euclidean distance
dist_matrix <- dist(df_scaled, method = "euclidean")

hc <- hclust(dist_matrix, method = "ward.D2")

plot(hc, main = "Dendrogram of Smartwatch Clusters", xlab = "Data Points", ylab = "Height", sub = "")
rect.hclust(hc, k = 3, border = "red")   # Highlighting 3-cluster solution
rect.hclust(hc, k = 4, border = "blue")  # Highlighting 4-cluster solution
rect.hclust(hc, k = 5, border = "green") # Highlighting 5-cluster solution

# Cut tree into 4 clusters (final decision)
df$Cluster <- cutree(hc, k = 4)

features <- c("ConstCom", "TimelyInf", "TaskMgm", "DeviceSt", "Wellness", "Athlete", "Style", "AmznP", "Female", "Degree", "Income", "Age")
df_cluster <- df[, features]

df_scaled <- scale(df_cluster)

# Compute the distance matrix using Euclidean distance
dist_matrix <- dist(df_scaled, method = "euclidean")

# Perform Hierarchical Clustering using Ward's method
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot the Dendrogram and cut at 4 clusters
dend <- as.dendrogram(hc)

# Color branches for 4 clusters
dend <- color_branches(dend, k = 4)

# Plot Dendrogram for 4 clusters
plot(dend, main = "Dendrogram of Smartwatch Clusters", xlab = "Data Points", ylab = "Height", sub = "")
rect.hclust(hc, k = 4, border = c("red", "blue", "green", "purple")) # Highlighting 4 clusters


df$Cluster <- cutree(hc, k = 4)

features <- c("ConstCom", "TimelyInf", "TaskMgm", "DeviceSt", "Wellness", "Athlete", "Style", "AmznP", "Female", "Degree", "Income", "Age")

# Scale the data for clustering
df_scaled <- scale(df[, features])

set.seed(123)

# Apply K-Means clustering for 3 clusters
kmeans_3 <- kmeans(df_scaled, centers = 3, nstart = 25)
df$Cluster_3 <- as.factor(kmeans_3$cluster)

# Apply K-Means clustering for 4 clusters
kmeans_4 <- kmeans(df_scaled, centers = 4, nstart = 25)
df$Cluster_4 <- as.factor(kmeans_4$cluster)

# Compute mean values for each feature within each cluster (for 3 clusters)
mean_values_3 <- df %>%
  group_by(Cluster_3) %>%
  summarise(across(all_of(features), mean))

# Compute mean values for each feature within each cluster (for 4 clusters)
mean_values_4 <- df %>%
  group_by(Cluster_4) %>%
  summarise(across(all_of(features), mean))

# Count the number of respondents in each cluster (for 3 clusters)
respondents_3 <- df %>%
  group_by(Cluster_3) %>%
  summarise(Respondent_Count = n())

# Count the number of respondents in each cluster (for 4 clusters)
respondents_4 <- df %>%
  group_by(Cluster_4) %>%
  summarise(Respondent_Count = n())


# Print the mean values tables for comparison
print("Mean values for 3 clusters:")
print(mean_values_3, width = Inf)

print("Mean values for 4 clusters:")
print(mean_values_4, width = Inf)

print("Number of respondents per cluster (3-cluster model):")
print(respondents_3)

print("Number of respondents per cluster (4-cluster model):")
print(respondents_4)

features <- c("ConstCom", "TimelyInf", "TaskMgm", "DeviceSt", "Wellness", 
              "Athlete", "Style", "AmznP", "Female", "Degree", "Income", "Age")

# Scale numerical features for clustering (excluding categorical variables)
df_scaled <- scale(df[, c(features)])

set.seed(123)

# Apply K-Means clustering with 4 clusters
kmeans_4 <- kmeans(df_scaled, centers = 4, nstart = 25)


df$Cluster_4 <- as.factor(kmeans_4$cluster)

mean_values_4 <- df %>%
  group_by(Cluster_4) %>%
  summarise(across(all_of(features), mean))

respondent_counts <- df %>%
  group_by(Cluster_4) %>%
  summarise(Respondent_Count = n())

respondent_counts <- respondent_counts %>%
  mutate(Percentage = (Respondent_Count / sum(Respondent_Count)) * 100)

mean_values_4 <- left_join(mean_values_4, respondent_counts, by = "Cluster_4")

print(mean_values_4,Inf)
