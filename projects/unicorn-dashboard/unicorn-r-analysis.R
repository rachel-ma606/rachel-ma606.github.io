# 703 R 

#Authors: 

#Group A3

#Clementine Wu 332824497
#Rachel Ma 390144673 
#Xiaomeng Mao 776667621
#Zhou Zhou 986984086






-----------------------------------------------------------------
#Association learning 
  
  
  
  
  
# Load necessary libraries
install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)




# Load your dataset
# Replace "your_dataset.csv" with the actual filename of your dataset
df_investors <- read.csv("BUSINFO_703_Dataset_Unicorns_$2B_(508x7).csv")

# Preview the dataset
head(df_investors)

# Preprocess the 'Select Investors' column
# Split the investors into a list format
investors_list <- strsplit(as.character(df_investors$Select.Investors), ", ")

# Convert the list to a transactions format for the Apriori algorithm
investors_txn <- as(investors_list, "transactions")

# Display a summary of the transactions
summary(investors_txn)

# Visualize the top investors by relative frequency
itemFrequencyPlot(investors_txn, topN = 10, main = "Top 10 Investors by Frequency")

# Generate frequent itemsets
min_support = 0.1 # Adjust this threshold as needed
frequent_itemsets <- apriori(investors_txn,
                             parameter = list(supp = min_support, 
                                              target = "frequent itemsets"))
# Inspect the frequent itemsets
inspect(frequent_itemsets)

# Apply Apriori algorithm to determine rules
min_confidence = 0.05 # Adjust this threshold as needed
rules <- apriori(investors_txn, 
                 parameter = list(supp = min_support, 
                                  conf = min_confidence, 
                                  target = "rules"))

rules <- apriori(investors_txn, 
                 parameter = list(supp = 0.005, # Lower support threshold
                                  conf = 0.3,  # Lower confidence threshold
                                  target = "rules"))


# Inspect the rules
inspect(rules)

# Filter & sort the rules
final_rules <- subset(rules, lift > 1 & confidence >= 0.3 & support >= 0.005)
inspect(final_rules)




# Convert filtered rules to a data frame for further analysis
df_final_rules <- as(final_rules, "data.frame")
# Sort on descending lift & confidence
df_final_rules_sorted <- df_final_rules[order(-df_final_rules$lift, -df_final_rules$confidence), ]
head(df_final_rules_sorted)

# Visualize the rules
plot(final_rules, method = "scatterplot", main = "Scatterplot of Rules") # Scatter with color
plot(final_rules, method = "graph", main = "Network of Rules") # Show as a network

#Findings
#The analysis revealed the top 10 investors, with Accel, Andreessen Horowitz, and Sequoia Capital 
#leading in frequency. Clustering identified distinct startup groups, emphasizing disparities in valuation 
#and venture capital raised, including key outliers. Association rule mining highlighted 
#strong co-investor relationships, particularly between Founders Fund and Khosla Ventures, 
#with high-lift and low-support rules indicating niche partnerships. These insights underline dominant investors, 
#key collaborations, and performance disparities within the startup ecosystem.
-------------------------------------------------------------------------------------------------------------
#Cluster
# Loading packages upfront
## Package for Visualisation
install.packages("ggplot2") # need to install package once!
library(ggplot2)
## Package for computing Silhouette Score
install.packages("cluster") # need to install package once!
library(cluster)
## Package for generating multiple scatterplots
install.packages("GGally") # need to install package once!
library(GGally)
install.packages("readxl")
library(readxl)

#read data
data <- read_excel("703-Merged Table.xlsx", sheet = 1) 
str(data)   #507 rows*11 columns
head(data)
#data cleaning
data_unicorn <- data[, c("Valuation ($B)",
                  "VC_raised_to_date"
)]
head(data_unicorn)
data_unicorn[data_unicorn == ""] <- NA
data_unicorn <- data_unicorn[complete.cases(data_unicorn), ]
any(is.na(data_unicorn))


#standard the dataset + cleaning
data_std <- as.data.frame(scale(data_unicorn))
head(data_std)
any(is.na(data_std))
data_std[data_std == ""] <- NA
data_std <- data_std[complete.cases(data_std), ]
head(data_std)
any(is.na(data_std))





## Set seed for reproducibility
set.seed(703) 

## Perform k-means clustering with 3 clusters
kmeans_result <- kmeans(data_std, centers = 3)

## Add cluster values to original data
data_unicorn$cluster <- factor(kmeans_result$cluster)

## Plot the scatter plot with clusters
ggplot(data_unicorn, aes(x = `Valuation ($B)`, y = `VC_raised_to_date`, color = cluster)) +
  geom_point() +
  labs(title = "Scatter Plot of valuation and VC_Raised by cluster",
       x = "Valuation ($b)",
       y = "VC_raised_to_date",
       color = "Cluster")
summary(data_unicorn$cluster)   


# Calculate silhouette scores for each data point using the k-means clustering result
silhouette_scores <- silhouette(kmeans_result$cluster, dist(data_std))

# Print the summary information of the silhouette scores
summary(silhouette_scores)

# Calculate the average silhouette score (extracting the third column from silhouette_scores)
avg_silhouette_score <- mean(silhouette_scores[, 3])

# Print the average silhouette score
print(avg_silhouette_score)



set.seed(703) # Set seed for reproducibility 
## Initialise lists for models & scores
fits <- list()
scores <- numeric()
## Loop through different cluster numbers (k) from 2 to 10
for (k in 2:10) {    
  model <- kmeans(data_std, centers = k) # Train the model for k 
  fits[[k]] <- model  # Append the model to fits 
  # Calculate the silhouette score
  silhouette_scores <- silhouette(model$cluster, dist(data_std))
  scores[k] <- mean(silhouette_scores[, 3]) # Add to list of scores 
}
## Create a data frame for plotting the scores
scores_df <- data.frame(k = 2:10, silhouette_score = scores[2:10]) 
ggplot(scores_df, aes(x = k, y = silhouette_score)) +
  geom_line() +
  geom_point() +
  labs(title = "Silhouette Scores for Different Values of k",
       x = "Number of Clusters (k)",
       y = "Average Silhouette Score") 
# Observation: k = 2 or 3 has the highest silhouette scores, indicating better clustering quality.
## Let's use k=3 from the list of models built already
data_unicorn$cluster <- factor(fits[[3]]$cluster) #Ad 3-cluster values
summary(data_unicorn$cluster) # Get counts for each cluster
#The clustering results indicate that Group 1 has 16 members, Group 2 has 332 members, and Group 3 has 2 members.

## Let's check the plots for k=3 
ggplot(data_unicorn,
       aes(x = `Valuation ($B)`, y = `VC_raised_to_date`, color = cluster)) +
  geom_point(size = 2) +
  labs(title = "Scatter Plot of Unicorn startups Data",
       x = "Valuation ($B)",
       y = "VC_raised_to_date",
       color = "cluster")




#boxpolt 


# Create a boxplot for 'Valuation ($B)' by cluster
ggplot(data_unicorn, aes(x = cluster, y = `Valuation ($B)`, fill = cluster)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of Valuation ($B) by Cluster",
    x = "Cluster",
    y = "Valuation ($B)"
  ) +
  theme_minimal()

# Create a boxplot for 'VC_raised_to_date' by cluster
ggplot(data_unicorn, aes(x = cluster, y = `VC_raised_to_date`, fill = cluster)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of VC Raised to Date by Cluster",
    x = "Cluster",
    y = "VC Raised to Date"
  ) +
  theme_minimal()



########Findings#######
#Summary of Findings
#The cleaned and standardized dataset was clustered using k-means, with silhouette score analysis identifying 2 or 3 clusters as the optimal choices. For k=3, the data was divided into:

#Cluster 1: 16 startups with high venture capital funding.
#Cluster 2: 332 startups with moderate valuations and funding (largest group).
#Cluster 3: 2 outlier startups with extremely high valuations.

  
