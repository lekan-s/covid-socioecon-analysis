# Load necessary libraries
library(dplyr)
library(ggplot2)
library(car)
library(psych)
library(stats)
library(ggcorrplot)
library(corrplot)
library(MASS)

# set working directory
setwd(dirname(file.choose()))
getwd()

# FileUpload
census_data <- read.csv("u2525507_DS7006_CW2_data.csv", stringsAsFactors = FALSE)

# Check the first few rows of your data to ensure it's uploaded correctly
head(census_data)

# Check structure of the dataset
str(census_data)
summary(census_data)

census_data[ , 3:27] <- lapply(census_data[ , 3:27], as.numeric)

# Handle missing data
census_data <- na.omit(census_data)

# Select only numeric columns
numeric_cols <- sapply(census_data, is.numeric)

#Standardize the dataset
census_data[, numeric_cols] <- round(
  sweep(census_data[, numeric_cols], 2, colSums(census_data[, numeric_cols], na.rm = TRUE), FUN = "/") * 1000,
  2
)

census_data[ , 3:27] = scale(census_data[ , 3:27])

# Frequency distribution for the total deaths
hist(census_data$Total.Covid.deaths, main="Distribution of Total Deaths", xlab="Total Deaths", col="skyblue", breaks=20)

boxplot(census_data$Total.Covid.deaths, 
        main = "Boxplot of Total Covid Deaths", 
        ylab = "Deaths")

# Create a Q-Q plot
qqPlot(census_data$`Total.Covid.deaths`, main = "Q-Q Plot of Total Covid Deaths")

shapiro_test <- shapiro.test(census_data$`Total.Covid.deaths`)
shapiro_test

# Transformation of Total Covid Deaths
census_data$Total.Covid.deaths <- sqrt(census_data$Total.Covid.deaths)
census_data$Total.Covid.deaths <- log(census_data$Total.Covid.deaths + 1) 

# Q-Q plot for the square root-transformed variable
qqPlot(census_data$`Total.Covid.deaths`, main = "Q-Q Plot of transformed Total Covid Deaths variable")

# Frequency distribution for the total deaths
hist(census_data$Total.Covid.deaths, main="Distribution of transformed Total Deaths", xlab="Total Deaths", col="skyblue", breaks=20)

# Shapiro-Wilk test for the transformed variable
shapiro_test <- shapiro.test(census_data$`Total.Covid.deaths`)
shapiro_test

# Select only numeric columns as list
numeric_cols_list <- census_data[, sapply(census_data, is.numeric)]

# Calculate Pearson correlation matrix
cor_matrix <- cor(numeric_cols_list, method = "pearson", use = "complete.obs")

# View the correlation matrix
#print(cor_matrix)

# Extract correlation of IDVs with the DV
cor_with_dv <- cor_matrix["Total.Covid.deaths", ]
print(cor_with_dv)

# Sort by strength of correlation
sorted_correlations <- sort(cor_with_dv, decreasing = TRUE)

print(sorted_correlations)


# Visualize with a heatmap
heatmap(cor_matrix, main = "Correlation Matrix", symm = TRUE, col = heat.colors(10))

# Create a correlation plot
corrplot(cor_matrix, method = "circle", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, addCoef.col = "black")

ggplot(census_data, aes(x = census_data$`Aged.10.to.15.years`, y = census_data$`Total.Covid.deaths`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatterplot of Total Covid Deaths vs Aged.10.to.15.years",
       x = "Aged.10.to.15.years",
       y = "Total Covid Deaths")

ggplot(census_data, aes(x = census_data$`Aged.50.to.64.years`, y = census_data$`Total.Covid.deaths`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatterplot of Total Covid Deaths vs Aged.50.to.64.years",
       x = "Aged.50.to.64.years",
       y = "Total Covid Deaths")

ggplot(census_data, aes(x = census_data$`X7.people.in.household`, y = census_data$`Total.Covid.deaths`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatterplot of Total Covid Deaths vs X7.people.in.household",
       x = "X7.people.in.household",
       y = "Total Covid Deaths")

# Compute the KMO statistic
kmo_result <- KMO(census_data[ , 4:27]) 
print(kmo_result)

# Check the overall KMO statistic and individual MSA values
print(kmo_result$MSA)

# Perform PCA
pca_result <- prcomp(census_data[ , 4:27], center = TRUE, scale. = TRUE)

# Summary of PCA
summary(pca_result)

# Eigenvalues
eigenvalues <- pca_result$sdev^2  # Square of standard deviations
variance_explained <- eigenvalues / sum(eigenvalues) * 100  # Variance as percentages

# Combine results into a table
pca_table <- data.frame(
  Component = seq_along(eigenvalues),
  Eigenvalue = eigenvalues,
  Variance_Explained = variance_explained,
  Cumulative_Variance = cumsum(variance_explained)
)
print(pca_table)

# Scree plot
plot(eigenvalues, type = "b", main = "Scree Plot", 
     xlab = "Principal Component", ylab = "Eigenvalue", pch = 19, col = "blue")
abline(h = 1, col = "red", lty = 2)  # Kaiser criterion line

# Cumulative scree plot
plot(cumsum(variance_explained), type = "b", main = "Cumulative Scree Plot",
     xlab = "Principal Component", ylab = "Cumulative Variance Explained (%)", 
     pch = 19, col = "blue")
abline(h = 70, col = "red", lty = 2)  # Target threshold (e.g., 70%)


# Biplot of the first two principal components
biplot(pca_result, scale = 0)

# PCA Loadings
print(pca_result$rotation)

# PCA Scores
head(pca_result$x)

# Extract the selected principal components
selected_pcs <- as.data.frame(pca_result$x[, 1:4])  # First 4 PCs

# Combine with the dependent variable
regression_data <- cbind(selected_pcs, DependentVar = census_data$Total.Covid.deaths)

# Fit the model
regression_model <- lm(DependentVar ~ PC1 + PC2 + PC3 + PC4, data = regression_data)

# Summarize the results
summary(regression_model)

plot(regression_model)

# Calculate VIF
vif_values <- vif(regression_model)
print(vif_values)

#Refit the Model 
regression_model_refined <- lm(DependentVar ~ PC1 + PC2, data = regression_data)
summary(regression_model_refined)

plot(regression_model)