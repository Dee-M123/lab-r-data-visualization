install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(ggplot2)
install.packages("VIM")
library(VIM)

# Data Visualization in R

# Step 1: Load and Explore the Dataset
superstore <- read.csv("C:/Users/makwe/Downloads/Sample - Superstore.csv")
View(superstore)

#Explore the dataset using str(), head(), and summary().

head(superstore)
str(superstore)
summary(superstore)

# Boxplot
# A summary table or boxplot can be useful to visualize the distribution of key variables.
# Generate a boxplot to visualize Sales


ggplot(superstore, aes(y = Sales)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Boxplot of Sales", y = "Sales")


# Generate a boxplot to visualize profits
ggplot(superstore, aes(y = Profit)) +
  geom_boxplot(fill = "skyblue") +
  labs(
    title = "Boxplot of Profit",
    y = "Profit"
  )

# Bar Plots
# Create a bar plot to show the top 10 orders with highest value of sales.

top10_sales <- superstore[order(-superstore$Sales), ][1:10, ]

ggplot(top10_sales, aes(x = reorder(Order.ID, Sales), y = Sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(
    title = "Top 10 Orders by Sales",
    x = "Order ID",
    y = "Sales"
  )



# Use a heatmap to visualize the pattern of missing data.

# Install and load the VIM package if not already installed
library(VIM)

# Visualize missing data pattern
missing_pattern <- aggr(superstore, col=c('navyblue','red'), 
                        numbers=TRUE, sortVars=TRUE, labels=names(superstore), 
                        cex.axis=.7, gap=3, ylab=c("Missing data","Pattern"))
# No missing data




# Histogram
# Generate a histogram that can show the distribution of the Profit column.

ggplot(superstore, aes(x = Profit)) +
  geom_histogram(binwidth = 350, fill = "skyblue", color = "yellow") +
  labs(title = "Distribution of Profit",
    x = "Profit",
    y = "Frequency")


# Bar Plots for Aggregated Data
# Generate a bar plot that shows Total Sales by Category

category_summary <- superstore %>%
  group_by(Category) %>%
  summarise(
    Total_Sales = sum(Sales),
    Total_Profit = sum(Profit))


ggplot(category_summary, aes(x = Category, y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "yellow") +
  labs(
    title = "Total Sales by Category",
    x = "Category",
    y = "Total Sales")



# A barplot that shows Profit by Category.

ggplot(category_summary, aes(x = Category, y = Total_Profit)) +
  geom_bar(stat = "identity", fill = "grey", color = "red") +
  labs(
    title = "Total Profit by Category",
    x = "Category",
    y = "Total Profit")
