install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(ggplot2)
install.packages("VIM")
library(VIM)

# Superstore EDA challenge 

# Step 1: Load and Explore the Dataset
superstore <- read.csv("C:/Users/makwe/Downloads/Sample - Superstore.csv")
View(superstore)

#Explore the dataset using str(), head(), and summary().

head(superstore)
str(superstore)
summary(superstore)

# Question 1: How does sales performance vary over time?

# Identify which months consistently show sales peaks

superstore$Order.Date <- as.Date(superstore$Order.Date, format = "%m/%d/%Y")
str(superstore$Order.Date)

superstore <- superstore %>%
  mutate(
    Year = format(Order.Date, "%Y"),
    Month = format(Order.Date, "%B"),
    Month_Num = as.numeric(format(Order.Date, "%m"))
  )

# Calculate TOTAL sales per month per year

monthly_sales <- superstore %>%
  group_by(Year, Month, Month_Num) %>%
  summarise(
    Monthly_Sales = sum(Sales),
    .groups = "drop"
  )

# Calculate AVERAGE sales for each month (across years) 

monthly_avg_sales <- monthly_sales %>%
  group_by(Month, Month_Num) %>%
  summarise(
    Avg_Monthly_Sales = mean(Monthly_Sales),
    .groups = "drop"
  ) %>%
  arrange(Month_Num)

monthly_avg_sales

ggplot(monthly_avg_sales, aes(x = Month, y = Avg_Monthly_Sales)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "yellow") +
  labs(
    title = "Average Monthly Sales Across All Years",
    x = "Month",
    y = "Average Sales"
  )

# Find any unusual dips in the trend line

monthly_trend <- superstore %>%
  mutate(Month_Year = format(Order.Date, "%Y-%m")) %>%
  group_by(Month_Year) %>%
  summarise(
    Total_Sales = sum(Sales),
    .groups = "drop"
  ) %>%
  arrange(Month_Year)

monthly_trend$Month_Year <- as.Date(paste0(monthly_trend$Month_Year, "-01"))


ggplot(monthly_trend, aes(x = Month_Year, y = Total_Sales)) +
  geom_line(color = "steelblue") +
  geom_point() +
  labs(
    title = "Monthly Sales Trend Over Time",
    x = "Month",
    y = "Total Sales"
  )

# seasonal discounts, seasonal demands

# Question 2: Which product categories have the best/worst profit margins?
# Identify which category has the thinnest margins

superstore <- superstore %>%
  mutate(Profit_Margin = Profit / Sales)

category_margin <- superstore %>%
  group_by(Category) %>%
  summarise(
    Avg_Profit_Margin = mean(Profit_Margin, na.rm = TRUE),
    .groups = "drop"
  )

category_margin %>%
  arrange(Avg_Profit_Margin)

#visual
ggplot(category_margin, aes(x = Category, y = Avg_Profit_Margin)) +
  geom_bar(stat = "identity", fill = "grey", color = "red") +
  labs(
    title = "Average Profit Margin by Category",
    x = "Category",
    y = "Average Profit Margin"
  )

# Calculate the profit margin difference between top and bottom categories

category_margin <- superstore %>%
  mutate(Profit_Margin = Profit / Sales) %>%
  group_by(Category) %>%
  summarise(
    Avg_Profit_Margin = mean(Profit_Margin, na.rm = TRUE),
    .groups = "drop"
  )

top_margin <- category_margin %>%
  arrange(desc(Avg_Profit_Margin)) %>%
  slice(1)

bottom_margin <- category_margin %>%
  arrange(Avg_Profit_Margin) %>%
  slice(1)

margin_difference <- top_margin$Avg_Profit_Margin - bottom_margin$Avg_Profit_Margin
margin_difference

margin_difference * 100

# 11.7 % margin difference

#Negotiate better terms with suppliers for low-margin categories.
#Reduce logistics, storage, and handling costs, especially for bulky items (e.g., Furniture).
#Identify products with high return or damage rates and address quality issues.



# Question 3: How do regional performances compare?

region_perf <- superstore %>%
  group_by(Region) %>%
  summarise(
    Total_Sales = sum(Sales),
    Total_Profit = sum(Profit),
    Avg_Profit_Margin = mean(Profit / Sales, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Sales))

region_perf

ggplot(region_perf, aes(x = Region, y = Total_Sales, fill = Region)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Total Sales by Region",
    x = "Region",
    y = "Total Sales"
  )

region_perf %>%
  arrange(desc(Total_Sales), desc(Total_Profit))

# Analyze if high sales always correlate with high profits

ggplot(region_perf, aes(x = Total_Sales, y = Total_Profit, label = Region)) +
  geom_point(size = 5, color = "steelblue") +
  geom_text(vjust = -0.5) +
  labs(
    title = "Sales vs Profit by Region",
    x = "Total Sales",
    y = "Total Profit"
  )

cor(region_perf$Total_Sales, region_perf$Total_Profit)
# central needs a cost review and restructure their promotion strategies 



# Question 4: What does customer segmentation reveal?
# Calculate percentage of customers in each segment

customer_segment <- superstore %>%
  group_by(Segment) %>%
  summarise(
    Customer_Count = n_distinct(Customer.ID),
    .groups = "drop"
  )

customer_segment <- customer_segment %>%
  mutate(
    Percentage = (Customer_Count / sum(Customer_Count)) * 100
  )

customer_segment
ggplot(customer_segment, aes(x = Segment, y = Percentage, fill = Segment)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Customer Distribution by Segment",
    x = "Segment",
    y = "Percentage of Customers"
  )


# Identify which segment generates the most revenue

segment_sales <- superstore %>%
  group_by(Segment) %>%
  summarise(
    Total_Sales = sum(Sales),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Sales))

segment_sales
# occasional buyers, unregistered buyers ("visitors"), dropped spending behaviour

# Suggest marketing approaches for "High Spenders"
#VIP prmotions and early access, reward system

# Question 5: How does shipping mode affect profitability?

shipping_perf <- superstore %>%
  mutate(Profit_Margin = Profit / Sales) %>%  # row-level margin
  group_by(Ship.Mode) %>%
  summarise(
    Avg_Profit_Margin = mean(Profit_Margin, na.rm = TRUE),
    Total_Profit = sum(Profit),
    Total_Sales = sum(Sales),
    .groups = "drop"
  ) %>%
  arrange(desc(Avg_Profit_Margin))
shipping_perf

ggplot(shipping_perf, aes(x = Ship.Mode, y = Avg_Profit_Margin, fill = Ship.Mode)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Average Profit Margin by Shipping Mode",
    x = "Shipping Mode",
    y = "Average Profit Margin"
  )


# Calculate profit per order for each shipping mode

profit_per_order <- superstore %>%
  group_by(Ship.Mode) %>%
  summarise(
    Total_Profit = sum(Profit),
    Total_Orders = n(),   # counts rows = number of orders
    Profit_Per_Order = Total_Profit / Total_Orders,
    .groups = "drop"
  ) %>%
  arrange(desc(Profit_Per_Order))

profit_per_order

