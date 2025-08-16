library(dplyr)
library(lubridate)
library(RcppRoll)
library(moments)

# Step 1: Load train_df
train_df <- read.csv("D:/Personal/study/CS/PythonSeminar/github/DS3/d/1train/time_train_df.csv")

# Step 2: Initialize hour_of_day column
if (!"hour_of_day" %in% colnames(train_df)) {
  train_df$DateTime <- as.POSIXct(train_df$DateTime)
  train_df$date <- as.Date(train_df$DateTime)
  train_df$hour_of_day <- as.integer(format(train_df$DateTime, "%H"))
}

# Step 3: Loop through each hour to handle outliers
for (h in 0:23) {
  idx <- which(train_df$hour_of_day == h)
  
  if (length(idx) > 0) {
    # Calculate median and MAD
    med <- median(train_df$Price[idx], na.rm = TRUE)
    mad_val <- mad(train_df$Price[idx], na.rm = TRUE)
    
    # Mark outliers
    train_df$is_outlier[idx] <- abs(train_df$Price[idx] - med) > 3 * mad_val
    
    # Winsorize
    upper <- med + 3 * mad_val
    lower <- med - 3 * mad_val
    
    train_df$Price_winsorized[idx] <- pmin(pmax(train_df$Price[idx], lower), upper)
  }
}

# Check the proportion of outliers
outlier_summary <- train_df %>%
  group_by(hour_of_day) %>%
  summarize(
    total = n(),
    outliers = sum(is_outlier, na.rm = TRUE),
    outlier_rate = outliers / total
  )

print(outlier_summary)
head(train_df)

write.csv(train_df, "D:/Personal/study/CS/PythonSeminar/github/DS3/d/1train/2train_df_Abnormalvalue.csv", row.names = FALSE)

# Plot the distribution comparison of original and winsorized prices
library(ggplot2)
ggplot(train_df) +
  geom_density(aes(x = Price, color = "Original")) +
  geom_density(aes(x = Price_winsorized, color = "Winsorized")) +
  labs(title = "Price Distribution Comparison")

cat("=== Skewness Check of Original Data ===\n")
cat("Overall Skewness of Price:", skewness(train_df$Price, na.rm = TRUE), "\n")
cat("Overall Skewness of Price_winsorized:", skewness(train_df$Price_winsorized, na.rm = TRUE), "\n")
cat("=== Overall Skewness of Price_winsorized: 0.5751164 ===\n")

# Q-Q Plot
ggplot(train_df, aes(sample = Price_winsorized)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of Winsorized Price")

# Density Plot
ggplot(train_df, aes(x = Price_winsorized)) +
  geom_density(fill = "lightblue") +
  geom_function(fun = dnorm, args = list(mean = mean(train_df$Price_winsorized), sd = sd(train_df$Price_winsorized)), color = "red") +
  labs(title = "Density Plot vs Normal Distribution")

# Outliers before winsorization (based on original Price)
med_original <- median(train_df$Price, na.rm = TRUE)
mad_original <- mad(train_df$Price, na.rm = TRUE)
upper_original <- med_original + 3 * mad_original
lower_original <- med_original - 3 * mad_original
outliers_original <- sum(train_df$Price > upper_original | train_df$Price < lower_original, na.rm = TRUE)

# Outliers after winsorization (should be close to 0)
outliers_winsorized <- sum(train_df$Price_winsorized > upper_original | train_df$Price_winsorized < lower_original, na.rm = TRUE)
cat("Number of original outliers:", outliers_original, "\nNumber of outliers after winsorization:", outliers_winsorized)

# Skewness and Kurtosis
skewness_original <- skewness(train_df$Price, na.rm = TRUE)
skewness_winsorized <- skewness(train_df$Price_winsorized, na.rm = TRUE)
kurtosis_original <- kurtosis(train_df$Price, na.rm = TRUE)
kurtosis_winsorized <- kurtosis(train_df$Price_winsorized, na.rm = TRUE)

cat("Original Skewness:", skewness_original, "\nSkewness after Winsorization:", skewness_winsorized,
    "\nOriginal Kurtosis:", kurtosis_original, "\nKurtosis after Winsorization:", kurtosis_winsorized)

# Check the distribution of outliers over time
ggplot(train_df, aes(x = DateTime, y = Price_winsorized, color = is_outlier)) +
  geom_point() +
  labs(title = "Remaining Outliers After Winsorization (3*MAD)")


ggplot(outlier_summary, aes(x = factor(hour_of_day), y = outlier_rate)) +
  geom_col(fill = "tomato") +
  labs(title = "Outlier Rate by Hour of Day", x = "Hour", y = "Outlier Rate")
