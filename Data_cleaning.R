library(tidyverse)  
library(data.table) 
library(lubridate)  
library(visdat) 
library(naniar)     
library(zoo)        
library(GGally)       
library(corrplot)     
library(ggplot2)      
library(ggcorrplot)   
library(car)          
library(tseries)      
library(forecast) 
library(patchwork)
library(plotly)
install.packages(c("patchwork","plotly"))



file_path <- "energy_demand_uk.csv"  
df <- fread(file_path)  
df$V1 <- NULL
# Preview dataset 
str(df)
glimpse(df)
summary(df)

# Convert date column to proper Date format
df$date <- as.Date(df$date, format = "%Y-%m-%d")

# Convert all time series columns to numeric
numeric_cols <- c("national_demand", "wind_generation", "solar_generation", 
                  "min_temp", "max_temp", "rain_mm", "wind_speed", "average_price_daily")
df[, (numeric_cols) := lapply(.SD, as.numeric), .SDcols = numeric_cols]

sapply(df, class)  # Verify data types

#
# 1.2 Handling Missing Data
#

# Count missing values per column
colSums(is.na(df))

# Handle missing data
df[, min_temp := na.approx(min_temp, na.rm = FALSE)]
df[, max_temp := na.approx(max_temp, na.rm = FALSE)]
df[, rain_mm := rollapply(rain_mm, width = 7, FUN = mean, fill = "extend", align = "right")]
df[, wind_speed := na.approx(wind_speed, na.rm = FALSE)]


# Check if missing values arefilled
colSums(is.na(df))

#
# 1.3 Check for Duplicates & Anomalies
#

# Remove duplicate rows if they exist
df <- unique(df)

# Apply log transformation to all numeric variables
df_log <- df %>%
  mutate(across(all_of(numeric_cols), ~log1p(.)))  

# Boxplot after log transformation
df_log %>% 
  pivot_longer(cols = numeric_cols, names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Variable, y = Value)) +
  geom_boxplot(outlier.colour = "red") +
  theme_minimal() +
  coord_flip() +
  ggtitle("Boxplots (Log-Transformed)")


# Identify extreme values using Z-score method
outlier_threshold <- 3  # Standard Z-score threshold
z_scores <- df[, lapply(.SD, function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)), .SDcols = numeric_cols]
outliers <- df[rowSums(abs(z_scores) > outlier_threshold) > 0]

# Print num of detected outliers
print(nrow(outliers))

# Manually Winsorizing numeric columns (capping extreme values at 1st and 99th percentile)
df[, (numeric_cols) := lapply(.SD, function(x) {
  q_low <- quantile(x, 0.01, na.rm = TRUE)   # 1st percentile
  q_high <- quantile(x, 0.99, na.rm = TRUE)  # 99th percentile
  x <- pmax(q_low, pmin(x, q_high))  # Cap values within this range
  return(x)
}), .SDcols = numeric_cols]

summary(df)
str(df)


