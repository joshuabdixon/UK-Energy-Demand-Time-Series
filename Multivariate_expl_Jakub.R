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

##########################################################################################################################
# 2 Initial Multivariate exploration

# Select only numeric variables for correlation and multivariate analysis
numeric_vars <- df %>% select(-date)

# 
# Pairwise Correlation Analysis
# 

# Compute correlation matrix
cor_matrix <- cor(numeric_vars, use="complete.obs", method="pearson")

# Plot the correlation matrix
ggcorrplot(cor_matrix, 
           method = "circle", 
           type = "upper", 
           lab = TRUE, 
           colors = c("red", "white", "blue"),
           title = "Correlation Matrix of Energy Demand Dataset")

#
# Cross-Correlation Function (CCF)
# 

# 
# 3. Adjusted Cross-Correlation Analysis (CCF)
# 



# Convert key variables to time series format
freq <- 365

# Convert all relevant variables into time series format
energy_ts     <- ts(df$national_demand, frequency = freq)
wind_ts       <- ts(df$wind_generation, frequency = freq)
solar_ts      <- ts(df$solar_generation, frequency = freq)
temp_ts       <- ts(df$min_temp, frequency = freq)  # Keeping only min_temp - collinearity
rain_ts       <- ts(df$rain_mm, frequency = freq)
wind_speed_ts <- ts(df$wind_speed, frequency = freq)
price_ts      <- ts(df$average_price_daily, frequency = freq)

# 
# 3. Cross-Correlation Function (CCF) Analysis
#

# Cross-Correlation: Energy Demand vs. Wind Generation
ccf(energy_ts, wind_ts, lag.max = 30, main = "CCF: Energy Demand vs. Wind Generation")

# Cross-Correlation: Energy Demand vs. Solar Generation
ccf(energy_ts, solar_ts, lag.max = 30, main = "CCF: Energy Demand vs. Solar Generation")

# Cross-Correlation: Energy Demand vs. Temperature
ccf(energy_ts, temp_ts, lag.max = 30, main = "CCF: Energy Demand vs. Temperature")

# Cross-Correlation: Energy Demand vs. Rainfall
ccf(energy_ts, rain_ts, lag.max = 30, main = "CCF: Energy Demand vs. Rainfall")

# Cross-Correlation: Energy Demand vs. Wind Speed
ccf(energy_ts, wind_speed_ts, lag.max = 30, main = "CCF: Energy Demand vs. Wind Speed")

# Cross-Correlation: Energy Demand vs. Electricity Price
ccf(energy_ts, price_ts, lag.max = 30, main = "CCF: Energy Demand vs. Electricity Price")


# 
# 4. Adjusted Multicollinearity Analysis (VIF)
# 

# Drop max_temp (because of 0.9 correlation with min_temp)
vif_df <- numeric_vars %>% select(-c(max_temp)) 

# Compute Variance Inflation Factor (VIF)
vif_results <- vif(lm(national_demand ~ ., data = vif_df))

# Print VIF results
print(vif_results)

# =====================================
# 5. Pairwise Scatterplots (For Validation)
# =====================================

# Pairwise scatterplots 
ggpairs(vif_df, 
        aes(color = "blue", alpha = 0.5),
        lower = list(continuous = wrap("points", alpha = 0.3, size = 0.8)),
        upper = list(continuous = wrap("cor", size = 4)),
        title = "Pairwise Scatterplots (Adjusted)")


# Time Series Plots
p1 <- ggplot(df, aes(x = date, y = national_demand)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Energy Demand Over Time", x = "Date", y = "Energy Demand (MW)") +
  theme_minimal()

# Interactive Plot for Energy Demand
p3 <- ggplot(df, aes(x = date, y = national_demand)) +
  geom_line(color = "darkblue", size = 1) +
  labs(title = "Interactive Energy Demand Plot", x = "Date", y = "Energy Demand (MW)") +
  theme_minimal()

interactive_p3 <- ggplotly(p3)

# Rolling Statistics
df <- df %>%
  mutate(
    rolling_avg_7d = rollmean(national_demand, 7, fill = NA, align = "right"),
    rolling_avg_30d = rollmean(national_demand, 30, fill = NA, align = "right"),
    rolling_std_7d = rollapply(national_demand, 7, sd, fill = NA, align = "right"),
    rolling_std_30d = rollapply(national_demand, 30, sd, fill = NA, align = "right")
  )

# Rolling Mean Plot
p4 <- ggplot(df, aes(x = date)) +
  geom_line(aes(y = national_demand, color = "Actual Demand"), size = 1) +
  geom_line(aes(y = rolling_avg_7d, color = "7-Day Rolling Avg"), size = 1) +
  geom_line(aes(y = rolling_avg_30d, color = "30-Day Rolling Avg"), size = 1) +
  labs(title = "Rolling Averages of Energy Demand", x = "Date", y = "Energy Demand (MW)") +
  scale_color_manual(values = c("Actual Demand" = "blue", "7-Day Rolling Avg" = "red", "30-Day Rolling Avg" = "green")) +
  theme_minimal()

# Rolling Correlation
df_rolling <- df %>%
  mutate(
    rolling_corr_temp = rollapply(national_demand, 30, function(x) cor(x, temp_ts[1:length(x)], use = "complete.obs"), fill = NA, align = "right"),
    rolling_corr_wind = rollapply(national_demand, 30, function(x) cor(x, wind_ts[1:length(x)], use = "complete.obs"), fill = NA, align = "right"),
    rolling_corr_solar = rollapply(national_demand, 30, function(x) cor(x, solar_ts[1:length(x)], use = "complete.obs"), fill = NA, align = "right")
  )

# Rolling Correlation Plot
p6 <- ggplot(df_rolling, aes(x = date)) +
  geom_line(aes(y = rolling_corr_temp, color = "Temp vs Demand"), size = 0.2) +
  geom_line(aes(y = rolling_corr_wind, color = "Wind vs Demand"), size = 0.2) +
  geom_line(aes(y = rolling_corr_solar, color = "Solar vs Demand"), size = 0.2) +
  labs(title = "Rolling Correlation Between Energy Demand & Weather", x = "Date", y = "Rolling Correlation") +
  scale_color_manual(values = c("Temp vs Demand" = "blue", "Wind vs Demand" = "red", "Solar vs Demand" = "green")) +
  theme_minimal()

# Display plots
print(p1)
print(p4)
print(p6)
interactive_p3
