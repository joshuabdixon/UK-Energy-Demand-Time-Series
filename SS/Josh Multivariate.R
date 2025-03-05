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
library(conflicted)
# install.packages(c("patchwork","plotly"))
help(fread)
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

# Manually Winsorising numeric columns (capping extreme values at 1st and 99th percentile)
df[, (numeric_cols) := lapply(.SD, function(x) {
  q_low <- quantile(x, 0.01, na.rm = TRUE)   # 1st percentile
  q_high <- quantile(x, 0.99, na.rm = TRUE)  # 99th percentile
  x <- pmax(q_low, pmin(x, q_high))  # Cap values within this range
  return(x)
}), .SDcols = numeric_cols]file_path <- "energy_demand_uk.csv"  
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

# Manually Winsorising numeric columns (capping extreme values at 1st and 99th percentile)
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
conflicted::conflict_prefer("select", "plotly")
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
           lab = FALSE, 
           colors = c("red", "white", "blue"),
           title = "Correlation Matrix of Energy Demand Dataset")

help(ggcorrplot)
# Cross-Correlation Function (CCF)
# 

# 
# 3. Adjusted Cross-Correlation Analysis (CCF)
# 



# Convert key variables to time series format
freq <- 365 # 1 lag is 1 day

## JD Modified TS
# install.packages("tsibble")
library(tsibble)

# Convert df to a tsibble using the date column as the time index
df_tsibble <- df %>% 
  as_tsibble(index = date)

# ts for each variable
energy_ts     <- df_tsibble %>% select(date, national_demand)
wind_ts       <- df_tsibble %>% select(date, wind_generation)
solar_ts      <- df_tsibble %>% select(date, solar_generation)
temp_ts       <- df_tsibble %>% select(date, min_temp)  # Keeping only min_temp
rain_ts       <- df_tsibble %>% select(date, rain_mm)
wind_speed_ts <- df_tsibble %>% select(date, wind_speed)
price_ts      <- df_tsibble %>% select(date, average_price_daily)

price_ts

# Print structure to verify
glimpse(df_tsibble)


# 3. Cross-Correlation Function (CCF) Analysis
#

# Cross-Correlation: Energy Demand vs. Wind Generation
ccf(energy_ts$national_demand, wind_ts$wind_generation, lag.max = 30, main = "CCF: Energy Demand vs. Wind Generation")

# Cross-Correlation: Energy Demand vs. Solar Generation
ccf(energy_ts$national_demand, solar_ts$solar_generation, lag.max = 30, main = "CCF: Energy Demand vs. Solar Generation")

# Cross-Correlation: Energy Demand vs. Temperature
ccf(energy_ts$national_demand, temp_ts$min_temp, lag.max = 30, main = "CCF: Energy Demand vs. Temperature")

# Cross-Correlation: Energy Demand vs. Rainfall
ccf(energy_ts$national_demand, rain_ts$rain_mm, lag.max = 30, main = "CCF: Energy Demand vs. Rainfall")

# Cross-Correlation: Energy Demand vs. Wind Speed
ccf(energy_ts$national_demand, wind_speed_ts$wind_speed, lag.max = 30, main = "CCF: Energy Demand vs. Wind Speed")

# Cross-Correlation: Energy Demand vs. Electricity Price
ccf(energy_ts$national_demand, price_ts$average_price_daily, lag.max = 30, main = "CCF: Energy Demand vs. Electricity Price")


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

# =====================================
# JD Additional Plots
# =====================================
price_ts

# All pairwise plots (not sure how to handle th elower correlated values possibly)
ggpairs(vif_df, 
        aes(color = "blue", alpha = 0.5),
        lower = list(continuous = wrap("points", alpha = 0.3, size = 0.8)),
        upper = list(continuous = wrap("cor", size = 4)),
        title = "Pairwise Scatterplots (Adjusted)")

# Scatterplot matrix with national_demand and solar_generation only
ggpairs(vif_df %>% select(national_demand, solar_generation),
        aes(alpha = 0.5),
        lower = list(continuous = wrap("points", alpha = 0.3, size = 0.8)),
        upper = list(continuous = wrap("cor", size = 4)),
        title = "Pairwise Scatterplots:* national_demand vs solar_generation")

# Scatterplot matrix with national_demand, solar_generationand min_temp only
ggpairs(vif_df %>% select(national_demand, solar_generation, min_temp),
        aes(alpha = 0.5),
        lower = list(continuous = wrap("points", alpha = 0.3, size = 0.8)),
        upper = list(continuous = wrap("cor", size = 4)),
        title = "Pairwise Scatterplots:* national_demand vs solar_generation vs min_temp")

# Scatterplot matrix with national_demand and log(solar_generation) only
ggpairs(vif_df %>% select(national_demand, solar_generation) %>% mutate(log_solar_generation = log(solar_generation)),
        aes(alpha = 0.5),
        lower = list(continuous = wrap("points", alpha = 0.3, size = 0.8)),
        upper = list(continuous = wrap("cor", size = 4)),
        title = "Pairwise Scatterplots:* national_demand vs log(solar_generation)")

# Check solar generation for outliers using Boxplot for solar generation
ggplot(df, aes(y = solar_generation)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16) +
  labs(title = "Boxplot of Solar Generation", y = "Solar Generation (MW)")

# Check the 0 values (or near 0) - WIP
df_tsibble
df_tsibble %>%
  filter(solar_generation < 100) %>%
  select(date, national_demand, solar_generation) %>%
  arrange(date) %>%
  head(20)  # Check first 20 occurrences
print("Number of rows with solar_generation < 10:")
print(nrow(df_tsibble %>% filter(solar_generation < 10)))
#--------------------------------
# Split by Winter and Summer Months
#--------------------------------
# Create monthly column
library(lubridate)
library(dplyr)

# Create categorical columns for each month
df_monthly <- df %>%
  mutate(Month = factor(month(date), levels = 1:12, 
                        labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")))

# Boxplot for monthly demand
ggplot(df_monthly, aes(x = Month, y = national_demand)) +
  geom_boxplot(outlier.color = "red") +
  labs(
    title = "Monthly Boxplots of National Demand",
    x = "Month",
    y = "Demand (MW)"
  ) +
  theme_minimal()

# Boxplot for monthly solar generation
ggplot(df_monthly, aes(x = Month, y = solar_generation)) +
  geom_boxplot(outlier.color = "red") +
  labs(
    title = "Monthly Boxplots of Solar Generation",
    x = "Month",
    y = "Solar Generation (MW)"
  ) +
  theme_minimal()

# Create categorical columns for each season
df_seasonal <- df %>%
  mutate(Season = case_when(
    month(date) %in% 3:5 ~ "Spr",
    month(date) %in% 6:8 ~ "Sum",
    month(date) %in% 9:11 ~ "Aut",
    TRUE ~ "Win"
  ))

# Boxplot for seasonal demand
ggplot(df_seasonal, aes(x = Season, y = national_demand)) +
  geom_boxplot(outlier.color = "red") +
  labs(
    title = "Seasonal Boxplots of National Demand",
    x = "Season",
    y = "Demand (MW)"
  ) +
  theme_minimal()

# Boxplot for seasonal solar generation
ggplot(df_seasonal, aes(x = Season, y = solar_generation)) +
  geom_boxplot(outlier.color = "red") +
  labs(
    title = "Seasonal Boxplots of Solar Generation",
    x = "Season",
    y = "Solar Generation (MW)"
  ) +
  theme_minimal()
# --------------------------------
# Stationarity Checks
# --------------------------------
# ADF Tests
library(tseries)

# For national_demand

help(adf.test)

adf_result_demand <- adf.test(df$national_demand, alternative = "stationary")
print(adf_result_demand) # reject this has a unit root, likely stationary

# For solar_generation
adf_result_solar <- adf.test(df$solar_generation, alternative = "stationary")
print(adf_result_solar) # fail to reject this has a unit root, likely non-stationary

# --------------------------------
# Partial Correlations of solar_generation correlates with national_demand after  controlling for other variables
# --------------------------------
df_tsibble
# Regression model (do not include in final report as this is predictive modelling)
lm_model <- lm(national_demand ~ solar_generation + min_temp + average_price_daily + wind_generation, data = df_tsibble)
summary(lm_model)

# Partial Correlation
# install.packages("ppcor")
library(ppcor)

# Select only the variables of interest
vars_interest <- df_tsibble %>%
  as_tibble() %>%                # convert tsibble back to normal tibble
  select(national_demand, solar_generation)

# Confirm everything is numeric
str(vars_interest)

# Partial correlation
pcor_result <- pcor(vars_interest, method = "pearson")
pcor_result

# --------------------------------
# Parallel Coordinates
# --------------------------------
# Subset numeric columns (excluding 'date')
var_subset <- c("national_demand", "solar_generation")

ggparcoord(
  data = df_tsibble,
  columns = match(var_subset, names(df)),
  groupColumn = NULL,    # or use a factor to color lines, e.g. "month" or "season"
  scale = "std",         # or "uniminmax" etc.
  alphaLines = 0.4
) +
  labs(title = "Parallel Coordinates Plot") +
  theme_minimal()

# --------------------------------
# Seasonal Decomposition
# --------------------------------
df
# Convert to ts object with daily frequency ~ 365
demand_ts_national <- ts(df$national_demand, frequency = 365, start = c(2020, 1))

# Perform STL decomposition (handles daily data if we treat frequency=365)
decomp_result <- stl(demand_ts_national, s.window = "periodic")  
plot(decomp_result, main = "STL Decomposition of National Demand")
help(stl)

# Stationarity Checks after decomposition ADF Tests
adf_result_demand_std <- adf.test(df$national_demand, alternative = "stationary")
print(adf_result_demand_std) # reject this has a unit root, likely stationary

# Convert for solar generation
solar_ts <- ts(df$solar_generation, frequency = 365, start = c(2020, 1))

# Perform STL decomposition (handles daily data if we treat frequency=365)
decomp_result_solar <- stl(solar_ts, s.window = "periodic")

# Convert for national_demand
adf_result_demand <- adf.test(df$national_demand, alternative = "stationary")
print(adf_result_demand) # reject this has a unit root, likely stationary

# Convert for average_price
price_ts <- ts(df$average_price_daily, frequency = 365, start = c(2020, 1))

# Perform STL decomposition (handles daily data if we treat frequency=365)
decomp_result_price <- stl(price_ts, s.window = "periodic")

#================================================================================================
# Final Multivariate Visualisations
#================================================================================================
str(df)
# --------------------------------
# Setup
# --------------------------------
# Create "Month", "Season", and "Period" columns
# prefer lubridate::month
conflicted::conflict_prefer("month", "lubridate")
df_ext <- df %>%
  mutate(
    Month = factor(month(date), 
                   levels = 1:12, 
                   labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")),
    Season = case_when(
      month(date) %in% 3:5  ~ "Spr",
      month(date) %in% 6:8  ~ "Sum",
      month(date) %in% 9:11 ~ "Aut",
      TRUE ~ "Win"
    ),
    Period = case_when(
      month(date) %in% c(10,11,12,1,2,3) ~ "Oct–Mar",
      TRUE ~ "Apr–Sep"
    ),
    Period_Solar = case_when(
      month(date) %in% c(11,12,1,2) ~ "Nov-Feb",
      TRUE ~ "Mar-Oct"
    ),
    
    log_solar_gen = log1p(
      solar_generation),
    
    log_wind_gen = log1p(
      wind_generation),
    
    log_national_demand = log1p(
      national_demand)
  )
# All are now factors
str(df_ext)

# # Add log transformation of solar generation
# df_ext <- df_ext %>%
#   mutate(log_solar_gen = log(solar_generation))

# Colour blind friendly pallette
cbbPalette <- c("#000000","#E69F00","#56B4E9","#009E73",
                "#F0E442","#0072B2","#D55E00","#CC79A7")

df_ext
# --------------------------------
# Uni-variate Visualisations
# --------------------------------
# Boxplot for national_demand



# 1. Histograms + Kernel Density
  plot_hist_density <- function(data, var_name, fill_color = "viridis") {
    # Basic histogram + density overlay
    ggplot(data, aes(x = .data[[var_name]])) +
      geom_density(alpha = 0.3, fill = "gray") +
      scale_fill_viridis_c(option = "magma") +  # color-blind friendly
      labs(title = paste("Distribution of ", var_name),
           x = var_name, y = "Density") +
      theme_minimal()
  }
  
  # Plot for for each variable:
  plot_hist_density(df_ext, "national_demand")
  plot_hist_density(df_ext, "solar_generation")
  plot_hist_density(df_ext, "min_temp")
  
  
  # Plot log solar generation
  plot_hist_density(df_ext, "log_solar_gen")

  df_ext
  # Combined distribution plot
  # Prefer plotly::select
  
  df_forplot <- df_ext %>%
    select(wind_generation, solar_generation)
  
  # Pivot longer to “variable” and “value” columns:
  df_melt <- df_forplot %>%
    pivot_longer(cols = everything(), 
                 names_to = "Variable", 
                 values_to = "Value")

  # 2 univariate solar and wind
  ggplot(df_melt, aes(x = Value, fill = Variable)) +
    geom_density(alpha = 0.3) +
    scale_fill_manual(values = c("#56B4E9", "#D55E00")) +
    labs(title = "Distribution of Wind and Solar Generation)#",
         x = "Value", y = "Density") +
    theme_minimal()
  
  # 2 univariate demand and wind
  df_forplot2 <- df_ext %>%
    select(wind_generation, national_demand)
  
  df_melt_demand_wind <- df_forplot2 %>%
    pivot_longer(cols = everything(), 
                 names_to = "Variable", 
                 values_to = "Value")
  
  ggplot(df_melt_demand_wind, aes(x = Value, fill = Variable)) +
    geom_density(alpha = 0.3) +
    scale_fill_manual(values = c("#56B4E9", "#D55E00")) +
    labs(title = "Distribution of National Demand and Wind") +
    theme_minimal()
  
  plot_box(df_ext, "national_demand")
  plot_box(df_ext, "solar_generation")
  plot_box(df_ext, "min_temp")
  plot_box(df_ext, "log_solar_gen")
  
  # 3. Violin Plots (univariate)
  
  plot_violin <- function(data, var_name) {
    ggplot(data, aes(x = "", y = .data[[var_name]])) +
      geom_violin(color = "black") +
      geom_boxplot(width = 0.2, outlier.color = "black", alpha = 0.4) +
      labs(title = paste("Violin Plot of", var_name),
           x = NULL, y = var_name) +
      theme_minimal()
  }
  
  plot_violin(df_ext, "national_demand")
  plot_violin(df_ext, "solar_generation")
  plot_violin(df_ext, "min_temp")
  plot_violin(df_ext, "log_solar_gen")

# --------------------------------
# Univariate Plots by Month/Season/Period
# --------------------------------

# 1. Violin by Month

# # National Demand
ggplot(df_ext, aes(x = Month, y = national_demand)) +
  geom_violin(color = "black", alpha = 0.7) +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.3) +
  labs(title = "Violin: National Demand by Month",
       x = "Month", y = "MW")
  
# Solar Generation
ggplot(df_ext, aes(x = Month, y = solar_generation)) +
  geom_violin(color = "black", alpha = 0.7) +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.3) +
  labs(title = "Violin: Solar Generation by Month",
       x = "Month", y = "MW")

# Check for 0 values for solar generaiton
df_ext %>%
  filter(solar_generation == 0) %>%
  select(date, national_demand, solar_generation) %>%
  arrange(date) %>%
  head(20)  # Check first 20 occurrences

# Return month labels that had 0 solar generation
df_ext %>%
  filter(solar_generation == 0) %>%
  pull(Month) %>%
  unique()

df_ext %>%
  filter(solar_generation == 0) %>%
  count(Month, name = "Zero_Generation_Days")

df_ext %>%
  filter(solar_generation == 0) %>%
  mutate(Year = lubridate::year(date)) %>%  # Extract Year from date
  count(Year, Month, name = "Zero_Generation_Days") %>%
  arrange(Year, Month)  # Order results by Year and Month

# Every year from November to February had 28-31 zero-generation fays

# Log Solar Generation
ggplot(df_ext, aes(x = Month, y = log_solar_gen)) +
  geom_violin(color = "black", alpha = 0.7) +
  labs(title = "Violin: Log(Solar Generation) by Month",
       x = "Month", y = "MW") +
  theme_minimal()

# Solar Generation Boxplot
ggplot(df_ext, aes(x = Month, y = solar_generation)) +
  geom_boxplot(outlier.color = "black") +
  labs(title = "Boxplot of Solar Generation by Month",
       x = "Month", y = "Solar Generation") +
  theme_minimal()

# Log Solar Generation Boxplot
ggplot(df_ext, aes(x = Month, y = log_solar_gen)) +
  geom_boxplot(outlier.color = "black") +
  labs(title = "Boxplot of Log(Solar Generation) by Month",
       x = "Month", y = "Log(Solar Generation)") +
  theme_minimal()

# min_temp
ggplot(df_ext, aes(x = Month, y = min_temp)) +
  geom_violin(color = "black", alpha = 0.7) +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.3) +
  labs(title = "Violin: Min Temperature by Month",
       x = "Month", y = "Min Temperature (°C)") +
  theme_minimal()

# 2. Violin by Season

# # National Demand
# ggplot(df_ext, aes(x = Season, y = national_demand)) +
#   geom_violin(aes(fill = Season), color = "black", alpha = 0.7) +
#   scale_fill_viridis_d(option = "plasma") +
#   geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.3) +
#   labs(title = "Violin: National Demand by Season",
#        x = "Season", y = "MW") +
#   theme_minimal()
# 
# # Solar Generation
# ggplot(df_ext, aes(x = Season, y = solar_generation)) +
#   geom_violin(aes(fill = Season), color = "black", alpha = 0.7) +
#   scale_fill_viridis_d(option = "plasma") +
#   geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.3) +
#   labs(title = "Violin: Solar Generation by Season",
#        x = "Season", y = "MW") +
#   theme_minimal()

# 3. Violin by Period (Oct–Mar vs. Apr–Sep)

# National Demand
ggplot(df_ext, aes(x = Period, y = national_demand)) +
  geom_violin() +
  # scale_fill_viridis_d(option = "D") +
  geom_boxplot(width = 0.2, alpha = .7) +
  labs(x = "Period", y = "National Demand (MW)")

# National Demand (no period)
ggplot(df_ext, aes(x = "", y = national_demand)) +
  geom_violin() +
  # scale_fill_viridis_d(option = "D") +
  geom_boxplot(width = 0.2, alpha = 0.7) +
  labs(x = NULL, y = "National Demand (MW)")

help(scale_fill_viridis_d)
help(aes)

# Solar Generation
ggplot(df_ext, aes(x = Period, y = solar_generation)) +
  geom_violin() +
  scale_fill_viridis_d(option = "plasma") +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.3) +
  labs(title = "Violin: Solar Generation by Period",
       x = "Period", y = "Solar Generation (MW)") +
  theme_minimal()

# Wind Generation
ggplot(df_ext, aes(x = Period, y = wind_generation)) +
  geom_violin() +
  scale_fill_viridis_d(option = "plasma") +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.3) +
  labs(title = "Violin: Wind Generation by Period",
       x = "Period", y = "Wind Generation (MW)") +
  theme_minimal()


# Temperature
ggplot(df_ext, aes(x = Period, y = min_temp)) +
  geom_violin() +
  # scale_fill_viridis_d(option = "plasma") +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.3) +
  labs(title = "Violin: Min Temperature by Period",
       x = "Period", y = "Min Temperature (°C)") +
  theme_minimal()

# Log solar generation
ggplot(df_ext, aes(x = Period, y = log_solar_gen)) +
  geom_violin() +
  scale_fill_viridis_d(option = "plasma") +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.3) +
  labs(title = "Violin: Log(Solar Generation) by Period",
       x = "Period", y = "Log(Solar Generation)") +
  theme_minimal()

library(ggplot2)
library(dplyr)
library(tidyr)

# Demand Solar and Wind
# Tidy format
df_tidy <- df_ext %>%
  select(Period, national_demand, solar_generation, wind_generation,) %>%
  pivot_longer(
    cols = c(national_demand, solar_generation, wind_generation),
    names_to = "variable",
    values_to = "value"
  )

# Box plot combined by period
ggplot(df_tidy, aes(x = Period, y = value)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y")

# Violin by period
ggplot(df_tidy, aes(x = Period, y = value)) +
  geom_violin() +
  facet_wrap(~ variable, scales = "free_y")

# Box combined with violin addition
# 1) Tidy data with 'Period' and 'variable', 'value'
df_tidy <- df_ext %>%
  select(Period, national_demand, solar_generation, wind_generation) %>%
  pivot_longer(
    cols = c(national_demand, solar_generation, wind_generation),
    names_to = "variable",
    values_to = "value"
  )

# 2) Plot a single violin ignoring Period, plus separate boxplots by Period
ggplot() +
  facet_wrap(~ variable, scales = "free_y") +
  
  # (a) Single violin for entire distribution, ignoring Period
  geom_violin(
    data = df_tidy,
    aes(x = "All", y = value),
    fill = "gray80", alpha = 0.3, color = "black", linewidth = 0.5
  ) +
  
  # (b) Boxplots split by Period
  geom_boxplot(
    data = df_tidy,
    aes(x = Period, y = value),
    width = 0.4, outlier.shape = 16
  ) +
  
  # 3) Force the x scale to have three discrete slots: "All", "Apr–Sep", "Oct–Mar"
  scale_x_discrete(
    name = NULL,
    limits = c("All", "Apr–Sep", "Oct–Mar") 
  ) +
  
  # labs(
  #   title = "Violin of Entire Distribution vs. Boxplots by Period"
  # ) +
  theme_minimal()


  
# Box plot combined overall
ggplot(df_tidy, aes(x = "", y = value)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y")

help(facet_wrap)
# Log 

ggplot(df_tidy, aes(x = variable, y = value, fill = Period)) +
  geom_violin(position = position_dodge(width=0.8)) +
  scale_y_log10() +
  labs(title = "Violin: Demand vs. Solar vs. Wind on Log Scale")

# Tidy with logs
df_tidy_dem_sol <- df_ext %>%
  select(Period, national_demand, solar_generation) %>%
  pivot_longer(
    cols = c(national_demand, solar_generation),
    names_to = "variable",
    values_to = "value"
  )

df_tidy_all_log <- df_ext %>%
  select(log_national_demand, log_solar_gen, log_wind_gen) %>%
  pivot_longer(
    cols = c(log_national_demand, log_solar_gen, log_wind_gen),
    names_to = "variable",
    values_to = "value"
  )

# Box plot combbined by period Demand vs Solar
ggplot(df_tidy_dem_sol, aes(x = Period, y = value)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "Boxplots by Variable and Period in MW")


# Demand Log_Solar and Wind 
# Tidy format just solar log
df_tidy_log_solar <- df_tidy_log_solar %>%
  mutate(
    variable = factor(variable,
                      levels = c("national_demand", "log_solar_gen", "wind_generation")
    )
  )


# Violins
## Just solar log
ggplot(df_tidy_log_solar, aes(x = Period, y = value)) +
  geom_violin() +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "national_demand (MW) vs. log_solar_gen (log(MW)) vs. wind_generation (MW)")



## Just solar log
ggplot(df_tidy, aes(x = variable, y = value, fill = Period)) +
  geom_violin(position = position_dodge(width=0.8)) +
  scale_y_log10() +
  labs(title = "Violin: Demand vs. Solar vs. Wind on Log Scale")

## Boxplots
ggplot(df_tidy_log_solar, aes(x = Period, y = value)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "Boxplots: log_solar_gen (log(MW)) vs. Demand (MW) vs. Wind (MW) by Period")

# All log
ggplot(df_tidy_all_log, aes(x = Period, y = value)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "Boxplots: log_solar_gen (log(MW)) vs. Demand (MW) vs. Wind (MW) by Period")

df_tidy_all_log
df_tidy_log_solar

#  Solar and Wind
# Tidy
df_tidy_no_demand <- df_ext %>%
  select(Period, solar_generation, wind_generation,) %>%
  pivot_longer(
    cols = c(solar_generation, wind_generation),
    names_to = "variable",
    values_to = "value"
  )

ggplot(df_tidy_no_demand, aes(x = Period, y = value)) +
  geom_violin() +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "Violin Plots by Variable and Period")

ggplot(df_tidy_no_demand, aes(x = Period, y = value)) +
  geom_violin() +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "Violin Plots by Variable and Period")
help(facet_wrap)
# Log 

ggplot(df_tidy_no_demand, aes(x = variable, y = value, fill = Period)) +
  geom_violin(position = position_dodge(width=0.8)) +
  scale_y_log10() +
  labs(title = "Violin: Solar vs. Wind on Log Scale")

# Scale adjustments
df_scaled <- df_ext %>%
  mutate(
    solar_scaled = solar_generation * 30,
    wind_scaled  = wind_generation * 10
  )

df_tidy_scaled <- df_scaled %>%
  select(Period, national_demand, solar_scaled, wind_scaled) %>%
  pivot_longer(
    cols = c("national_demand", "solar_scaled", "wind_scaled"),
    names_to = "variable",
    values_to = "value"
  )

ggplot(df_tidy_scaled, aes(x = variable, y = value, fill = Period)) +
  geom_violin(position = position_dodge(width=0.8)) +
  labs(title = "Demand vs. Scaled Solar & Wind")


# Secondary Axis Plot
scaleFactor <- 20

df_tidy_scaled2 <- df_ext %>%
  mutate(
    solar_scaled = solar_generation * scaleFactor,
    wind_scaled  = wind_generation  * scaleFactor
  ) %>%
  pivot_longer(
    cols = c("national_demand", "solar_scaled", "wind_scaled"),
    names_to = "variable",
    values_to = "value"
  )

ggplot(df_tidy_scaled2, aes(x = variable, y = value, fill = Period)) +
  geom_violin(position = position_dodge(width=0.8)) +
  scale_y_continuous(
    name = "National Demand (MW)",
    sec.axis = sec_axis(
      ~ . / scaleFactor,
      name = "Solar / Wind (MW, real scale)"
    )
  ) +
  labs(title = "Demand vs. Scaled Solar & Wind, With Two Axes") +
  theme_minimal()

# 3.1 Violin by Period (Mar-Oct vs. Nov-Feb) - Solar Generation

# --------------------------------
# Bivariate plots
# --------------------------------
# 1. General
# install.packages("ggExtra")
library(ggExtra)

# Colour blind friendly pallette
cbbPalette <- c("#000000","#E69F00","#56B4E9","#009E73",
                "#F0E442","#0072B2","#D55E00","#CC79A7")

# Demand vs Solar Gen
p_scatter <- ggplot(df_ext, aes(x = national_demand, y = solar_generation)) +
  geom_point(alpha = 0.3) +
  labs(title = "Demand vs. Solar Generation",
       x = "National Demand (MW)",
       y = "Solar Generation (MW)") +
  theme_minimal()

# Add violin marginal distributions
ggExtra::ggMarginal(p_scatter, type = "violin", fill = "#000000", alpha = .1)
ggExtra::ggMarginal(p_scatter, type = "violin", fill = "#000000", alpha = .2)

help(ggMarginal)

# Demand vs Wind
p_scatter_wind <- ggplot(df_ext, aes(x = national_demand, y = wind_generation)) +
  geom_point(alpha = 0.3) +
  labs(title = "Demand vs. Wind Generation",
       x = "National Demand (MW)",
       y = "Wind Generation (MW)") +
  theme_minimal()

# Add violin marginal distributions
ggExtra::ggMarginal(p_scatter_wind, type = "violin", fill = "#56B4E9")

# Demand vs Min Temp
p_scatter_temp <- ggplot(df_ext, aes(x = national_demand, y = min_temp)) +
  geom_point(alpha = 0.3) +
  labs(title = "Demand vs. Min Temperature",
       x = "National Demand (MW)",
       y = "Min Temperature (°C)") +
  theme_minimal()

# Add violin marginal distributions
ggExtra::ggMarginal(p_scatter_temp, type = "violin", fill = "#56B4E9")


# # 2. Separate scatter for each season
# p_scatter_season <- ggplot(df_ext, aes(x = national_demand, y = solar_generation)) +
#   geom_point(alpha = 0.4) +
#   facet_wrap(~ Season) +
#   labs(title = "Demand vs. Solar Generation by Season",
#        x = "National Demand",
#        y = "Solar Generation") +
#   theme_minimal()
# 
# p_scatter_season
# 
# # With Colours
# p_scatter_color <- ggplot(df_ext, aes(x = national_demand, y = solar_generation, color = Season)) +
#   geom_point(alpha = 0.6) +
#   scale_color_viridis_d(option = "plasma") +
#   labs(title = "Demand vs. Solar Generation (Coloured by Season)",
#        x = "National Demand", y = "Solar Generation") +
#   theme_minimal()
# 
# # Add violin or box marginal:
# ggExtra::ggMarginal(p_scatter_color, type = "violin", groupColour = TRUE, groupFill = TRUE)

# 3. Bivariate Plots by Period

# One panel per period
ggplot(df_ext, aes(x = national_demand, y = solar_generation)) +
  geom_point(alpha = 0.4) +
  facet_wrap(~ Period) +
  labs(title = "Demand vs. Solar Generation",
       x = "National Demand (MW)", y = "Solar Generation (MW)") +
  theme_minimal()

# combined scatter with color for periods:
p_scatter_period <- ggplot(df_ext, aes(x = national_demand, y = solar_generation, color = Period)) +
  geom_point(alpha = 0.4) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Demand vs. Solar Generation",
       x = "National Demand (MW)", y = "Solar Generation (MW)") +
  theme_minimal()

ggMarginal(p_scatter_period, type = "violin", groupColour = TRUE, groupFill = TRUE)

# combined scatter with color for periods (no marginal):
p_scatter_period <- ggplot(df_ext, aes(x = national_demand, y = solar_generation, color = Period)) +
  geom_point(alpha = 0.4) +
  scale_fill_brewer(palette = "") +
  labs(title = "Demand vs. Solar Generation",
       x = "National Demand (MW)", y = "Solar Generation (MW)") +
  theme_minimal()

p_scatter_period

ggExtra::ggMarginal(p_scatter_period, type = "violin")
help(ggMarginal)


# Log solar generation equivalent
p_scatter_log <- ggplot(df_ext, aes(x = national_demand, y = log_solar_gen, color = Period)) +
  geom_point(alpha = 0.4) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Demand vs. Log(Solar Generation)",
       x = "National Demand (MW)", y = "Log(Solar Generation)") +
  theme_minimal()

ggMarginal(p_scatter_log, type = "violin", groupColour = TRUE, groupFill = TRUE)
ggMarginal(p_scatter_log, type = "boxplot", groupColour = TRUE, groupFill = TRUE)
ggMarginal(p_scatter_log, type = "densigram", groupColour = TRUE, groupFill = TRUE)
ggMarginal(p_scatter_log, type = "densigram", groupColour = TRUE, groupFill = TRUE)


# Demand vs Temperature
ggplot(df_ext, aes(x = national_demand, y = min_temp)) +
  geom_point(alpha = 0.4) +
  facet_wrap(~ Period) +
  labs(title = "Demand vs. Min Temperature",
       x = "National Demand (MW)", y = "Min Temperature (°C)") +
  theme_minimal()

# combined scatter with color for periods:
p_scatter_temp_period <- ggplot(df_ext, aes(x = national_demand, y = min_temp, color = Period)) +
  geom_point(alpha = 0.4) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Demand vs. Min Temperature",
       x = "National Demand (MW)", y = "Min Temperature (°C)") +
  theme_minimal()

ggMarginal(p_scatter_temp_period, type = "violin", groupColour = TRUE, groupFill = TRUE)

# --------------------------------
# 4. Pairwise Scatterplots
# --------------------------------

# Colour blind friendly pallette
cbbPalette <- c("#000000","#E69F00","#56B4E9","#009E73",
                "#F0E442","#0072B2","#D55E00","#CC79A7")

df_ext

# define a smaller data set
df_gpairs <- df_ext %>%
  dplyr::select(national_demand,#1  
                solar_generation,#2
                min_temp,#3
                log_solar_gen,#4
                Period,#5
                wind_generation,#6
                Month, #7
                Season) #8

# # 1. ggpairs coloured by seasob
# ggpairs(df_gpairs,
#         columns = 1:3,   # numeric columns (demand, solar, min_temp)
#         aes(color = Season, alpha = 0.5)) +
#   scale_color_viridis_d(option = "plasma") +
#   labs(title = "Pairwise Scatterplots: Demand, Solar, Min Temp") +
#   theme_minimal()
df_gpairs

# 2. ggpairs coloured by period
ggpairs(df_gpairs,
        columns = 1:3,
        upper = list(continuous = wrap("cor", method = "spearman")), # spearman allows for non-linear relationships (warnings are ok, p values approx)
        aes(color = Period, alpha = 0.5)) +
  labs(title = "Pairwise Scatterplots: Demand, Solar Generation , Temperature") +
  theme_minimal()

# 3. ggpairs with log(solar_generation) by period
str(df_gpairs)              # Check structure
colSums(is.na(df_gpairs))   # Ensure no column is all NA


# demand solar wind and temp
ggpairs(
  data = df_gpairs,
  columns = c("national_demand", "log_solar_gen", "wind_generation", "min_temp"),
  mapping = aes(color = Period, alpha = 0.5),
  diag = list(continuous = "densityDiag"),
  lower = list(continuous = "points"),
  upper = list(continuous = wrap("cor", method = "spearman")) # spearman allows for non-linear relationships

)

# demand solar wind and temp and season
ggpairs(
  data = df_gpairs,
  columns = c("national_demand", "log_solar_gen", "min_temp", "Season"),
  mapping = aes(color = Period, alpha = 0.5),
  diag = list(continuous = "densityDiag"),
  lower = list(continuous = "points"),
  upper = list(continuous = "cor")
)

# demand solar temp
ggpairs(
  data = df_gpairs,
  columns = c("national_demand", "log_solar_gen", "min_temp"),
  mapping = aes(color = Period, alpha = 0.5),
  diag = list(continuous = "densityDiag"),
  lower = list(continuous = "points"),
  upper = list(continuous = "cor")
)

# wind temp and solar Demand
ggpairs(
  data = df_gpairs,
  columns = c("national_demand", "log_solar_gen", "wind_generation", "min_temp"),
  mapping = aes(color = Period, alpha = 0.5),
  diag = list(continuous = "densityDiag"),
  lower = list(continuous = "points"),
  upper = list(continuous = wrap("cor", method = "spearman")) # spearman allows for non-linear relationships
)

# wind temp and solar Season
ggpairs(
  data = df_gpairs,
  columns = c("national_demand", "log_solar_gen", "wind_generation", "min_temp", "Season"),
  mapping = aes(color = Period, alpha = 0.5),
  diag = list(continuous = "densityDiag"),
  lower = list(continuous = "points"),
  upper = list(continuous = wrap("cor", method = "spearman")) # spearman allows for non-linear relationships
)

# Wind solar demand season
ggpairs(
  data = df_gpairs,
  columns = c("national_demand", "log_solar_gen", "wind_generation", "Season"),
  mapping = aes(color = Period, alpha = 0.5),
  diag = list(continuous = "densityDiag"),
  lower = list(continuous = "points"),
  upper = list(continuous = wrap("cor", method = "spearman")) # spearman allows for non-linear relationships
)
help(ggpairs)
# solar and season
ggpairs(
  data = df_gpairs,
  columns = c("national_demand", "log_solar_gen", "Season"),
  mapping = aes(color = Period, alpha = 0.5),
  diag = list(continuous = "densityDiag"),
  lower = list(continuous = "points"),
  upper = list(continuous = "cor")
)

help(ggpairs)

help(scale_color_viridis_d)

# Colour blind friendly pallette
cbbPalette <- c("#000000","#E69F00","#56B4E9","#009E73",
                "#F0E442","#0072B2","#D55E00","#CC79A7")

# ggpairs demand vs temperature
ggpairs(df_gpairs,
        columns = c(1, 3),   # demand, min_temp
        aes(color = Period, alpha = 0.5)) +
  labs(title = "Pairwise Scatterplots: Demand, Temperature") +
  theme_minimal()

# ggpairs demand vs wind
ggpairs(df_gpairs,
        columns = c(1, 6),   # demand, wind
        aes(color = Period, alpha = 0.5)) +
  labs(title = "Pairwise Scatterplots: Demand, Wind") +
  theme_minimal()

help(ggpairs)

df_gpairs

summary(df_gpairs$solar_generation)

# --------------------------------
# 4. Pairwise Scatterplots with Histograms
# --------------------------------
df_ext

# define a smaller data set
df_gpairs <- df_ext %>%
  dplyr::select(national_demand, solar_generation, min_temp, log_solar_gen, Period)

# # 1. ggpairs coloured by seasob
# ggpairs(df_gpairs,
#         columns = 1:3,   # numeric columns (demand, solar, min_temp)
#         aes(color = Season, alpha = 0.5)) +
#   scale_color_viridis_d(option = "plasma") +
#   labs(title = "Pairwise Scatterplots: Demand, Solar, Min Temp") +
#   theme_minimal()

# 2. ggpairs coloured by period
ggpairs(df_gpairs,
        columns = 1:3,
        aes(color = Period, alpha = 0.5)) +
  labs(title = "Pairwise Scatterplots: Demand, Solar Generation , Temperature") +
  theme_minimal()

help(ggpairs)
help(aes)

# 3. ggpairs with log(solar_generation) by period
ggpairs(df_gpairs,
        columns = c(1, 4, 3),   # demand, log(solar), min_temp
        aes(color = Period, alpha = 0.5)) +
  labs(title = "Pairwise Scatterplots: Demand, Log(Solar Generation), Temperature") +
  theme_minimal()

df_gpairs

summary(df_gpairs$solar_generation)

