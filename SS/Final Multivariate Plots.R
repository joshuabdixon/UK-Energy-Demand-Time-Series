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
# install.packages(c("patchwork","plotly"))

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
    month(date) %in% 3:5 ~ "Spring",
    month(date) %in% 6:8 ~ "Summer",
    month(date) %in% 9:11 ~ "Autumn",
    TRUE ~ "Winter"
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

# Since we are not carrying out statistical modelling, there is no need to make the solar generation stationary

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

# --------------------------------
# Setup
# --------------------------------
# Create "Month", "Season", and "Period" columns
df_ext <- df %>%
  mutate(
    Month = factor(month(date), 
                   levels = 1:12, 
                   labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")),
    Season = case_when(
      month(date) %in% 3:5  ~ "Spring",
      month(date) %in% 6:8  ~ "Summer",
      month(date) %in% 9:11 ~ "Autumn",
      TRUE ~ "Winter"
    ),
    Period = case_when(
      month(date) %in% c(10,11,12,1,2,3) ~ "Oct–Mar",
      TRUE ~ "Apr–Sep"
    )
  )
# All are now factors
str(df_ext)

# --------------------------------
# Uni-variate Visualisations
# --------------------------------

# 1. Histograms + Kernel Density
  plot_hist_density <- function(data, var_name, fill_color = "viridis") {
    # Basic histogram + density overlay
    ggplot(data, aes(x = .data[[var_name]])) +
      geom_histogram(aes(y = ..density.., fill = ..count..),
                     bins = 30, alpha = 0.6, color = "black") +
      geom_density(alpha = 0.3, fill = "red") +
      scale_fill_viridis_c(option = "magma") +  # color-blind friendly
      labs(title = paste("Histogram + Density of", var_name),
           x = var_name, y = "Density") +
      theme_minimal()
  }
  
  # Plot for for each variable:
  plot_hist_density(df_ext, "national_demand")
  plot_hist_density(df_ext, "solar_generation")
  plot_hist_density(df_ext, "min_temp")
  
  # 2. Boxplots (univariate)
  
  plot_box <- function(data, var_name) {
    ggplot(data, aes(x = "", y = .data[[var_name]])) +
      geom_boxplot(outlier.color = "red") +
      labs(title = paste("Boxplot of", var_name),
           x = NULL, y = var_name) +
      theme_minimal()
  }
  
  plot_box(df_ext, "national_demand")
  plot_box(df_ext, "solar_generation")
  plot_box(df_ext, "min_temp")
  
  # 3. Violin Plots (univariate)
  
  plot_violin <- function(data, var_name) {
    ggplot(data, aes(x = "", y = .data[[var_name]])) +
      geom_violin(fill = "skyblue", color = "black") +
      geom_boxplot(width = 0.2, outlier.color = "red", alpha = 0.4) +
      labs(title = paste("Violin Plot of", var_name),
           x = NULL, y = var_name) +
      theme_minimal()
  }
  
  plot_violin(df_ext, "national_demand")
  plot_violin(df_ext, "solar_generation")
  plot_violin(df_ext, "min_temp")
  

plot_violin(df_ext, "national_demand")
plot_violin(df_ext, "solar_generation")
plot_violin(df_ext, "min_temp")

# --------------------------------
# Univariate Plots by Month/Season/Period
# --------------------------------

# 1. Violin by Month

# National Demand
ggplot(df_ext, aes(x = Month, y = national_demand)) +
  geom_violin(aes(fill = Month), color = "black", alpha = 0.7) +
  scale_fill_viridis_d(option = "plasma") +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.3) +
  labs(title = "Violin: National Demand by Month",
       x = "Month", y = "MW") +
  theme_minimal()

# Solar Generation
ggplot(df_ext, aes(x = Month, y = solar_generation)) +
  geom_violin(aes(fill = Month), color = "black", alpha = 0.7) +
  scale_fill_viridis_d(option = "plasma") +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.3) +
  labs(title = "Violin: Solar Generation by Month",
       x = "Month", y = "MW") +
  theme_minimal()

# 2. Violin by Season

# National Demand
ggplot(df_ext, aes(x = Season, y = national_demand)) +
  geom_violin(aes(fill = Season), color = "black", alpha = 0.7) +
  scale_fill_viridis_d(option = "plasma") +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.3) +
  labs(title = "Violin: National Demand by Season",
       x = "Season", y = "MW") +
  theme_minimal()

# Solar Generation
ggplot(df_ext, aes(x = Season, y = solar_generation)) +
  geom_violin(aes(fill = Season), color = "black", alpha = 0.7) +
  scale_fill_viridis_d(option = "plasma") +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.3) +
  labs(title = "Violin: Solar Generation by Season",
       x = "Season", y = "MW") +
  theme_minimal()

# 3. Violin by Period (Oct–Mar vs. Apr–Sep)

# National Demand
ggplot(df_ext, aes(x = Period, y = national_demand)) +
  geom_violin(aes(fill = Period), color = "black", alpha = 0.7) +
  scale_fill_viridis_d(option = "plasma") +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.3) +
  labs(title = "Violin: National Demand by Period",
       x = "Period", y = "MW") +
  theme_minimal()

# Solar Generation
ggplot(df_ext, aes(x = Period, y = solar_generation)) +
  geom_violin(aes(fill = Period), color = "black", alpha = 0.7) +
  scale_fill_viridis_d(option = "plasma") +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.3) +
  labs(title = "Violin: Solar Generation by Period",
       x = "Period", y = "MW") +
  theme_minimal()

# --------------------------------
# Bivariate plots
# --------------------------------
# 1. General
# install.packages("ggExtra")
library(ggExtra)

p_scatter <- ggplot(df_ext, aes(x = national_demand, y = solar_generation)) +
  geom_point(alpha = 0.4) +
  labs(title = "Demand vs. Solar Generation",
       x = "National Demand (MW)",
       y = "Solar Generation (MW)") +
  theme_minimal()

# Add violin marginal distributions
ggExtra::ggMarginal(p_scatter, type = "violin", fill = "lightblue")


# 2. Separate scatter for each season
p_scatter_season <- ggplot(df_ext, aes(x = national_demand, y = solar_generation)) +
  geom_point(alpha = 0.4) +
  facet_wrap(~ Season) +
  labs(title = "Demand vs. Solar Generation by Season",
       x = "National Demand",
       y = "Solar Generation") +
  theme_minimal()

p_scatter_season

# With Colours
p_scatter_color <- ggplot(df_ext, aes(x = national_demand, y = solar_generation, color = Season)) +
  geom_point(alpha = 0.6) +
  scale_color_viridis_d(option = "plasma") +
  labs(title = "Demand vs. Solar Generation (Coloured by Season)",
       x = "National Demand", y = "Solar Generation") +
  theme_minimal()

# Add violin or box marginal:
ggExtra::ggMarginal(p_scatter_color, type = "violin", groupColour = TRUE, groupFill = TRUE)

# 3. Bivariate Plots by Period

# One panel per period
ggplot(df_ext, aes(x = national_demand, y = solar_generation)) +
  geom_point(alpha = 0.4) +
  facet_wrap(~ Period) +
  labs(title = "Demand vs. Solar Generation by Period",
       x = "National Demand", y = "Solar Generation") +
  theme_minimal()

# combined scatter with color for periods:
p_scatter_period <- ggplot(df_ext, aes(x = national_demand, y = solar_generation, color = Period)) +
  geom_point(alpha = 0.6) +
  scale_color_viridis_d(option = "plasma") +
  labs(title = "Demand vs. Solar Generation (Coloured by Period)",
       x = "National Demand", y = "Solar Generation") +
  theme_minimal()

ggMarginal(p_scatter_period, type = "violin", groupColour = TRUE, groupFill = TRUE)

# --------------------------------
# 4. Pairwise Scatterplots
# --------------------------------

# define a smaller data set
df_gpairs <- df_ext %>%
  select(national_demand, solar_generation, min_temp, Season, Period)

# 1. ggpairs coloured by seasob
ggpairs(df_gpairs,
        columns = 1:3,   # numeric columns (demand, solar, min_temp)
        aes(color = Season, alpha = 0.5)) +
  scale_color_viridis_d(option = "plasma") +
  labs(title = "Pairwise Scatterplots: Demand, Solar, Min Temp") +
  theme_minimal()

# 2. ggpairs coloured by period
ggpairs(df_gpairs,
        columns = 1:3,
        aes(color = Period, alpha = 0.5)) +
  scale_color_viridis_d(option = "plasma") +
  labs(title = "Pairwise Scatterplots: Demand, Solar, Min Temp") +
  theme_minimal()




