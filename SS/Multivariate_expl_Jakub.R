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
library(gridExtra)
library(grid)
library(ggplotify)
library(lmtest)
library(vars)
library(urca)
library(factoextra)


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


# Identify numeric columns
numeric_cols <- colnames(df)[sapply(df, is.numeric)]

# Compute Z-scores for numeric columns
outlier_threshold <- 3  # Standard Z-score threshold
z_scores <- df[, lapply(.SD, function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)), .SDcols = numeric_cols]

# Identify which variables contain outliers
outliers_matrix <- abs(z_scores) > outlier_threshold  # TRUE where an outlier exists
outlier_counts <- colSums(outliers_matrix)  # Count number of outliers per variable

# Filter only variables that contain outliers
outlier_vars <- names(outlier_counts[outlier_counts > 0])

# Print variables containing outliers & their count
outlier_summary <- data.frame(Variable = outlier_vars, Outlier_Count = outlier_counts[outlier_vars])
print(outlier_summary)


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

# Compute correlation matrix ## UNUSED
cor_matrix <- cor(numeric_vars, use="complete.obs", method="pearson")

# Plot the UNUSED correlation matrix
ggcorrplot(cor_matrix, 
           method = "circle", 
           type = "upper", 
           lab = TRUE, 
           colors = c("red", "white", "blue"),
           title = "Correlation Matrix of Energy Demand Dataset")

##############################################################################################

# Custom correlation panel (upper panels)
custom_cor <- function(data, mapping, ...) {
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  corr <- cor(x, y, use = "complete.obs")
  corr_text <- sprintf("%.2f", corr)
  
  df <- data.frame(x = 1, y = 1, corr = corr)
  
  ggplot(df, aes(x, y, fill = corr)) +
    geom_tile(color = "white") +
    geom_text(aes(label = corr_text), size = 5) +
    scale_fill_gradient2(
      low = "blue",     # Negative correlation
      mid = "white",    # Zero correlation
      high = "red",     # Positive correlation
      midpoint = 0,
      limits = c(-1, 1)
    ) +
    theme_void() +
    theme(legend.position = "right")
}


my_column_labels <- c(
  "National\nDemand",
  "Wind\nGen",
  "Solar\nGen",
  "Min\nTemp",
  "Max\nTemp",
  "Rain\nmm",
  "Wind\nSpeed",
  "Avg\nPrice\nDaily"
)

options(scipen = 999)

# Create the ggpairs plot
p <- ggpairs(
  numeric_vars,
  columnLabels = my_column_labels,  
  lower = list(
    continuous = wrap("points", alpha = 0.3, size = 0.8, color = "gray40")
  ),
  diag = list(
    continuous = wrap("densityDiag", fill = "gray40", color = "gray40", alpha = 0.3)
  ),
  upper = list(
    continuous = custom_cor
  ),
  title = "Pairwise Scatterplot with Correlation Matrix"
)

# Adjust theme to fix label overlap & give more space
p <- p + theme(
  axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
  axis.text.y = element_text(size = 8),
  strip.text  = element_text(size = 8),
  plot.margin = margin(10, 10, 10, 10)
)


print(p)



#PCA
# Standardize data 
df_scaled <- scale(numeric_vars)

# Compute PCA
pca_model <- prcomp(df_scaled, center = TRUE, scale. = TRUE)

#Visualize PCA Biplot
fviz_pca_biplot(pca_model, label = "var", col.var = "red", col.ind = "blue",
                title = "PCA Biplot of Energy Demand Data")

#Scree Plot (Variance Explained)
fviz_eig(pca_model, addlabels = TRUE, title = "Scree Plot of PCA")

# PCA Contributions
fviz_pca_var(pca_model, col.var = "contrib",
             gradient.cols = c("blue", "red"),
             title = "Variable Contributions to PCA")

# 
# 4. Adjusted Multicollinearity Analysis (VIF)
# 

# Drop max_temp (because of 0.9 correlation with min_temp)
#vif_df <- numeric_vars %>% select(-c(max_temp)) 
vif_df <- numeric_vars 
# Compute Variance Inflation Factor (VIF)
vif_results <- vif(lm(national_demand ~ ., data = vif_df))

# Print VIF results
print(vif_results)

#Drop max_temp (because of 0.9 correlation with min_temp)
vif_df <- numeric_vars %>% select(-c(max_temp)) 


# Set up a grid layout: 3 rows and 2 columns
par(mfrow = c(3, 2))

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


par(mfrow = c(1, 1))

ccf(wind_ts, temp_ts, lag.max = 30, main = "CCF: Wind energy Generation vs. Temperature")

# =====================================
# 5.2 GRANGER CAUSALITY ANALYSIS

optimal_lag <- VARselect(cbind(energy_ts, solar_ts), lag.max = 150, type = "const")
print(optimal_lag)

# Granger Test: Does Temperature Granger-Cause Energy Demand?
grangertest(energy_ts ~ temp_ts, order = 9)

# Granger Test: Does Temperature Granger-Cause Energy Demand?
grangertest(solar_ts ~ temp_ts, order = 9)

# Granger Test: Does Temperature Granger-Cause Energy Demand?
grangertest(temp_ts ~ solar_ts, order = 9)

c# Granger Test: Does Wind Generation Granger-Cause Energy Demand?
grangertest(energy_ts ~ wind_ts, order = 9)

# Granger Test: Does Solar Generation Granger-Cause Energy Demand?
grangertest(energy_ts ~ solar_ts, order = 5)

# Granger Test: Does Energy Demand Granger-Cause Electricity Price?
grangertest(price_ts ~ energy_ts, order = 5)

# Granger Test: Does Electricity Price Granger-Cause Energy Demand?
grangertest(energy_ts ~ price_ts, order = 5)

# Granger Test: Rainfall Granger-Cause Energy Demand?
grangertest(wind_ts ~ energy_ts, order = 5)

# Granger Test: Rainfall Granger-Cause Energy Demand?
grangertest(wind_ts ~ solar_ts, order = 5)

# =====================================
# 5.3 IDENTIFY OPTIMAL LAG STRUCTURES
# =====================================

# **Impulse Response Function (IRF)**
irf_model <- irf(var_model, impulse = "solar", response = "energy", n.ahead = 30)
plot(irf_model)

# **Plot 3: Interactive Multivariate Plot**
p3 <- ggplot(df, aes(x = date)) +
  geom_line(aes(y = national_demand, color = "Energy Demand"), size = 1) +
  geom_line(aes(y = wind_generation, color = "Wind Generation"), size = 1) +
  geom_line(aes(y = solar_generation, color = "Solar Generation"), size = 1) +
  labs(title = "Interactive Multivariate Time Series Plot", x = "Date", y = "Value") +
  theme_minimal()

interactive_p3 <- ggplotly(p3)  # Convert to interactive plot
interactive_p3

# Vector Autoregression (VAR) to Determine Optimal Lags
data_var <- data.frame(energy_ts, solar_ts)

# Find Optimal Lag Length for VAR Model
lag_selection <- VARselect(data_var, lag.max = 365, type = "const")

# Print Recommended Lag
print(lag_selection$selection)

packages <- c("tidyverse", "tseries", "urca", "vars", "forecast")
install.packages(setdiff(packages, installed.packages()[,"Package"]), dependencies = TRUE)

# =====================================
# 7.1 VECTOR AUTOREGRESSION (VAR)
# =====================================

# **Check for Stationarity using Augmented Dickey-Fuller (ADF) Test**
adf_energy <- adf.test(energy_ts)
adf_wind   <- adf.test(wind_ts)
adf_solar  <- adf.test(solar_ts)


# Print results
print(adf_energy)
print(adf_wind)
print(adf_solar)


# **If non-stationary, take first differences**
diff_solar  <- diff(solar_ts)

adf_solar  <- adf.test(diff_solar)
print(adf_solar)


# Combine into a data frame for VAR modeling
var_data <- data.frame(
  energy = energy_ts[-1],  # Remove first row
  wind   = wind_ts[-1],    # Remove first row
  solar  = diff_solar      # Already differenced, so it matches
)

# **Fit a VAR Model**
var_model <- VAR(var_data, p = 29, type = "const")  # Using lag = 5 (adjust if needed)
summary(var_model)



# =====================================
# 7.2 VECTOR ERROR CORRECTION MODEL (VECM)
# =====================================

# **Test for Cointegration using Johansen Test**
johansen_test <- ca.jo(var_data, type = "trace", ecdet = "const", K = 5)
summary(johansen_test)

# **If Cointegration Exists, Fit a VECM Model**
if (johansen_test@teststat[2] > johansen_test@cval[2,2]) {  # Checking significance of the test
  vecm_model <- cajorls(johansen_test, r = 1)  # Rank = 1 means at least one cointegrating relationship
  print(summary(vecm_model))
} else {
  print("No significant cointegration found. Proceeding without VECM.")
}


