install.packages(c("readxl", "urca","forecast","FinTS","fGarch","timeSeries","var","svars"))

#Clear the workspace
rm(list = ls())

library(readxl)
library(dplyr)
library(tseries)
library(urca)
library(forecast)
library(FinTS)
library(timeSeries)
library(fGarch)
library(vars)
library(svars)

par(mfrow = c(1, 1))

#Data preparation and cleaning for analysis in R
################################################################################################
# Data import
data <- read_excel("S176067.xlsx",
                   col_types = c("date", "numeric", "numeric", "numeric", "numeric"),
                   skip = 1)

# Remove non-data row 
data <- data[-1, ]

# Rename columns
colnames(data) <- c("Date", "SPX_price", "SPX_volume", "DJ_price", "DJ_volume")

# Order by date
data <- data %>% arrange(Date)

# Format date
data$Date <- as.Date(data$Date, format = "%Y/%m/%d")

# Create and add to the dataset log prices 
data <- data %>%
  mutate(SPX_log = log(SPX_price),
         DJ_log  = log(DJ_price))

# Create and add to the dataset log volumes
data <- data %>%
  mutate(SPX_log_vol = log(SPX_volume),
         DJ_log_vol  = log(DJ_volume))

################################################################################################
#Initial plots and graphs
################################################################################################

# Plot log prices SPX & DJ
matplot(data$Date, cbind(data$SPX_log, data$DJ_log), type = "l", lty = 1, 
        col = c("blue", "red"), xlab = "Date", ylab = "", main = "Log Prices: SPX vs DJ")
legend("topleft", legend = c("SPX", "DJ"), col = c("blue", "red"), lty = 1, bty = "n")
grid()

plot(data$Date, data$SPX_log, type = "l")
plot(data$Date, data$DJ_log, type = "l")

# Standardize log-prices for comparison
data$SPX_log <- scale(data$SPX_log)
data$DJ_log <- scale(data$DJ_log)

# Comparison of standardised lof prices
plot(data$Date, data$SPX_log, type = "l", col = "blue", xlab = "Date", ylab = "", main = "Log Prices: SPX vs DJ standardized")
legend("topleft", legend = c("SPX", "DJ"), col = c("blue", "red"), lty = 1, bty = "n")
lines(data$Date, data$DJ_log, col = "red")

################################################################################################
# Augmented Dickey-Fuller and KPSS Tests for SPX_log and DJ_log

# ADF test with drift (we start from lag 12 as we did in the exercise class)
adf_spx <- ur.df(data$SPX_log, type = "drift", lags = 12)
summary(adf_spx)

adf_dj <- ur.df(data$DJ_log, type = "drift", lags = 12)
summary(adf_dj)

#reducing the number of lags until the last lag is significant leads to:
adf_spx <- ur.df(data$SPX_log, type = "drift", lags = 9)
summary(adf_spx)

adf_dj <- ur.df(data$DJ_log, type = "drift", lags = 9)
summary(adf_dj)

# KPSS test 
kpss_spx <- kpss.test(data$SPX_log, null = "Level")
print(kpss_spx)

kpss_dj <- kpss.test(data$DJ_log, null = "Level")
print(kpss_spx)

# ADF test with trend
adf_spx <- ur.df(data$SPX_log, type = "trend", lags = 9)
summary(adf_spx)

adf_dj <- ur.df(data$DJ_log, type = "trend", lags = 9)
summary(adf_dj)

################################################################################################
#ARIMA ANALYSIS:
################################################################################################

data$time <- 1:nrow(data)

# Detrend 
SPX_detr <- residuals(lm(SPX_log ~ time, data = data))
DJ_detr  <- residuals(lm(DJ_log  ~ time, data = data))

# Test again whether the trend is significant or not:
ACF_spx_detr <- ur.df(SPX_detr, type = "none", lags = 9)
summary(ACF_spx_detr)

ACF_dj_detr <- ur.df(DJ_detr, type = "none", lags = 9)
summary(ACF_dj_detr)

# Let's ploot the residuals to check if FD is needed
# ACF and PACF plots for SPX detrended 
acf(SPX_detr)
pacf(SPX_detr)

# ACF and PACF plots for DJ detrended 
acf(DJ_detr)
pacf(DJ_detr)

# We can clearly see that ACF is persistent and decreases slowly (almost linearly)
# First difference is required

# First difference of detrended log prices
SPX_detr_diff <- diff(SPX_detr)
DJ_detr_diff  <- diff(DJ_detr)

# Now that residuals have been detrended and FD, we can plot ACF/PACF again

# ACF and PACF plots for SPX detrended and differenced
acf(SPX_detr_diff)
pacf(SPX_detr_diff)

# ACF and PACF plots for DJ detrended and differenced
acf(DJ_detr_diff)
pacf(DJ_detr_diff)

#residuals plot
plot(SPX_detr_diff, type = "l")
plot(DJ_detr_diff, type = "l")

# Ljung-Box test on SPX and spx residuals
Box.test(SPX_detr_diff, lag = 10, type = "Ljung-Box")
Box.test(DJ_detr_diff,  lag = 10, type = "Ljung-Box")

# SPX - ARMA model selection
arma_spx_aic <- auto.arima(SPX_detr_diff, d = 0, stationary = TRUE, stepwise = FALSE, approximation = FALSE)
summary(arma_spx_aic)

# DJ - ARMA model selection
arma_dj_aic <- auto.arima(DJ_detr_diff, d = 0, stationary = TRUE, stepwise = FALSE, approximation = FALSE)
summary(arma_dj_aic)

# Ljung-Box test on SPX and spx residuals
Box.test(residuals(arma_spx_aic), lag = 4, type = "Ljung-Box")
Box.test(residuals(arma_dj_aic), lag = 4, type = "Ljung-Box")

# Ljung-Box test on SPX and spx residuals
Box.test(residuals(arma_spx_aic), lag = 5, type = "Ljung-Box")
Box.test(residuals(arma_dj_aic), lag = 5, type = "Ljung-Box")

################################################################################################
#VOLATILITY ANALYSIS:
################################################################################################

# Compute squared residuals
spx_sq <- SPX_detr_diff^2
dj_sq  <- DJ_detr_diff^2

# Plot squared residuals
plot(spx_sq, type = "l")
plot(dj_sq,  type = "l")

# Computing square residuals SPX
acf(SPX_detr_diff^2)
pacf(SPX_detr_diff^2)

# Computing square residuals DJ
acf(DJ_detr_diff^2)
pacf(DJ_detr_diff^2)

# ARCH test (lag = 30)
ArchTest(SPX_detr_diff, lags = 30)
ArchTest(DJ_detr_diff,  lags = 30)

# ARCH test (lag = 5)
ArchTest(SPX_detr_diff, lags = 5)
ArchTest(DJ_detr_diff,  lags = 5)


# HERE I HAVE RUNNED SEVERAL MODELS TO CHECK WHICH ONE WAS THE BEST ONE TO INCLUDE IN MY PAPER
# IF YOU ARE NOT INTRESTED IN RUNNING ALL OF THEM I INCLUDED THE FINAL MODELS THAT I USED IN MY
# PAPER AT THE END IN THE SECTION CALLED "FINAL MODELS"

################################### ARCH MODELS ################################################## 
# ARCH(1) - SPX
arch_spx_1 <- garch(na.omit(SPX_detr_diff), order = c(1, 0))
summary(arch_spx_1)

# ARCH(2) - SPX
arch_spx_2 <- garch(na.omit(SPX_detr_diff), order = c(2, 0))
summary(arch_spx_2)

# ARCH(1) - DJ
arch_dj_1 <- garch(na.omit(DJ_detr_diff), order = c(1, 0))
summary(arch_dj_1)

# ARCH(2) - DJ
arch_dj_2 <- garch(na.omit(DJ_detr_diff), order = c(2, 0))
summary(arch_dj_2)

################################### GARCH MODELS ################################################## 

# SPX (normal distribution)
garch_spx <- garchFit(~ arma(2,2) + garch(1,1), data = na.omit(SPX_detr_diff), trace = FALSE)
summary(garch_spx)

garch_spx <- garchFit(~ arma(2,2) + garch(1,2), data = na.omit(SPX_detr_diff), trace = FALSE)
summary(garch_spx)

garch_spx <- garchFit(~ arma(2,2) + garch(2,1), data = na.omit(SPX_detr_diff), trace = FALSE)
summary(garch_spx)

garch_spx <- garchFit(~ arma(2,2) + garch(2,2), data = na.omit(SPX_detr_diff), trace = FALSE)
summary(garch_spx)

# DJ (normal distribution)
garch_dj <- garchFit(~ arma(2,2) + garch(1,1), data = na.omit(DJ_detr_diff), trace = FALSE)
summary(garch_dj)

garch_dj <- garchFit(~ arma(2,2) + garch(1,2), data = na.omit(DJ_detr_diff), trace = FALSE)
summary(garch_dj)

garch_dj <- garchFit(~ arma(2,2) + garch(2,1), data = na.omit(DJ_detr_diff), trace = FALSE)
summary(garch_dj)

garch_dj <- garchFit(~ arma(2,2) + garch(2,2), data = na.omit(DJ_detr_diff), trace = FALSE)
summary(garch_dj)

# SPX (t-student distribution)
garch_spx_t_11 <- garchFit(~ arma(2,2) + garch(1,1), data = na.omit(SPX_detr_diff), cond.dist = "std", trace = FALSE)
summary(garch_spx_t_11)

garch_spx <- garchFit(~ arma(2,2) + garch(1,2), data = na.omit(SPX_detr_diff), cond.dist = "std", trace = FALSE)
summary(garch_spx)

garch_spx <- garchFit(~ arma(2,2) + garch(2,1), data = na.omit(SPX_detr_diff), cond.dist = "std", trace = FALSE)
summary(garch_spx)

garch_spx <- garchFit(~ arma(2,2) + garch(2,2), data = na.omit(SPX_detr_diff), cond.dist = "std", trace = FALSE)
summary(garch_spx)

# DJ (t-student distribution)
garch_dj_t_11 <- garchFit(~ arma(2,2) + garch(1,1), data = na.omit(DJ_detr_diff), cond.dist = "std", trace = FALSE)
summary(garch_dj_t_11)

garch_dj <- garchFit(~ arma(2,2) + garch(1,2), data = na.omit(DJ_detr_diff), cond.dist = "std", trace = FALSE)
summary(garch_dj)

garch_dj <- garchFit(~ arma(2,2) + garch(2,1), data = na.omit(DJ_detr_diff), cond.dist = "std", trace = FALSE)
summary(garch_dj)

garch_dj <- garchFit(~ arma(2,2) + garch(2,2), data = na.omit(DJ_detr_diff), cond.dist = "std", trace = FALSE)
summary(garch_dj)


################################### FINAL MODELS ################################################# 

garch_spx_t_11 <- garchFit(~ arma(2,2) + garch(1,1), data = na.omit(SPX_detr_diff), cond.dist = "std", trace = FALSE)
summary(garch_spx_t_11)

garch_dj_t_11 <- garchFit(~ arma(2,2) + garch(1,1), data = na.omit(DJ_detr_diff), cond.dist = "std", trace = FALSE)
summary(garch_dj_t_11)

# GARCH(1,1) on SPX
garch_spx_11 <- garchFit(~ garch(1,1), data = na.omit(SPX_detr_diff),cond.dist = "std", trace     = FALSE)
summary(garch_spx_11)

# GARCH(1,1) on DJ
garch_dj_11 <- garchFit(~ garch(1,1),data = na.omit(DJ_detr_diff),cond.dist = "std",trace     = FALSE)
summary(garch_dj_11)

# results of GARCH (1,1) are very similar with slightly lower values for AIC and BIC; in the paper I used as a reference 
# model the ARIMA + GARCH just to show that the coefficients of ARIMA were irrelevant when estimating; moreover the coefficients
# and their significance are not that different when estimating.

###############################################################################################
# TRADING VOLUME ANALYSIS
###############################################################################################

spx_log_vol_sq <- (log(data$SPX_volume[-1]))^2
dj_log_vol_sq  <- (log(data$DJ_volume[-1]))^2

# TRADING VOLUME ANALYSIS
###############################################################################################

spx_log_vol_sq <- (log(data$SPX_volume[-1]))^2
dj_log_vol_sq  <- (log(data$DJ_volume[-1]))^2

# ADF test - SPX_log_vol
adf_spx_log_vol <- ur.df(data$SPX_log_vol[-1], type = "trend", lags = 10)
summary(adf_spx_log_vol)

# ADF test - DJ_log_vol
adf_dj_log_vol <- ur.df(data$DJ_log_vol[-1], type = "trend", lags = 10)
summary(adf_dj_log_vol)

# volume graphs
plot(spx_log_vol_sq, type = "l")
plot(dj_log_vol_sq, type = "l")

# graph comparison between the two indexes
plot(data$Date[-1], spx_log_vol_sq, type = "l", col = "blue",
     ylim = c(min(dj_log_vol_sq), max(spx_log_vol_sq)),
     xlab = "Date", ylab = "Log(Volume)^2")
lines(data$Date[-1], dj_log_vol_sq, col = "red")

# Comparison between residuals^2 (volatility) 
df <- data.frame(
  Date         = data$Date[-1],  
  SPX_LogVol   = scale(spx_log_vol_sq),
  SPX_ResSq    = scale(spx_sq),
  DJ_LogVol    = scale(dj_log_vol_sq),
  DJ_ResSq     = scale(dj_sq)
)

# Plot SPX
matplot(df$Date, cbind(df$SPX_ResSq, df$SPX_LogVol), type = "l", lty = 1, col = c("blue", "red"),
        lwd = 0.4, ylab = "", xlab = "Date", main = "SPX: Log(Volume)^2 vs Residuals^2")
legend("topleft", legend = c("Residuals^2", "Log(Volume)^2"), col = c("blue", "red"), lty = 1, bty = "n")
grid()


# Plot DJ
matplot(df$Date, cbind(df$DJ_ResSq, df$DJ_LogVol), type = "l", lty = 1, col = c("blue", "red"),
        lwd = 0.3, ylab = "", xlab = "Date", main = "DJ: Log(Volume)^2 vs Residuals^2")
legend("topleft", legend = c("Residuals^2", "Log(Volume)^2"), col = c("blue", "red"), lty = 1, bty = "n")
grid()

# Filter only 2020
df_2020 <- df[df$Date >= as.Date("2020-01-01") & df$Date <= as.Date("2020-12-31"), ]

# Plot SPX 2020
matplot(df_2020$Date, cbind(df_2020$SPX_ResSq, df_2020$SPX_LogVol), type = "l", lty = 1, lwd = 0.5, col = c("blue", "red"),
        ylab = "", xlab = "Date", main = "SPX (2020): Log(Volume)^2 vs Residuals^2")
legend("topright", legend = c("Residuals^2", "Log(Volume)^2"), col = c("blue", "red"), lty = 1, bty = "n")
grid()

# Plot DJ 2020
matplot(df_2020$Date, cbind(df_2020$DJ_ResSq, df_2020$DJ_LogVol), type = "l", lty = 1, lwd = 0.5, col = c("blue", "red"),
        ylab = "", xlab = "Date", main = "DJ (2020): Log(Volume)^2 vs Residuals^2")
legend("topright", legend = c("Residuals^2", "Log(Volume)^2"), col = c("blue", "red"), lty = 1, bty = "n")
grid()


###############################################################################################
# COINTEGRATION ANALYSIS
###############################################################################################

###############################################################################################
# PRICES ANALYSIS
###############################################################################################

########################### Johansen test #########################

log_data <- na.omit(data[, c("SPX_log", "DJ_log")])

# Choosing the optimal number of lags given our previous output for the Johansen test
# AIC=10-1=9
VARselect(log_data, lag.max = 10, type = "trend")$selection

# Johansen test 
johansen <- ca.jo(log_data, type = "trace", ecdet = "trend", K = 9)
summary(johansen)

########################### Granger test #########################

# SPX → DJ (order 2)
grangertest(DJ_detr_diff ~ SPX_detr_diff, order = 2)
# DJ → SPX (order 2)
grangertest(SPX_detr_diff ~ DJ_detr_diff, order = 2)

# SPX → DJ (order 6)
grangertest(DJ_detr_diff ~ SPX_detr_diff, order = 6)
# DJ → SPX (order 6)
grangertest(SPX_detr_diff ~ DJ_detr_diff, order = 6)


###############################################################################################
# VOLUME ANALYSIS
###############################################################################################

# You may want to check "TRADING VOLUME ANALYSIS" section for ADF tests & traiding volume graph

########################### Granger test #########################

# SPX_log_vol → DJ_log_vol (order 2)
grangertest(DJ_log_vol ~ SPX_log_vol, order = 2, data = data)

# DJ_log_vol → SPX_log_vol (order 2)
grangertest(SPX_log_vol ~ DJ_log_vol, order = 2, data = data)

# SPX_log_vol → DJ_log_vol (order 6)
grangertest(DJ_log_vol ~ SPX_log_vol, order = 6, data = data)

# DJ_log_vol → SPX_log_vol (order 6)
grangertest(SPX_log_vol ~ DJ_log_vol, order = 6, data = data)


################################################################################
#VAR MODEL ESTIMATION

# Define the data for log-prices 
diff_detrended_data <- data.frame(SPX_detr_diff, DJ_detr_diff)

# Optimal number of lags for log-prices
VARselect(diff_detrended_data, lag.max = 10, type = "const")$selection

# Estimate VAR model on log-prices 
var_prices <- VAR(diff_detrended_data, p = 9, type = "const")
summary(var_prices)

# Estimate VAR model on log-volumes (level specification)
log_volume_data <- data.frame(SPX_log_vol = data$SPX_log_vol, DJ_log_vol = data$DJ_log_vol)

# Optimal number of lags for log-volumes
VARselect(log_volume_data, lag.max = 10, type = "const")$selection

# Estimate VAR model on log-volumes
var_volumes <- VAR(log_volume_data, p = 10, type = "const")
summary(var_volumes)

# Svar (not included in the paper)
# here I estimated the SVAR just to check structural breaks; our goal is not to
# identify structural breaks, but its still interesting to see it.
# for this reason I mentioned only the VAR output in the paper

Amat <- matrix(c(NA, 0,
                 NA, NA),
               nrow = 2, byrow = TRUE,
               dimnames = list(
                 c("SPX_detr_diff","DJ_detr_diff"),
                 c("SPX_detr_diff","DJ_detr_diff")
               ))

# SVAR
svar_short <- SVAR(var_prices,
                   estmethod = "direct",
                   Amat     = Amat)

summary(svar_short)


irf_short <- irf(svar_short,
                 n.ahead = 20,
                 ortho   = FALSE,
                 boot    = TRUE,
                 runs    = 1000,
                 ci      = 0.95)
plot(irf_short)

fevd_short <- fevd(svar_short, n.ahead = 20)
plot(fevd_short)

# results confirm abd support our argument in the paper
