#==============================================================================
# Program p0_setup_v2.R
# Monica Barros
# Date: 29/08/2017

today = as.character(Sys.Date())  # to be used to name output data files

#==============================================================================
## Definition of auxiliary functions
# =============================================================================
# Function Instala_Pacote - NEW VERSION
# NEW VERSION - Specifies default mirror
instala_pacote <- function(pacote, mirror = "http://cloud.r-project.org/") {
  novo_pacote = pacote [!(pacote %in% installed.packages()[, "Package"])]
  if (length(novo_pacote))
    install.packages(novo_pacote, dependencies = TRUE, repos = mirror)
  #sapply(pacote, require, character.only = TRUE)
}

#============================================================================
# Plot OHLC of data read in the Cryptocompare API format
#============================================================================
plot_OHLC_CRYPTO <- function (series, n = 120) {
  OHLC = tail(eval(as.name(series)), n)
  if ("return" %in% colnames(OHLC))
    { OHLC$return <- NULL }
    OHLC = OHLC[,-5:-6]
# rearrange columns
    OHLC2 <-OHLC
#OHLC2 <- matrix()
  OHLC2[,1] <- OHLC[,4]
  OHLC2[,2] <- OHLC[,2]
  OHLC2[,3] <- OHLC[,3]
  OHLC2[,4] <- OHLC[,1]
  fig = dygraph(OHLC2 , main = paste0(c('Candlechart    '),series,'  up to  ',n, ' days')) %>%
  dyCandlestick() %>%
  dyRangeSelector()
  print(fig)
}

  
#+==============================================================================
# Function to plot a time series with autoplot and a specified layout
#==============================================================================
plot_ts_std <- function(series){
  # plots a time series with "autoplot" and specified parameters
  series_name = deparse(substitute(series))
  fig = autoplot(as.xts(series)) + geom_line(color = 'midnightblue', cex = 1.05) +
    ggtitle(series_name) +theme(panel.background = element_rect(fill = 'gray88', colour = 'black'))
  print(fig)
}

  
#==============================================================================
# Function to import daily data (from Cryptocompare)
#==============================================================================
nobs = 2000 # number of observations to be read
read_crypto <-function(coin1, coin2 = "BTC", sample_size = 2000){
  # reads historical data using the cryptocompare api
  quotes <- fromJSON(paste0("https://min-api.cryptocompare.com/data/histoday?fsym=",coin1, "&tsym=",coin2,"&limit=",sample_size))
  quotes <- quotes$Data
  quotes$time <-as.POSIXct(quotes$time, origin = "1970-01-01")
  quotes$time <- as.Date(quotes$time) # eliminates hour,min, sec information
  return(quotes)
}

#==============================================================================
# Function to import daily data (from Cryptocompare)
# Specification of END date possible
# useful for backtesting
#==============================================================================
nobs = 2000 # number of observations to be read
today_utc = as.numeric(as.POSIXct(today, tz = "UTC")) # default end date (in GMT time zone)
read_crypto_2 <-function(coin1, coin2 = "BTC", sample_size = 2000, 
                         unix_time_stamp_GMT = today_utc){
    # reads historical data using the cryptocompare api
    quotes <- fromJSON(paste0("https://min-api.cryptocompare.com/data/histoday?fsym=",coin1, "&tsym=",coin2,"&limit=",sample_size,"&toTs=",unix_time_stamp_GMT))
    quotes <- quotes$Data
    quotes$time <-as.POSIXct(quotes$time, origin = "1970-01-01")
    quotes$time <- as.Date(quotes$time) # eliminates hour,min, sec information
    return(quotes)
}


#==============================================================================
# Function to import daily data from POLONIEX using Cryptocompare
#==============================================================================
nobs = 2000 # number of observations to be read
read_crypto_polo <-function(coin1, coin2 = "BTC", sample_size = 2000){
  # reads historical data using the cryptocompare api
  quotes <- fromJSON(paste0("https://min-api.cryptocompare.com/data/histoday?fsym=",coin1, "&tsym=",coin2,"&limit=",sample_size, "&e=Poloniex"))
  quotes <- quotes$Data
  quotes$time <-as.POSIXct(quotes$time, origin = "1970-01-01")
  quotes$time <- as.Date(quotes$time) # eliminates hour,min, sec information
  return(quotes)
}


#============================================================================
# Convert all series to xts  -  need to implement as
# function that can read vector of series names
#============================================================================
convert_xts <- function(series){
  # reads series name and converts to xts
  # takes "time" variable and converts to POSIXlt
  print(str(series))
  series$time = as.POSIXlt(series$time, origin = "1970-01-01")
  # Converts to xts object, eliminating first column (which contained the time)
  series = xts(series[,-1], order.by = series[,1])
  return(series)
}

#==============================================================================
# Compute summary statistics - requires DistributionUtils package
#==============================================================================
lots_stats <- function(x) {
  library(DistributionUtils)
  minimum = sapply(x, min, na.rm = TRUE)
  p_01perc= sapply(x, quantile, probs = 0.01, na.rm = TRUE)
  p_05perc= sapply(x, quantile, probs = 0.05, na.rm = TRUE)
  p_10perc= sapply(x, quantile, probs = 0.10, na.rm = TRUE)
  p_25perc= sapply(x, quantile, probs = 0.25, na.rm = TRUE)
  mean = sapply(x, mean, na.rm = TRUE)
  median = sapply(x, median, na.rm = TRUE)
  std_dev  = sapply(x, sd, na.rm = TRUE)
  p_75perc= sapply(x, quantile, probs = 0.75, na.rm = TRUE)
  p_90perc= sapply(x, quantile, probs = 0.90, na.rm = TRUE)
  p_95perc= sapply(x, quantile, probs = 0.95, na.rm = TRUE)
  p_99perc= sapply(x, quantile, probs = 0.99, na.rm = TRUE)
  maximum = sapply(x, max, na.rm = TRUE)
  # skewness and kurtosis require the DistributionUtils package
  skewness = sapply(x, DistributionUtils::skewness, na.rm = TRUE)
  kurtosis = sapply(x, DistributionUtils::kurtosis, na.rm = TRUE)
  lots_stats = rbind(minimum, p_01perc,p_05perc, p_10perc, p_25perc, mean, median, p_75perc, p_90perc, p_95perc, p_99perc, maximum, std_dev, skewness,kurtosis)
  return(lots_stats)
}


#==============================================================================
# The actual reading and import of data starts HERE
#==============================================================================
# Changes repo to Fundacao Oswaldo Cruz
mirror1="http://cran.fiocruz.br/"


pacs = c("jsonlite","quantmod", "RCurl", "httr", "zoo", "xts","urca", "ggplot2", "DistributionUtils", "xlsx")
pacs = c(pacs,"dygraphs", "RColorBrewer", "stringr", "corrplot","PerformanceAnalytics", "TTR", "timeSeries", 'fPortfolio' )


#instala_pacote(pacs, mirror1)
# =============================================================================
# Calls "require" to all packages at once
# =============================================================================
#lapply(pacs, require, character.only = TRUE)


instala_pacote_results <- sapply(pacs, instala_pacote, mirror = mirror1)
require_results <- sapply(pacs, require, character.only = TRUE)

if (!all(require_results)) print("Erro nos pacotes requeridos!");


#==============================================================================
# To make centered titles in all plots
#=============================================================================+
theme_update(plot.title = element_text(hjust = 0.5))


# Prints version information about R, the OS and attached or loaded packages.
sessionInfo()

