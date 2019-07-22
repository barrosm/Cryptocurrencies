# Read file with bitcoin and google trends data and perform basic analysis
#
# Date: 20/09/2017

source('..//Programas/p0_setup_v2.R')  #.. setup, basic functions, etc...

today = as.character(Sys.Date())  # to be used to name output data files

# btc_gtrends =read.xlsx("bitcoin_GT_consolidated_until_20170830.xlsx", sheetIndex = 1)
#btc_gtrends =read.xlsx("bitcoin_GT_consolidated_until_20170906.xlsx", sheetIndex = 1)
#btc_gtrends =read.xlsx("bitcoin_GT_consolidated_until_20170913.xlsx", sheetIndex = 1)
btc_gtrends =read.xlsx("bitcoin_GT_consolidated_until_20170920.xlsx", sheetIndex = 1)
View(btc_gtrends)
str(btc_gtrends)

colnames(btc_gtrends) <- c("date", "BTC_price", "GTrends")
btc_gtrends$date = as.POSIXlt(btc_gtrends$date, origin = "1970-01-01")

n = nrow(btc_gtrends)
# dates column is usually messed up, so reconstruct it
a = as.POSIXct(btc_gtrends$date[1])
b = round(as.POSIXct(btc_gtrends$date[n]), units = "hours")
btc_gtrends$date = seq(a, b, by="hour")
rm(a,b)

# delete empty column (junk) if it appears
btc_gtrends = btc_gtrends[,-4] 
View(btc_gtrends)

# Convert series to xts
btc_gtrends$BTC_price = as.xts(btc_gtrends$BTC_price, order.by = btc_gtrends$date)
btc_gtrends$GTrends = as.xts(btc_gtrends$GTrends, order.by = btc_gtrends$date)

data.to.plot <- cbind(btc_gtrends$BTC_price, btc_gtrends$GTrends)
colnames(data.to.plot) <- c("BTC_price", "GTrends")

fig= autoplot(as.xts(data.to.plot), facets = NULL) +
  geom_line(cex = 1.05)  + geom_point() + 
  ggtitle("Bitcoin Price and Google Trends\n") + 
  scale_y_continuous(sec.axis = ~./50) +
  theme(legend.position="none") #+ guides(fill=FALSE) #scale_fill_discrete(guide=FALSE)
print(fig)

# Create function to normalize data to 0-1 range
normalize01 <- function(x) {
  # normalizes series to 0-1 range
  score = (x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T))
  return(score)
}
#=============================================================================
# Graphs - BTC price and GTrends data 
# I had to change the axis AND rescale the GTrends data so both series would
# fit nicely in the same graph
# Change accordingly for other currencies
#=============================================================================
fig = ggplot() + xlab('date') + ylab('values') +
  ggtitle("Bitcoin Price and Google Trends\n (both scaled to 0-100)") + 
  geom_line(data = btc_gtrends, aes(x = date, y = 100*normalize01(BTC_price), col = "BTC price"), color = "red", cex = 1.05) +
  geom_line(data = btc_gtrends, aes(x = date, y = GTrends, col = "Google Trends"), color = "steelblue", cex = 1.05) +
  #scale_y_continuous(sec.axis = ~./50) +  
  #ylim = c(0.0, max(btc_gtrends$BTC_price)) + 
  theme(legend.position="bottom")
print(fig)
# Scatter plot
fig = ggplot(data = btc_gtrends, aes(x = as.vector(GTrends), 
                               y = as.vector(BTC_price))) +
  geom_point(colour = "steelblue", size = 3, shape = 15) +  #shape = 16 are filled circles
  ggtitle("Bitcoin Price and Google Trends\n")
print(fig)  
#==============================================================================
# Cross correlations of series (in the original scale)
# ============================================================================

lag_max = as.integer(readline(prompt = "****** MAXIMUM LAG ?? *****  "))

ccf_BTC_GT = stats::ccf(as.vector(btc_gtrends$BTC_price), 
                        as.vector(btc_gtrends$GTrends), lag.max = lag_max, na.action = na.pass, plot = FALSE)
# maximum cross correlation
max(ccf_BTC_GT$acf)
# show the index position where the maximum ccf occurs
index_pos_max_ccf=which(ccf_BTC_GT$acf == max(ccf_BTC_GT$acf))  
index_pos_max_ccf
# the corresponding lag is
lag_max_ccf = index_pos_max_ccf - (lag_max + 1)
#which(ccf_BTC_GT$acf == max(ccf_BTC_GT$acf)) 
lag_max_ccf

# plot ccf
fig = plot(ccf_BTC_GT, main = "Cross Correlations - BTC and GT - original scale")
print(fig)


# Creates bitcoin LOG-return series 
btc_gtrends$return = log(btc_gtrends$BTC_price) - log(quantmod::Lag(btc_gtrends$BTC_price, k = 1))
#quantmod::dailyReturn(btc_gtrends$BTC_price, type = "log")

# Creates differenced google trends series
btc_gtrends$dif_GT = diff(btc_gtrends$GTrends,lag = 1, differences = 1, na.pad = TRUE)
# creates lagged DIFFERENCED google trends
btc_gtrends$dif_GT_lag1 = quantmod::Lag(btc_gtrends$dif_GT, k = 1)
btc_gtrends$dif_GT_lag2 = quantmod::Lag(btc_gtrends$dif_GT_lag1, k = 1)
btc_gtrends$dif_GT_lag3 = quantmod::Lag(btc_gtrends$dif_GT_lag2, k = 1)
btc_gtrends$dif_GT_lag4 = quantmod::Lag(btc_gtrends$dif_GT_lag3, k = 1)
btc_gtrends$dif_GT_lag5 = quantmod::Lag(btc_gtrends$dif_GT_lag4, k = 1)
btc_gtrends$dif_GT_lag6 = quantmod::Lag(btc_gtrends$dif_GT_lag5, k = 1)
btc_gtrends$dif_GT_lag7 = quantmod::Lag(btc_gtrends$dif_GT_lag6, k = 1)
btc_gtrends$dif_GT_lag8 = quantmod::Lag(btc_gtrends$dif_GT_lag7, k = 1)
btc_gtrends$dif_GT_lag9 = quantmod::Lag(btc_gtrends$dif_GT_lag8, k = 1)
btc_gtrends$dif_GT_lag10 = quantmod::Lag(btc_gtrends$dif_GT_lag9, k = 1)
btc_gtrends$dif_GT_lag11 = quantmod::Lag(btc_gtrends$dif_GT_lag10, k = 1)
btc_gtrends$dif_GT_lag12 = quantmod::Lag(btc_gtrends$dif_GT_lag11, k = 1)
#==============================================================================
# Cross correlation - bitcoin RETURNS and GT differenced
#==============================================================================
ccf_BTC_ret_GT_dif = stats::ccf(as.vector(btc_gtrends$return), 
                        as.vector(btc_gtrends$dif_GT), lag.max = lag_max, na.action = na.pass, plot = FALSE)
# maximum cross correlation in ABSOLUTE value
max_abs_ccf=max(abs(ccf_BTC_ret_GT_dif$acf))
max_abs_ccf
# show the index position where the maximum ccf occurs
index_pos_max_ccf_ret = which(ccf_BTC_ret_GT_dif$acf == max((ccf_BTC_ret_GT_dif$acf))) 
index_pos_max_ccf_ret

# the corresponding lag is
lag_max_ccf_ret = index_pos_max_ccf_ret - (lag_max + 1)
lag_max_ccf_ret

# plot ccf
fig = plot(ccf_BTC_ret_GT_dif, main = "Cross Correlations -\n Return BTC and Differenced GT")
print(fig)
print(ccf_BTC_ret_GT_dif)



# creates lagged google trends
# btc_gtrends$GT_lag1 = quantmod::Lag(btc_gtrends$GTrends, k = 1)
# btc_gtrends$GT_lag2 = quantmod::Lag(btc_gtrends$GT_lag1, k = 1)
# btc_gtrends$GT_lag3 = quantmod::Lag(btc_gtrends$GT_lag2, k = 1)
# btc_gtrends$GT_lag4 = quantmod::Lag(btc_gtrends$GT_lag3, k = 1)
# btc_gtrends$GT_lag5 = quantmod::Lag(btc_gtrends$GT_lag4, k = 1)
# btc_gtrends$GT_lag6 = quantmod::Lag(btc_gtrends$GT_lag5, k = 1)
# btc_gtrends$GT_lag7 = quantmod::Lag(btc_gtrends$GT_lag6, k = 1)
# btc_gtrends$GT_lag8 = quantmod::Lag(btc_gtrends$GT_lag7, k = 1)
# btc_gtrends$GT_lag9 = quantmod::Lag(btc_gtrends$GT_lag8, k = 1)
# btc_gtrends$GT_lag10 = quantmod::Lag(btc_gtrends$GT_lag9, k = 1)
# btc_gtrends$GT_lag11 = quantmod::Lag(btc_gtrends$GT_lag10, k = 1)
# btc_gtrends$GT_lag12 = quantmod::Lag(btc_gtrends$GT_lag11, k = 1)



# PerformanceAnalytics::charts.TimeSeries(data.to.plot)

# library(ggplot2)
# ggCcf(as.vector(btc_gtrends$BTC_price), 
#       as.vector(btc_gtrends$GTrends), lag.max = lag_max, 
#       type = c("correlation"), plot = TRUE)
# 
# ggCcf(ccf_BTC_GT)


# ============================================================================
# Create image file
# ============================================================================
# Note: "today" defined in the setup file (p0_setup_v2.r)
file_name = paste0("bitcoin_google_trends_analysis_",today,".Rdata")
file_name
save.image(file_name)

