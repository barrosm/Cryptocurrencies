# Econometric Analysis of Alt-Coin Time Series
# Monica Barros
#
# Current Version: 01/06/2017
#
# Changing directory to where the PROGRAM is 
# (use // in Windows, sometimes a single / works, in Unix, use \)


setwd('D://Users//monica.barros.IAGPUC.000//Dropbox//Post-Doc')
# setwd('C://Dropbox//post-doc')
# setwd('C://Users//Monica//Dropbox//post-doc')
# setwd('C://Users//barro/Dropbox///post-doc')

# ==================================================================================
# Function Instala_Pacote
# It works silently, echoing nothing if package "pacote" is already installed 
# and installing it otherwise. 
# Don't forget to write the name of the package between quotes!

instala_pacote <- function(pacote) {
    if (!pacote %in% installed.packages()) install.packages(pacote , dep = T)
}

read_quandl <- function(code) {
  #y = strsplit(code, "/")
  #y = unlist(y)
  #exchange = y[1]
  #pair = y[2]
  quandl_xts =  Quandl(code, api_key=key, type = "xts")
  return(quandl_xts)
}

plot_closing_price <- function (series) {
  fig = dygraph(eval(as.name(series))$Close, main = paste0(c('Closing Price   '),series)) %>%
    dyRangeSelector() # add range selector to the bottom of a dygraph
  print(fig)
}

plot_OHLC <- function (series, n = 360) {
  OHLC = tail(eval(as.name(series)), n) 
  if ("return" %in% colnames(OHLC))
  {OHLC$return <- NULL}
  OHLC = OHLC[,-5:-7] 
  fig = dygraph(OHLC , main = paste0(c('Candlechart    '),series,'  up to  ',n, ' days')) %>%
    dyCandlestick() %>%
    dyRangeSelector()
  print(fig)
}


start_date <- function(x) {
  min(index(eval(as.name(x))))
}

end_date <- function(x) {
  max(index(eval(as.name(x))))
}

# ==================================================================================

instala_pacote("rvest")   # web scraping
instala_pacote("forecast")
instala_pacote("readr")   # Faster file reading
instala_pacote("xlsx")
instala_pacote("magrittr") # "pipe" operator
instala_pacote("ggplot2")
instala_pacote("Quandl")  # to download series from Quandl
instala_pacote("jsonlite")
instala_pacote("lubridate") # manipulate dates and times
instala_pacote("dplyr") # data wrangling
instala_pacote("xts")  # time series manipulation 
instala_pacote("dygraphs") # easy charting time series objects
instala_pacote("RColorBrewer") # color palettes
instala_pacote("stringr")  # string manipulation
instala_pacote("quantmod")
instala_pacote("corrplot")


library("rvest")
library("forecast")
library("xlsx")
library("readr")
library("magrittr")
library("ggplot2")
library("Quandl")  
library("jsonlite")
library("lubridate")
library("dplyr")
library("xts")
library("dygraphs")
library("RColorBrewer")
library("stringr")
library("quantmod")
library("corrplot")


sessionInfo()

# to make centered titles in all plots
theme_update(plot.title = element_text(hjust = 0.5))

#========================================================================
# Note - to avoid error in the installation of xlsx
# make sure Java and R are both 32 or 64 bits
# 
# To check the current Java version, write at the Windows command prompt:
# 
# If necessary, go to Java.com and manually install the 64 bit version 
# (provided you?re using R 64 bits)
#========================================================================

# Key to use in Quandl - get your key at Quandl
key = 'use_your_own_key'


#========================================================================

#========================================================================
#========================================================================
# READING DATA FROM DATABASES IN QUANDL "only"
#========================================================================
#========================================================================

# BITCOIN SERIES IN DIFFERENT CURRENCIES

#======================================================================
# BITCOIN - CHF
#======================================================================
# Bitcoin rate in CHF (downloaded from Quandl)
btc_CHF = read_quandl("BCHARTS/LOCALBTCCHF")

#======================================================================
# BITCOIN - GBP
#======================================================================
# Bitcoin rate in GBP - Exchange = Coinbase (downloaded from Quandl)
btc_coinbase_GBP = read_quandl("BCHARTS/COINBASEGBP")
# Bitcoin rate in GBP - Exchange = Coinfloor (downloaded from Quandl)
btc_coinfloor_GBP = read_quandl("BCHARTS/COINFLOORGBP")

#======================================================================
# BITCOIN - EUR
#======================================================================
# Bitcoin rate in EUR - Exchange = Coinbase (downloaded from Quandl)
btc_coinbase_EUR =  read_quandl("BCHARTS/COINBASEEUR")
# Bitcoin rate in EUR - Exchange = ItBit (downloaded from Quandl)
btc_itbit_EUR = read_quandl("BCHARTS/ITBITEUR")


#======================================================================
# BITCOIN - USD
#======================================================================
# Bitcoin rate in USD - Exchange = Coinbase (downloaded from Quandl)
btc_coinbase_USD =read_quandl("BCHARTS/COINBASEUSD")
# Bitcoin rate in USD - Exchange = ItBit (downloaded from Quandl)
btc_itbit_USD = read_quandl("BCHARTS/ITBITUSD")
# Bitcoin rate in USD - Exchange = CBX (downloaded from Quandl)
btc_cbx_USD = read_quandl("BCHARTS/CBXUSD")
#Bitcoin rate in USD - Exchange = BITFINEX (downloaded from Quandl)
btc_bitfinex_USD=read_quandl("BITFINEX/BTCUSD")


#======================================================================
# Other coins in USD - Exchange = BITFINEX
#======================================================================

# BCC (Bitconnect)/USD Exchange Rate
bcc_bitfinex_USD =read_quandl("BITFINEX/BCCUSD")

# BCU (Bitcoin Unlimited)/USD Exchange Rate
bcu_bitfinex_USD =read_quandl("BITFINEX/BCUUSD")

# DRK (Darkcoin) rates in USD, exchange = bitfinex 
drk_bitfinex_USD = read_quandl("BITFINEX/DRKUSD")

#DSH (Dash)/USD Exchange Rate
dsh_bitfinex_USD = read_quandl("BITFINEX/DSHUSD")

#ETC (Ethereum Classic)/USD Exchange Rate
etc_bitfinex_USD = read_quandl("BITFINEX/ETCUSD")

#ETH/USD Exchange Rate
eth_bitfinex_USD = read_quandl("BITFINEX/ETHUSD")

# LTC (Litecoin) rates in USD, exchange = BITFINEX
ltc_bitfinex_USD = read_quandl("BITFINEX/LTCUSD")

# RRT (Recovery Rights Token)/USD Exchange Rate 
rrt_bitfinex_USD = read_quandl("BITFINEX/RRTUSD")

#XMR (Monero)/USD Exchange Rate
xmr_bitfinex_USD= read_quandl("BITFINEX/XMRUSD")

# XRP (Ripple)/USD Exchange Rate
xrp_bitfinex_USD = read_quandl("BITFINEX/XRPUSD")

# ZEC (ZCash)/USD Exchange Rate
zec_bitfinex_USD = read_quandl("BITFINEX/ZECUSD")


#======================================================================
# Other coins - Exchange rate in BTC - Exchange = BITFINEX
#======================================================================
# BCC (Bitconnect)/BTC Exchange Rate
bcc_bitfinex_BTC= read_quandl("BITFINEX/BCCBTC")

# BCU (bitcoin unlimited)/BTC Exchange Rate
bcu_bitfinex_BTC=read_quandl("BITFINEX/BCUBTC")

# DSH (Dash)/BTC Exchange Rate
dsh_bitfinex_BTC=read_quandl("BITFINEX/DSHBTC")

# ETC (Ethereum Classic)/BTC Exchange Rate
etc_bitfinex_BTC=read_quandl("BITFINEX/ETCBTC")

# ETH (Ethereum) /BTC Exchange Rate
eth_bitfinex_BTC=read_quandl("BITFINEX/ETHBTC")

# LTC (Litecoin)/BTC Exchange Rate
ltc_bitfinex_BTC=read_quandl("BITFINEX/LTCBTC")

# RRT (Recovery Right Tokens)/BTC Exchange Rate
rrt_bitfinex_BTC=read_quandl("BITFINEX/RRTBTC")

# XMR (Monero)/BTC Exchange Rate
xmr_bitfinex_BTC=read_quandl("BITFINEX/XMRBTC")

# XRP (Ripple)/BTC Exchange Rate
xrp_bitfinex_BTC=read_quandl("BITFINEX/XRPBTC")

#ZEC (ZCash)/BTC Exchange Rate
zec_bitfinex_BTC=read_quandl("BITFINEX/ZECBTC")

#==============================================================
# Rearrange data frames in ascending (chronological order)
# Uses function 'arrange' from dplyr
# 
# NOT NECESSARY IF FORMAT WHEN READING QUANDL IS XTS
#btc_CHF = arrange(btc_CHF, Date)
#btc_coinbase_GBP = arrange(btc_coinbase_GBP, Date)
#btc_coinfloor_GBP = arrange(btc_coinfloor_GBP, Date)
#==============================================================


#========================================================================
# plot closing prices and returns - btc_CHF
#========================================================================
plot_closing_price("btc_CHF")

#fig = dygraph(btc_CHF$Close, main = 'BitCoin Daily Closing Price in CHF') %>%
#dyRangeSelector() # add range selector to the bottom of a dygraph
#print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code

#========================================================================
# Candlestick up to last 360 days
#========================================================================
plot_OHLC("btc_CHF",360)
# OHLC <- tail(btc_CHF, n = 360) 
# OHLC = OHLC[,-5:-7] 
# fig = dygraph(OHLC, main = 'BitCoin OHLC in CHF') %>%
# dyCandlestick() %>%
# dyRangeSelector()
# print(fig)

#========================================================================
# compute and plot returns based on Closing prices
#========================================================================
re = diff(log(btc_CHF$Close))
# statement belwo not necessary when data imported as xts
# re = append(re, 0, after = 0) # inserts a zero at the beginning to have the right length
btc_CHF$return = re
rm(re) # remove temporary variable
#qplot(Date,return, data = btc_cbx_USD, geom = 'line', main = 'BitCoin Daily Return in USD - cbx Exchange (btc_cbx_USD)', colour = I('blue'))
#fig = ggplot(data = btc_CHF, aes(x=Date, y=return)) + geom_line(color = 'dodgerblue4') + ggtitle("BitCoin Daily Return in CHF")
fig = dygraph(btc_CHF$return, main ="BitCoin Daily Return in CHF") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"), drawPoints = TRUE, pointSize = 2)%>%
  dyRangeSelector()
print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code


#========================================================================
# plot closing prices and returns - btc_coinbase_GBP
#========================================================================
plot_closing_price("btc_coinbase_GBP")
#fig = dygraph(btc_coinbase_GBP$Close, main = 'BitCoin Daily Closing Price in GBP - Exchange = Coinbase ') %>%
#  dyRangeSelector() # add range selector to the bottom of a dygraph
#print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code

#fig = qplot(Date,Close, data = btc_coinbase_GBP, geom = 'line', main = 'BitCoin Daily Closing Price in GBP - Exchange = Coinbase ', colour = I('red'))
#print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code

#========================================================================
# Candlestick up to last 360 days
#========================================================================
plot_OHLC("btc_coinbase_GBP",360)

# compute and plot returns based on Closing prices
re = diff(log(btc_coinbase_GBP$Close))
# re = append(re, 0, after = 0) # inserts a zero at the beginning to have the right length
btc_coinbase_GBP$return = re
rm(re) # remove temporay variable
fig = dygraph(btc_coinbase_GBP$return, main = "BitCoin Daily Return in in GBP - Exchange = Coinbase") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"), drawPoints = TRUE, pointSize = 2)%>%
  dyRangeSelector()
print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code



#========================================================================
# plot closing prices and returns - btc_coinfloor_GBP
#========================================================================
plot_closing_price("btc_coinfloor_GBP")

#========================================================================
# Candlestick up to last 360 days
#========================================================================
plot_OHLC("btc_coinfloor_GBP", 180) # last 180 days


# compute and plot returns based on Closing prices
re = diff(log(btc_coinfloor_GBP$Close))
btc_coinfloor_GBP$return = re
rm(re) # remove temporay variable
#fig = ggplot(data = btc_coinfloor_GBP, aes(x=Date, y=return)) + geom_line(color = 'dodgerblue4') + ggtitle("BitCoin Daily Return in in GBP - Exchange = Coinfloor")
fig = dygraph(btc_coinfloor_GBP$return, main = "BitCoin Daily Return in in GBP - Exchange = Coinfloor") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"), drawPoints = TRUE, pointSize = 2)%>%
  dyRangeSelector()
print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code


#========================================================================
# plot closing prices and returns - btc_coinbase_EUR
#========================================================================
plot_closing_price("btc_coinbase_EUR")

#========================================================================
# Candlestick up to last 360 days
#========================================================================
plot_OHLC("btc_coinbase_EUR", 180) # 180 days


# compute and plot returns based on Closing prices
re = diff(log(btc_coinbase_EUR$Close))
btc_coinbase_EUR$return = re
rm(re) # remove temporay variable
fig = dygraph(btc_coinbase_EUR$return, main = "BitCoin Daily Return in in EUR - Exchange = Coinbase") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"), drawPoints = TRUE, pointSize = 2)%>%
  dyRangeSelector()
#fig = ggplot(data = btc_coinbase_EUR, aes(x=Date, y=return)) + geom_line(color = 'dodgerblue4') + ggtitle("BitCoin Daily Return in in EUR - Exchange = Coinbase")
print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code


#========================================================================
# plot closing prices and returns - btc_itbit_EUR
#========================================================================
plot_closing_price("btc_itbit_EUR")

#fig = qplot(Date,Close, data = btc_itbit_EUR, geom = 'line', main = 'BitCoin Daily Closing Price in EUR - Exchange = ITBIT', colour = I('red'))
#print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code

#========================================================================
# Candlestick up to last 360 days
#========================================================================
plot_OHLC ("btc_itbit_EUR", 180) # up to 180 days


# compute and plot returns based on Closing prices
re = diff(log(btc_itbit_EUR$Close))
#re = append(re, 0, after = 0) # inserts a zero at the beginning to have the right length
btc_itbit_EUR$return = re
rm(re) # remove temporay variable
fig = dygraph(btc_itbit_EUR$return, main = "BitCoin Daily Return in EUR - Exchange = ITBIT") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"), drawPoints = TRUE, pointSize = 2)%>%
  dyRangeSelector()
#fig = ggplot(data = btc_itbit_EUR, aes(x=Date, y=return)) + geom_line(color = 'dodgerblue4') + ggtitle("BitCoin Daily Return in in EUR - Exchange = ITBIT")
print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code



#========================================================================
# plot closing prices and returns - btc_coinbase_USD
#========================================================================
plot_closing_price("btc_coinbase_USD")

#fig = qplot(Date,Close, data = btc_coinbase_USD, geom = 'line', main = 'BitCoin Daily Closing Price in USD - Exchange = Coinbase ', colour = I('red'))
#print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code

#========================================================================
# Candlestick up to last 360 days
#========================================================================
plot_OHLC("btc_coinbase_USD", 180) # 180 days

# compute and plot returns based on Closing prices
re = diff(log(btc_coinbase_USD$Close))
#re = append(re, 0, after = 0) # inserts a zero at the beginning to have the right length
btc_coinbase_USD$return = re
rm(re) # remove temporay variable
fig = dygraph(btc_coinbase_USD$return, main = "BitCoin Daily Return in USD - Exchange = Coinbase") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"), drawPoints = TRUE, pointSize = 2)%>%
  dyRangeSelector()
#fig = ggplot(data = btc_coinbase_USD, aes(x=Date, y=return)) + geom_line(color = 'dodgerblue4') + ggtitle("BitCoin Daily Return in in USD - Exchange = Coinbase")
print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code



#========================================================================
# plot closing prices and returns - btc_itbit_USD
#========================================================================
plot_closing_price("btc_itbit_USD")


#========================================================================
# Candlestick up to last 360 days
#========================================================================
plot_OHLC("btc_itbit_USD", 180) # n = 180 days

# compute and plot returns based on Closing prices
re = diff(log(btc_itbit_USD$Close))
#re = append(re, 0, after = 0) # inserts a zero at the beginning to have the right length
btc_itbit_USD$return = re
rm(re) # remove temporay variable
fig = dygraph(btc_itbit_USD$return, main = "BitCoin Daily Return in USD - Exchange = ITBIT") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"), drawPoints = TRUE, pointSize = 2)%>%
  dyRangeSelector()
#fig = ggplot(data = btc_itbit_USD, aes(x=Date, y=return)) + geom_line(color = 'dodgerblue4') + ggtitle("BitCoin Daily Return in in USD - Exchange = ITBIT")
print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code


#========================================================================
# plot closing prices and returns - btc_cbx_USD
#========================================================================
fig = dygraph(btc_cbx_USD$Close, main = 'BitCoin Daily Closing Price in USD - Exchange = cbx ') %>%
  dyRangeSelector() # add range selector to the bottom of a dygraph
print(fig) 

#fig = qplot(Date,Close, data = btc_cbx_USD, geom = 'line', main = 'BitCoin Daily Closing Price in USD - cbx Exchange (btc_cbx_USD) ', colour = I('red'))
#print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code

#========================================================================
# Candlestick up to last 360 days
#========================================================================
OHLC <- tail(btc_cbx_USD, n = 360) 
OHLC = OHLC[,-5:-7] 
fig = dygraph(OHLC, main = 'BitCoin OHLC in USD - Exchange = cbx ') %>%
  dyCandlestick() %>%
  dyRangeSelector()
print(fig)

# compute and plot returns based on Closing prices
re = diff(log(btc_cbx_USD$Close))
#re = append(re, 0, after = 0) # inserts a zero at the beginning to have the right length
btc_cbx_USD$return = re
rm(re) # remove temporary variable
fig = dygraph(btc_cbx_USD$return, main = "BitCoin Daily Return in USD - Exchange = cbx") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"), drawPoints = TRUE, pointSize = 2)%>%
  dyRangeSelector()
# qplot(Date,return, data = btc_cbx_USD, geom = 'line', main = 'BitCoin Daily Return in USD - cbx Exchange (btc_cbx_USD)', colour = I('blue'))
#fig = ggplot(data = btc_cbx_USD, aes(x=Date, y=return)) + geom_line(color = 'dodgerblue4') + ggtitle("BitCoin Daily Return in USD - cbx Exchange")
print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code


#========================================================================
# plot closing prices and returns - ltc_bitfinex_USD
#========================================================================
fig = dygraph(ltc_bitfinex_USD$Last, main = 'Litecoin Daily Closing Price in USD - Exchange = bitfinex ') %>%
  dyRangeSelector() # add range selector to the bottom of a dygraph
print(fig) 

#fig = qplot(Date,Last, data = ltc_bitfinex_USD, geom = 'line', main = 'Litecoin Daily Closing Price in USD - Exchange = bitfinex ', colour = I('red'))
#print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code

#========================================================================
# Candlestick up to last 360 days
#========================================================================
OHLC <- tail(ltc_bitfinex_USD, n = 360) 
OHLC = OHLC[,-5:-7] 
fig = dygraph(OHLC, main = 'Litecoin OHLC in USD - Exchange = bitfinex ') %>%
  dyCandlestick() %>%
  dyRangeSelector()
print(fig)


# compute and plot returns based on Closing prices
re = diff(log(ltc_bitfinex_USD$Last))
#re = append(re, 0, after = 0) # inserts a zero at the beginning to have the right length
ltc_bitfinex_USD$return = re
rm(re) # remove temporary variable
fig = dygraph(ltc_bitfinex_USD$return, main = "Litecoin Daily Return in USD - Exchange = bitfinex") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"), drawPoints = TRUE, pointSize = 2)%>%
  dyRangeSelector()
# qplot(Date,return, data = ltc_bitfinex_USD, geom = 'line', main = 'Litecoin Daily Return in USD - Exchange = bitfinex, colour = I('blue'))
#fig = ggplot(data = ltc_bitfinex_USD, aes(x=Date, y=return)) + geom_line(color = 'dodgerblue4') + ggtitle("BitCoin Daily Return in USD - cbx Exchange")
print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code



#========================================================================
# plot closing prices and returns - drk_bitfinex_USD
#========================================================================
fig = dygraph(drk_bitfinex_USD$Last, main = 'Darkcoin Daily Closing Price in USD - Exchange = bitfinex ') %>%
  dyRangeSelector() # add range selector to the bottom of a dygraph
print(fig) 

#fig = qplot(Date,Last, data = drk_bitfinex_USD, geom = 'line', main = 'Darkcoin Daily Closing Price in USD - Exchange = bitfinex ', colour = I('red'))
#print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code


#========================================================================
# Candlestick up to last 360 days
#========================================================================
OHLC <- tail(drk_bitfinex_USD, n = 360) 
OHLC = OHLC[,-5:-7] 
fig = dygraph(OHLC, main = 'Darkcoin OHLC in USD - Exchange = bitfinex ') %>%
  dyCandlestick() %>%
  dyRangeSelector()
print(fig)

# compute and plot returns based on Closing prices
re = diff(log(drk_bitfinex_USD$Last))
#re = append(re, 0, after = 0) # inserts a zero at the beginning to have the right length
drk_bitfinex_USD$return = re
rm(re) # remove temporay variable
# qplot(Date,return, data = drk_bitfinex_USD, geom = 'line', main = 'Litecoin Daily Return in USD - Exchange = bitfinex, colour = I('blue'))
fig = dygraph(drk_bitfinex_USD$return, main = "Darkcoin Daily Return in USD - Exchange = bitfinex") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"), drawPoints = TRUE, pointSize = 2)%>%
  dyRangeSelector()
#fig = ggplot(data = drk_bitfinex_USD, aes(x=Date, y=return)) + geom_line(color = 'dodgerblue4') + ggtitle("DarkCoin Daily Return in USD - cbx Exchange")
print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code



#========================================================================
# Other Data from the BLOCKCHAIN DATABASE (from Quandl)
#========================================================================
# Bitcoin My Wallet Number of Transaction Per Day
btc_bchain_num_trans=Quandl("BCHAIN/MWNTD", api_key=key, type = "xts")
names(btc_bchain_num_trans) <- "Num_Transactions"

# Bitcoin My Wallet Transaction Volume
btc_bchain_vol_trans=Quandl("BCHAIN/MWTRV", api_key=key, type = "xts")
names(btc_bchain_vol_trans) <- "MyWallet_trans_vol"

# Bitcoin average block size
btc_bchain_block_size=Quandl("BCHAIN/AVBLS",api_key=key, type = "xts")
names(btc_bchain_block_size) <- "Avg_Block_Size"

# Bitcoin USD Exchange trade volume
btc_bchain_exch_tr_vol=Quandl("BCHAIN/TRVOU",api_key=key, type = "xts")
names(btc_bchain_exch_tr_vol) <- "Bchain_USD_trade_vol"

# Not necessary if read as xts
# Rearranging all the BCHAIN Database series
#btc_bchain_num_trans = arrange(btc_bchain_num_trans, Date)
#btc_bchain_vol_trans = arrange(btc_bchain_vol_trans, Date)
#btc_bchain_block_size = arrange(btc_bchain_block_size, Date)
#btc_bchain_exch_tr_vol= arrange(btc_bchain_exch_tr_vol, Date)


#========================================================================
# plot Other Data from BLOCKCHAIN DATABASE (from Quandl)
#========================================================================
fig = dygraph(btc_bchain_num_trans$Num_Transactions, main = "BLOCKCHAIN DATABASE - Number of Transactions per Day") %>%
  dyOptions(colors = "darkblue") %>%
  #dyOptions(colors = RColorBrewer::brewer.pal(5, "RdYlBu")) %>%
  dyRangeSelector() # add range selector to the bottom of a dygraph
print(fig) 


fig = dygraph(btc_bchain_vol_trans$MyWallet_trans_vol, main = "BLOCKCHAIN DATABASE - My Wallet Transaction Volume per Day")  %>%
  dyOptions(colors = "midnightblue") %>%
  dyRangeSelector() # add range selector to the bottom of a dygraph
print(fig) 

fig = dygraph(btc_bchain_block_size$Avg_Block_Size, main = "BLOCKCHAIN DATABASE - Average Block Size")  %>%
  dyOptions(colors = "steelblue") %>%
  dyRangeSelector() # add range selector to the bottom of a dygraph
print(fig) 


fig = dygraph(btc_bchain_vol_trans$MyWallet_trans_vol, main = "BLOCKCHAIN DATABASE - My Wallet Transaction Volume per Day")  %>%
  dyOptions(colors = "midnightblue") %>%
  dyRangeSelector() # add range selector to the bottom of a dygraph
print(fig) 

fig = dygraph(btc_bchain_exch_tr_vol$Bchain_USD_trade_vol, main = "BLOCKCHAIN DATABASE - Bitcoin USD Exchange trade volume")  %>%
  dyOptions(colors = "steelblue") %>%
  dyRangeSelector() # add range selector to the bottom of a dygraph
print(fig)



#fig = ggplot(data = btc_bchain_num_trans, aes(x=Date, y=Value)) + geom_line(color = 'midnightblue') + ggtitle("BLOCKCHAIN DATABASE - Number of Transactions per Day")
#print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code

#fig = ggplot(data = btc_bchain_vol_trans, aes(x=Date, y=Value)) + geom_line(color = 'midnightblue') + ggtitle("BLOCKCHAIN DATABASE - My Wallet Transaction Volume per Day")
#print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code


#fig = ggplot(data = btc_bchain_block_size, aes(x=Date, y=Value)) + geom_line(color = 'midnightblue') + ggtitle("BLOCKCHAIN DATABASE - Bitcoin Average Block Size")
#print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code

#fig = ggplot(data = btc_bchain_exch_tr_vol, aes(x=Date, y=Value)) + geom_line(color = 'midnightblue') + ggtitle("BLOCKCHAIN DATABASE - Bitcoin USD Exchange trade volume")
#print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code



# =================================================================
#  Compute common start and end period for a given group of series
# =================================================================
rm(series_list)
rm(exclusion_list)
rm(min_index_df)
rm(max_index_df)
exclusion_list = c("instala_pacote", "key","read_quandl", "start_date", "end_date", "plot_closing_price", "plot_OHLC", "exclusion_list")
series_list = ls()
# Create group of series with everything but those in the exclusion list
# Min and maximum periods will be calculated for the series in series_list
# Using start_date and end_date functions previously defined

series_list = series_list[!series_list %in% exclusion_list]

min_index_df <- as.data.frame(as.Date(sapply(series_list, start_date)))

max_index_df <- as.data.frame(as.Date(sapply(series_list, end_date)))

names(min_index_df)[1]="start_date"
names(max_index_df)[1]="end_date"

# rename series list and convert to dataframe
x = as.data.frame(series_list, stringsAsFactors = FALSE)

min_index_df <- as.data.frame(as.Date(sapply(series_list, start_date)))
tt <- sapply(eval(as.name(x$series_list)), merge.xts)

