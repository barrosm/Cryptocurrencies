# Econometric Analysis of Alt-Coin Time Series
# Monica Barros
#
# Date: 22/05/2017
#
# Changing directory to where the PROGRAM is 
# (use // in Windows, sometimes a single / works, in Unix, use \)

setwd('C://Dropbox//post-doc')
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

key = "x2BF4wDiE8bLpH6NCsqz"
#========================================================================
# READING DATA FROM QUANDL
#========================================================================

# Bitcoin rate in CHF (downloaded from Quandl)
btc_CHF = Quandl("BCHARTS/LOCALBTCCHF", api_key = key, type = "xts")

# Bitcoin rate in GBP - Exchange = Coinbase (downloaded from Quandl)
btc_coinbase_GBP = Quandl("BCHARTS/COINBASEGBP", api_key=key, type = "xts")
# Bitcoin rate in GBP - Exchange = Coinfloor (downloaded from Quandl)
btc_coinfloor_GBP = Quandl("BCHARTS/COINFLOORGBP", api_key=key, type = "xts")


# Bitcoin rate in EUR - Exchange = Coinbase (downloaded from Quandl)
btc_coinbase_EUR = Quandl("BCHARTS/COINBASEEUR", api_key=key, type = "xts")
# Bitcoin rate in EUR - Exchange = ItBit (downloaded from Quandl)
btc_itbit_EUR = Quandl("BCHARTS/ITBITEUR", api_key=key, type = "xts")


# Bitcoin rate in USD - Exchange = Coinbase (downloaded from Quandl)
btc_coinbase_USD =Quandl("BCHARTS/COINBASEUSD", api_key=key, type = "xts")
# Bitcoin rate in USD - Exchange = ItBit (downloaded from Quandl)
btc_itbit_USD = Quandl("BCHARTS/ITBITUSD", api_key=key, type = "xts")
# Bitcoin rate in USD - Exchange = CBX (downloaded from Quandl)
btc_cbx_USD = Quandl("BCHARTS/CBXUSD", api_key=key, type = "xts")


#======================================================================
#
# Other coins - Exchange = BITFINEX
#======================================================================

# ltc rates in USD, exchange = BITFINEX
ltc_btifinex_USD = Quandl("BITFINEX/LTCUSD", api_key=key, type = "xts")

# drk (Darkcoin) rates in USD, exchange = BTIFinex 
drk_btifinex_USD = Quandl("BITFINEX/DRKUSD", api_key=key, type = "xts")

#==============================================================
# Rearrange data frames in ascending (chronological order)
# Uses function 'arrange' from dplyr
# 
# NOT NECESSARY IF FORMAT WHEN READING QUANDL IS XTS
#==============================================================
#btc_CHF = arrange(btc_CHF, Date)
#btc_coinbase_GBP = arrange(btc_coinbase_GBP, Date)
#btc_coinfloor_GBP = arrange(btc_coinfloor_GBP, Date)

#btc_coinbase_EUR = arrange(btc_coinbase_EUR, Date)
#btc_itbit_EUR = arrange(btc_itbit_EUR, Date)

#btc_coinbase_USD = arrange(btc_coinbase_USD, Date)
#btc_itbit_USD = arrange(btc_itbit_USD, Date)
#btc_cbx_USD = arrange(btc_cbx_USD, Date)

#ltc_btifinex_USD = arrange(ltc_btifinex_USD, Date)
#drk_btifinex_USD = arrange(drk_btifinex_USD, Date)

#========================================================================
# PLOTS DATA AND CALCULATES RETURNS
#========================================================================
# plot closing prices and returns - btc_CHF
#========================================================================
fig = dygraph(btc_CHF$Close, main = 'BitCoin Daily Closing Price in CHF') %>%
dyRangeSelector() # add range selector to the bottom of a dygraph
print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code

#========================================================================
# Candlestick up to last 360 days
#========================================================================
OHLC <- tail(btc_CHF, n = 360) 
OHLC = OHLC[,-5:-7] 
fig = dygraph(OHLC, main = 'BitCoin OHLC in CHF') %>%
dyCandlestick() %>%
dyRangeSelector()
print(fig)


# compute and plot returns based on Closing prices
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
fig = dygraph(btc_coinbase_GBP$Close, main = 'BitCoin Daily Closing Price in GBP - Exchange = Coinbase ') %>%
  dyRangeSelector() # add range selector to the bottom of a dygraph
print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code

#fig = qplot(Date,Close, data = btc_coinbase_GBP, geom = 'line', main = 'BitCoin Daily Closing Price in GBP - Exchange = Coinbase ', colour = I('red'))
#print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code

#========================================================================
# Candlestick up to last 360 days
#========================================================================
OHLC <- tail(btc_coinbase_GBP, n = 360) 
OHLC = OHLC[,-5:-7] 
fig = dygraph(OHLC, main = 'BitCoin OHLC in GBP - Exchange = Coinbase ') %>%
  dyCandlestick() %>%
  dyRangeSelector()
print(fig)


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
fig = dygraph(btc_coinfloor_GBP$Close, main = 'BitCoin Daily Closing Price in GBP - Exchange = Coinfloor ') %>%
  dyRangeSelector() # add range selector to the bottom of a dygraph
print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code

#fig = qplot(Date,Close, data = btc_coinfloor_GBP, geom = 'line', main = 'BitCoin Daily Closing Price in GBP - Exchange = Coinfloor ', colour = I('red'))
#print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code


#========================================================================
# Candlestick up to last 360 days
#========================================================================
OHLC <- tail(btc_coinfloor_GBP, n = 360) 
OHLC = OHLC[,-5:-7] 
fig = dygraph(OHLC, main = 'BitCoin OHLC in GBP - Exchange = Coinfloor ') %>%
  dyCandlestick() %>%
  dyRangeSelector()
print(fig)


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
fig = dygraph(btc_coinbase_EUR$Close, main = 'BitCoin Daily Closing Price in EUR - Exchange = Coinbase ') %>%
  dyRangeSelector() # add range selector to the bottom of a dygraph
print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code

#fig = qplot(Date,Close, data = btc_coinbase_EUR, geom = 'line', main = 'BitCoin Daily Closing Price in EUR - Exchange = Coinbase ', colour = I('red'))
#print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code

#========================================================================
# Candlestick up to last 360 days
#========================================================================
OHLC <- tail(btc_coinbase_EUR, n = 360) 
OHLC = OHLC[,-5:-7] 
fig = dygraph(OHLC, main = 'BitCoin OHLC in EUR - Exchange = Coinbase ') %>%
  dyCandlestick() %>%
  dyRangeSelector()
print(fig)


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
fig = dygraph(btc_itbit_EUR$Close, main = 'BitCoin Daily Closing Price in EUR - Exchange = ITBIT ') %>%
  dyRangeSelector() # add range selector to the bottom of a dygraph
print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code

#fig = qplot(Date,Close, data = btc_itbit_EUR, geom = 'line', main = 'BitCoin Daily Closing Price in EUR - Exchange = ITBIT', colour = I('red'))
#print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code

#========================================================================
# Candlestick up to last 360 days
#========================================================================
OHLC <- tail(btc_itbit_EUR, n = 360) 
OHLC = OHLC[,-5:-7] 
fig = dygraph(OHLC, main = 'BitCoin OHLC in EUR - Exchange = ITBIT ') %>%
  dyCandlestick() %>%
  dyRangeSelector()
print(fig)


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
fig = dygraph(btc_coinbase_USD$Close, main = 'BitCoin Daily Closing Price in USD - Exchange = Coinbase ') %>%
  dyRangeSelector() # add range selector to the bottom of a dygraph
print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code

#fig = qplot(Date,Close, data = btc_coinbase_USD, geom = 'line', main = 'BitCoin Daily Closing Price in USD - Exchange = Coinbase ', colour = I('red'))
#print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code

#========================================================================
# Candlestick up to last 360 days
#========================================================================
OHLC <- tail(btc_coinbase_USD, n = 360) 
OHLC = OHLC[,-5:-7] 
fig = dygraph(OHLC, main = 'BitCoin OHLC in USD - Exchange = Coinbase ') %>%
  dyCandlestick() %>%
  dyRangeSelector()
print(fig)


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
fig = dygraph(btc_itbit_USD$Close, main = 'BitCoin Daily Closing Price in USD - Exchange = ITBIT ') %>%
  dyRangeSelector() # add range selector to the bottom of a dygraph
print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code

#fig = qplot(Date,Close, data = btc_itbit_USD, geom = 'line', main = 'BitCoin Daily Closing Price in USD - Exchange = ITBIT ', colour = I('red'))
#print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code

#========================================================================
# Candlestick up to last 360 days
#========================================================================
OHLC <- tail(btc_itbit_USD, n = 360) 
OHLC = OHLC[,-5:-7] 
fig = dygraph(OHLC, main = 'BitCoin OHLC in USD - Exchange = ITBIT ') %>%
  dyCandlestick() %>%
  dyRangeSelector()
print(fig)


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
# plot closing prices and returns - ltc_btifinex_USD
#========================================================================
fig = dygraph(ltc_btifinex_USD$Last, main = 'Litecoin Daily Closing Price in USD - Exchange = Btifinex ') %>%
  dyRangeSelector() # add range selector to the bottom of a dygraph
print(fig) 

#fig = qplot(Date,Last, data = ltc_btifinex_USD, geom = 'line', main = 'Litecoin Daily Closing Price in USD - Exchange = Btifinex ', colour = I('red'))
#print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code

#========================================================================
# Candlestick up to last 360 days
#========================================================================
OHLC <- tail(ltc_btifinex_USD, n = 360) 
OHLC = OHLC[,-5:-7] 
fig = dygraph(OHLC, main = 'Litecoin OHLC in USD - Exchange = Btifinex ') %>%
  dyCandlestick() %>%
  dyRangeSelector()
print(fig)


# compute and plot returns based on Closing prices
re = diff(log(ltc_btifinex_USD$Last))
#re = append(re, 0, after = 0) # inserts a zero at the beginning to have the right length
ltc_btifinex_USD$return = re
rm(re) # remove temporary variable
fig = dygraph(ltc_btifinex_USD$return, main = "Litecoin Daily Return in USD - Exchange = Btifinex") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"), drawPoints = TRUE, pointSize = 2)%>%
  dyRangeSelector()
# qplot(Date,return, data = ltc_btifinex_USD, geom = 'line', main = 'Litecoin Daily Return in USD - Exchange = Btifinex, colour = I('blue'))
#fig = ggplot(data = ltc_btifinex_USD, aes(x=Date, y=return)) + geom_line(color = 'dodgerblue4') + ggtitle("BitCoin Daily Return in USD - cbx Exchange")
print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code



#========================================================================
# plot closing prices and returns - drk_btifinex_USD
#========================================================================
fig = dygraph(drk_btifinex_USD$Last, main = 'Darkcoin Daily Closing Price in USD - Exchange = Btifinex ') %>%
  dyRangeSelector() # add range selector to the bottom of a dygraph
print(fig) 

#fig = qplot(Date,Last, data = drk_btifinex_USD, geom = 'line', main = 'Darkcoin Daily Closing Price in USD - Exchange = Btifinex ', colour = I('red'))
#print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code


#========================================================================
# Candlestick up to last 360 days
#========================================================================
OHLC <- tail(drk_btifinex_USD, n = 360) 
OHLC = OHLC[,-5:-7] 
fig = dygraph(OHLC, main = 'Darkcoin OHLC in USD - Exchange = Btifinex ') %>%
  dyCandlestick() %>%
  dyRangeSelector()
print(fig)

# compute and plot returns based on Closing prices
re = diff(log(drk_btifinex_USD$Last))
#re = append(re, 0, after = 0) # inserts a zero at the beginning to have the right length
drk_btifinex_USD$return = re
rm(re) # remove temporay variable
# qplot(Date,return, data = drk_btifinex_USD, geom = 'line', main = 'Litecoin Daily Return in USD - Exchange = Btifinex, colour = I('blue'))
fig = dygraph(drk_btifinex_USD$return, main = "Darkcoin Daily Return in USD - Exchange = Btifinex") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"), drawPoints = TRUE, pointSize = 2)%>%
  dyRangeSelector()
#fig = ggplot(data = drk_btifinex_USD, aes(x=Date, y=return)) + geom_line(color = 'dodgerblue4') + ggtitle("DarkCoin Daily Return in USD - cbx Exchange")
print(fig)  # need to explicitly call print to be able to see plots when "sourcing" code



#========================================================================
# Other Data from BLOCKCHAIN DATABASE (from Quandl)
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



#===============================================================================
# Download BTC-ETH pair data from poloniex
#===============================================================================
btc_ETH_pair <- fromJSON("https://poloniex.com/public?command=returnChartData&currencyPair=BTC_ETH&start=1435699200&end=9999999999&period=14400")

# list of variables in the btc_ETH_pair dataframe
# names(btc_ETH_pair)

# converts date
btc_ETH_pair$date=as.POSIXlt(btc_ETH_pair$date, origin = "1970-01-01")

#============================
# Converts to xts object
#============================
btc_ETH_pair = xts(btc_ETH_pair[,-1], order.by =btc_ETH_pair[,1])

# plot closing prices BTC-ETH prices
fig = dygraph(btc_ETH_pair$close, main = 'BTC/ETH Pair - Exchange = Poloniex ') %>%
  dyOptions(colors = "forestgreen") %>%
  dyRangeSelector() # add range selector to the bottom of a dygraph
print(fig)
#qplot(btc_ETH_pair$date,btc_ETH_pair$close, data = btc_ETH_pair, geom = 'line')
#fig = qplot(date,close, data = btc_ETH_pair, geom = 'line', main = 'BTC/ETH Pair - Poloniex - Closing Price', colour = I('navy'))
#print(fig) 


#========================================================================
# Candlestick up to last 360 days
#========================================================================
OHLC <- tail(btc_ETH_pair, n = 360) 
OHLC = OHLC[,-5:-7] 
fig = dygraph(OHLC, main = 'BTC/ETH Pair - OHLC in USD - Exchange = Poloniex ') %>%
  dyCandlestick() %>%
  dyRangeSelector()
print(fig)

# compute and plot BTC-ETH returns
re = diff(log(btc_ETH_pair$close))
#re = append(re, 0, after = 0) # inserts a zero at the beginning to have the right length
btc_ETH_pair$return = re
rm(re) # remove temporay variable
#fig = qplot(date,return, data = btc/ETH_pair, geom = 'line', main = 'BTC-ETH Return - Poloniex (every 4 hours)', colour = I('dodgerblue3'))
fig = dygraph(btc_ETH_pair$return, main = "BTC-ETH Return - Exchange = Poloniex (every 4 hours)") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"), drawPoints = TRUE, pointSize = 2)%>%
  dyRangeSelector()
print(fig)


# plot volume of BTC-ETH pair
fig = dygraph(btc_ETH_pair$volume, main = 'Btc/ETH Pair Volume - Exchange = Poloniex ') %>%
  dyOptions(colors = "forestgreen", drawPoints = TRUE, pointSize = 2) %>%
  dyRangeSelector() # add range selector to the bottom of a dygraph
print(fig)
#qplot(btc_ETH_pair$date,btc_ETH_pair$close, data = btc_ETH_pair, geom = 'line')
#fig = qplot(date,volume, data = btc_ETH_pair, geom = 'line', main = 'BTC/ETH Pair - Poloniex - Volume', colour = I('forestgreen'))




#===============================================================================
# Download BTC-XRP (Ripple) pair data from poloniex
#===============================================================================

btc_XRP_pair <- fromJSON("https://poloniex.com/public?command=returnChartData&currencyPair=BTC_XRP&start=1435699200&end=9999999999&period=14400")

# list of variables in the btc_XRP_pair dataframe
# names(btc_XRP_pair)

# converts date
btc_XRP_pair$date=as.POSIXlt(btc_XRP_pair$date, origin = "1970-01-01")

#============================
# Converts to xts object
#============================
btc_XRP_pair = xts(btc_XRP_pair[,-1], order.by =btc_XRP_pair[,1])

# plot closing prices BTC-XRP prices
fig = dygraph(btc_XRP_pair$close, main = 'BTC/XRP Pair - Exchange = Poloniex ') %>%
  dyOptions(colors = "forestgreen") %>%
  dyRangeSelector() # add range selector to the bottom of a dygraph
print(fig)
#qplot(btc_XRP_pair$date,btc_XRP_pair$close, data = btc_XRP_pair, geom = 'line')
#fig = qplot(date,close, data = btc_XRP_pair, geom = 'line', main = 'BTC/XRP Pair - Poloniex - Closing Price', colour = I('navy'))
#print(fig) 

#========================================================================
# Candlestick up to last 360 days
#========================================================================
OHLC <- tail(btc_XRP_pair, n = 360) 
OHLC = OHLC[,-5:-7] 
fig = dygraph(OHLC, main = 'BTC/XRP Pair - OHLC in USD - Exchange = Poloniex ') %>%
  dyCandlestick() %>%
  dyRangeSelector()
print(fig)

# compute and plot BTC-XRP returns
re = diff(log(btc_XRP_pair$close))
#re = append(re, 0, after = 0) # inserts a zero at the beginning to have the right length
btc_XRP_pair$return = re
rm(re) # remove temporay variable
fig = dygraph(btc_XRP_pair$return, main = "BTC/XRP Return - Exchange = Poloniex (every 4 hours)") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"), drawPoints = TRUE, pointSize = 2)%>%
  dyRangeSelector()
print(fig)
#fig = qplot(date,return, data = btc_XRP_pair, geom = 'line', main = 'BTC-XRP Return - Poloniex (every 4 hours)', colour = I('dodgerblue3'))


# plot volume BTC-XRP pair
#qplot(btc_XRP_pair$date,btc_XRP_pair$close, data = btc_XRP_pair, geom = 'line')
fig = dygraph(btc_XRP_pair$volume, main = 'Btc/XRP Pair Volume - Exchange = Poloniex ') %>%
  dyOptions(colors = "forestgreen", drawPoints = TRUE, pointSize = 2) %>%
  dyRangeSelector() # add range selector to the bottom of a dygraph
print(fig)
#fig = qplot(date,volume, data = btc_XRP_pair, geom = 'line', main = 'BTC/XRP Pair - Poloniex - Volume', colour = I('forestgreen'))
#print(fig)


#===============================================================================
# Download BTC-XEM (NEM) pair data from poloniex
#===============================================================================

btc_XEM_pair <- fromJSON("https://poloniex.com/public?command=returnChartData&currencyPair=BTC_XEM&start=1435699200&end=9999999999&period=14400")

# list of variables in the btc_XEM_pair dataframe
# names(btc_XEM_pair)

# converts date
btc_XEM_pair$date=as.POSIXlt(btc_XEM_pair$date, origin = "1970-01-01")

#============================
# Converts to xts object
#============================
btc_XEM_pair = xts(btc_XEM_pair[,-1], order.by =btc_XEM_pair[,1])

# plot closing prices BTC-XEM prices
fig = dygraph(btc_XEM_pair$close, main = 'BTC/XEM Pair - Exchange = Poloniex ') %>%
  dyOptions(colors = "forestgreen") %>%
  dyRangeSelector() # add range selector to the bottom of a dygraph
print(fig)
#qplot(btc_XEM_pair$date,btc_XEM_pair$close, data = btc_XEM_pair, geom = 'line')
#fig = qplot(date,close, data = btc_XEM_pair, geom = 'line', main = 'BTC/XEM Pair - Poloniex - Closing Price', colour = I('navy'))
#print(fig) 


#========================================================================
# Candlestick up to last 360 days
#========================================================================
OHLC <- tail(btc_XEM_pair, n = 360) 
OHLC = OHLC[,-5:-7] 
fig = dygraph(OHLC, main = 'BTC/XEM Pair - OHLC in USD - Exchange = Poloniex ') %>%
  dyCandlestick() %>%
  dyRangeSelector()
print(fig)


# compute and plot BTC-XEM returns
re = diff(log(btc_XEM_pair$close))
#re = append(re, 0, after = 0) # inserts a zero at the beginning to have the right length
btc_XEM_pair$return = re
rm(re) # remove temporay variable
fig = dygraph(btc_XEM_pair$return, main = "BTC/XEM Return - Exchange = Poloniex (every 4 hours)") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"), drawPoints = TRUE, pointSize = 2)%>%
  dyRangeSelector()
print(fig)
#fig = qplot(date,return, data = btc_XEM_pair, geom = 'line', main = 'BTC-XEM Return - Poloniex (every 4 hours)', colour = I('dodgerblue3'))
#print(fig)

# plot volume BTC-XEM pair
#qplot(btc_XEM_pair$date,btc_XEM_pair$close, data = btc_XEM_pair, geom = 'line')
fig = dygraph(btc_XEM_pair$volume, main = 'Btc/XEM Pair Volume - Exchange = Poloniex ') %>%
  dyOptions(colors = "forestgreen", drawPoints = TRUE, pointSize = 2) %>%
  dyRangeSelector() # add range selector to the bottom of a dygraph
print(fig)
#fig = qplot(date,volume, data = btc_XEM_pair, geom = 'line', main = 'BTC/XEM Pair - Poloniex - Volume', colour = I('forestgreen'))
#print(fig)


#===============================================================================
# Download Ether prices (hourly)
#===============================================================================
ETH = fromJSON("https://etherchain.org/api/statistics/price")
# Copy the actual time and ETH price data to a new dataframe and delete old
ETH_hour = ETH$data
rm(ETH)
# Data cleaning on the 'time' column of the ETH_hour dataframe
# if kept as dataframe, first column contains date, second column renamed "ETH
ETH_hour$time <- gsub('T', ' ', ETH_hour$time)
ETH_hour$time <- gsub('Z', ' ', ETH_hour$time)
ETH_hour$time <- gsub('.000', ' ', ETH_hour$time)
ETH_hour$time=as.POSIXlt(strptime(ETH_hour$time, format = "%Y-%m-%d %H:%M:%S"),origin = "1970-01-01")
names(ETH_hour)[names(ETH_hour)=="time"] <- "date"
names(ETH_hour)[names(ETH_hour)=="usd"] <- "ETH"


#=======================================
# Converts to xts object - DID NOT WORK
# EVEN WHEN I TRIED TO SPECIFY THE FREQUENCY
# 
# LEFT DATA AS DATAFRAME
#=======================================
#ETH_hour = xts(ETH_hour[,-1], order.by = ETH_hour[,1], frequency = 1)
#names(ETH_hour)<-"ETH"

# plot hourly ether price data
#fig = dygraph(ETH_hour$ETH, main = 'ETH Hourly Price - source = Etherchain.org ') %>%
#  dyOptions(colors = "red") %>%
#  dyRangeSelector() # add range selector to the bottom of a dygraph
#print(fig)

fig = qplot(date,ETH, data = ETH_hour, geom = 'line', main = 'ETH Hourly Price', colour = I('red'))
print(fig)

# compute and plot hourly returns
re = diff(log(ETH_hour$ETH))
re = append(re, 0, after = 0) # inserts a zero at the beginning to have the right length
ETH_hour$return = re
rm(re) # remove temporary variable

#fig = dygraph(ETH_hour$return, main = "ETH hourly Return - source = Etherchain.org") %>%
#  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"), drawPoints = TRUE, pointSize = 2)%>%
# dyRangeSelector()
pdf(file = figure.pdf)
fig = ggplot(data = ETH_hour, aes(x=date, y=return)) + geom_line(color = 'dodgerblue4') + ggtitle("ETH Hourly Return - source = Etherchain.org")
#fig = qplot(date,return, data = ETH_hour, geom = 'line', main = 'ETH Hourly Return', colour = I('dodgerblue3'))
print(fig)





