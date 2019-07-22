# Program p1_Quand_BTER_series_read.R
# Monica Barros
#
# Date: 12/06/2017
#
# Reading BITCOIN SERIES IN DIFFERENT CURRENCIES
# DATA IS READ AS XTS
#

# BTER Exchange - Data Format
# Date
# High
# Low
# Last
# Average
# Sell
# Buy
# Volume (in the currency)
# Volume (BTC)

# The (geometric) Daily Returns will be calculated using the "Last" column


# Need to specify the plot_OHLC function in a different way because of the 
# Format of the data
plot_OHLC_BTER <- function (series, n = 360) {
  OHLC = tail(eval(as.name(series)), n) 
  if ("return" %in% colnames(OHLC))
     { OHLC$return <- NULL }
      OHLC = OHLC[,-5:-8] 
  # rearrange columns
  OHLC2 <-OHLC
  #OHLC2 <- matrix()
  OHLC2[,1] <- OHLC[,4]
  OHLC2[,2] <- OHLC[,1]
  OHLC2[,3] <- OHLC[,2]
  OHLC2[,4] <- OHLC[,3]
  fig = dygraph(OHLC2 , main = paste0(c('Candlechart    '),series,'  up to  ',n, ' days')) %>%
  dyCandlestick() %>%
  dyRangeSelector()
  print(fig)
}
#============================================================================
# Returns are also computed - requires the QUANTMOD library
# BTER - Historical exchange rate data for crypto currencies.
# BTER/BTSBTC Historical exchange rate data for BTS/BTC from BTER (Bitshares)
#============================================================================
btc_BTER_BTSBTC	= read_quandl("BTER/BTSBTC")
# BTER/CNCBTC (CNHCoin)
btc_BTER_CNCBTC	= read_quandl("BTER/CNCBTC")
# BTER/DASHBTC Historical exchange rate data for DASH/BTC
btc_BTER_DASHBTC	= read_quandl("BTER/DASHBTC")
# BTER/ETHBTC Historical exchange rate data for ETH/BTC from BTER
btc_BTER_ETHBTC	= read_quandl("BTER/ETHBTC")
# BTER/LTCBTC	LTC/BTC Exchange Rate
btc_BTER_LTCBTC	= read_quandl("BTER/LTCBTC")
# BTER/NXTBTC	NXT/BTC Exchange Rate
btc_BTER_NXTBTC	= read_quandl("BTER/NXTBTC")
# BTER/PPCBTC	PPC/BTC Exchange Rate (Peercoin)
btc_BTER_PPCBTC	= read_quandl("BTER/PPCBTC")
# BTER/PTSBTC	PTS/BTC Exchange Rate
btc_BTER_PTSBTC	= read_quandl("BTER/PTSBTC")
# BTER/PRTBTC	PRT/BTC Exchange Rate
btc_BTER_PRTBTC	= read_quandl("BTER/PRTBTC")
# BTER/REPBTC	REP/BTC Exchange Rate (Augur)
btc_BTER_REPBTC	= read_quandl("BTER/REPBTC")
# BTER/XEMBTC	XEM/BTC Exchange Rate (Xem)
btc_BTER_XEMBTC	= read_quandl("BTER/XEMBTC")
# BTER/XMRBTC	XMR/BTC Exchange Rate
btc_BTER_XMRBTC	= read_quandl("BTER/XMRBTC")
# BTER/XTCBTC	XTC/BTC Exchange Rate (Tilecoin)
btc_BTER_XTCBTC	= read_quandl("BTER/XTCBTC")
# BTER/ZECBTC	ZEC/BTC Exchange Rate (ZCash)
btc_BTER_ZECBTC	= read_quandl("BTER/ZECBTC")
# BTER/ZETBTC	ZET/BTC Exchange Rate
btc_BTER_ZETBTC	= read_quandl("BTER/ZETBTC")
# BTER/ETCETH - Historical exchange rate data for ETC/ETH from BTER. Updated at 7:00pm EST
btc_BTER_ETCETH	= read_quandl("BTER/ETCETH")

#============================================================================
# Exchange = BitFinex
#
# Format:
# High
# Low
# Mid
# Last
# Bid
# Ask
# Volume

# BITFINEX	
#============================================================================
#  LTC/BTC Exchange Rate
btc_BITFINEX_LTCBTC = read_quandl("BITFINEX/LTCBTC")
# RRT (Recovery Right Tokens)/BTC Exchange Rate
btc_bitfinex_RRTBTC=read_quandl("BITFINEX/RRTBTC")
# XMR/BTC Exchange Rate
btc_BITFINEX_XMRBTC	= read_quandl("BITFINEX/XMRBTC")
# BITFINEX/XRPBTC	XRP/BTC Exchange Rate
btc_BITFINEX_XRPBTC	= read_quandl("BITFINEX/XRPBTC")
# BITFINEX/ZECBTC	ZEC/BTC Exchange Rate
btc_BITFINEX_ZECBTC	= read_quandl("BITFINEX/ZECBTC")

# GDAX
# Format: Open, High, Low, Close, Volume
#============================================================================
#  LTC/BTC Exchange Rate
btc_GDAX_LTCBTC = read_quandl("GDAX/LTC_BTC")
#  LTC/BTC Exchange Rate
btc_GDAX_ETHBTC = read_quandl("GDAX/ETH_BTC")

#============================================================================
# Compute log-returns
#============================================================================
r_BTER_BTSBTC	= dailyReturn(btc_BTER_BTSBTC$Last, type = "log")

r_BTER_CNCBTC	= dailyReturn(btc_BTER_CNCBTC$Last, type = "log")

r_BTER_DASHBTC	= dailyReturn(btc_BTER_DASHBTC$Last, type = "log")

r_BTER_ETHBTC	= dailyReturn(btc_BTER_ETHBTC$Last, type = "log")

r_BTER_LTCBTC	= dailyReturn(btc_BTER_LTCBTC$Last, type = "log")

r_BTER_NXTBTC	= dailyReturn(btc_BTER_NXTBTC$Last, type = "log")

r_BTER_PPCBTC	= dailyReturn(btc_BTER_PPCBTC$Last, type = "log")

r_BTER_PTSBTC	= dailyReturn(btc_BTER_PTSBTC$Last, type = "log")

r_BTER_PRTBTC	= dailyReturn(btc_BTER_PRTBTC$Last, type = "log")

r_BTER_REPBTC	= dailyReturn(btc_BTER_REPBTC$Last, type = "log")

r_BTER_XEMBTC	= dailyReturn(btc_BTER_XEMBTC$Last, type = "log")

r_BTER_XMRBTC	= dailyReturn(btc_BTER_XMRBTC$Last, type = "log")

r_BTER_XTCBTC	= dailyReturn(btc_BTER_XTCBTC$Last, type = "log")

r_BTER_ZECBTC	= dailyReturn(btc_BTER_ZECBTC$Last, type = "log")

r_BTER_ZETBTC	= dailyReturn(btc_BTER_ZETBTC$Last, type = "log")

r_BTER_ETCETH	= dailyReturn(btc_BTER_ETCETH$Last, type = "log")

# BITFINEX - Returns
# LTC/BTC Exchange Rate
r_BITFINEX_LTCBTC = dailyReturn(btc_BITFINEX_LTCBTC$Last, type = "log")
# RRT/BTC Exchange Rate
r_BITFINEX_RRTBTC = dailyReturn(btc_BITFINEX_RRTBTC$Last, type = "log")
# XMR/BTC Exchange Rate
r_BITFINEX_XMRBTC	= dailyReturn(btc_BITFINEX_XMRBTC$Last, type = "log")
# BITFINEX/XRPBTC	XRP/BTC Exchange Rate
r_BITFINEX_XRPBTC	= dailyReturn(btc_BITFINEX_XRPBTC$Last, type = "log")
# BITFINEX/ZECBTC	ZEC/BTC Exchange Rate
r_BITFINEX_ZECBTC	= dailyReturn(btc_BITFINEX_ZECBTC$Last, type = "log")	


# GDAX - Returns
# 
#  NEED TO CHANGE THE WAY RETURNS ARE CALCULATED
# INPUT DATA CONTAINS ONLY: Data, Open, High, Low, Volume
# For now, I'm calculating returns based on opening data
# Maybe avg of opem, high, low migh be better choice

# LTC/BTC Exchange Rate
r_GDAX_LTCBTC = dailyReturn(btc_GDAX_LTCBTC$Open, type = "log")
# ETH/BTC Exchange Rate
r_GDAX_ETHBTC = dailyReturn(btc_GDAX_ETHBTC$Open, type = "log")

# The xts merge(.) function will only accept two series at a time.
# We can, however, merge multiple columns by downcasting to *zoo* objects.
# Remark:  "all = FALSE" uses an inner join to merge the data.

retornos = merge(as.zoo(r_BTER_BTSBTC),as.zoo(r_BTER_CNCBTC),as.zoo(r_BTER_DASHBTC),
                 as.zoo(r_BTER_ETHBTC),as.zoo(r_GDAX_ETHBTC),
                 as.zoo(r_BTER_LTCBTC), as.zoo(r_BITFINEX_LTCBTC),as.zoo(r_GDAX_LTCBTC),
                 as.zoo(r_BTER_NXTBTC), as.zoo(r_BTER_PPCBTC),as.zoo(r_BTER_PTSBTC),as.zoo(r_BTER_PRTBTC),
                 as.zoo(r_BTER_REPBTC), as.zoo(r_BITFINEX_RRTBTC), as.zoo(r_BTER_XEMBTC),
                 as.zoo(r_BTER_XMRBTC), as.zoo(r_BITFINEX_XMRBTC),as.zoo(r_BITFINEX_XRPBTC), as.zoo(r_BTER_XTCBTC),as.zoo(r_BTER_ZETBTC),
                 as.zoo(r_BTER_ZECBTC), as.zoo(r_BITFINEX_ZECBTC),
                 as.zoo(r_BTER_ETCETH))

myColnames  <- c("BTS-BTC","CNC-BTC","DASH-BTC",
                 "ETH-BTC", "GDAX ETH-BTC", "LTC-BTC", 
                 "BitFinex LTC-BTC", "GDAX LTC-BTC","NXT-BTC",
                 "PPC-BTC","PTS-BTC","PRT-BTC",
                 "REP-BTC","BitFinex RRT-BTC","XEM-BTC",
                 "XMR-BTC", "BitFinex XMR-BTC","BitFinex XRP-BTC",
                 "XTC-BTC","ZET-BTC","ZEC-BTC", 
                 "BitFinex ZEC-BTC", "ETC-ETH")
                 
#============================================================================
# summary statistics of returns
#============================================================================

colnames(retornos) <- myColnames

# Cast back to an xts object:
retornos <- as.xts(retornos)

#=======================================================
# Compute descriptive stats and quantiles of log returns
#=======================================================
ret_min = sapply(retornos, min, na.rm = TRUE)
ret_max = sapply(retornos, max, na.rm = TRUE)
ret_mean = sapply(retornos, mean, na.rm = TRUE)
ret_median = sapply(retornos, median, na.rm = TRUE)
ret_std  = sapply(retornos, sd, na.rm = TRUE)
ret_quantiles = sapply(retornos, quantile, probs = seq(0.05,0.95,0.1), na.rm = TRUE)
n_missing = sapply(retornos, function(x) sum(length(which(is.na(x)))))
n_obs = sapply(retornos, function(x) nrow(x) -sum(length(which(is.na(x)))))

# summary_general=as.data.frame(lapply(retornos, summary))
summary_general=as.data.frame(rbind(n_obs, n_missing, ret_mean,ret_median, ret_std, ret_min, ret_quantiles, ret_max))


# Compute version of return matrix without "bad" series

ret_name_number<-as.data.frame(colnames(retornos))

# Alterar por causa das novas series - 
retornos2 = retornos[,-23]
retornos2 = retornos2[, -22]
retornos2 = retornos2[, -21]
retornos2 = retornos2[, -18]
retornos2 = retornos2[, -17]
retornos2 = retornos2[, -13]
retornos2 = retornos2[, -8]
retornos2 = retornos2[, -5]

nrow(retornos2)
ncol(retornos2)

names(retornos2)

#=======================================================
# Compute correlation of returns (based on the smaller matrix)
#=======================================================
ret_corr_matrix = cor(retornos2, use = "complete.obs")

plot_title = "   Correlation of Returns  "
# Create a function to Generate Heat Map based on correlation matrix
# corrplot(ret_corr_matrix, method = "square")
# corrplot(ret_corr_matrix, method = "color")
corrplot(ret_corr_matrix, title = plot_title, method = "number", type = "upper",tl.col="black", tl.srt= 45)

# Displays Heatmap of correlation matrix, upper triangular portion only
corrplot(ret_corr_matrix, method = "color", type = "upper")


#The correlation matrix can be reordered according to the correlation coefficient. This is important to identify the hidden structure and pattern
#in the matrix. "hclust" for hierarchical clustering order is used in the following examples.
# correlogram with hclust reordering
# tl.col="black" changes color of labels to black
# tl.srt= 45 rotates vertical text labels
corrplot(ret_corr_matrix, method = "color", type="upper", order="hclust", tl.col="black", tl.srt= 45)

corrplot(ret_corr_matrix, method = "number", type="upper", order="hclust", tl.col="black", tl.srt= 45)

