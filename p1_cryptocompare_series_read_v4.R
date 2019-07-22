#==============================================================================
# Program p1_cryptocompare_series_read_v4.R
# Monica Barros
# Date: 13/09/2017

# Series reading procedure MODIFIED to be able to specify ending date
# Uses read_crypto2 function (instead of read_crypto) defined in the p0_setup_v2 file
#==============================================================================
# USES LISTS 
# SELECTS DATA FROM "BIG" VECTOR OF PAIRS USING LOGICALS
#==============================================================================

# Function to read series using ticker as input
read_series <- function (ticker, nobs=2000, unix_time_stamp_GMT = today_utc) {
  coins <- unlist(strsplit(ticker, "-"))
  coin1 <- coins[1]
  coin2 <- coins[2]
  series <- read_crypto_2(coin1, coin2, nobs, unix_time_stamp_GMT)
  
  # Remove the last line because the data is incomplete (still trading)
  # series <- series[-nrow(series), ]
  return(series)
}

# Specify pairs to be read
# =============================================================================
# Included BTC-USD pair - will serve as reference to the others
pairs = c("BTC-USD", "BTS-BTC", "DASH-BTC","DGB-BTC","DOGE-BTC","ETC-BTC","ETH-BTC",
          "GNO-BTC", "GNT-BTC","GRC-BTC", "LTC-BTC","NXT-BTC", "REP-BTC", "STRAT-BTC",
          "XEM-BTC", "XLM-BTC", "XMR-BTC","XRP-BTC","ZEC-BTC")

# Create logical vector to indicate which pairs to be read
logic_vector = rep(0,length(pairs))
logic_vector[1]=1
logic_vector[4]=1
logic_vector[6]=1
logic_vector[7]=1
logic_vector[11] = 1
logic_vector
pairs2 = subset(pairs, logic_vector == 1)
pairs2

#==============================================================================
# Specify end date to be used - if not specified, the function
# read_crypto_2 will use the current date
end_date = (readline(prompt = "****** end date (format YYYY-MM-DD) ?? *****  "))
end_date_unix = as.numeric(as.POSIXct(end_date , origin = '1970-01-01', tz = 'UTC'))
#==============================================================================


nobs=50 # for testing purposes use smaller sample 

#==============================================================================
# Read series and put them in a list
#==============================================================================
# if end_date WAS specified and converted to epoch
if (exists("end_date_unix")) {
  # NAO FUNCIONA POR NADA DESTE MUNDO PARA PEGAR A DATA FINAL
  # EMBORA ESTEJA ENTRANDO CORRETAMENTE NESTA PARTE DO IF
  # COMO CONFIRMADO PELO VALOR DE tt
  list_series <- lapply(pairs2, read_series, nobs, end_date_unix) #today_utc)
  names(list_series) <- pairs2
  print(paste("number of series read:   ",length(pairs2))) 
  tt = TRUE
} else {  # end_date was NOT specified - use today as last date
  list_series <- lapply(pairs2, read_series, nobs, today_utc)
  names(list_series) <- pairs2
  print(paste("number of series read:   ",length(pairs2)))
  tt = FALSE
}
    

# Read series into R and assign names starting with "CC"
# converts each CC_ series to xts - avoids the convert_xts function
x <- NULL; #series_names <-NULL;
for (i in 1:length(pairs2)) {
  x[i] = as.data.frame(unlist(strsplit(pairs2[i],"-")));
  coin1 = x[[i]][1];
  coin2 = x[[i]][2];
  # converts time column to POSIXlt
  # the next two lines are just equivalent to applying the "convert_xts" function
  list_series[[i]][['time']]= as.POSIXlt(list_series[[i]][['time']], origin = '1970-01-01', tz = "UTC")
  list_series[[i]] = xts(list_series[[i]][,-1], order.by = list_series[[i]][,1])
  assign(paste0("cc_",coin1,"_",coin2), list_series[[i]])
  }
rm(x); rm(i); rm(coin1); rm(coin2);


# creates list of "cc_" series (that were already converted to xts)
cclist = ls(pattern="cc_")

#==============================================================================
### Creation of Short Series (last n_days days)
#==============================================================================
n_days = as.integer(readline(prompt = "****** Enter length of the short series ***** "))

x <- NULL; 
for (i in 1:length(pairs2)) {
  x[i] = as.data.frame(unlist(strsplit(pairs2[i],"-")));
  coin1 = x[[i]][1];
  coin2 = x[[i]][2];
  a = eval(as.name(cclist[i]))
  assign(paste0("short_",coin1,"_",coin2), xts::last(a, n_days) )
}
rm(x); rm(i); rm(coin1); rm(coin2); rm(a);


#==============================================================================
# Places all "short" series into a single object
#==============================================================================
# creates list of "short series names"

# BE CAREFUL !!! - IF RUN AFTER RUNNING OTHER PROGRAMS WILL "CATCH" SERIES THAT DO NOT
# START WITH "short_" BUT MERELY INCLUDE THIS PATTERN AND WILL LEAD TO ERRORS
serieslist = ls(pattern="short_")

short_series <-NULL;
for (i in 1:length(pairs2)) { 
  #print(str(eval(as.name(serieslist[i]))))
 short_series[[i]] = do.call(cbind, list(eval(as.name(serieslist[i]))))
}

# =============================================================================
# Need to change time zone, other will run into trouble when doing zoo and xts
Sys.setenv(TZ="UTC")





