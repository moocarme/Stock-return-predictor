# https://www.quantstart.com/articles/ARIMA-GARCH-Trading-Strategy-on-the-SP500-Stock-Market-Index-Using-R

# Import the necessary libraries
library(httr)
library(twitteR)
library(httpuv)
library(googletrend)
library(caret)
library(stringr)
library(tm)
library(dplyr)
library(plyr)
library(quantmod)
library(lubridate)
library(lattice)
library(timeSeries)
library(rugarch)
library(googletrend)
library(gmailr)

gtrendApple <- googletrend::gettrend(keyword = "apple", geo = "US-NY",year = 2016, plot = TRUE)
gtrendApple <- datareader('../../../Downloads/report.csv', simple = TRUE)$trend
gtrendApplen <- gtrendApple[!is.na(gtrendApple[,1]),]
gtrendApplen[,1] <-gtrendApplen[,1]+1 # adjust date to Monday

windowLength = 100

indices = c('AAPL')
# Obtain the AAPL returns and truncate the NA value
getSymbols(indices, from=Sys.Date()-45*windowLength)

ggplot() + geom_line(data = AAPL, aes(x = index(AAPL), y = Ad(AAPL))) + labs(x = 'Date', y = 'Adjusted Stock Price')
spReturns = diff(log(Ad(AAPL))) # may have to use the adjusted rate because of stock split
spReturns[as.character(head(index(Cl(AAPL)),1))] = 0

plot(spReturns)

spReturns2 <- data.frame(week=index(spReturns), coredata(spReturns))
tot_df <- left_join(spReturns2, gtrendApplen, by = 'week')
tot_df$index <-na.spline(tot_df$index)# spline gtrend data
tot_df[tot_df$index<0] <- 0


# Create the forecasts vector to store the predictions
foreLength = length(spReturns) - windowLength
forecasts <- vector(mode="character", length=foreLength)
forecasts.conserv <- vector(mode="character", length=foreLength)

bsh <- 0.001 # buy/sell/hold condition

ptm <- proc.time()

for (d in 0:foreLength) {
  # Obtain the AAPL rolling window for this day
  spReturnsOffset = spReturns[(1+d):(windowLength+d)]
  
  # Fit the ARIMA model
  final.aic <- Inf
  final.order <- c(0,0,0)
  
  #go through all pq combinations
  for (p in 0:5) for (q in 0:5) {
    if ( p == 0 && q == 0) {
      next
    }
    
    # Add xreg for googletrend/twitter
    arimaFit = tryCatch( arima(spReturnsOffset, order=c(p, 0, q), xreg = tot_df$index),
                         error=function( err ) FALSE,
                         warning=function( err ) FALSE )
    
    if( !is.logical( arimaFit ) ) {
      current.aic <- AIC(arimaFit)
      if (current.aic < final.aic) {
        final.aic <- current.aic
        final.order <- c(p, 0, q)
        # Add xreg for googletrend/
        final.arima <- arima(spReturnsOffset, order=final.order, xreg = tot_df$index)
      }
    } else {
      next
    }
  }
  
  # Specify and fit the GARCH model
  spec = ugarchspec(
    variance.model=list(garchOrder=c(1,1)),
    mean.model=list(armaOrder=c(final.order[1], final.order[3]), include.mean=T),
    distribution.model="sged"
  )
  fit = tryCatch(
    ugarchfit(
      spec, spReturnsOffset, solver = 'hybrid'
    ), error=function(e) e, warning=function(w) w
  )
  
  # If the GARCH model does not converge, set the direction to "long" else
  # choose the correct forecast direction based on the returns prediction
  # Output the results to the screen and the forecasts vector
  if(is(fit, "warning")) {
    forecasts[d+1] = paste(index(spReturnsOffset[windowLength]), 1, sep=",")
    forecasts.conserv[d+1] = paste(index(spReturnsOffset[windowLength]), 0, sep=",")
    print(paste(index(spReturnsOffset[windowLength]), 1, sep=","))
  } else {
    fore = ugarchforecast(fit, n.ahead=1)
    ind = fore@forecast$seriesFor
    forecasts[d+1] = paste(colnames(ind), ifelse(ind[1] < 0, -1, 1), sep=",")
    forecasts.conserv[d+1] = paste(colnames(ind), ifelse(ind[1] > bsh, 1, ifelse(ind[1] < (-bsh), -1, 0)), sep=",")
    print(paste(colnames(ind), ind, ifelse(ind[1] < 0, -1, 1), ifelse(ind[1] > bsh, 'buy', ifelse(ind[1]< (-bsh), 'sell', 'hold')), sep=",")) 
  }
}
proc.time() - ptm

forecasts.corr <- as.data.frame(matrix(unlist(sapply(forecasts, function(x) strsplit(x,','))), ncol = 2, byrow = T))
forecasts.corr <- forecasts.corr %>% mutate(BSH = lag(forecasts.corr$V2)) %>% select(-V2)
forecasts.corr$BSH <- as.numeric(forecasts.corr$BSH)
forecasts.corr[is.na(forecasts.corr)] <- 1

forecasts.conserv.corr <- as.data.frame(matrix(unlist(sapply(forecasts.conserv, function(x) strsplit(x,','))), ncol = 2, byrow = T))
forecasts.conserv.corr <- forecasts.conserv.corr %>% mutate(BSH = lag(forecasts.conserv.corr$V2)) %>% select(-V2)
forecasts.conserv.corr$BSH <- as.numeric(forecasts.conserv.corr$BSH)
forecasts.conserv.corr[is.na(forecasts.conserv.corr)] <- 0

spArimaGarch_wgtrend2 <- xts(forecasts.corr[,-1], 
                             order.by = as.Date(forecasts.corr[,1]))
spArimaGarch_wgtrend2.conserv <- xts(forecasts.conserv.corr[,-1], 
                                     order.by = as.Date(forecasts.conserv.corr[,1]))
# go through with new xts object and check it works

# Create the ARIMA+GARCH returns
spIntersect_wogtrend = merge( spArimaGarch_wogtrend[,1], spReturns, all=F )
spIntersect_w2gtrend2 = merge( spArimaGarch_wgtrend2[,1], spReturns, all=F )
spArimaGarchReturns_wogtrend = spIntersect_wogtrend[,1] * spIntersect_wogtrend[,2]
spArimaGarchReturns_wgtrend = spArimaGarch_wgtrend* spIntersect_wogtrend[,2]

spIntersect_wgtrend2 = merge( spArimaGarch_wgtrend2[,1], spReturns, all=F )
spArimaGarchReturns_wgtrend2 = as.numeric(spArimaGarch_wgtrend2)* spIntersect_wgtrend2[,2]
spIntersect_wgtrend2.conserv = merge( spArimaGarch_wgtrend2.conserv[,1], spReturns, all=F )
spArimaGarchReturns_wgtrend2.conserv = as.numeric(spArimaGarch_wgtrend2.conserv)* spIntersect_wgtrend2.conserv[,2]


# Create the backtests for ARIMA+GARCH and Buy & Hold
spArimaGarchCurve_wogtrend = log( cumprod( 1 + spArimaGarchReturns_wogtrend ) )
spArimaGarchCurve_wgtrend = log( cumprod( 1 + spArimaGarchReturns_wgtrend ) )
spBuyHoldCurve = log( cumprod( 1 + spIntersect_wogtrend[,2] ) )
spCombinedCurve = merge(merge( spArimaGarchCurve_wogtrend, spArimaGarchCurve_wgtrend, all=F ), spBuyHoldCurve, all = F)

spBuyHoldCurve2 = log( cumprod( 1 + spIntersect_wgtrend2[,2] ) )
spArimaGarchCurve_wgtrend2 = log( cumprod( 1 + spArimaGarchReturns_wgtrend2 ) )
spArimaGarchCurve_wgtrend2.conserv = log( cumprod( 1 + spArimaGarchReturns_wgtrend2.conserv ) )

spCombinedCurve2 = merge(merge(spBuyHoldCurve2, spArimaGarchCurve_wgtrend2, all=F ), spArimaGarchCurve_wgtrend2.conserv, all= F)


# Plot the equity curves

# ggplot() + geom_line(aes(x = index(spBuyHoldCurve), y = spBuyHoldCurve), color = 'blue', lwd = 1) + 
#   geom_line(aes(x = index(spArimaGarchCurve_wogtrend), y = spArimaGarchCurve_wogtrend), color = 'red', lwd = 1) + 
#   geom_line(aes(x = index(spArimaGarchCurve_wgtrend), y = spArimaGarchCurve_wgtrend), color = 'darkgreen', lwd = 1) +
#   labs(x = 'Date', y = 'Equity')+ guides(col = guide_legend(nrow = 3))

xyplot(
  spCombinedCurve2,
  superpose=T,
  col=c( "darkblue", "darkgreen", "darkred"),
  lwd=2,
  key=list(
    text=list(
      c("Buy & Hold", "ARIMA + GARCH + Google Trend", "conservative")
    ),
    lines=list(
      lwd=2, col=c( "darkblue", "darkgreen", 'darkred')
    )
  )
)

use_secret_file("project_gmail_creds.json")
complete_email <- mime(
  To = "moocarme@gmail.com",
  From = "moocarme@gmail.com",
  Subject = paste(toString(Sys.Date()),"Stock prediction finished",
  body = paste0('The trading prediction has finished for ',toString(Sys.Date()), '. \n', 
                'The predridction for AAPL was', toString(ind[1]), '. \n',
                'You should ', toString(ifelse(ind[1]>0, 'buy', 'sell')), '. \n',
                'A more conservative model at a daily buy/sell rate of ', toString(bsh),
                ' percent would suggest you ', ifelse(ind[1]>bsh, 'buy.', ifelse(ind[1]<(-bsh), 'sell.', 'hold.'))))
send_message(complete_email)