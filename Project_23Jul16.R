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
library(readr)

Ind1 <- read_csv('indices.csv')

Ind1 <- Ind1[11:nrow(Ind1),]
for(i in 1:nrow(Ind1)){
  indices = Ind1$index.1[i]
  keyword = Ind1$Company[i]
  gtrend <- googletrend::gettrend(keyword = keyword, geo = "US-NY", plot = FALSE)
  gtrend <- datareader('../../../Downloads/report.csv', simple = TRUE)$trend
  gtrend2 <- gtrend[!is.na(gtrend[,1]),]
  gtrend2[,1] <-gtrend2[,1]+1 # adjust date to Monday
  if(is.null(gtrend)) next
  windowLength = 200
  
  # Obtain the AAPL returns and truncate the NA value
  getSymbols(indices, from=Sys.Date()-5*windowLength)
  
  ggplot() + geom_line(data = get(indices), aes(x = index(get(indices)), y = Ad(get(indices)))) + labs(x = 'Date', y = 'Adjusted Stock Price')
  spReturns = diff(log(Ad(get(indices)))) # may have to use the adjusted rate because of stock split
  spReturns[as.character(head(index(Cl(get(indices))),1))] = 0
  
  # plot(spReturns)
  
  spReturns2 <- data.frame(week=index(spReturns), coredata(spReturns))
  tot_df <- left_join(spReturns2, gtrend2, by = 'week')
  tot_df$index <-na.spline(tot_df$index)# spline gtrend data
  tot_df[tot_df$index<0] <- 0
  
  
  # Create the forecasts vector to store the predictions
  foreLength = length(spReturns) - windowLength
  # forecasts <- vector(mode="character", length=foreLength)
  # forecasts.conserv <- vector(mode="character", length=foreLength)
  bsh1 <- as.data.frame(matrix(ncol = 2, nrow = foreLength+1))
  
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
      print("warning in fit")
    } else {
      fore = ugarchforecast(fit, n.ahead=1)
      ind = fore@forecast$seriesFor
      bsh1$date[d+1] <- colnames(ind)
      # print(colnames(ind))
      bsh1$ind[d+1] <- ind[1]
       }
  }
  proc.time() - ptm
  
  adstock <- (Ad(get(indices)))[(nrow(get(indices))-nrow(bsh1)+1):nrow(get(indices)),]
  
  out1 <- mutate(select(bsh1, date, ind), Adjusted.Stock = adstock)
  write_csv(gtrend2, paste0('Output Files/', indices, '_gtrend.csv'))
  write_csv(out1, paste0('Output Files/',indices, '_output.csv'))
}

# # This section now in the app
# tol <- 0.001
# bsh1$position <- ifelse(bsh1$ind > tol, 1, ifelse(bsh1$ind < (-tol), -1, 0))
# 
# i <- 1
# portfolio.value <- data.frame(matrix(ncol = 2, nrow = nrow(bsh1)))
# pos1 <- 1 # look for 
# profit <- 0
# while (i<nrow(bsh1)){
#   newi = min(which(bsh1$position[i:nrow(bsh1)] == pos1)) + i-1
#   if(is.infinite(newi)){newi <- nrow(bsh1)}
#   portfolio.value[i:newi,1] <- -0.5*pos1 + 0.5
#   portfolio.value[i:newi,2] <- ifelse(pos1 == 1, profit, 
#                                      profit + as.data.frame(adstock[i:newi]-
#                                        as.numeric(adstock[i])))
#   if(pos1 == -1){
#     profit <- profit + as.numeric(adstock[newi]) - 
#       as.numeric(adstock[i])
#   }
#   i <- newi
#   pos1 <- -1*pos1
# }
# 
