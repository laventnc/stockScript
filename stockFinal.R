library(quantmod)
library(TTR)
library(rvest)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
#install.packages(C('quantmod','TTR','rvest','plyr','dplyr','tidyr'))

setwd('/Users/NicholasLaventis/Desktop/GitHub/stockScript')

#List of Stocks, Remove or Add as Necessary
Stocks <- list("AA","ARNC","AAPL","ADM","AMGN","AMZN","AWK","BA","BF.B","BRK.B","BX","CAT","CMI","CSCO","CSX","CVS",
               "CVX","DAL","DIS","DOW","DUK","EMR","F","FB","GBX","GE","GILD","GLW","GOOGL","HD","HUM","JNJ",
               "JPM","KMI","KO","KR","LMT","LOW","LUV","MCD","MMM","MSFT","PG","PSA","PYPL","SO","T","UPS","VLO","WMT","YUM")


stockLinks <- list()
currPrices <- list()
vals <- list() # list for general stock price informatio pulled from nasdaq using rvest
ttrDF <- data_frame() # specialized stock indicators using TTR, etc.
finalDF <- data_frame() # final DF


updateStocks <- function(Stocks) {
  wrapper <- list() #wrapper to hold TTR data
  
  # Initialize the list of links
  base_url <- 'https://nasdaq.com/symbol/'
  for (h in 1:length(Stocks)) {
    stockLinks[h] <- paste0(base_url, Stocks[h])
  }
  
  # Get Current Prices
  stocks_string <- paste(unlist(x = Stocks), collapse = ";")
  currPrices <<- list(getQuote(Symbols = stocks_string)$Last)

  # iterate through the list of links, storing the data in a list of dataframes
  for (i in 1:length(stockLinks)) {
    temp <- vector("list", 7) # temporary list to hold each row of TTR data
    
    # Get Dividends, 52 Week High/Low, Beta, Dividend, EPS, P/E Ratios, etc...
    read_html(stockLinks[[i]]) %>%
      html_nodes(".genTable table") %>%
      html_table() -> sumTable
    
    vals[[i]] <<- as.data.frame(t(sumTable[[1]]$X2))
    
    temp[[1]] <- unlist(Stocks[i])
    
    # -80 to account for only 5 days of information per week, and holidays, with room for error
    # Not all stocks can pull from yahoo, therefore use google if it can't
    stock <- tryCatch({
      getSymbols(toString(Stocks[i]), env = NULL, src='google', from=toString(Sys.Date()-80))
    }, warning = function(w) {
      getSymbols(toString(Stocks[i]), env = NULL, src='yahoo', from=toString(Sys.Date()-80))
      # }, finally  = {
      #   # setDefaults here?
      # }
      next()
    }, error = function(e) {
      getSymbols(toString(Stocks[i]), env = NULL, src='yahoo', from=toString(Sys.Date()-80))
    })
    
    # Prep Data Structure for BBands, SMA & RSI which look only at closing prices
    closeName <- paste0(toString(Stocks[i]),".Close")
    stockClose <- stock[,closeName]
    
    # Bollinger Bands (calculated with 20 days)
    bBandValues <- tail(BBands(HLC(stock)[,closeName]), n=1)
    temp[[2]] <- coredata(bBandValues$'dn')[1]
    temp[[3]] <- coredata(bBandValues$'up')[1]
    
    # 50 & 200 Day Simple Moving Average 
    temp[[4]] <- as.double(coredata(tail(SMA(stockClose, n=50), n=1)))
    temp[[5]] <- twoSMA(toString(Stocks[i]))

    # 14 Day RSI
    RSI_temp <- coredata(tail(RSI(stockClose, n=14), n=14)) #All RSI Values in past 14 days
    temp[[6]] <- as.double(tail(RSI_temp, n=1)) #Current RSI Value
    
    # 14 Day Stochastic RSI
    minRSI <- coredata(RSI_temp[which.min(RSI_temp)])
    maxRSI <- coredata(RSI_temp[which.max(RSI_temp)])
    temp[[7]] <- as.double((tail(RSI_temp, n=1) - minRSI)/(maxRSI - minRSI))

    wrapper[[i]] <- temp # build a list of lists
  }
  
  ttrDF <<- data.frame(matrix(unlist(wrapper), nrow = length(Stocks), byrow = TRUE), stringsAsFactors = FALSE)
  colnames(ttrDF) <<- c("Symbol","lowerBB","upperBB","SMA_50","SMA_200","RSI","stochRSI")
  
  finalDF <<- mergeAndClean()
  wsjWrapper <- wsjData(Stocks)
  
  wsjDF <<- data.frame(do.call("rbind", wsjWrapper))
  colnames(wsjDF) <- c("EBITDA margin","Book Val/share","CF/share","FCF/share","Gross margin","ROA","ROIC","Tang Book Val","Debt-to-Equity","Liquidity (Current Ratio)")
  rownames(wsjDF) <- Stocks
  combinedDF <- cbind(finalDF, wsjDF)
  
  write.csv(x=combinedDF, file=paste(format(Sys.time(), "%Y-%m-%d %I-%p"), 'csv', sep = "."))

}

# calculates the 200 day SMA, because only yahoo has enough data
twoSMA <- function(Stock) {
  stock <- tryCatch({
    getSymbols(Stock, env=NULL, src='yahoo', from=toString(Sys.Date()-295))
  }, warning = function(w) {
    getSymbols(Stock, env=NULL, src='google', from=toString(Sys.Date()-295))
    next()
  }, error = function(e) {
    getSymbols(Stock, env=NULL, src='google', from=toString(Sys.Date()-295))
  })
  
  closeName <- paste0(toString(Stock),".Close")
  return(as.double(coredata(tail(SMA(stock[,closeName], n=200), n=1))))
}

# Grab more specific information from wallstreetjournal (annual data, not quarterly)
wsjWrapper <- list() 
# order is EBITDA margin, Book Value/share, CF/share, FCF/share, Gross margin, ROA, ROIC, Tang Book Val, Debt-to-Equity, Liquidity (Current Ratio)
wsjData <- function(Stocks) {
  stockLinks <- list()
  base_url <- "http://quotes.wsj.com/"
  for (i in 1:length(Stocks)) {
    stockLinks[i] <- paste0(base_url, Stocks[i],"/financials")
  }
  
  for (j in 1:length(stockLinks)) {
    wsjTemp <- list()
    webpage <- read_html(stockLinks[[j]])
    
    html_nodes(webpage, ".cr_financials_table") %>%
      html_table(fill = TRUE) -> wsj_table_1
    
    month <- colnames(wsj_table_1[[1]])[[2]] # get the current month so that you can reference the value without row number/col num

    wsjTemp[[1]] <- tryCatch({ # hanlde stocks where EBITDA is not present
      wsj_table_1[[1]][[month]][wsj_table_1[[1]][,1] == "EBITDA"][[2]]
    }, error = function(e) {
      "N/A"
    })
    
    wsjTemp[[2]] <- wsj_table_1[[2]][[month]][wsj_table_1[[2]][,1] == "Book Value Per Share"][[2]]
    wsjTemp[[3]] <- wsj_table_1[[3]][[month]][wsj_table_1[[3]][,1] == "Cash Flow Per Share"][[2]]
   
    wsjTemp[[4]] <- wsj_table_1[[3]][[month]][wsj_table_1[[3]][,1] == "Free Cash Flow Per Share"][[2]]
    
    html_nodes(webpage, xpath="//table[@class='cr_dataTable cr_sub_profitability']") %>%
      html_table(fill = TRUE) -> wsj_table_2
    wsjTemp[[5]] <- gsub("([[:alpha:]]|[[:blank:]])+", "", wsj_table_2[[1]][1,])
    if (wsjTemp[[5]] == "-") {
      wsjTemp[[5]] <- "N/A"
    }
    wsjTemp[[6]] <- gsub("([[:alpha:]]|[[:blank:]])+", '', wsj_table_2[[1]][5,])
    if (wsjTemp[[6]] == "-") {
      wsjTemp[[6]] <- "N/A"
    }
    wsjTemp[[7]] <- gsub("([[:alpha:]]|[[:blank:]])+", '', wsj_table_2[[1]][8,])
    if (wsjTemp[[7]] == "-") {
      wsjTemp[[7]] <- "N/A"
    }
    
    
    html_nodes(webpage, xpath="//table[@class='cr_dataTable cr_mod_pershare']") %>%
      html_table(fill = TRUE) -> wsj_table_3
    wsjTemp[[8]] <- gsub("([[:alpha:]]|[[:blank:]])+", "", wsj_table_3[[1]][2,][[1]])
    
    
    html_nodes(webpage, xpath="//table[@class='cr_dataTable cr_sub_capital']") %>%
      html_table(fill=TRUE) -> wsj_table_4
    wsjTemp[[9]] <- gsub("([[:alpha:]]|[[:blank:]])+", "",  wsj_table_4[[1]][5,][[1]])
    if (wsjTemp[[9]] == "--") {
      wsjTemp[[9]] <- "N/A"
    }
    
    
    html_nodes(webpage, xpath="//table[@class='cr_dataTable cr_sub_liquidity']") %>%
      html_table(fill=TRUE) -> wsj_table_5
    wsjTemp[[10]] <- gsub("([[:alpha:]]|[[:blank:]])+", "", wsj_table_5[[1]][1,][[1]])
    if (wsjTemp[[10]] == "-") {
      wsjTemp[[10]] <- "N/A"
    }
    
    wsjWrapper[[j]] <<- unlist(wsjTemp) # wrap the numbers in a list of list, for easy binding to data.frame
  }
  
  return(wsjWrapper)
}

# Merge the Data so that the different stocks format correctly into the DF
mergeAndClean <- function() {
  for (j in 1:length(vals)) {
    # shift Annualized Dividend over 4
    if (length(vals[[j]]) == 12) {
      end <- vals[[j]][8:12]
      nameVector <- c("V11","V12","V13", "V14", "V15")
      vals[[j]][,nameVector] <- end
    }
    
    # drop Best Bid/Ask if present
    if (length(vals[[j]]) == 21) {
      end1 <- vals[[j]][2:21]
      nameVector1 <- c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11",
                       "V12","V13","V14","V15","V16","V17","V18","V19","V20")
      vals[[j]][nameVector1] <- end1
    }
    
    # handle if Beta is at the end
    if (length(vals[[j]]) == 18) {
      vals[[j]][15] <- vals[[j]][18]
      vals[[j]]
    }
  }
  
  temp <- do.call(rbind.fill, vals)
  stockDF <- temp[1:15]
  columnNames <- c("1 Year Target","Today's High/Low","Share Volume","X Day Avg. Volume","Prev Close",
                   "52 Week High/Low","Market Cap","P/E Ratio","Forward P/E (1y)","EPS","Annualized Dividend",
                   "Ex Dividend Date","Dividend Payment Date","Current Yeild","Beta")
  colnames(stockDF) <- columnNames
  stockDF$'Symbol' <- Stocks
  
  # get rid of dollar signs & spaces
  for (k in 1:length(stockDF)) {
    stockDF[[k]] <- gsub('\\$|[[:space:]]',"",stockDF[[k]])
  }
  
  # split 52 week high low
  stockDF <- separate(data = stockDF, col = '52 Week High/Low', into = c("52 Week High", "52 Week Low"), sep = "/")
  
  cols_to_keep <- c('Symbol','Annualized Dividend','Beta','EPS','P/E Ratio','Forward P/E (1y)',
                    '52 Week High','52 Week Low')
  
  # Convert Data that was scraped via the web into a dataframe, preserving the data within to not convert
  valsDF <- data.frame(stockDF[cols_to_keep], stringsAsFactors = FALSE)
  
  # dplyr Inner Join, matching the Stock Symbols
  finalDF <<- data.frame(inner_join(valsDF, ttrDF, by='Symbol'), row.names = Stocks, stringsAsFactors = FALSE)[2:14]
  finalDF$'Current Price' <<- currPrices[[1]]
  
  
  finalDF <<- finalDF[,c(ncol(finalDF),1:(ncol(finalDF)-1))]
  colnames(finalDF) <<- c('Current Price','Annualized Dividend','Beta','EPS','P/E Ratio','Forward P/E (1y)',
                        '52 Week High','52 Week Low',"Lower BB","Upper BB","SMA_50","SMA_200","RSI","Stochastic RSI")
  
  finalDF[finalDF==""] <<- 'N/A'
  
  # swap out the current prices that are NA with Data scraped from nasdaq
  locations <- which(finalDF$`Current Price` == "N/A")
  sapply(X=locations, function(loc) {
    finalDF$`Current Price`[loc] <<- levels(vals[[loc]][[1]])
  })
  return (finalDF)
}

updateStocks(Stocks)

quit(save="no")
