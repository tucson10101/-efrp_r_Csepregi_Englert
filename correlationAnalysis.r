correlationAnalysis <- function(X){
  TS1 <- X$LAST %>% dplyr::select(X$Maturity[1])#saving appropriate timeseries from dataframe based on maturities
  TS2 <- X$LAST %>% dplyr::select(X$Maturity[2])
  FirstTS <- TS1 %>%  log(.)  %>% lapply(., diff) %>% unlist(.) #calculating log return for the two time series
  SecondTS <- TS2 %>% log(.)  %>% lapply(., diff) %>% unlist(.)
  
  Window <- X$CorrelationWindow
  Lag <- X$CorrelationLag
  Dates <- X$Date
  
  
  X$CasualityData$Values$Window <- list()
  X$CasualityData$Dates$Window <- list()
  
  for (i in 1:(length(Window))){
    X$CasualityData$Values$Window[[i]] <- list()
    X$CasualityData$Dates$Window[[i]] <- list()
    
    for (j in 1:(length(Lag))){
      Correlation <- staticCorrelation(FirstTS, SecondTS, Window[i], Lag[j])
      X$CasualityData$Values$Window[[i]]$Lag[[j]] <- Correlation
      
      X$CasualityData$Dates$Window[[i]]$Lag[[j]] <- Dates[(Window[i]+Lag[j]):length(Dates), 1]
    }
  }
  return(X)

}
