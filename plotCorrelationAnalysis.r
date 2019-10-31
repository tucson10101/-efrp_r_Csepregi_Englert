plotCorrelationAnalysis <- function(X){
  Window <- X$CorrelationWindow
  Lag <- X$CorrelationLag
  layout(matrix(c(1:(length(Window)*length(Lag))),length(Lag),length(Window),byrow = TRUE))
  
  for (i in 1:length(Window)){
    for (j in 1:length(Lag)){
      mytext <- paste0("Window=", Window, " Lag=", Lag)
      plot(X$CasualityData$Values$Window[[i]]$Lag[[j]], type = "line", col="red", 
           xlab = "Date", ylab = "Correlation", main = paste("Window=",i,"Lag=",j))
      
    }
  }

}


