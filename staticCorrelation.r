staticCorrelation <- function(FirstTS, SecondTS, Window, Lag){
  Correlation <- c()
  for (t in 1:(length(FirstTS) - Window - Lag + 1)){
    FirstTSsubint <- FirstTS[t:(t+Window-1)]
    SecondTSsubint <- SecondTS[(t+Lag):(t+Lag+Window-1)]
    
    Correlation <- Correlation %>% append(., cor(FirstTSsubint,SecondTSsubint)) 
  }
  
  return(Correlation)
  
}
