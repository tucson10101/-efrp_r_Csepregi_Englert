defineParametersdummy <- function(X){
  X$CorrelationWindow <- c(100,200,250)
  X$CorrelationLag <- c(1,5)
  X$Maturity <- c(3,10)
  return(X)
}
