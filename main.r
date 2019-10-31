library(dplyr) 
library(magrittr)
library(rio)


X <- list()
X <- loadData(X)
X <- defineParametersdummy(X)
X <- correlationAnalysis(X)
X <- plotCorrelationAnalysis(X)
