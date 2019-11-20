library("readxl")
library("rstudioapi")
library("tidyr")
library(ggplot2)
library(dplyr)
library(tidyverse)
library(corrr)

#loading all functions from Beadando-functions-new
paste0(getwd(), '/Beadando-functions-new.r') %>% source(.)


##### DATA PREPARATION #############

#A WTI arfolyamokat tartalmazo excel file betoltese
wti <- loadData()

#A parameterek beolvasasa, megvaltoztathatoak a fuggvenyes halmazban
AnalysisInputs <- CrossCorrInput()

#A megadott feltetelekre a WTI alap adatbazis leszukitese
RawData <- PrepareDatabase(wti,AnalysisInputs[[2]],AnalysisInputs[[3]],AnalysisInputs[[1]])

#Az idosor egyszeri differenciazasa, hogy stacioner legyen. (az utolso sor torlesre kerul a RawData-bol)
usedata <- TSDiff(RawData)

#A dinamikus keresztkorrelacio kiszamolasa majd eltarolasa egy data.frame-ba, melynek az oszlopai (Date, (CL1,CL2), (CL1,CL3)...)
Results<-CrossCorrAnalysis(usedata,AnalysisInputs[[5]],AnalysisInputs[[4]])

#A Results data.frame datum oszlopa numerikus alakbol datum formatumba atirasa
Results <- Results %>%
  tidyr::as_tibble() %>%
  dplyr::mutate(Date=as.Date(Date,origin="1970-01-01"))



####### OPTIONS ##########

#Run this to see the correlation between two assets over time, eg.:grafikon$`(CL1,CL2) 
dinCorrelationPlot(Results,Results$`(CL1,CL3)`)


#Run this to see minimum, maximum, average over all assets as a function of time
minMaxMean <- minMaxMeanCalculator(Results)
minMaxMeanPlot(minMaxMean)


#Run this to see correlation network for given day
correlationNetworkPlot("2011-06-24")




