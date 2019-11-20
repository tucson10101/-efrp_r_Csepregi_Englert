loadData <- function(){
  wti<-readxl::read_excel(paste(dirname(getActiveDocumentContext()$path), "/WTI2.xlsx", sep=""))
  wti$Date = as.Date(wti$Date, format="%m/%d %M:%S")
  return(wti)
}

##############################
CrossCorrInput <- function(){
  #A vizsgalando oszlopok (CL2,CL2...stb megadasa. 
  #Legyen mindig megadva a kezdeti illetve vegso datuma a vizsgalando idoszaknak
  #Valamint az ablak es lag meretek megadasa
  AssetList <- c("Date","CL1", "CL2","CL3","CL4","CL5","CL6","CL7","CL8","CL9","CL10","CL11","CL12","CL13","CL14","CL15","CL16","CL17","CL18","CL19","CL20","CL21","CL22","CL23","CL24")
  StartDate <- "2011-04-29"
  EndDate <- "2012-05-04"
  WindowLength <- 130
  LagLength <- 30
  
  variables <- list(AssetList,StartDate,EndDate,WindowLength,LagLength)
  return(variables)
}

##############################

PrepareDatabase <- function(df,StartDate,EndDate,SelectColumns){
  x <- as.Date(StartDate)
  y <- as.Date(EndDate)
  RawData <- df[df$Date >= x & df$Date <= y,] %>%
    dplyr::as_tibble() %>%
    dplyr::select(SelectColumns)
  
  RawData<- subset(RawData,(weekdays(RawData$Date) %in% c('hétfõ','kedd', 'szerda', 'csütörtök','péntek' ))) 
  #ha angol a felhasznaloi felulet:
  #RawData<- subset(RawData,(weekdays(RawData$Date) %in% c('Monday','Tuesday', 'Wednesday', 'Thursday','Friday' ))) 
  
  return(RawData)}

##############################

TSDiff <- function(df){
  colnumbers <- ncol(df)
  rownumbers <- nrow(df)
  
  for (asset3 in 2:rownumbers-1){
    for (asset4 in 2:colnumbers){
      df[[asset3,asset4]]<-df[[asset3+1,asset4]]-df[[asset3,asset4]]
    }
  }
  
  #a differenciazas miatt az utolso sort torolhetjuk
  df<- df[-rownumbers,]
  return(df)
}

##############################

CrossCorrAnalysis <- function(RawData,lengthlag,lengthwindow){
  colnumbers <- ncol(RawData)
  rownumbers <- nrow(RawData)
  colnames_tray <- c()
  runwindow=0
  k=1
  
  eredmeny <- data.frame(1,nrow = rownumbers-lengthwindow-lengthlag,ncol = ((colnumbers-1)*(colnumbers-2))+1)
  newelem <- "Date"
  colnames_tray <- c(colnames_tray,newelem)
  
  for (asset1 in 2:colnumbers) {
    for (asset2 in 2:colnumbers){

        
        k=k+1
        
        #elnevezzÃ¼k az oszlopokat
        
        newelem <-paste0("(",colnames(RawData)[asset1],",",colnames(RawData)[asset2],")")
        colnames_tray <- c(colnames_tray,newelem)
        
        #majd feltÃ¶ltjÃ¼k a korrelÃ¡ciÃ³ eremÃ©nyeivel
        
        for (runwindow in 1:(rownumbers-lengthwindow-lengthlag)){
          
          eredmeny[runwindow,1]=RawData[runwindow,1]
          eredmeny[runwindow,k]=cor(RawData[runwindow:(lengthwindow+runwindow),asset1],RawData[(runwindow+lengthlag):(lengthwindow+runwindow+lengthlag),asset2])
        }
      
    }
  }
  #elneveztÃ¼k a ciklusban kapott nevekre az oszlopokat
  
  colnames(eredmeny)=colnames_tray    
  return(eredmeny)
}

##############################
dinCorrelationPlot <- function(Results, timeseries){
  #plot dynamic correlation between two assets over time
  
  mydate <- Results$Date
  timeLength <- length(mydate)
  numberOfPoints <- timeLength%/%20
  xAxisPoints <- seq(0, numberOfPoints*20, by = 20)
  
  plotTitle <- "Correlation"
  plot(timeseries, type = "line", col="red",
       xlab = "Date", ylab = "Correlation", xaxt = 'n', main = plotTitle)
  axis(1, at=xAxisPoints, labels=mydate[xAxisPoints + 1])
}
  

#############################

minMaxMeanCalculator <- function(df){
  #calculates minimum, maximum, average over all maturities for given day
  minMaxMean <- data.frame("Date" = df$Date)
  minMaxMean$Average <- df %>% select(c(2:length(df))) %>% apply(., 1, mean)
  minMaxMean$Min <- df %>% select(c(2:length(df))) %>% apply(., 1, min)
  minMaxMean$Max <- df %>% select(c(2:length(df))) %>% apply(., 1, max)
  
  return(minMaxMean)
}

#############################
minMaxMeanPlot <- function(minMaxMean){
  #plots minimum, maximum, average over all assets as a function of time
  
  mydate <- minMaxMean$Date
  myOptions <- list("Average", 'Minimum', 'Maximum')
  
  timeLength <- length(Results$Date)
  numberOfPoints <- timeLength%/%20
  xAxisPoints <- seq(0, numberOfPoints*20, by = 20)
  colours <-c("red","blue","black")
  plotTitle <- 'Correlation analysis for all maturities'
  for (i in 1:3){
   
    plot(minMaxMean[[i+1]], type = "line", col=colours[i],ylim=c(-0.4,0.1), legend=paste(myOptions[i], 'of all maturities'),
         xlab = "Date", ylab = "Correlation", xaxt = 'n', main = plotTitle)
    axis(1, at=xAxisPoints, labels=mydate[xAxisPoints + 1])
    par(new=TRUE)
  }
  legend("bottomleft",legend=c("Avg", "Min","Max"),
         col=c("red", "blue","black"), lty=1:1, cex=0.8,
         title="Line types", text.font=4, bg='lightblue')
  
}


#############################
correlationNetworkPlot <- function(myDate){
  #plots the correlation between any two assets for given 'day'
  
  myDate = as.Date(myDate)
  OneDay <- Results[Results$Date == myDate,]
  OneDay<- as.double(OneDay[-(1)])
  ResultMatrix <- matrix(OneDay,nrow = 24,ncol = 24)#matrixxa alakitas
  ResultMatrix <- as.data.frame(ResultMatrix) #COl es ROW nevek elnevezese
  network_plot(ResultMatrix,min_cor = 0.01, colours= c("indianred2","gray","skyblue1"))
  
  
  
}

