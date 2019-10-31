loadData <- function(X){
  data <- import_list("WTI2.xlsx", setclass = "tbl")
  X<- as.ts(data)
  return(X)
}
