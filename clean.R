
clean_data <- raw_data
myfunction <-function()
{
  dfStates<-dfStates[-53,]
  dfStates<-dfStates[-1,]
  cnames <- colnames(dfStates)
  cnames[5] <-"stateName"
  cnames[6] <-"population"
  cnames[7] <-"popOver18"
  cnames[8] <-"percentOver18"
  colnames(dfStates) <-cnames
  dfStates<-dfStates[,-1:-4]
  return(dfStates)
}
