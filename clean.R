
clean_data <- raw_data

myfunction <-function()
{
  clean_data<-clean_data[-53,]
  clean_data<-clean_data[-1,]
  cnames <- colnames(clean_data)
  cnames[5] <-"stateName"
  cnames[6] <-"population"
  cnames[7] <-"popOver18"
  cnames[8] <-"percentOver18"
  colnames(clean_data) <-cnames
  clean_data<-clean_data[,-1:-4]
  return(clean_data)
}

str(clean_data)
