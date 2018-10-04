
clean_data <- raw_data

# 1) Read in the census dataset (using the function created in HW 3)

myfunction <-function(clean_data)
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

cleanCensus<- myfunction(clean_data)
str(cleanCensus)

# 2) Copy the USArrests dataset into a local variable (similar to HW 2)
USArrests 
arrests <- data.frame(USArrests)

# 3) Create a merged dataframe -- with the attributes from both dataframes
# Hint: get the state names from the USArests dataframe with the rownames 
# Hint: use the merge command 

cleanCensus <- cleanCensus[-9,]
cleanCensus$stateName

arrests$stateName <-rownames(arrests[])
arrests

MergedDf<- merge(cleanCensus, arrests, by ="stateName")
MergedDf







