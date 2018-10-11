
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

#first eliminate the "District of Columbia" in cleanCensus to make match with 
# states name in arrests
cleanCensus <- cleanCensus[-9,]
cleanCensus$stateName
arrests$stateName <-rownames(arrests[])
arrests

# and then, merge the two dataframe
MergedDf<- merge(cleanCensus, arrests, by ="stateName")
MergedDf


# Step B: Explore the Data – Understanding distributions
#4)	Create a histogram using GGPLOT for the population and a different histogram for the murder rate
# Hint: Don’t forget to install and library the ggplot2 package.
# Ensure each line of code is explained (comments) in terms of what it is doing. 
# Then build similar code to create histograms of each of the other three variables in the merged data frame.
# What parameter will you have to adjust to make the other histograms look right?
library("ggplot2")

gPopulation <- ggplot(MergedDf, aes(x=population)) + geom_histogram(bins = 30,color="white", fill= "blue") +ggtitle("State Population Histogram")
gPopulation
gMurderate <- ggplot(MergedDf, aes(x=Murder)) + geom_histogram(bins = 30,color="white", fill= "blue") +ggtitle("State Murder Rate Histogram")
gMurderate

# Then build similar code to create histograms of each of the other three variables in the merged data frame.
# What parameter will you have to adjust to make the other histograms look right?
# In my opinion, other three variables in the merged data frame also look right.
gAssualt <- ggplot(MergedDf, aes(x=Assault)) + geom_histogram(bins = 30, color="white", fill= "blue") +ggtitle("State Assualt Rate Histogram")
gAssualt
gRape <- ggplot(MergedDf, aes(x=Rape)) + geom_histogram(bins = 30, color="white", fill= "blue")+ggtitle("State Rape Rate Histogram")
gRape
gUrbanPop <- ggplot(MergedDf, aes(x=UrbanPop)) + geom_histogram(bins = 30,color="white", fill= "blue")+ggtitle("State UrbanPop Rate Histogram")
gUrbanPop

# 5) Create a boxplot for the population, and a different boxplot for the murder rate.
bPopulation <- ggplot(MergedDf, aes(x=factor(0), y= population)) + geom_boxplot()
bPopulation <- bPopulation + ylab("Number of Population") +xlab("Population") +ggtitle("State Population Boxplot")
bPopulation

bMurderate <- ggplot(MergedDf, aes(x=factor(0), y=Murder)) + geom_boxplot()
bMurderate <-bMurderate + ylab("Rate of Murder") + xlab("Murder") +ggtitle("State Population Boxplot")
bMurderate

# 6) Create a block comment explaining which visualization (boxplot or histogram) you thought was more helpful (explain why)
# In my opinion, histogram is more helpful to understand the distribution of the data. 
# It is way easier to identify general distribution trends compare to boxplot.
# But boxplot also graphically represents the five most important descriptive values for a data set. 
# The values include the minimum value,the first quartile, the median, the third quartile, and the maximum value


# Step C: Which State had the Most Murders – bar charts
# 7) Calculate the number of murders per state
MergedDf$numMurSta <-round(MergedDf$population*MergedDf$Murder/100000)
MergedDf$numMurSta
# 8) Generate a bar chart, with the number of murders per state
# Hint: use the geom_col function
BarMurNum<-ggplot(MergedDf,aes(x=reorder(stateName, -numMurSta),y=numMurSta, fill=Murder))
BarMurNum<-BarMurNum+geom_col()+ggtitle("State Murder Number Bar Chart")+
  xlab("State")+ylab("Number of Murder")+
  theme(plot.title = element_text(hjust = 0.5,colour = "blue", size = 13))
BarMurNum

# 9) Generate a bar chart, with the number of murders per state. 
# Rotate text (on the X axis), so we can see x labels, also add a title named “Total Murders”.
BarMurNum1<-ggplot(MergedDf,aes(x=reorder(stateName, -numMurSta),y=numMurSta, fill=Murder))
BarMurNum1<-BarMurNum1+geom_col()+ggtitle("Total Murders")
BarMurNum1<-BarMurNum1+theme(axis.text.x = element_text(angle = 90,hjust = 1))+
  xlab("States")+ylab("Number of Murder")+
  theme(plot.title = element_text(hjust = 0.5,colour = "blue", size = 13))
BarMurNum1
# 10) Generate a new bar chart, the same as in the previous step, 
# but also sort the x-axis by the murder rate
BarMurNum3<-ggplot(MergedDf,aes(x=reorder(stateName, Murder),y=numMurSta, fill=Murder))
BarMurNum3<-BarMurNum3+geom_col()+ggtitle("State Murder Number Bar Chart")
BarMurNum3<-BarMurNum3+theme(axis.text.x = element_text(angle = 90,hjust = 1))+
  xlab("States")+ylab("Number of Murder")+
  theme(plot.title = element_text(hjust = 0.5,colour = "blue", size = 13))
BarMurNum3

# 11) Generate a third bar chart, the same as the previous step,
# but also showing percentOver18 as the color of the bar

BarMurNum4<-ggplot(MergedDf,aes(x=reorder(stateName, -numMurSta),y=numMurSta, fill=percentOver18))
BarMurNum4<-BarMurNum4+geom_col()+ggtitle("State Murder Number Bar Chart")
BarMurNum4<-BarMurNum4+theme(axis.text.x = element_text(angle = 90,hjust = 1))+
  xlab("States")+ylab("Number of Murder")+
  theme(plot.title = element_text(hjust = 0.5,colour = "blue",size = 13))
BarMurNum4

# Step D: Explore Murders – scatter chart
# 12)	Generate a scatter plot – have population on the X axis, 
# the percent over 18 on the y axis, and the size & color represent the murder rate
ScatPop<- ggplot(MergedDf, aes(x=population, y = percentOver18))
ScatPop<- ScatPop + geom_point(aes(size = MergedDf$Murder, color = MergedDf$Murder))
ScatPop<-ScatPop+ggtitle("Scatter plot for population, percent over 18 and murder rate")+
  theme(plot.title = element_text(size = 13, color = "blue", hjust =0.5))+
  labs(x="Number of Population", y="PercentOver 18")
ScatPop





