library(data.table)
library(dplyr)
library(ggplot2)
library(plyr)

#download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
#                       destfile ="./stormdata.csv.bz2")

StormRawData<-fread("./stormdata.csv.bz2")

names(StormRawData)
str(StormRawData)

ColumnnameH<-c("EVTYPE","FATALITIES","INJURIES")
ColumnnameE<-c("EVTYPE","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")

StormHealthData<-select(StormRawData,ColumnnameH)
StormEconomyData<-select(StormRawData,ColumnnameE)

print(table(is.na(StormHealthData$FATALITIES)))
print(table(is.na(StormHealthData$INJURIES)))

StormFatal<-aggregate(FATALITIES~EVTYPE,data=StormHealthData,FUN=sum)
StormFatal<-arrange(StormFatal,desc(FATALITIES))
Top10StormFatal<-StormFatal[1:10,]

max(Top10StormFatal$FATALITIES)
graph1<-ggplot(data=Top10StormFatal,aes(x=reorder(EVTYPE,-FATALITIES),y=FATALITIES,fill=EVTYPE))
graph1<-graph1+geom_bar(stat="identity")
graph1<-graph1+theme(axis.text.x = element_text(angle =90))+xlab("Type of Event")
graph1<-graph1+ylab("Number of Fatal Cases")+ggtitle("Top 10 Fatalities Event")+theme(legend.position="none")
graph1<-graph1+ylim(c(0,6000))

StormInjure<-aggregate(INJURIES~EVTYPE,StormHealthData,FUN=sum)
StormInjure<-arrange(StormInjure,desc(INJURIES))
Top10StormInjure<-StormInjure[1:10,]

print(max(Top10StormInjure$INJURIES))
graph2<-ggplot(data = Top10StormInjure,aes(x=reorder(EVTYPE,-INJURIES),y=INJURIES,fill=EVTYPE))
graph2<-graph2+geom_bar(stat="identity")
graph2<-graph2+theme(axis.text.x= element_text(angle= 90))+xlab("Type of Event")
graph2<-graph2+ylab("NUmber of Injuries Cases")+ggtitle("Top 10 Injuries Event")
graph2<-graph2+ylim(c(0,100000))


print(unique(StormEconomyData$PROPDMGEXP))
print(unique(StormEconomyData$CROPDMGEXP))

tomap1<-unique(StormEconomyData$PROPDMGEXP)
tomap2<-unique(StormEconomyData$CROPDMGEXP)

map1<-c(10^3,10^6,1,10^9,10^6,1,1,10^5,10^6,1,10^4,10^2,10^3,10^2,10^7,10^2,1,10,10^8)
map2<-c(1,10^6,10^3,10^6,10^9,1,1,10^3,10^2)

StormEconomyData$PROPDMGEXP<-mapvalues(StormEconomyData$PROPDMGEXP,from = tomap1, to = map1)
StormEconomyData$CROPDMGEXP<-mapvalues(StormEconomyData$CROPDMGEXP,from = tomap2, to = map2)

str(StormEconomyData)

StormEconomyData$PROPDMGEXP<-as.numeric(as.character(StormEconomyData$PROPDMGEXP))
StormEconomyData$CROPDMGEXP<-as.numeric(as.character(StormEconomyData$CROPDMGEXP))

StormPropertyDamage<-aggregate(PROPDMG*PROPDMGEXP~EVTYPE,StormEconomyData,sum)
colnames(StormPropertyDamage)<-c("EVTYPE","TotalDamage")
StormPropertyDamage<-arrange(StormPropertyDamage,desc(TotalDamage))
StormPropertyDamage<-mutate(StormPropertyDamage,TotalInBillion= TotalDamage/10^9)
StormPropertyDamage<-StormPropertyDamage[1:10,]

graph3<-ggplot(StormPropertyDamage,aes(x=reorder(EVTYPE,-TotalInBillion),y=TotalInBillion,fill=EVTYPE))
graph3<-graph3+geom_bar(stat="identity")
graph3<-graph3+theme(axis.text.x = element_text(angle=90))+xlab("Type of Event")
graph3<-graph3+ylab("Property Damage Cost in Billion")+ggtitle("Top 10 Property Damage Event")
graph3<-graph3+theme(legend.position = "none")


StormCropDamage<-aggregate(CROPDMG*CROPDMGEXP~EVTYPE,StormEconomyData,sum)
colnames(StormCropDamage)<-c("EVTYPE","TotalDamage")
StormCropDamage<-arrange(StormCropDamage,desc(TotalDamage))
StormCropDamage<-mutate(StormPropertyDamage,TotalInBillion=TotalDamage/10^9)
StormCropDamage<-StormCropDamage[1:10,]

graph4<-ggplot(StormCropDamage,aes(x=reorder(EVTYPE,-TotalInBillion),y=TotalInBillion,fill=EVTYPE))
graph4<-graph4+geom_bar(stat="identity")
graph4<-graph4+theme(axis.text.x = element_text(angle = 90))+xlab("Type of Event")
graph4<-graph4+ylab("Crop Damage Cost in Billion")+ggtitle("Top 10 Crop Damage Event")
graph4<-graph4+theme(legend.position = "none")