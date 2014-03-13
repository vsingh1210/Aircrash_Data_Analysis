library(ggplot2)
adata<-read.csv("adata1.csv")
adata$InjSev <- as.numeric(gsub("[^[:digit:]]","",adata$InjurySeverity))
adata$EDate <- as.Date(adata$EventDate,"%m/%d/%Y")
library(doBy)
adataINJ<-subset(adata,adata$InjurySeverity > 0)
plot(adataINJ$EDate,adataINJ$InjurySeverity,"l",col="red")
title(main = "Fatality over Years")
adataINJ$injCat<-cut(adataINJ$InjurySeverity,breaks=10)
stats<-function(x){c(len=length(x),min=min(x),mean=mean(x),max=max(x))}
summaryBy(InjurySeverity~injCat, adataINJ,FUN=stats)
summaryBy(InjurySeverity~FlightPhase, adataINJ,FUN=stats)
ggplot(subset(adataINJ,InjurySeverity>10),aes(x=InjurySeverity,fill=injCat))+geom_histogram(binwidth=1)
countF<-function(x){sum=sum(x)}
dataC<-summaryBy(InjurySeverity~Country,adataINJ,FUN=countF)
#Entire World
testmap <- get_googlemap(c(lon=-18.0258994,lat=22.6984528) ,zoom=2)
ggmap(testmap)+geom_point(aes(x= adataINJ$Longitude, y=adataINJ$Latitude),data=adataINJ, colour="red", size=1)
#US Only
onlyUS<-subset(adataINJ,adataINJ$Country=="United States")
testmap <- get_googlemap(c(lon=-107.7779676,lat=40.7823644) ,zoom=3)
ggmap(testmap)+geom_point(aes(x= adataINJ$Longitude, y=adataINJ$Latitude),data=adataINJ, colour="red", size=1)

countAF<-function(x){count=length(x)}
dataAF<-summaryBy(InjurySeverity~FlightPhase,adataINJ,FUN=countAF)
dataMAKE<-summaryBy(InjurySeverity~Make,adataINJ,FUN=countAF)
dataAD<-summaryBy(InjurySeverity~AircraftDamage,adataINJ,FUN=countAF)
dataAC<-summaryBy(InjurySeverity~AirCarrier,adataINJ,FUN=countAF)
dataPF<-summaryBy(InjurySeverity~FlightPurpose,adataINJ,FUN=countAF)
ggplot(subset(adataINJ,InjurySeverity>10),aes(x=InjurySeverity,fill=injCat))+geom_density()
#Cluster Plot
x <- data.frame(adata$Total.Serious.Injuries + adata$Total.Minor.Injuries, adata$Total.Uninjured)
mat<-as.matrix(x)
kclus<-kmeans(mat,3)
plot(mat,col = kclus$cluster)
title("Injured VS Uninjured")
#Accident with no fatality = 59636
#Accidents with fatality = 14691
#Max Casuality = 349 , Mid Air Collision in India
#Country with maximum acidents: United States, Total Fatality: 26789
#Most Dangerous Flight phase: MANEUVERING
#Purpose of flight with maximum fatality: Personal


