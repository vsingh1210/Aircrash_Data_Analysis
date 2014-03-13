#Clear Screen
cat("\014")
#Installing required packages
install.packages("doBy")
install.packages("ggplot2")
#Load required libraries
library(ggplot2)
library(doBy)
#
#Reading NYT data
#
nydata<-read.csv("nyt1.csv")
#
#Question 1
#
nydata$age_group<-cut(nydata$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
#
#Question 2 Part 1 Distribution of Impressions for Age groups
#
cat("\014")
#Distribution of CTR for Age groups
ggplot(subset(nydata,Impressions>0),aes(x=Clicks/Impressions)) + geom_density()
ggplot(subset(nydata, Impressions > 0), aes(x=Clicks/Impressions,fill=age_group)) + geom_histogram(binwidth=1)
ggplot(subset(nydata,Clicks>0),aes(x=Clicks/Impressions)) + geom_density()
ggplot(subset(nydata, Clicks > 0), aes(x=Clicks/Impressions,fill=age_group)) + geom_histogram(binwidth=1)
#
#Question 2 Part 2
#Create category based on Signed in or not
nydata$signCat<-cut(nydata$Signed_In,c(0,1))
#Plot Histogram - Signed In vs Not Signed In
ggplot(nydata,aes(x=Signed_In,fill=signCat))+geom_histogram(binwidth=1)
#Create category based on Gender
nydata$genderCat<-cut(nydata$Gender,c(0,1))
#Plot Histogram - <18, Male vs Female
ggplot(subset(nydata,Age<18),aes(x=Gender,fill=genderCat))+geom_histogram(binwidth=1)
#
#Question 2 Part 3
#
#CTR by Gender
ggplot(subset(nydata,Gender==0), aes(x=Clicks/Impressions,fill=age_group)) + geom_histogram(binwidth=1)
ggplot(subset(nydata,Gender==1), aes(x=Clicks/Impressions,fill=age_group)) + geom_histogram(binwidth=1)
#Impressions and Clicks
nydata$scode[nydata$Impressions==0] <- "NoImps"
nydata$scode[nydata$Impressions >0] <- "Imps"
nydata$scode[nydata$Clicks >0] <- "Clicks"
nydata$scode <- factor(nydata$scode)
clen <- function(x){c(length(x))}
etable<-summaryBy(Impressions~scode+Gender+agecat,data = nydata, FUN=clen)
#Plotting 9 days of data and merging it
nydata<-read.csv("nyt1.csv")
nydata2<-read.csv("nyt2.csv")
nydata3<-read.csv("nyt3.csv")
nydata4<-read.csv("nyt4.csv")
nydata5<-read.csv("nyt5.csv")
nydata6<-read.csv("nyt6.csv")
nydata7<-read.csv("nyt7.csv")
nydata8<-read.csv("nyt8.csv")
nydata9<-read.csv("nyt9.csv")
install.packages("gridExtra")
library(gridExtra)
plot1<-ggplot(subset(nydata,Impressions>0),aes(x=Age,colour=age_group)) + geom_density()
plot2<-ggplot(subset(nydata2,Impressions>0),aes(x=Age,colour=age_group)) + geom_density()
plot3<-ggplot(subset(nydata3,Impressions>0),aes(x=Age,colour=age_group)) + geom_density()
plot4<-ggplot(subset(nydata4,Impressions>0),aes(x=Age,colour=age_group)) + geom_density()
plot5<-ggplot(subset(nydata5,Impressions>0),aes(x=Age,colour=age_group)) + geom_density()
plot6<-ggplot(subset(nydata6,Impressions>0),aes(x=Age,colour=age_group)) + geom_density()
plot7<-ggplot(subset(nydata7,Impressions>0),aes(x=Age,colour=age_group)) + geom_density()
plot8<-ggplot(subset(nydata8,Impressions>0),aes(x=Age,colour=age_group)) + geom_density()
plot9<-ggplot(subset(nydata9,Impressions>0),aes(x=Age,colour=age_group)) + geom_density()
grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9)
