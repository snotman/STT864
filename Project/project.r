# Sentinel
# Preparation
setwd("C://Users//nan66//Google Drive//stt864//Project")
set.seed(52871775)
# Sentinel<-read.csv("Sentinel_Data_Set_1A-B_515890_7.csv",head=T)
Sentinel<-read.csv("Sentinel_Data_combination.csv",head=T)

# modify the form of date
SubSentinel<-Sentinel
SubSentinel[,2]<-as.Date(SubSentinel[,2],format="%m/%d/%Y")
Startdate<-as.Date("2016-02-16",format="%Y-%m-%d")
DateDiff<-as.numeric(SubSentinel[,2]-Startdate)
SubSentinel<-data.frame(SubSentinel,DateDiff)

# plot the original data
plot(SubSentinel[,13],SubSentinel[,4],xlab="Days",ylab="Pb",main="Original data:Pb against Days")
plot(SubSentinel[,13],SubSentinel[,6],xlab="Days",ylab="Cu")






# slelect samples with specific pipe material	
Material<-c("Copper","Galvanized","Lead")
SubSentinel<-SubSentinel[SubSentinel[,11]%in%Material,]

## remove date with few samples
# table(SubSentinel[,13])
# SelectedDate<-SubSentinel[,13]%in%c(0,8,9,10)
# SubSentinel<-SubSentinel[SelectedDate,]

# # remove the outlier of Pb level
# SubSentinel<-SubSentinel[order(SubSentinel[,4]),]
# SubSentinel<-SubSentinel[1:(nrow(SubSentinel)-2),]
# OutlierPbs0<-(SubSentinel[,4]>200)&(SubSentinel[,13]==0) # 200
# RemovedPb0<-as.logical(1-OutlierPbs0)
# SubSentinel<-SubSentinel[RemovedPb0,]
# OutlierPb8<-(SubSentinel[,4]>75)&(SubSentinel[,13]==8) # 75
# RemovedPb8<-as.logical(1-OutlierPb8)
# SubSentinel<-SubSentinel[RemovedPb8,]
# OutlierPb10<-(SubSentinel[,4]>15)&(SubSentinel[,13]==10) # 75
# RemovedPb10<-as.logical(1-OutlierPb10)
# SubSentinel<-SubSentinel[RemovedPb10,]

# # remove the outlier of Cu level
# SubSentinel<-SubSentinel[order(SubSentinel[,6]),]
# SubSentinel<-SubSentinel[1:(nrow(SubSentinel)-2),]
# OutlierCu8<-(SubSentinel[,6]>400)&(SubSentinel[,13]==8) # 400
# RemovedCu8<-as.logical(1-OutlierCu8)
# SubSentinel<-SubSentinel[RemovedCu8,]
# OutlierCu10<-(SubSentinel[,6]>300)&(SubSentinel[,13]==9) # 300
# RemovedCu10<-as.logical(1-OutlierCu10)
# SubSentinel<-SubSentinel[RemovedCu10,]


SubSentinel<-SubSentinel[SubSentinel[,4]<100,] # 100

# remove the individual without zipcode
Zip<-c(48503,48504,48505,48506,48507,48532)
SubSentinel<-SubSentinel[SubSentinel[,9]%in%Zip,]

# remove the individual without sample location
SamLocation<-SubSentinel[,12]%in%c("BATHROOM","KITCHEN")
SubSentinel<-SubSentinel[SamLocation,]

# Frequency of sitecode =4 codes for all 4 round
R1<-SubSentinel[SubSentinel[,1]==1,]
R2<-SubSentinel[SubSentinel[,1]==2,]
R3<-SubSentinel[SubSentinel[,1]==3,]
R4<-SubSentinel[SubSentinel[,1]==4,]
SubSentinel<-SubSentinel[SubSentinel[,10]%in%R1[,10],]
SubSentinel<-SubSentinel[SubSentinel[,10]%in%R2[,10],]
SubSentinel<-SubSentinel[SubSentinel[,10]%in%R3[,10],]
SubSentinel<-SubSentinel[SubSentinel[,10]%in%R4[,10],]
Freq<-table(SubSentinel[,10])
namelist<-names(Freq)[which(Freq==4)]
SubSentinel<-SubSentinel[SubSentinel[,10]%in%namelist,]

# extract and transform variables
Time<-SubSentinel[,13]
ZipFAC<-as.factor(SubSentinel[,9])
MaterialFAC<-as.factor(SubSentinel[,11])
LocationFAC<-as.factor(SubSentinel[,12])
DumCu<-(SubSentinel[,11]=="Copper")+0
DumGa<-(SubSentinel[,11]=="Galvanized")+0
DumPb<-(SubSentinel[,11]=="Lead")+0
RespPb<-unlist(SubSentinel[,4])
RespCu<-unlist(SubSentinel[,6])

# plot the plots for dataset sentinel
plot(Time,RespPb,xlab="Days",ylab="Pb",main="Data after removing the outliers and sample with missing values")
plot(Time,RespCu,xlab="Days",ylab="Cu")

# fit the linear model between Pb and Time for least p-value
for (i in 1:10){
	iTime<-Time^i
	iReg<-lm(RespPb~iTime)
	print(coef(summary(iReg))[2,4])
}
regPb<-lm(RespPb~Time)
summary(regPb)
regCu<-lm(RespCu~Time)
summary(regCu)

# fit the nls model between Cu and Time for least p-value
nregPb<-nls(RespPb~b1*exp(-b2*Time),start=list(b1=1,b2=-0.01))
summary(nregPb)
nregCu<-nls(RespCu~b1*exp(-b2*Time),start=list(b1=1,b2=-0.01))
summary(nregCu)
# Fit glm model for time material
greg<-glm(RespPb~Time+MaterialFAC+ZipFAC+LocationFAC+Time,family="binomial")
summary(greg)

R1f<-SubSentinel[SubSentinel[,1]==1,4]
R2f<-SubSentinel[SubSentinel[,1]==2,4]
R3f<-SubSentinel[SubSentinel[,1]==3,4]
R4f<-SubSentinel[SubSentinel[,1]==4,4]
Pblevel<-data.frame(R1f,R2f,R3f,R4f)
RoundTime<-c(0,30,60,90)
matplot(RoundTime,t(Pblevel),pch=18,type="o",main="Pb level changes over 4 rounds")

SamTime<-rep(RoundTime,dim(R1f))
RoundRespPb<-unlist(Pblevel)


BinaryPb<-(RespPb<15)+0
greg1<-(BinaryPb~MaterialFAC+LocationFAC+ZipFAC,family="binomial")
greg1<-glm(RespPb~MaterialFAC+LocationFAC,family="gaussian")
summary(greg1)
plot(residuals(greg1))
qqnorm(residuals(greg1))
abline(0,1)
for (i in 1 : 6){
	sampleZip<-SubSentinel[SubSentinel[,9]==Zip[i],]
	samplePb<-sampleZip[,4]
	matfac<-as.factor(sampleZip[,11])
	locfac<-as.factor(sampleZip[,12])
	reg<-glm(samplePb~matfac+locfac,family="gaussian")
	print(Zip[i])
	print(summary(reg))
}

# fitting linear mixed model
library("nlme")
library("lme4")


lmmd<-lmer(RespPb~MaterialFAC+LocationFAC+(1|ZipFAC))
summary(lmmd)