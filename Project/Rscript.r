# Data Reading
setwd("C://Users//nan66//Google Drive//stt864//Project")
set.seed(52871775)
# Round1
SentinelR1<-read.csv("Sentinel_Data_Set_1A-B_515890_7.csv",head=T)
# Round2
SentinelR2<-read.csv("Sentinel_Data_Set_1A-B_Rnd_2_517916_7.csv",head=T)
# Round3
SentinelR3<-read.csv("Sentinel_Data_Round_3_521415_7.csv",head=T)
# Round4
SentinelR4<-read.csv("Sentinel_Data_Round_4_521993_7.csv",head=T)
# Combination of all 4 rounds
SentinelBind<-rbind(SentinelR1,SentinelR2,SentinelR3,SentinelR4)

##############################
# Analysis with only 1 round #
##############################
# SubSentinel<-SentinelR1
# SubSentinel<-SentinelR2
# SubSentinel<-SentinelR3
# SubSentinel<-SentinelR4


##############################
# Analysis with all 4 rounds #
##############################
SubSentinel<-SentinelBind

# modify the form of date
SubSentinel[,1]<-as.Date(SubSentinel[,1],format="%m/%d/%Y")
Startdate<-as.Date("2016-02-16",format="%Y-%m-%d")
DateDiff<-as.numeric(SubSentinel[,1]-Startdate)
SubSentinel<-data.frame(SubSentinel,DateDiff)

# plot the original data
plot(SubSentinel[,13],SubSentinel[,3],xlab="Days",ylab="Pb")
######################################################################
### For analysis with 1 round #
###############################
# # remove date with few sample
# table(SubSentinel[,13])
# SelectedDate<-SubSentinel[,13]%in%c(0,8,9,10)
# SubSentinel<-SubSentinel[SelectedDate,]

# # remove the outlier of Pb level
# SubSentinel<-SubSentinel[order(SubSentinel[,3]),]
# SubSentinel<-SubSentinel[1:(nrow(SubSentinel)-2),]
# OutlierPbs0<-(SubSentinel[,3]>200)&(SubSentinel[,13]==0) # 200
# RemovedPb0<-as.logical(1-OutlierPbs0)
# SubSentinel<-SubSentinel[RemovedPb0,]
# OutlierPb8<-(SubSentinel[,3]>75)&(SubSentinel[,13]==8) # 75
# RemovedPb8<-as.logical(1-OutlierPb8)
# SubSentinel<-SubSentinel[RemovedPb8,]
# OutlierPb10<-(SubSentinel[,3]>15)&(SubSentinel[,13]==10) # 15
# RemovedPb10<-as.logical(1-OutlierPb10)
# SubSentinel<-SubSentinel[RemovedPb10,]

# # remove the outlier of Cu level
# SubSentinel<-SubSentinel[order(SubSentinel[,5]),]
# SubSentinel<-SubSentinel[1:(nrow(SubSentinel)-2),]
# OutlierCu8<-(SubSentinel[,5]>400)&(SubSentinel[,13]==8) # 400
# RemovedCu8<-as.logical(1-OutlierCu8)
# SubSentinel<-SubSentinel[RemovedCu8,]
# OutlierCu10<-(SubSentinel[,5]>300)&(SubSentinel[,13]==9) # 300
# RemovedCu10<-as.logical(1-OutlierCu10)
# SubSentinel<-SubSentinel[RemovedCu10,]
######################################################################

# remove the individual without zipcode
Zip<-c(48503,48504,48505,48506,48507,48532)
SubSentinel<-SubSentinel[SubSentinel[,8]%in%Zip,]

# slelect samples with specific pipe material	
Material<-c("Copper","Galvanized","Lead")
SubSentinel<-SubSentinel[SubSentinel[,10]%in%Material,]

# remove the individual without sample location
SamLocation<-SubSentinel[,11]%in%c("BATHROOM","KITCHEN")
SubSentinel<-SubSentinel[SamLocation,]

######################################################################
### For analysis with 4 rounds #
###############################

#remove outliers
SubSentinel<-SubSentinel[SubSentinel[,3]<100,] # 100

# Frequency of sitecode =4 codes for all 4 round
R1<-SubSentinel[SubSentinel[,12]==1,]
R2<-SubSentinel[SubSentinel[,12]==2,]
R3<-SubSentinel[SubSentinel[,12]==3,]
R4<-SubSentinel[SubSentinel[,12]==4,]
SubSentinel<-SubSentinel[SubSentinel[,9]%in%R1[,9],]
SubSentinel<-SubSentinel[SubSentinel[,9]%in%R2[,9],]
SubSentinel<-SubSentinel[SubSentinel[,9]%in%R3[,9],]
SubSentinel<-SubSentinel[SubSentinel[,9]%in%R4[,9],]
Freq<-table(SubSentinel[,9])
namelist<-names(Freq)[which(Freq==4)]
SubSentinel<-SubSentinel[SubSentinel[,9]%in%namelist,]
######################################################################
dim(SubSentinel)


# extract and transform variables
Time<-SubSentinel[,13]
ZipFAC<-as.factor(SubSentinel[,8])
MaterialFAC<-as.factor(SubSentinel[,10])
LocationFAC<-as.factor(SubSentinel[,11])
DumCu<-(SubSentinel[,10]=="Copper")+0
DumGa<-(SubSentinel[,10]=="Galvanized")+0
DumPb<-(SubSentinel[,10]=="Lead")+0
RespPb<-unlist(SubSentinel[,3])
RespCu<-unlist(SubSentinel[,5])

# plot the plots for dataset sentinelR1
plot(Time,RespPb,xlab="Days",ylab="Pb")
plot(Time,RespCu,xlab="Days",ylab="Cu")

# fit the linear model between Pb and Time for least p-value
for (i in 1:10){
	iTime<-Time^i
	iReg<-lm(RespPb~iTime)
	print(coef(summary(iReg))[2,4])
}
regPb<-lm(RespPb~Time)
summary(regPb)
plot(residuals(regPb))
qqnorm((residuals(regPb)))

# fit the nls decay model between Resp and Time for least p-value
nregPb<-nls(RespPb~b1*exp(-b2*Time),start=list(b1=1,b2=-0.01))
summary(nregPb)
plot(residuals(nregPb))
qqnorm(residuals(nregPb))
# Fit glm model for time material
greg1<-glm(RespPb~MaterialFAC+LocationFAC,family="gaussian")
summary(greg1)
plot(residuals(greg1))
qqnorm(residuals(greg1))
BinaryPb<-(RespPb<15)+0
logreg1<-glm(BinaryPb~Time+MaterialFAC+ZipFAC+LocationFAC+Time,family="binomial")
summary(logreg1)
plot(residuals(logreg1))
qqnorm(residuals(logreg1))
# fitting linear mixed model
library("nlme")
library("lme4")
lmmd<-lmer(RespPb~MaterialFAC+LocationFAC+(1|ZipFAC))
summary(lmmd)
plot(residuals(lmmd))
qqnorm(residuals(lmmd))




# modify the form of date
SubSentinel[,1]<-as.Date(SubSentinel[,1],format="%m/%d/%Y")
Startdate<-as.Date("2016-02-16",format="%Y-%m-%d")
DateDiff<-as.numeric(SubSentinel[,1]-Startdate)
SubSentinel<-data.frame(SubSentinel,DateDiff)

# plot the original data
plot(SubSentinel[,13],SubSentinel[,3],xlab="Days",ylab="Pb")
plot(SubSentinel[,13],SubSentinel[,3],xlab="Days",ylab="Cu")

# remove date with few sample
table(SubSentinel[,13])
SelectedDate<-SubSentinel[,13]%in%c(0,8,9,10)
SubSentinel<-SubSentinel[SelectedDate,]

# remove the outlier of Pb level
SubSentinel<-SubSentinel[order(SubSentinel[,3]),]
SubSentinel<-SubSentinel[1:(nrow(SubSentinel)-2),]
OutlierPbs0<-(SubSentinel[,3]>200)&(SubSentinel[,13]==0) # 200
RemovedPb0<-as.logical(1-OutlierPbs0)
SubSentinel<-SubSentinel[RemovedPb0,]
OutlierPb8<-(SubSentinel[,3]>75)&(SubSentinel[,13]==8) # 75
RemovedPb8<-as.logical(1-OutlierPb8)
SubSentinel<-SubSentinel[RemovedPb8,]
OutlierPb10<-(SubSentinel[,3]>15)&(SubSentinel[,13]==10) # 15
RemovedPb10<-as.logical(1-OutlierPb10)
SubSentinel<-SubSentinel[RemovedPb10,]

# remove the outlier of Cu level
SubSentinel<-SubSentinel[order(SubSentinel[,5]),]
SubSentinel<-SubSentinel[1:(nrow(SubSentinel)-2),]
OutlierCu8<-(SubSentinel[,5]>400)&(SubSentinel[,13]==8) # 400
RemovedCu8<-as.logical(1-OutlierCu8)
SubSentinel<-SubSentinel[RemovedCu8,]
OutlierCu10<-(SubSentinel[,5]>300)&(SubSentinel[,13]==9) # 300
RemovedCu10<-as.logical(1-OutlierCu10)
SubSentinel<-SubSentinel[RemovedCu10,]

# remove the individual without zipcode
Zip<-c(48503,48504,48505,48506,48507,48532)
SubSentinel<-SubSentinel[SubSentinel[,8]%in%Zip,]

# remove the individual without sample location
SamLocation<-SubSentinel[,11]%in%c("BATHROOM","KITCHEN")
SubSentinel<-SubSentinel[SamLocation,]

# extract and transform variables
Time<-SubSentinel[,13]
ZipFAC<-as.factor(SubSentinel[,8])
MaterialFAC<-as.factor(SubSentinel[,10])
LocationFAC<-as.factor(SubSentinel[,11])
DumCu<-(SubSentinel[,10]=="Copper")+0
DumGa<-(SubSentinel[,10]=="Galvanized")+0
DumPb<-(SubSentinel[,10]=="Lead")+0
RespPb<-unlist(SubSentinel[,3])
RespCu<-unlist(SubSentinel[,5])

# plot the plots for dataset sentinelR1
plot(Time,RespPb,xlab="Days",ylab="Pb")
plot(Time,RespCu,xlab="Days",ylab="Cu")

# fit the linear model between Pb and Time for least p-value
for (i in 1:10){
	iTime<-Time^i
	iReg<-lm(RespPb~iTime)
	print(coef(summary(iReg))[2,4])
}
regPb<-lm(RespPb~Time)
summary(regPb)
plot(residuals(regPb))
qqnorm((residuals(regPb)))

# fit the nls decay model between Resp and Time for least p-value
nregPb<-nls(RespPb~b1*exp(-b2*Time),start=list(b1=1,b2=-0.01))
summary(nregPb)
plot(residuals(nregPb))
qqnorm(residuals(nregPb))
# Fit glm model for time material
greg1<-glm(RespPb~MaterialFAC+LocationFAC,family="gaussian")
summary(greg1)
plot(residuals(greg1))
qqnorm(residuals(greg1))
BinaryPb<-(RespPb<15)+0
logreg1<-glm(BinaryPb~Time+MaterialFAC+ZipFAC+LocationFAC+Time,family="binomial")
summary(logreg1)
plot(residuals(logreg1))
qqnorm(residuals(logreg1))
# fitting linear mixed model
library("nlme")
library("lme4")
lmmd<-lmer(RespPb~MaterialFAC+LocationFAC+(1|ZipFAC))
summary(lmmd)
anova(lmmd)
plot(residuals(lmmd))
qqnorm(residuals(lmmd))