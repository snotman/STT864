# Testing dataset
setwd("C://Users//nan66//Google Drive//stt864//Project")
set.seed(52871775)
SubTestRlt<-TestResults
TestResults<-read.csv("Test_Results_Flint_513922_7.csv",head=T)

# modify the form of date
SubTestRlt[,2]<-as.Date(SubTestRlt[,2],format="%m/%d/%Y")
Startdate<-as.Date("2015-09-03",format="%Y-%m-%d")
DateDiff<-as.numeric(SubTestRlt[,2]-Startdate)
SubTestRlt<-data.frame(SubTestRlt,DateDiff)

# plot the original data
plot(SubTestRlt[,11],SubTestRlt[,4],xlab="Days",ylab="Pb")
plot(SubTestRlt[,11],SubTestRlt[,6],xlab="Days",ylab="Cu")

# remove samples which were not collected from flint
SubTestRlt<-SubTestRlt[SubTestRlt[,9]=="FLINT",]

# optional: remove the date before /after 120
SubTestRlt<-SubTestRlt[SubTestRlt[,11]>120,]
SubTestRlt<-SubTestRlt[SubTestRlt[,11]<=120,]

# remove the outlier of Pb level
SubTestRlt<-SubTestRlt[order(SubTestRlt[,4]),]
SubTestRlt<-SubTestRlt[1:(nrow(SubTestRlt)-2),]
SubTestRlt<-SubTestRlt[SubTestRlt[,4]<200,]

#  extract and transform variables
rTime<-SubTestRlt[,11]
rRespPb<-unlist(SubTestRlt[,4])
rRespCu<-unlist(SubTestRlt[,6])
plot(rTime,rRespPb)
plot(rTime,rRespCu)

#residregPb<-rRespPb-(coef(regPb)[1]+coef(regPb)[2]*(rTime^2))
plot(rTime,residregPb)
residnregPb<-rRespPb-coef(regPb)[1]*exp(-coef(regPb)[2]*rTime)
predict<-coef(regPb)[1]*exp(-coef(regPb)[2]*rTime)
plot(rTime,residnregPb)

