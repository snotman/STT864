setwd("C://Users//nan66//Documents//MSU//stt864//LAB2")
flintlead<-read.csv(file="Flint-water-lead-dataset.csv",header=FALSE)
colnames(flintlead)=c("SampleID","Zip Code","Ward", "0sec", "5sec", "120sec")
time<-c(0, 45, 120)
flintlead2<-flintlead[flintlead[,5]<1000,]



f<-function(a,b,c,i){zipcode<-which(flintlead2[,2]==i)
subsetflintlead<-flintlead2[zipcode,]
responses1<-unlist(subsetflintlead[,4:6])
sampletime1<-rep(time,each=dim(subsetflintlead)[1])
nlsreg2<-nls(responses1~theta1/(1+theta2*(exp(sampletime1*theta3))),
             start=list(theta1=a,theta2=b,theta3=c))
return(nlsreg2)
}

d<-function(i){zipcode<-which(flintlead2[,2]==i)
subsetflintlead<-flintlead2[zipcode,]
responses1<-unlist(subsetflintlead[,4:6])
sampletime1<-rep(time,each=dim(subsetflintlead)[1])
matplot(sampletime1,responses1,pch=18)
}


f<-function(a,b,c){
	zipcode<-which(flintlead2[,2]==48504)
	subsetflintlead<-flintlead2[zipcode,]
	responses1<-unlist(subsetflintlead[,4:6])
	sampletime1<-rep(time,each=dim(subsetflintlead)[1])
	nlsreg2<-nls(responses1~theta1/(1+theta2*(exp(sampletime1*theta3))),
             start=list(theta1=a,theta2=b,theta3=c))
return(nlsreg2)
}


##48503: 2.68,-0.75,-0.008
##48504?????????
##48505:1.44, -0.779, -0.0074
##48506: 2.19 , -0.81 -0.024
##48507 : 4.343 -0.61,-0.015