setwd("C://Users//nan66//Documents//MSU//stt864//LAB2")
flintlead<-read.csv(file="Flint-water-lead-dataset.csv",header=FALSE)
colnames(flintlead)=c("SampleID","Zip Code","Ward", "0sec", "5sec", "120sec")
time<-c(0, 45, 120)
flintlead2<-flintlead[flintlead[,5]<1000,]
flintlead2<-flintlead2[flintlead2[,4]<100,]
flintlead2<-flintlead2[flintlead2[,5]<100,]
flintlead2<-flintlead2[flintlead2[,6]<100,]



f48504<-function(a,b,c){
	zipcode48504<-which(flintlead2[,2]==48504)
	subsetflintlead48504<-flintlead2[zipcode48504,]
	responses48504<-unlist(subsetflintlead48504[,4:6])
	sampletime48504<-rep(time,each=dim(subsetflintlead48504)[1])
	plot(sampletime48504,responses48504)
	nlsreg2<-nls(responses48504~theta1/(1+theta2*(exp(sampletime48504*theta3))),
             start=list(theta1=a,theta2=b,theta3=c))
return(nlsreg2)
}