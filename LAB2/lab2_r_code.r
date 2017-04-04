###library computer
setwd("E://")
# setwd("C://Users//caonan//Desktop//LAB2")
setwd("C://Users//nan66//Google Drive//stt864//LAB2")
###basic
set.seed(52871775)

flintlead<-read.csv(file="Flint-water-lead-dataset.csv",header=FALSE)
time<-c(0, 45, 120)
flintlead2<-flintlead[(flintlead[,4]<100)&(flintlead[,5]<100)&(flintlead[,6]<100),]
flintlead<-read.csv(file="Flint-water-lead-dataset.csv",header=FALSE)

##Q(a)

colnames(flintlead)=c("SampleID","Zip Code","Ward", "0sec", "5sec", "120sec")
table(flintlead[2])

##Q1(b)
hist(flintlead[,4])
hist(flintlead[,5])
hist(flintlead[,6])

##Q1(c)
time<-c(0, 45, 120)
matplot(time, t(flintlead[,4:6]),pch=18,type="o")

##remove
# flintlead2<-flintlead[flintlead[,5]<1000,]
flintlead2<-flintlead[(flintlead[,4]<100)&(flintlead[,5]<100)&(flintlead[,6]<100),]
###Test of Extreme Value
# Temp_FlintL<-flintlead
# FlintL_Lar100<-Temp_FlintL[(Temp_FlintL[,4]>100)|(Temp_FlintL[,5]>100)|(Temp_FlintL[,6]>100),]
# FlintL_Incre<-Temp_FlintL[(Temp_FlintL[,4]<Temp_FlintL[,5])|(Temp_FlintL[,5]<Temp_FlintL[,6])|(Temp_FlintL[,4]<Temp_FlintL[,6]),]

##Q1(d)
for(i in 48503:48507){
	zipcode<-which(flintlead2[,2]==i)
	matplot(time,t(flintlead2[zipcode,4:6]),type="o",ylab="levels",pch=18,
	          add= (i!=48503),ylim=c(0,ceiling(max(flintlead2[,4:6]))))
}

# for(i in 48503:48507){
# 	flintlead3<-flintlead2[flintlead2[,2]==i,4:6]
# 	matplot(time,t(flintlead3),pch=18,type="o",ylab="levels",add= (i!=48503),ylim=c(0,ceiling(max(flintlead2[,4:6]))))
# }

##Q2-commonly used exponential decay model
Coef_Matr1<-matrix(0,5,2)
Coef_Matr2<-matrix(0,5,3)
Sigma_Matr<-matrix(0,5,2)
for(i in 48503:48507){
zipcode<-which(flintlead2[,2]==i)
subsetflintlead<-flintlead2[zipcode,]
responses1<-unlist(subsetflintlead[,4:6])
sampletime1<-rep(time,each=dim(subsetflintlead)[1])
plot(sampletime1, responses1)
nlsreg1<-nls(responses1~theta1*exp(-sampletime1*theta2),
             start=list(theta1=5,theta2=0.02))
nlsreg2<-nls(responses1~theta1/(1+theta2*(exp(sampletime1*theta3))),
             start=list(theta1=2,theta2=-0.7,theta3=-0.025))
Coef_Matr1[i-48502,]<-coef(nlsreg1)
Coef_Matr2[i-48502,]<-coef(nlsreg2)
Sigma_Matr[i-48502,]<-c(summary(nlsreg1)$sigma,summary(nlsreg2)$sigma)
print(summary(nlsreg1))
print(summary(nlsreg2))
}

# f48504<-function(a,b,c){
# 	zipcode48504<-which(flintlead2[,2]==48504)
# 	subsetflintlead48504<-flintlead2[zipcode48504,]
# 	responses48504<-unlist(subsetflintlead48504[,4:6])
# 	sampletime48504<-rep(time,each=dim(subsetflintlead48504)[1])
# 	plot(sampletime48504,responses48504)
# 	nlsreg2<-nls(responses48504~theta1/(1+theta2*(exp(sampletime48504*theta3))),
#              start=list(theta1=a,theta2=b,theta3=c))
# return(nlsreg2)
# }




###Q3
var(flintlead2[,4])
var(flintlead2[,6])
var.test(flintlead2[,4],flintlead2[,6])
t.test(flintlead2[,4],flintlead2[,6],var.equal=FALSE,paired=F)

##Q4
# Contam_48503<-flintlead2[flintlead2[,2]==48503,4]
# Contam_48504<-flintlead2[flintlead2[,2]==48504,4]
# Contam_48505<-flintlead2[flintlead2[,2]==48505,4]
# Contam_48506<-flintlead2[flintlead2[,2]==48506,4]
# Contam_48507<-flintlead2[flintlead2[,2]==48507,4]
# Contam<-c(Contam_48503,Contam_48504,Contam_48505,Contam_48506,Contam_48507)
# L<-c(0,0,0,0,0)
# for(i in 48503:48507){
#   L[i-48502]<-length(which(flintlead2[,2]==i))
# }
# groups = factor(rep(48503:48507,L))
# bartlett.test(Contam, groups)
# qchisq(0.95,4)
# Aov_Test<-aov(Contam~groups)
# Aov_Test
# TH<-TukeyHSD(Aov_Test)
# TH
# plot(TH)



#logY=log(theta1)+theta21*X*I(X in ZIP1)+theta22*X*(X in ZIP2)
Ftest_Mat2<-matrix(0,nrow=10,ncol=5)
i<-1
for(i1 in 48503:48506){
	for (i2 in (i1+1):48507){
		zipcode<-which(flintlead2[,2]==i1|flintlead2[,2]==i2)
		subsetflintlead<-flintlead2[zipcode,]
		subsetflintlead[,7]<-(subsetflintlead[,2]==i1)+0
		ini1<-rep(subsetflintlead[,7],each=3)
		responses1<-unlist(subsetflintlead[,4:6])
		Log_res<-log(responses1)
		sampletime1<-rep(time,each=dim(subsetflintlead)[1])*ini1
		sampletime2<-rep(time,each=dim(subsetflintlead)[1])*(1-ini1)
		reg2<-lm(Log_res~ini1+sampletime1+sampletime2)
		f_stat<-summary(reg2)$fstatistic[1]
		df1<-summary(reg2)$fstatistic[2]
		df2<-summary(reg2)$fstatistic[3]
		Ftest_Mat2[i,1]<-i1
		Ftest_Mat2[i,2]<-i2
		Ftest_Mat2[i,3]<-f_stat
		Ftest_Mat2[i,4]<-qf(0.95,df1,df2)
		Ftest_Mat2[i,5]<-(f_stat<=qf(0.95,df1,df2))
		i<-i+1
	}
}





##Q5
for(i in 48503:48507){
	time0<-0
	theta1hat<-Coef_Matr1[i-48502,1]
	theta2hat<-Coef_Matr1[i-48502,2]
	meany<-theta1hat*exp(-time0*theta2hat)
	sigmahat<-Sigma_Matr[i-48502,1]
	y90quantile<-qnorm(0.9, meany, sigmahat)
	print(i)
	print(y90quantile)
}
##Q6
library(MASS)
f<-function(theta1,theta2,x){
	return(theta1*exp(-theta2*x))
}
Db_hat<-function(theta1,theta2,x){
  D<-cbind(exp(-theta2*x),-x*theta1*exp(-theta2*x))
  return(D)
}
G_hat<-function(theta1,theta2,x){
  G<-matrix(c(exp(-theta2*x),-x*theta1*exp(-theta2*x)),nrow=1,ncol=2)
  return(G)
}
CI<-function(para1,para2,sig,X,x0,alpha){
  D<-Db_hat(para1,para2,X)
  G<-G_hat(para1,para2,x0)
  delta<-qnorm(1-0.5*alpha)*sig*sqrt(G%*%(ginv(t(D)%*%D))%*%t(G))
  up<-f(para1,para2,x0)+delta
  low<-f(para1,para2,x0)-delta
  Interval<-c(low,up)
  return(Interval)
}
for (i in 48503:48507){
	theta1hat<-Coef_Matr1[i-48502,1]
	theta2hat<-Coef_Matr1[i-48502,2]
	sigmahat<-Sigma_Matr[i-48502,1]
	zipcode<-which(flintlead2[,2]==i)
	subsetflintlead<-flintlead2[zipcode,]
	sampletime1<-rep(time,each=dim(subsetflintlead)[1])
	CI_0<-CI(theta1hat,theta2hat,sigmahat,sampletime1,0,0.05)
	CI_45<-CI(theta1hat,theta2hat,sigmahat,sampletime1,45,0.05)
	CI_120<-CI(theta1hat,theta2hat,sigmahat,sampletime1,120,0.05)
	print(i)
	print(CI_0)
	print(CI_45)
	print(CI_120)

}
###Q7
flintlead3<-flintlead2[flintlead2[,2]!=48502,]
flintlead3<-flintlead3[flintlead3[,2]!=48529,]
flintlead3<-flintlead3[flintlead3[,2]!=48532,]
resp_ALL<-unlist(flintlead3[,4:6])
logresp<-log(resp_ALL)
x48503<-rep(((flintlead3[,2]==48503)+0),3)
x48504<-rep(((flintlead3[,2]==48504)+0),3)
x48505<-rep(((flintlead3[,2]==48505)+0),3)
x48506<-rep(((flintlead3[,2]==48506)+0),3)
x48507<-rep(((flintlead3[,2]==48507)+0),3)
stime<-rep(time,each=dim(flintlead3)[1])
regzip<-nls(resp_ALL~b0*exp(-(
	b1*x48503*stime+
	b2*x48504*stime+
	b3*x48505*stime+
	b4*x48506*stime+
	b5*x48507*stime)),
	start=list(b0=4,b1=0.002,b2=0.001,b3=0.002,b4=-0.0002,b5=0))
regzip
###
###Q8
######test bootstrap
for(i in 48503:48507){
	BootS_Times<-100
	N<-nrow(flintlead2)
	Num<-150
	meany_Accumul<-0
	sigma_Accumul<-0
	for(step in 1:BootS_Times){
		Sample_Num<-sample(1:N,Num,replace = FALSE)
		Boot_Sample<-flintlead2[Sample_Num,]
		time0<-0
		zipcode_BS<-which(Boot_Sample[,2]==i)
		subsetflintlead_BS<-Boot_Sample[zipcode_BS,]
		responses_BS<-unlist(subsetflintlead_BS[,4:6])
		sampletime_BS<-rep(time,each=dim(subsetflintlead_BS)[1])
		nlsreg1_BS<-nls(responses_BS~theta1*exp(-sampletime_BS*theta2),
             	start=list(theta1=5,theta2=0.02))
		theta1hat_BS<-coef(nlsreg1_BS)[1]
		theta2hat_BS<-coef(nlsreg1_BS)[2]
		meany_BS<-theta1hat_BS*exp(-time0*theta2hat_BS)
		sigmahat_BS<-summary(nlsreg1_BS)$sigma
		meany_Accumul<-meany_Accumul+meany_BS
		sigma_Accumul<-sigma_Accumul+sigmahat_BS^2
	}
	y90quantile_BS<-qnorm(0.9, (meany_Accumul/BootS_Times), sqrt(sigma_Accumul/BootS_Times))
	print(i)
	print(y90quantile_BS)
}




