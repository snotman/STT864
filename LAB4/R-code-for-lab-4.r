setwd("C://Users//nan66//Google Drive//stt864//LAB4")
library("nlme")
library("lme4")
load(file="GOTermlist.RData")
load(file="Alldata.Rdata")
load(file="pvalset.Rdata")
load(file="pvalset2.Rdata")
## Attach packages needed. Attach them by the following order!! 


## Find the genes belong to the GO term GO:0006955

Goterms<-Alldata$originaldata[,128:130]
mc<-dim(Goterms)[1]
# GOTermlist<-NULL
# for (j in 1:mc)
# {
#  	list<-NULL
#  	for (k in 1:3)
# 	     {
# 		GOTerm22283<-as.character(Goterms[j,k])
# 		getGoTerms<-unlist(strsplit(GOTerm22283,"///"))
# 		repj<-rep(j,length(getGoTerms))	
# 		newlist<-cbind(repj,getGoTerms)
# 		list<-rbind(list,newlist)	
# 	     }
#  	GOTermlist<-rbind(GOTermlist,list)	
# }	

GO0006955<-which(GOTermlist[,2]=="GO:0006955")
rownums<-as.numeric(GOTermlist[GO0006955,1])
subsetGenes<-Alldata$originaldata[rownums,c(3:110)]
write.table(subsetGenes,file="subsetGenes.txt")

## Define factors: Beverage, Subject and Hours 

samIDs<-names(subsetGenes)
Beverages<-(
	 (samIDs%in%Alldata$trt1)*1
	+(samIDs%in%Alldata$trt2)*2
	+(samIDs%in%Alldata$trt3)*3
	+(samIDs%in%Alldata$trt4)*4
	)
Subject<-(
	 (samIDs%in%Alldata$ind1)*1
	+(samIDs%in%Alldata$ind2)*2
	+(samIDs%in%Alldata$ind3)*3
	+(samIDs%in%Alldata$ind4)*4
	+(samIDs%in%Alldata$ind5)*5
	+(samIDs%in%Alldata$ind6)*6
	)
hours<-(
	 (samIDs%in%Alldata$time_h0)*0
	+(samIDs%in%Alldata$time_h1)*1
	+(samIDs%in%Alldata$time_h2)*2
	+(samIDs%in%Alldata$time_h4)*4
	+(samIDs%in%Alldata$time_h12)*12
	)
## Q3 Fit a linear mixed model with Beverage as fixed effects, time and individual effects as random effects

BeverFac<-as.factor(Beverages)
hourFac<-as.factor(hours)
resp<-as.numeric(subsetGenes[1,])
library("lme4")
numgenes<-dim(subsetGenes)[1]
# pvalset<-rep(0,numgenes)
# pvalset2<-rep(0,numgenes)
# for (i in 1:numgenes)
# {
#  resp<-as.numeric(subsetGenes[i,])
#  lmmd2<-lmer(resp~BeverFac+(1|Subject)+(1|hours))
#  Zstat<-summary(lmmd2)$coefficients[4,3]
#  Zstat2<-(summary(lmmd2)$coefficients[3,1]-summary(lmmd2)$coefficients[4,1])/sqrt(vcov(lmmd2)[3,3]+vcov(lmmd2)[4,4]-2*vcov(lmmd2)[4,3])
#  pvalset[i]<-2*(1-pnorm(abs(Zstat)))
#  pvalset2[i]<-2*(1-pnorm(abs(Zstat2)))
# }
smallest<-which.min(pvalset)
smallest2<-which.min(pvalset2)

newresp<-as.numeric(subsetGenes[smallest,])
lmmd3<-lmer(newresp~BeverFac+(1|Subject)+(1|hours))
summary(lmmd3)


(summary(lmmd3)$coefficients[3,1]-summary(lmmd3)$coefficients[4,1])/sqrt(vcov(lmmd3)[3,3]+vcov(lmmd3)[4,4]-2*vcov(lmmd3)[4,3])

###REML for time random effect
lm3<-lm(newresp~BeverFac+Subject+hours)
summary(lm3)
anova(lm3)
sigmahat<-sqrt(0.07058)
sigmahat
sigmahat_Subject<-(0.15870-0.07058)/1
sigmahat_hours<-(0.34036-0.07058)/1
sigmahat_hours

## Q5 Plot the curves: gene expression versus hours

plot(hours,newresp,type="n",xlab="Hours", ylab="gene expression")
for (sub in 1:6)
 for (bever in 1:4)
{
points(
	hours[(Subject==sub)&(Beverages==bever)],
	newresp[(Subject==sub)&(Beverages==bever)],
	col=bever,pch=sub
	)
lines(
	hours[(Subject==sub)&(Beverages==bever)],
	newresp[(Subject==sub)&(Beverages==bever)],
	col=bever
	)
}

## Linear mixed effects models with cubic time effects

hours2<-(hours^2)
hours3<-(hours^3)
combfacs<-Subject*10+Beverages
glscsh<-gls(
	resp~BeverFac+hours+hours2+hours3,
	correlation=corCompSymm(form=~1|combfacs),
	weights=varIdent(form=~1|combfacs),
	method="REML"
	)

glsarh1<-gls(
	resp~BeverFac+hours+hours2+hours3,
	correlation=corAR1(form=~1|combfacs),
	weights=varIdent(form=~1|combfacs),
	method="REML"
	)

glscsM2<-gls(
	resp~BeverFac+BeverFac*hours+BeverFac*hours2+BeverFac*hours3,
	correlation=corCompSymm(form=~1|combfacs),
	method="REML"
	)

glscshM2<-gls(
	resp~BeverFac+BeverFac*hours+BeverFac*hours2+BeverFac*hours3,
	correlation=corCompSymm(form=~1|combfacs),
	weights=varIdent(form=~1|combfacs),
	method="REML"
	)

## Define new data set for Q7

Combdata<-cbind(newresp,hours,Subject,Beverages,combfacs)
unicombfacs<-Combdata[Combdata[,2]==0,5]
newcombdata<-NULL
for (i in 1:length(unicombfacs))
{
 subcomb<-Combdata[Combdata[,5]==unicombfacs[i],]
 Yvec<-subcomb[,1]
 Hvec<-subcomb[,2]
 Zvec<-((Yvec[Hvec!=0]-Yvec[Hvec==0])>0)+0
 newcombdata<-rbind(newcombdata,cbind(Zvec,subcomb[Hvec!=0,c(2:5)]))
}

newcombdata1<-list(
	Zvec=newcombdata[,1],
	hours=newcombdata[,2],
	Subject=newcombdata[,3],
	Beverages=newcombdata[,4],
	combfacs=newcombdata[,5])
newcombdata1$Beverages<-as.factor(newcombdata1$Beverages)
newcombdata1$hours<-as.factor(newcombdata1$hours)

## GLMM with the default Laplace approximation

glmRandomIntcept1<- glmer(
	 newcombdata1$Zvec~newcombdata1$Beverages
	+newcombdata1$hours
	+(1|newcombdata1$combfacs),
	family=binomial)
summary(glmRandomIntcept1)

## Another way to fit using Laplace approximation

library(glmmML)
glmRandomIntcept2<-glmmML(
	newcombdata1$Zvec~newcombdata1$Beverages+newcombdata1$hours,
	family=binomial,
	cluster=newcombdata1$combfacs)
summary(glmRandomIntcept2)

## Fit the model using the penalized quasi-likelihood

library(MASS)
newcombdata2<-data.frame(newcombdata1)
glmRandomIntcept3<-glmmPQL(Zvec~Beverages+hours,
	random=~1|combfacs,
	family=binomial,data=newcombdata2)
summary(glmRandomIntcept3)

## Fit the model using adaptive Gauss-Hermite qudrature(AGQ)  
## nAGQ: the number of points per axis for evaluating the adaptive Gauss-Hermite approximation to the log-likelihood.  

glmRandomIntcept4<-glmer(
						newcombdata1$Zvec~newcombdata1$Beverages
						+newcombdata1$hours+(1|newcombdata1$combfacs),
						nAGQ=8,family=binomial
						)
summary(glmRandomIntcept4)

oldpar <- par(mfrow = c(2, 3))
plot(residuals(glmRandomIntcept1))
plot(residuals(glmRandomIntcept3))
plot(residuals(glmRandomIntcept4))
qqnorm(residuals(glmRandomIntcept1))
abline(0,1)
qqnorm(residuals(glmRandomIntcept3))
abline(0,1)
qqnorm(residuals(glmRandomIntcept4))
abline(0,1)
par(oldpar)








