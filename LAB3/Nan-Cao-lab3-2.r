setwd("C://Users//nan66//Google Drive//stt864//LAB3")
set.seed(52871775)
library(MASS)
#Q1
#reading data
polls2008<-read.csv(file="2008-polls.csv",header=TRUE)
polls2012<-read.csv(file="2012-polls.csv",header=TRUE)
results2008<-read.csv(file="2008-results.csv",header=TRUE)
results2012<-read.csv(file="2012-results.csv",header=TRUE)
#select pollsters that conducted polls >5 states.
atleast5<-table(polls2008[,5])[table(polls2008[,5])>=5]
atleast5
pollers<-c("ARG", "EPICMRA", "InsiderAdvantage", 
		   "MaristColl", "MasonDixon", "MuhlenbergColl", 
           "QuinnipiacU", "Rasmussen", "SienaColl",
           "SuffolkU","SurveyUSA", "UofCincinnati",
           "UofNewHampshire","Zogby")
po08sub<-polls2008[polls2008[,5]%in%pollers,]
po12sub<-polls2012[polls2012[,5]%in%pollers,]
#Q2
#reformatting the poll and true results dataset as desired
#Dem win=1 Rep win=0
winers2008<-(results2008[,2]-results2008[,3]>0)+0
#name of 51 states
StateID2008<-results2008[,1]
Allresp<-NULL
for (sid in 1:51){
  ##operate on state=sid
	po08subID<-po08sub$State==StateID2008[sid]
	#polls (at least5), 
	PoWin08SubSta<-po08sub[po08subID,]
	#Dem win=1 Rep win=0
	PoWin08Sta<-(PoWin08SubSta[,2]-PoWin08SubSta[,3]>0)+0
	#whether the polls is correct
	pollwinersIND<-(PoWin08Sta==winers2008[sid])+0
	#conbine it to "Allresp"
	Allresp<-c(Allresp,pollwinersIND)
}
#absolute difference between Supp rates of Dem and Rep.
margins2008<-abs(po08sub[,2]-po08sub[,3])
lagtime2008<-rep(0,dim(po08sub)[1])
electiondate2008<-c("Nov 04 2008")
EdDa08<-as.Date(electiondate2008, format="%b %d %Y")
for (i in 1:dim(po08sub)[1]){
	StDa08<-as.Date(as.character(po08sub[i,4]), 
		format="%b %d %Y")
	lagtime2008[i]<-EdDa08-StDa08
}
data08<-cbind(Allresp,as.character(po08sub[,1]),
	margins2008,lagtime2008,as.character(po08sub[,5]))
#Q3
# select the states with at least one failure prediction
# aka data08$Allresp=0
stateslist<-unique(data08[which(data08[,1]=="0"),2])
subdata08<-data08[data08[,2]%in%stateslist,]
#Q4 need the significance.!!!
# define new variables and fit a logistic regression model
resp<-as.integer(subdata08[,1])
statesFAC<-as.factor(subdata08[,2])
margins<-as.double(subdata08[,3])
lagtime<-as.double(subdata08[,4])
pollersFAC<-as.factor(subdata08[,5])
logitreg<-glm(resp~statesFAC+margins+lagtime+pollersFAC,
	family="binomial")
summary(logitreg)
#Q5
#Fit a simple logistic regression model without states
logitreg1<-glm(resp~margins+lagtime+pollersFAC,
	family="binomial")
summary(logitreg1)
anova(logitreg1,logitreg)
anova(logitreg1,logitreg,test="Chisq")
# reformating the 2012 poll data for prediction purpose
pollwiners2012<-(po12sub[,2]-po12sub[,3]>0)+0
margins2012<-abs(po12sub[,2]-po12sub[,3])
lagtime2012<-rep(0,dim(po12sub)[1])
electiondate2012<-c("Nov 06 2012")
EdDa12<-as.Date(electiondate2012, format="%b %d %Y")
for (i in 1:dim(po12sub)[1]){
	StDa12<-as.Date(as.character(po12sub[i,4]), 
		format="%b %d %Y")
 lagtime2012[i]<-EdDa12-StDa12
}
data12<-cbind(pollwiners2012,as.character(po12sub[,1]),
	margins2012,lagtime2012,as.character(po12sub[,5]))
#Q6 Q7 Q8
# Focusing on the states in the state list of 2008
# select the states with at least one failure prediction
# aka data08$Allresp=0
subdata12<-data12[data12[,2]%in%stateslist,]
# Predict with logistic and simple logistic model
margins2012<-as.double(subdata12[,3])
lagtime2012<-as.double(subdata12[,4])
pollersFAC2012<-as.factor(subdata12[,5])
StateName<-c("FL","MI","MO","CO")
n<-c(0,0,0,0)
SSE<-c(0,0,0,0)
SSE1<-c(0,0,0,0)
# Pred for Q6
Pred<-matrix(0,4,4)
rownames(Pred)<-StateName
colnames(Pred)<-c("LDemWin","LRepWin","sLDemWin","sLRepWin")
# Pred for Q7
Pred1<-matrix(0,4,4)
rownames(Pred1)<-StateName
colnames(Pred1)<-c("LDemWin","LRepWin","sLDemWin","sLRepWin")
# log regression CI for Q8
PredCI<-matrix(0,4,4)
rownames(PredCI)<-StateName
colnames(PredCI)<-c("DemWinLow","DemWinUp","RepWinLow","RepWinUp")
# sample log regression CI for Q8
PredCI1<-matrix(0,4,4)
rownames(PredCI1)<-StateName
colnames(PredCI1)<-c("DemWinLow","DemWinUp","RepWinLow","RepWinUp")
# bootstrap CI of log regression for Q8
BSCI<-matrix(0,4,4)
rownames(BSCI)<-StateName
colnames(BSCI)<-c("DemWinLow","DemWinUp","RepWinLow","RepWinUp")
# bootstrap CI of sample log regression for Q8
BSCI1<-matrix(0,4,4)
rownames(BSCI1)<-StateName
colnames(BSCI1)<-c("DemWinLow","DemWinUp","RepWinLow","RepWinUp")
# Weighted Pred of log regression for Q9
S.Pred<-matrix(0,4,4)
rownames(S.Pred)<-StateName
colnames(S.Pred)<-c("LDemWin","LRepWin","sLDemWin","sLRepWin")
# computation of weight of pollers for Q9
poNum<-length(pollers)
ErrRates<-rep(0,poNum)
for (poID in 1:poNum){
  PoPred08<-NULL
  PoPredRt<-NULL
  Po08Rt<-NULL
  StateID<-NULL
  PoPred08<-po08sub[po08sub[,5]==pollers[poID],]
  PoPredRt<-(PoPred08[,2]>PoPred08[,3])+0
  StateID<-PoPred08[,1]
  Po08Rt<-winers2008[StateID]
  ErrRates[poID]<-sum(1-(PoPredRt==winers2008[StateID]))/length(PoPredRt)
}
PoRank<-rank(ErrRates)
Weight<-1/(PoRank^2)
PollWeight<-data.frame(pollers,Weight)
#the loop

StateFACName<-sort(unique(subdata08[,2]))
PollerFACName<-sort(unique(subdata08[,5]))


for (k in 1:4){
	# number of locations
	NOpolls<-sum(subdata12[,2]==StateName[k])
	locations<-which(subdata12[,2]==StateName[k])
	n[k]<-length(locations)
	# clearance of variables in the loop
    probDemwin<-NULL
	probGopwin<-NULL
	subWeight<-NULL

	
	XStateFac<-(StateFACName==StateName[k])+0
	XPollerFAC<-(PollerFACName==pollersFAC2012[k])+0
	X<-matrix(0,n[k],nrow(vcov(logitreg)))
	Xs<-matrix(0,n[k],nrow(vcov(logitreg1)))

	
	
	# container of predictions 
	LogPR<-cbind(as.double(subdata12[locations,1]),
		rep(0,n[k]))
	sLogPR<-cbind(as.double(subdata12[locations,1]),
		rep(0,n[k]))
	DeLogPR<-cbind(as.double(subdata12[locations,1]),
		rep(0,n[k]))
	DesLogPR<-cbind(as.double(subdata12[locations,1]),
		rep(0,n[k]))
	counts<-0
	for (i in locations){
		counts<-counts+1
 		LogDPs<-data.frame(statesFAC=StateName[k],
 			margins=margins2012[i],lagtime=lagtime2012[i], 
 			pollersFAC=pollersFAC2012[i])
 		sLogDPs<-data.frame(margins=margins2012[i],
 			lagtime=lagtime2012[i],pollersFAC=pollersFAC2012[i])

 		
 		
		LogPR[counts,2]<-unlist(predict(logitreg,LogDPs,
			type="response"))
		sLogPR[counts,2]<-unlist(predict(logitreg1,sLogDPs,
			type="response"))
 		X[counts,]<-c(1,XStateFac[2:length(XStateFac)],margins2012[i],
 		              lagtime2012[i],XPollerFAC[2:length(XPollerFAC)])
		Xs[counts,]<-c(1,margins2012[i],lagtime2012[i],XPollerFAC[2:length(XPollerFAC)])
    }

	P1<-LogPR[,1]*LogPR[,2]+(1-LogPR[,1])*(1-LogPR[,2])
	P2<-sLogPR[,1]*sLogPR[,2]+(1-sLogPR[,1])*(1-sLogPR[,2])

	# Predictions for Q6
	Pred[k,1]<-mean(P1)
	Pred[k,2]<-mean(1-P1)
	Pred[k,3]<-mean(P2)
	Pred[k,4]<-mean(1-P2)
	# Predictions for Q7
	Pred1[k,1]<-mean(P1>0.5+0)
	Pred1[k,2]<-mean(P1<0.5+0)
	Pred1[k,3]<-mean(P2>0.5+0)
	Pred1[k,4]<-mean(P2<0.5+0)
	# Q8.1 
	# clearance of containers
	GL<-rep(0,nrow(vcov(logitreg)))
	GsL<-rep(0,nrow(vcov(logitreg1)))
	# define the function of var of two models
  for(t in 1:n[k]){
    GL<-LogPR[t,2]*(1-LogPR[t,2])*(-1)^(LogPR[t,1]+1)*X[t,]/n[k]+0
    GsL<-sLogPR[t,2]*(1-sLogPR[t,2])*(-1)^(sLogPR[t,1]+1)*Xs[t,]/n[k]+0
  }
# 	varp<-(sumGL/n[k])%*%vcov(logitreg)%*%t((sumGL/n[k]))
# 	varp1<-(sumGsL/n[k])%*%vcov(logitreg)%*%t((sumGsL/n[k]))
	PredCI[k,1]<-mean(P1)-qnorm(0.975)*sqrt(GL%*%vcov(logitreg)%*%GL)
	PredCI[k,2]<-mean(P1)+qnorm(0.975)*sqrt(GL%*%vcov(logitreg)%*%GL)
	PredCI[k,3]<-1-PredCI[k,2]
	PredCI[k,4]<-1-PredCI[k,1]
	PredCI1[k,1]<-mean(P2)-qnorm(0.975)*sqrt(GsL%*%vcov(logitreg1)%*%GsL)
	PredCI1[k,2]<-mean(P2)+qnorm(0.975)*sqrt(GsL%*%vcov(logitreg1)%*%GsL)
	PredCI1[k,3]<-1-PredCI1[k,2]
	PredCI1[k,4]<-1-PredCI1[k,1]
	# Q8.2 Bootstrap prediction CI
	xStar<-NULL
	xStar1<-NULL
	for (boot in 1:500){
		BS.sample<-sample(P1,n[k],replace=TRUE)
		xStar[boot]<-mean(BS.sample)
		BS.sample1<-sample(P2,n[k],replace=TRUE)
		xStar1[boot]<-mean(BS.sample)
	}
	BSCI[k,1]<-mean(xStar)-qnorm(0.975)*sd(xStar)
	BSCI[k,2]<-mean(xStar)+qnorm(0.975)*sd(xStar)
	BSCI[k,3]<-mean(1-xStar)-qnorm(0.975)*sd(1-xStar)
	BSCI[k,4]<-mean(1-xStar)+qnorm(0.975)*sd(1-xStar)
	BSCI1[k,1]<-mean(xStar1)-qnorm(0.975)*sd(xStar1)
	BSCI1[k,2]<-mean(xStar1)+qnorm(0.975)*sd(xStar1)
	BSCI1[k,3]<-mean(1-xStar1)-qnorm(0.975)*sd(1-xStar1)
	BSCI1[k,4]<-mean(1-xStar1)+qnorm(0.975)*sd(1-xStar1)
	#Q9
	subPoName<-subdata12[subdata12[,2]==StateName[k],5]
	subWeight<-rep(0,NOpolls)
	for (f in 1:NOpolls){
	  subWeight[f]<-PollWeight[PollWeight[,1]==subPoName[f],2]
	}
	W.P1<-P1*subWeight/sum(subWeight)
	W.P2<-P2*subWeight/sum(subWeight)
	W.P1.C<-(1-P1)*subWeight/sum(subWeight)
	W.P2.C<-(1-P2)*subWeight/sum(subWeight)
	S.Pred[k,1]<-sum(W.P1)
	S.Pred[k,2]<-sum(W.P1.C)
	S.Pred[k,3]<-sum(W.P2)
	S.Pred[k,4]<-sum(W.P2.C)
} 
	
# 	varp<-(sumGL/n[k])%*%vcov(logitreg)%*%t((sumGL/n[k]))
# 	varp1<-(sumGsL/n[k])%*%vcov(logitreg)%*%t((sumGsL/n[k]))
	# print(GL%*%vcov(logitreg)%*%GL)
	# print(GsL%*%vcov(logitreg1)%*%GsL)
	PredCI[k,1]<-mean(P1)-qnorm(0.975)*sqrt(GL%*%vcov(logitreg)%*%GL)
	PredCI[k,2]<-mean(P1)+qnorm(0.975)*sqrt(GL%*%vcov(logitreg)%*%GL)
	PredCI[k,3]<-1-PredCI[k,2]
	PredCI[k,4]<-1-PredCI[k,1]
	PredCI1[k,1]<-mean(P2)-qnorm(0.975)*sqrt(GsL%*%vcov(logitreg1)%*%GsL)
	PredCI1[k,2]<-mean(P2)+qnorm(0.975)*sqrt(GsL%*%vcov(logitreg1)%*%GsL)
	PredCI1[k,3]<-1-PredCI1[k,2]
	PredCI1[k,4]<-1-PredCI1[k,1]
	# Q8.2 Bootstrap prediction CI
	xStar<-NULL
	xStar1<-NULL
	for (boot in 1:500){
		BS.sample<-sample(P1,n[k],replace=TRUE)
		xStar[boot]<-mean(BS.sample)
		BS.sample1<-sample(P2,n[k],replace=TRUE)
		xStar1[boot]<-mean(BS.sample)
	}
	BSCI[k,1]<-mean(xStar)-qnorm(0.975)*sd(xStar)
	BSCI[k,2]<-mean(xStar)+qnorm(0.975)*sd(xStar)
	BSCI[k,3]<-mean(1-xStar)-qnorm(0.975)*sd(1-xStar)
	BSCI[k,4]<-mean(1-xStar)+qnorm(0.975)*sd(1-xStar)
	BSCI1[k,1]<-mean(xStar1)-qnorm(0.975)*sd(xStar1)
	BSCI1[k,2]<-mean(xStar1)+qnorm(0.975)*sd(xStar1)
	BSCI1[k,3]<-mean(1-xStar1)-qnorm(0.975)*sd(1-xStar1)
	BSCI1[k,4]<-mean(1-xStar1)+qnorm(0.975)*sd(1-xStar1)
	#Q9
	subPoName<-subdata12[subdata12[,2]==StateName[k],5]
	subWeight<-rep(0,NOpolls)
	for (f in 1:NOpolls){
	  subWeight[f]<-PollWeight[PollWeight[,1]==subPoName[f],2]
	}
	W.P1<-P1*subWeight/sum(subWeight)
	W.P2<-P2*subWeight/sum(subWeight)
	W.P1.C<-(1-P1)*subWeight/sum(subWeight)
	W.P2.C<-(1-P2)*subWeight/sum(subWeight)
	S.Pred[k,1]<-sum(W.P1)
	S.Pred[k,2]<-sum(W.P1.C)
	S.Pred[k,3]<-sum(W.P2)
	S.Pred[k,4]<-sum(W.P2.C)
} 
#Q6
Pred
#Q7
Pred1
results2012[results2012[,1]%in%StateName,]
#Q8
PredCI
PredCI1
BSCI
BSCI1
#Q9
S.Pred

