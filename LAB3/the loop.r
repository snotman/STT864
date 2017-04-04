StateName<-c("FL","MI","MO","CO")
StateFACName<-sort(unique(subdata08[,2]))
PollerFACName<-sort(unique(subdata08[,5]))
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
for (k in 1:4){
  # number of locations
  NOpolls<-sum(subdata12[,2]==StateName[k])
  locations<-which(subdata12[,2]==StateName[k])
  n[k]<-length(locations)
 
  # clearance of variables in the loop
  probDemwin<-NULL
  probGopwin<-NULL
  subWeight<-NULL

  # for log model to get the X
  XStateFac<-(StateFACName==StateName[k])+0
  XPollerFAC<-(PollerFACName==pollersFAC2012[i])+0
  X<-matrix(0,n[k],nrow(vcov(logitreg)))
  Xs<-matrix(0,n[k],nrow(vcov(logitreg1)))
    
    
    
    
    
    
    
    
    
  # container of predictions 
  LogPR<-cbind(as.double(subdata12[locations,1]),
               rep(0,n[k]),rep(0,n[k]),rep(0,n[k]))
  sLogPR<-cbind(as.double(subdata12[locations,1]),
                rep(0,n[k]),rep(0,n[k]),rep(0,n[k]))
  DeLogPR<-cbind(as.double(subdata12[locations,1]),
                 rep(0,n[k]),rep(0,n[k]),rep(0,n[k]),rep(0,n[k]))
  DesLogPR<-cbind(as.double(subdata12[locations,1]),
                  rep(0,n[k]),rep(0,n[k]),rep(0,n[k]))
  counts<-0
  sumGL<-rep(0,nrow(vcov(logitreg)))
  sumGsL<-rep(0,nrow(vcov(logitreg1)))
  for (i in locations){
    counts<-counts+1
    LogDPs<-data.frame(statesFAC=StateName[k],
                       margins=margins2012[i],lagtime=lagtime2012[i], 
                       pollersFAC=pollersFAC2012[i])
    sLogDPs<-data.frame(margins=margins2012[i],
                        lagtime=lagtime2012[i],pollersFAC=pollersFAC2012[i])
#     X[counts,2:3]<-as.matrix(LogDPs[2:3])
#     Xs[counts,1:2]<-as.matrix(sLogDPs[1:2])
    # col1 :winner; col2:p(fit); col3:se.fit; col4:residual scale
    LogPR[counts,2]<-unlist(predict(logitreg,LogDPs,
                                      type="response",se.fit=TRUE))
    sLogPR[counts,2]<-unlist(predict(logitreg1,sLogDPs,
                                       type="response",se.fit=TRUE))
    #derivative
#     DeLogPR[counts,2:5]<-(1-LogPR[counts,2])*LogPR[counts,2]*unlist(LogDPs)
#     DesLogPR[counts,2:4]<-(1-sLogPR[counts,2])*sLogPR[counts,2]*unlist(sLogDPs)
#     
#     
    
    X[counts,]<-c(1,XStateFac[2:length(XStateFac)],margins2012[i],lagtime2012[i],XPollerFAC[2:length(XPollerFAC)])
    Xs[counts,]<-c(1,margins2012[i],lagtime2012[i],XPollerFAC[2:length(XPollerFAC)])
    sumGL<-sumGL+LogPR[counts,2]*(1-LogPR[counts,2])*X[counts,]
    sumGsL<-sumGsL+LogPR[counts,2]*(1-LogPR[counts,2])*X[counts,]
    }
  SSE<-sum(LogPR[,3]^2)
  SSE1<-sum(sLogPR[,3]^2)
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

  # define the function of var of two models
  for (a in 1:n[k]){
    v<-v+(P1[i]*(1-P1[i]))^2*X[i,]%*%vcov(logitreg)%*%t(X[i,])
    v1<-v+(P2[i]*(1-P2[i]))^2*Xs[i,]%*%vcov(logitreg1)%*%t(Xs[i,])
  }

  
  PredCI[k,1]<-mean(P1)-qnorm(0.975)*sqrt(mean(varbeta)/n[k])
  PredCI[k,2]<-mean(P1)+qnorm(0.975)*sqrt(mean(varbeta)/n[k])
  PredCI[k,3]<-1-PredCI[k,2]
  PredCI[k,4]<-1-PredCI[k,1]
  PredCI1[k,1]<-mean(P2)-qnorm(0.975)*sqrt(mean(varbeta1)/n[k])
  PredCI1[k,2]<-mean(P2)+qnorm(0.975)*sqrt(mean(varbeta1)/n[k])
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

