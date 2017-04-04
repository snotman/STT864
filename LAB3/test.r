# Q8.2 delta method
# h(beta_hat)+-Z(alpha/2)*t(dp(x))/dbeta)*var(beta)(dp(x)/dbeta)
# p(x)=exp(t(beta)X)/(1+exp(t(beta)X))
# dp(x)/d(beta)=x*exp(t(beta)*x)

px<-function(beta,x){
	return(exp(t(beta)%*%x)/(1+exp(t(beta)%*%x)))
}
dpx.dbeta<-function(beta,x){
	return(t(x)%*%exp(t(beta)%*%x)/(1+exp(t(beta)%*%x))^2)

}
colnames(coef(logitreg))==paste0(colnames(LogDPs)[1],StateName[k])

