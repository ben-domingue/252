b0<-0
b1<-.5
x<-rnorm(5000)
p<-1/(1+exp(-1*(b0+b1*x)))
y<-rbinom(length(x),1,p)

##likelihood
p<-function(b,x) 1/(1+exp(-1*(b[1]+b[2]*x)))
ll<-function(b,x,y) {
    pr<-p(b,x)
    -1*sum(y*log(pr)+(1-y)*log(1-pr)) ##note the -1
}
##
b0.est<-0
b1.est<-seq(-2,2,length.out=100)
loglik<-numeric()
for (i in 1:length(b1.est)) loglik[i]<-ll(c(b0.est,b1.est[i]),x=x,y=y)
plot(b1.est,loglik)

optim(c(0,0),ll,x=x,y=y)
coef(glm(y~x,family='binomial'))
