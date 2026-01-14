##number of items and people.
##we'll start small just so that you can see everything, but you'll want to make this bigger downstream.
ni<-20
np<-200
##now we're going to simulate data according to this model and examine some key properties
set.seed(12311)
##first let's describe the individual level part of the model
th<-rnorm(np)
th.mat<-matrix(th,np,ni,byrow=FALSE) #these are the true abilities. we don't observe them, which will be a real problem for us downstream. but we'll not worry about that today. 
##now the item level part.
b<-rnorm(ni)
b.mat<-matrix(b,np,ni,byrow=TRUE) #these are the item difficulties

################################################################
##now we have to put this stuff together. what we want is a probability of a correct response for a person to an item
##we're going to use what you may know from logistic regression
inv_logit<-function(x) exp(x)/(1+exp(x))
##now the probability of a correct response is:
pr<-inv_logit(-1*(th.mat-b.mat)) #note this is pairwise multiplication not matrix multiplication.

##we can simulate data using those probabilities
resp<-pr
for (i in 1:ncol(resp)) resp[,i]<-rbinom(nrow(resp),1,resp[,i])

#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
##now let's consider 2 ways of estimating ability
##1. via mirt
library(mirt)
mod<-mirt(data.frame(resp),1,itemtype="Rasch")
th1<-fscores(mod,'ML')

##note that we did ML estimation of the item parameters via the mirt::fscores() function. we'll talk more about alternatives downstream

##via ML using KNOWN item parameters
loglik.2pl<-function(th,x,b) {
    p<-1/(1+exp(-1*(th-b)))
    q<-1-p
    -1*sum(x*log(p)+(1-x)*log(q)) #to get maxim from optim
}
th2<-numeric()
for (i in 1:nrow(resp)) th2[i]<-optim(0,loglik.2pl,x=resp[i,],b=b,method="Brent",lower=-5,upper=5)$par

z<-data.frame(true=th,th.mirt=th1,th.ml=th2)
plot(z)


