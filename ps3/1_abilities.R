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



#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
##Bonus: Let's now focus on a simple two item test and look at the likelihood surfaces for all possible response patterns

## select two items
b_pair <- b[1:2]

## log-likelihood for a single person and two items
loglik_pair <- function(th, x, b) {
    p <- 1 / (1 + exp(-(th - b)))
    sum(x * log(p) + (1 - x) * log(1 - p))
}

## theta grid
th_grid <- seq(-5, 5, length.out = 400)
###########################
                                        # question: why we need to set it between -5 and 5 in optim(), when would happen if not (especially for patterns 00 and 11)
###########################

## response patterns
patterns <- list(
    "00" = c(0, 0),
    "01" = c(0, 1),
    "10" = c(1, 0),
    "11" = c(1, 1)
)

par(mfrow = c(2, 2))

for (nm in names(patterns)) {
    x <- patterns[[nm]]
    ll <- sapply(th_grid, loglik_pair, x = x, b = b_pair)
    plot(
        th_grid,
        ll,
        type = "l",
        main = paste("Response pattern", nm),
        xlab = "Latent variable theta",
        ylab = "Person likelihood"
    )
    ## mark MLE if finite
    th_hat <- th_grid[which.max(ll)]
    abline(v = th_hat, lty = 2)
}

par(mfrow = c(1, 1))
