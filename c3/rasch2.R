## Goals
## We're going to take a tour of how well the RM fits different data. 
## We'll look at simulated data relative to some empirical datasets

df <- irw::irw_fetch("chess_lnirt")
resp<-irw::irw_long2resp(df)
resp$id<-NULL


library(mirt) # might have to run install.packages("mirt")
m1 <- mirt(resp, 1, itemtype = "Rasch")

plot(m1, type = "trace") 

## here is a fun way of looking at comparing the estimated icc to empirical data
itemfit(m1,empirical.plot=5)
##explore the fit across more items [change 5 to some other number]. what do you think? look, for example, at 13

################################################################
df <- irw::irw_fetch("wirs") ##https://cran.r-project.org/web/packages/ltm/refman/ltm.html#WIRS
resp<-irw::irw_long2resp(df)
resp$id<-NULL
m1 <- mirt(resp, 1, itemtype = "Rasch")
itemfit(m1,empirical.plot=1) ##again, consider a range of items here. what do you think about the fit in this case relative to the chess_lnirt data?

################################################################
##Let's now turn to simulated data. What will you expect wrt fit of the RM here? 

##number of items and people.
##we'll start small just so that you can see everything, but you'll want to make this bigger downstream.
ni<-50
np<-2000
##now we're going to simulate data according to this model and examine some key properties
set.seed(12311)
##first let's describe the individual level part of the model
th<-rnorm(np)
th.mat<-matrix(th,np,ni,byrow=FALSE) #these are the true abilities. we don't observe them, which will be a real problem for us downstream. but we'll not worry about that today. 
th.mat #abilities, on per row
##now the item level part.
b<-rnorm(ni)
b.mat<-matrix(b,np,ni,byrow=TRUE) #these are the item difficulties
b.mat #difficulties, one per item
##now we have to put this stuff together. what we want is a probability of a correct response for a person to an item
##we're going to use what you may know from logistic regression
inv_logit<-function(x) exp(x)/(1+exp(x))
##now the probability of a correct response is:
pr<-inv_logit(th.mat-b.mat) #note this is pairwise multiplication not matrix multiplication.

##we can simulate data using those probabilities
resp<-pr
for (i in 1:ncol(resp)) resp[,i]<-rbinom(nrow(resp),1,resp[,i])
resp<-data.frame(resp)
resp

m1 <- mirt(resp, 1, itemtype = "Rasch")
itemfit(m1,empirical.plot=5) ##Question: how would you expect this to change if i changed ni or np?



