## Goals
## Learn to use mirt() package to estimate Rasch model
## Make sense of output
## construct Wright map!

df <- irw::irw_fetch("chess_lnirt")
resp<-irw::irw_long2resp(df)
resp$id<-NULL

## fit the rasch model
library(mirt) # might have to run install.packages("mirt")
m1 <- mirt(resp, 1, itemtype = "Rasch")
m1 #here is the object containing the estimated rasch model. it contains lots of stuff, we're just seeing a quick summary here

##it has plot methods attached that will generate item response functions (or trace lines, as they are called here)
plot(m1, type = "trace") ## which is the easiest item? the most difficult item?

## we can use the below to get item parameters
coef(m1)

##that was unwieldy, here is a better way of getting that output
get_coef <- function(mod) {
  co <- coef(mod)
  co <- co[-length(co)]#why do i get rid of this last bit?
  do.call("rbind", co)
}
coef<-get_coef(m1) #first column is alpha (note that they don't vary), second column is 'difficulty', ignore third and fourth
coef

##note that there is something different when we compare "our" version of the Rasch model to the mirt version.
##It's very important that you note this difference!
##so, be able to make sure you can explain this! [see below hint]
plot(colMeans(resp,na.rm=TRUE),coef[,2],xlab="p-values",ylab="raw mirt estimates")

## hint: look over this relevant part of the mirt manual:
## Rasch Only one intercept estimated, and the latent variance of
##      theta is freely estimated. If the data have more than two
##      categories then a partial credit model is used instead (see
##      'gpcm' below).
##           P(x = 1|theta, d) = \frac{1}{1 + exp(-(theta + d))}      


## abilities (note: the abilities get estimated in a second step that uses output of previous call to mirt(). we'll talk more about this)
th<-fscores(m1)

## wright map
library(WrightMap)
wrightMap(th,coef[,2])
##what does this tell us about the relative difficulty of this assessment for the sample who took it? 
