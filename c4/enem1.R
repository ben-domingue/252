enem<-irw::irw_fetch("enem_2013_1mil_lc")
df<-enem[enem$booklet==175,]
set.seed(8675309)
ids<-sample(unique(df$id),5000)
df<-df[df$id %in% ids,]
resp<-irw::irw_long2resp(df)
resp$id<-NULL

library(mirt)
mods<-list()
n<-ncol(resp)
model_syntax <- paste(paste('F1 = 1-',n,sep=''),
                      paste('CONSTRAIN = (1-',n,', a1)',sep=''),
                      sep="\n")
mods$`1pl` <- mirt(resp, model_syntax)
mods$`2pl`<-mirt(resp,1,'2PL')
mods$`3pl`<-mirt(resp,1,'3PL')

f<-function(m) coef(m,IRTpars=TRUE,simplify=TRUE)
lapply(mods,f) ##the item parameters, note IRTpars=TRUE means we have difficulties rather than easiness parameters


##look at the item parameters for the three models
##how different are the difficulty parameters for each? how difficult is this assessment in general?
##what happens as you go from 2pl to 3pl with the discrimination parameters
##what do you think of the range of guessing params?

##Let's look at curves for all items from each of the models
par(mfrow=c(5,8),mgp=c(2,1,0),mar=c(0,0,0,0))
th<-seq(-4,4,length.out=500)
cols<-c("black","blue","red")
for (i in 1:ncol(resp)) {
    plot(NULL,xlim=range(th),ylim=0:1,xlab='',ylab='',xaxt='n',yaxt='n')
    for (j in 1:length(mods)) {
        ei<-extract.item(mods[[j]],i)
        yv<-expected.item(ei,th)
        lines(th,yv,col=cols[j])
        lines(th,2*dnorm(th),lwd=.5,col='gray',lty=2) ##this is to emphasize that these are where the abilities are centered; differences at edges less meanginful
    }
}
##What do you think?

##extensions if you like: consider the 4pl and 5pl?
