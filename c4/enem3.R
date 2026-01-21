df<-irw::irw_fetch("enem_2013_1mil_lc")
df<-df[df$booklet==175,]
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

th<-list()
for (i in 1:length(mods)) {
    th[[i]]<-fscores(mods[[i]],
                     methods="EAP",
                     full.scores.SE=TRUE)
}

par(mfrow=c(1,3),mgp=c(2,1,0),mar=c(3,3,1,1))
plot(th[[1]][,1],th[[2]][,1],xlab='1pl',ylab='2pl'); abline(0,1)
plot(th[[1]][,1],th[[3]][,1],xlab='1pl',ylab='3pl'); abline(0,1)
cols<-c("black","blue","red")
plot(th[[1]],col=cols[1],ylim=c(0,1),pch=19,cex=.5)
points(th[[2]],col=cols[2],pch=19,cex=.5)
points(th[[3]],col=cols[3],pch=19,cex=.5)

    ##let's look at how different things are if we assume missingness?
