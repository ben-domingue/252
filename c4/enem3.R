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

th<-seq(-4,4,length.out=1000)
ii<-list()
for (i in 1:ncol(resp)) {
    ei<-extract.item(mods$`2pl`,i)
    ii[[i]]<-iteminfo(ei,th)
}
par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,1))
plot(NULL,xlim=range(th),ylim=c(0,1))
for (i in 1:length(ii)) lines(th,ii[[i]])
ti<-do.call("cbind",ii)
ti<-rowSums(ti)
plot(th,ti,col='red',type='l',ylim=c(0,6))
lines(th,1/sqrt(ti),col='blue') ##do these numbers match up to SE values below?


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

