df<-irw::irw_fetch("andrich_mudfold")
resp<-irw::irw_long2resp(df)
resp$id<-NULL

library(mirt)
m1<-mirt(resp,1,'Rasch')
m2<-mirt(resp,1,'ideal')


##Let's now compare the CRFs for the three models for item n
f<-function(mod,n) { 
    extr <- extract.item(mod,n)
    Theta <- matrix(seq(-6,6, length.out=2000))
    pr <- probtrace(extr, Theta) 
    list(Theta,pr)
}
mods<-list(dominant=m1,ideal=m2)
n<-1
out<-list()
for (n in 1:ncol(resp)) {
    z1<-f(mods[[1]],n)
    z2<-f(mods[[2]],n)
    out[[n]]<-cbind(z1[[1]][,1],z1[[2]][,2],z2[[2]][,2])
}


par(mfrow=c(2,4),mgp=c(2,1,0),mar=c(3,3,1,1))
cols<-c("red","blue")
for (i in 1:length(out)) {
    plot(NULL,xlim=c(-5,5),ylim=0:1,xlab='theta',ylab='Pr(y=1)')
    z<-out[[i]]
    lines(z[,1],z[,2],col=cols[1])
    lines(z[,1],z[,3],col=cols[2])
}
legend("topleft",bty='n',fill=cols,legend=c("dominant","ideal"))
       
