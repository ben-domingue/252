load("promis1wave1_pain.Rdata")

ids<-sample(unique(df$id),5000)
df<-df[df$id %in% ids,]
ids<-sample(unique(df$item),25)
df<-df[df$item %in% ids,]
df$resp<-df$resp-1

resp<-irw::irw_long2resp(df)
library(mirt)
resp<-resp[,-1]

mod.grm <- mirt(resp, 1,itemtype="graded") 
mod.pcm <- mirt(resp, 1,itemtype="Rasch") #this will estimate the PCM
mod.srm <- mirt(resp, 1,itemtype="Tutz")

##Let's now compare the CRFs for the three models for item n
f<-function(mod,n) { #see line 41
    extr <- extract.item(mod,n)
    Theta <- matrix(seq(-6,6, length.out=2000))
    pr <- probtrace(extr, Theta) #min() of first item
    list(Theta,pr)
}
mods<-list(grm=mod.grm,pcm=mod.pcm,srm=mod.srm)
n<-1
out<-list()
for (i in 1:length(mods)) out[[i]]<-f(mods[[i]],n)

par(mfrow=c(2,2),mgp=c(2,1,0),mar=c(3,3,1,1))
for (i in 1:4) {
    plot(NULL,xlim=c(-5,5),ylim=0:1,xlab='theta',ylab='Pr')
    for (j in 1:length(out)) {
        z<-out[[j]]
        lines(z[[1]],z[[2]][,i])
    }
}


f<-function(mod,n) { #see line 41
    extr <- extract.item(mod,n)
    Theta <- matrix(seq(-6,6, length.out=2000))
    pr <- expected.item(extr, Theta) #min() of first item
    cbind(Theta,pr)
}
cols<-c("black","red","blue")
par(mfrow=c(5,5),mgp=c(2,1,0),mar=c(3,3,1,1))
for (i in 1:25) {
    plot(NULL,xlim=c(-5,5),ylim=c(0,4),xlab='theta',ylab='Pr')
    for (j in 1:length(mods)) {
        z<-f(mods[[j]],i)
        lines(z,col=cols[j])
    }
}
legend("bottomright",bty='n',fill=cols,names(mods))
