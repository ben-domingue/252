df<-irw::irw_fetch("preference_inventory",source='nom')

resp<-irw::irw_long2resp(df,resp_col='text')
resp$id<-NULL
library(mirt)
m<-mirt(resp,1,'nominal')


f<-function(mod,n) { 
    extr <- extract.item(mod,n)
    Theta <- matrix(seq(-6,6, length.out=2000))
    pr <- probtrace(extr, Theta) 
    list(Theta,pr)
}
out<-list()
for (n in 1:ncol(resp)) out[[n]]<-f(m,n)

par(mfrow=c(4,3),mgp=c(2,1,0),mar=c(3,3,1,1))
cols<-c("black","red","blue","green")
for (i in 1:length(out)) {
    plot(NULL,xlim=c(-5,5),ylim=0:1,xlab='theta',ylab='Pr')
    z<-out[[i]]
    for (j in 1:4) lines(z[[1]],z[[2]][,j],col=cols[j])
}


##Three questions
##1. Are there any 'distractors' that look like potential problems?
##2. Any items where you don't like what you see?
##3. Could you/would you plot the expected response function here? 
