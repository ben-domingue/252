b<- .5
x<-rnorm(1000)
p<-exp(b*x)/(1+exp(b*x))
y<-rbinom(length(x),1,p)
f<-function(b,x,y) {
    p<-exp(b*x)/(1+exp(b*x))
    sum((y-p)*x)
}
bs<-seq(-3,3,length.out=100)
z<-numeric()
for (i in 1:length(bs)) z[i]<-f(bs[i],x=x,y=y)
plot(bs,z,xlab='candidate b',ylab='derivative',type='l',ylim=c(-1000,1000)); abline(h=0,col='gray')

