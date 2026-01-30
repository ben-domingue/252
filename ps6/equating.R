df <- irw::irw_fetch("enem_2014_1mil_mt")



df<-df[df$booklet %in% c(207:209),]
library(mirt)
x<-irw::irw_long2resp(df)
x$id<-NULL
rs<-rowSums(x)
x<-x[rs>0,]
m<-mirt(x,1,'Rasch') ##concurrent calibration. easy!

L<-split(df,df$booklet)
f<-function(df) {
    ids<-sample(unique(df$id),5000)
    df<-df[df$id %in% ids,]
    df
}
L<-lapply(L,f)


f<-function(x) {
    library(mirt)
    x<-irw::irw_long2resp(x)
    x$id<-NULL
    rs<-rowSums(x)
    x<-x[rs>0,]
    ##mean(rowSums(x))
    m<-mirt(x,1,'Rasch')
    z<-coef(m,simplify=TRUE)$items
    data.frame(item=rownames(z),b=z[,2])
}
est<-lapply(L,f)


x0<-est[[1]]
names(x0)[2]<-names(est)[1]
for (i in 2:length(est)) {
    x<-est[[i]]
    names(x)[2]<-names(est)[i]
    x0<-merge(x0,x)
}
plot(density(x0[,2]-x0[,3]),col='blue',type='l')
lines(density(x0[,2]-x0[,4]),col='red')

#######################################################################

f<-function(x) {
    library(mirt)
    x<-irw::irw_long2resp(x)
    x$id<-NULL
    rs<-rowSums(x)
    x<-x[rs>0,]
    m<-mirt(x,1,'Rasch')
    th<-fscores(m)[,1]
    quantile(th,seq(.01,.99,length.out=25))
}
th<-lapply(L,f)
plot(th[[1]],th[[2]],col='gray',pch=19,xlab='207-yellow',ylab='second form'); abline(0,1)
points(th[[1]],th[[3]],col='red',pch=19)
legend("topleft",bty='n',fill=c("gray","red"),c("208-grey","209-blue"))


## the variable that holds the form is id_prova_mt and it takes values 207-210. 207 is yellow, 208 is grey, 209 is blue, and 210 is pink
##blue=bad


