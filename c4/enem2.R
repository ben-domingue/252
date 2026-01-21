##some different groups
g1<-enem[enem$booklet==175,]
g2<-enem[enem$booklet==176,]
ss<-by(g1$resp,g1$id,sum)
m<-mean(ss)
id.hi<-names(ss)[ss>m]
id.lo<-names(ss)[ss<m]
g3<-g1[g1$id %in% id.hi,]
g4<-g1[g1$id %in% id.lo,]

L<-list(g1=g1,g2=g2,g3=g3,g4=g4)

f<-function(df) {
    set.seed(8675309)
    ids<-sample(unique(df$id),5000)
    df<-df[df$id %in% ids,]
    resp<-irw::irw_long2resp(df)
    resp$id<-NULL
    library(mirt)
    m<-mirt(resp,1,"2PL")
    co<-coef(m,simplify=TRUE,IRTpars=TRUE)$items
}
z<-lapply(L,f)

par(mfcol=c(3,2),mgp=c(2,1,0),mar=c(3,3,1,1))
plot(z[[1]][,2],z[[2]][,2]); abline(0,1)
plot(z[[3]][,2],z[[4]][,2]); abline(0,1)
plot(z[[2]][,2],z[[4]][,2]); abline(0,1)
plot(z[[1]][,1],z[[2]][,1]); abline(0,1)
plot(z[[3]][,1],z[[4]][,1]); abline(0,1)
plot(z[[2]][,1],z[[4]][,1]); abline(0,1)

## ##Things to look at lot better if you run the Rasch model
## f<-function(df) {
##     set.seed(8675309)
##     ids<-sample(unique(df$id),5000)
##     df<-df[df$id %in% ids,]
##     resp<-irw::irw_long2resp(df)
##     resp$id<-NULL
##     library(mirt)
##     m<-mirt(resp,1,"Rasch")
##     co<-coef(m,simplify=TRUE,IRTpars=TRUE)$items
## }
## z<-lapply(L,f)
## plot(z[[3]][,2],z[[4]][,2],xlab='G3',ylab='G4'); abline(0,1)

