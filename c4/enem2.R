df<-irw::irw_fetch("enem_2013_1mil_lc")

##some different groups
g1<-df[df$booklet==175,]
g2<-df[df$booklet==176,]
ss<-by(g1$resp,g1$id,sum)
m<-mean(ss)
id.hi<-names(ss)[ss>m]
id.lo<-names(ss)[ss<m]
g3<-g1[g1$id %in% id.hi,]
g4<-g1[g1$id %in% id.lo,]

L<-list(g1=g1,g2=g2,g3=g3,g4=g4)

f<-function(df) {
    set.seed(8675309)
    ids<-sample(unique(df$id),1000)
    df<-df[df$id %in% ids,]
    resp<-irw::irw_long2resp(df)
    resp$id<-NULL
    library(mirt)
    m<-mirt(resp,2,"2PL")
    co<-coef(m,simplify=TRUE,IRTpars=TRUE)$items
}
z<-lapply(L,f)
