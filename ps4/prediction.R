
df<-irw::irw_fetch("gilbert_meta_2") 
resp<-irw::irw_long2resp(df)
resp$id<-NULL
summary(colMeans(resp,na.rm=TRUE))
df<-irw::irw_fetch("gilbert_meta_14") 
resp<-irw::irw_long2resp(df)
resp$id<-NULL
summary(colMeans(resp,na.rm=TRUE))

f<-function(tab.nm) {
    library(mirt)
    df<-irw::irw_fetch(tab.nm) 
    N<-nrow(df)
    df$test<-rbinom(N,1,.2)
    ##fit models in training data (df0)
    df0<-df[df$test==0,]
    resp<-irw::irw_long2resp(df0)
    m1<-mirt(resp[,-1],1,'1PL')
    m2<-mirt(resp[,-1],1,'2PL')
    ##now let's go back to test data
    df1<-df[df$test==1,]
    df1$item<-paste("item_",df1$item,sep='')
    tmp<-df1[,c("id","item","resp")]
    ##get predictions from m1
    th1<-fscores(m1)
    z1<-data.frame(id=resp$id,th=th1[,1])
    z1<-merge(tmp,z1)
    co1<-coef(m1,simplify=TRUE,IRTpars=TRUE)$items
    co1<-data.frame(item=rownames(co1),b=co1[,2])
    z1<-merge(z1,co1)    
    z1$p1<-1/(1+exp(-1*(z1$th-z1$b)))
    p1<-z1[,c("id","item","p1")]
    ##get predictions from m2
    th2<-fscores(m2)
    z2<-data.frame(id=resp$id,th=th2[,1])
    z2<-merge(tmp,z2)
    co2<-coef(m2,simplify=TRUE,IRTpars=TRUE)$items
    co2<-data.frame(item=rownames(co2),a=co2[,1],b=co2[,2])
    z2<-merge(z2,co2)    
    z2$p2<-1/(1+exp(-z2$a*(z2$th-z2$b)))
    p2<-z2[,c("id","item","p2")]
    ##arrange everything
    pred<-merge(tmp,p1)
    pred<-merge(pred,p2)
    rmse<-function(x,y) sqrt(mean((x-y)^2))
    r1<-rmse(pred$p1,pred$resp)
    r2<-rmse(pred$p2,pred$resp)
    c(r1,r2)
}
f('gilbert_meta_2')
f('gilbert_meta_14')


##if we look at the rmses for the rasch model, which data is better described by the rasch?
##if we look at the change from 1pl to 2pl, which model change results in bigger improvement? 
