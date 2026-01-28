getpred<-function(m,resp,tmp) {
    ##get predictions from m2
    th<-fscores(m)
    z<-data.frame(id=resp$id)
    for (i in 1:ncol(th)) z[[paste('th',i,sep='')]]<-th[,i]
    z<-merge(tmp,z)
    co<-coef(m,simplify=TRUE)$items
    co<-data.frame(co)
    co$item<-rownames(co)
    z<-merge(z,co)    
    if ("a2" %in% names(z)) {
        z$p<-1/(1+exp(-1*(z$a1*z$th1+z$a2*z$th2+z$d)))
    } else {
        z$p<-1/(1+exp(-1*(z$a1*z$th1+z$d)))
    }
    z[,c("id","item","p")]
}


f<-function(df,gr) {
    library(mirt)
    df0<-df[df$group!=gr,]
    resp<-irw::irw_long2resp(df0)
    ##
    ni<-ncol(resp)-1
    s<-paste("F=1-",ni,"
             PRIOR = (1-",ni,", a1, lnorm, 0.0, 1.0)",sep="")
    model<-mirt.model(s)
    m1<-mirt(resp[,-1],model,itemtype=rep("2PL",ni),method="EM",technical=list(NCYCLES=2000))
    s <- paste("F1=1-", ni, ",\nF2=1-", ni,
               "\nCOV=F1*F2",
               "\nPRIOR = (1-", ni, ", a1, lnorm, 0.0, 1.0),(1-", ni, ", a2, lnorm, 0.0, 1.0)", 
               sep = "")
    model<-mirt.model(s)
    m2<-mirt(resp[,-1],model,itemtype=rep("2PL",ni),method="EM",technical=list(NCYCLES=2000))
    ##now let's go back to test data
    df1<-df[df$group==gr,] 
    df1$item<-paste("item_",df1$item,sep='')
    tmp<-df1[,c("id","item","resp")]
    ##get predictions 
    p1<-getpred(m1,resp,tmp)
    p2<-getpred(m2,resp,tmp)
    ##arrange everything
    names(p1)[3]<-'p1'
    names(p2)[3]<-'p2'
    pred<-merge(tmp,p1)
    pred<-merge(pred,p2)
    imv::imv.binary(pred$resp,pred$p1,pred$p2)
}

df<-enem[enem$booklet==175,]
set.seed(8675309)
ids<-sample(unique(df$id),1000)
df<-df[df$id %in% ids,]

df$group<-sample(1:5,replace=TRUE,nrow(df))
df<-as.data.frame(df)

om<-numeric()
for (iii in 1:5) om[iii]<-f(df,gr=iii)
mean(om)
