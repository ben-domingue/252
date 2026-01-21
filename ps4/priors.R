df<-irw::irw_fetch('gilbert_meta_2') 
resp<-irw::irw_long2resp(df)
resp$id<-NULL

ni<-ncol(resp)
s<-paste("F=1-",ni,"
             PRIOR = (1-",ni,", a1, lnorm, 0.0, 1.0)",sep="")
model<-mirt.model(s)
m1<-mirt(resp,model,itemtype=rep("2PL",ni),method="EM",technical=list(NCYCLES=2000))
