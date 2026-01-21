df<-irw::irw_fetch('gilbert_meta_2') 
resp<-irw::irw_long2resp(df)
resp$id<-NULL

library(mirt)
ni<-ncol(resp)
s<-paste("F=1-",ni,"
     PRIOR = (1-",ni,", a1, lnorm, 0.0, 1.0),(1-",ni,", g, expbeta, 2, 17)",sep="")
model<-mirt.model(s)
m1<-mirt(resp,model,itemtype=rep("3PL",ni),method="EM",technical=list(NCYCLES=5000))
coef(m1,simplify=TRUE,IRTpars=TRUE)


##some initial comparisons from ben betwen our model with priors and the model without
m0<-mirt(resp,1,'3PL',technical=list(NCYCLES=5000))

co1<-coef(m1,simplify=TRUE,IRTpars=TRUE)$items
co0<-coef(m0,simplify=TRUE,IRTpars=TRUE)$items
par(mfrow=c(1,3),mgp=c(2,1,0),mar=c(3,3,1,1))
plot(co0[,2],co1[,2],main='diff'); abline(0,1)
plot(co0[,1],co1[,1],main='disc'); abline(0,1)
plot(co0[,3],co1[,3],main='gues'); abline(0,1)

apply(co0,2,sd)
apply(co1,2,sd)
