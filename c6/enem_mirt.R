enem<-irw::irw_fetch("enem_2013_1mil_lc")
df<-enem[enem$booklet==175,]
set.seed(8675309)
ids<-sample(unique(df$id),5000)
df<-df[df$id %in% ids,]
resp<-irw::irw_long2resp(df)
resp$id<-NULL

library(mirt)
mods<-list()
n<-ncol(resp)
s<-paste("F1=1-40,
     F2=1-40,
     PRIOR = (1-40, a1, lnorm, 0.0, 1.0),
     PRIOR = (1-40, a2, lnorm, 0.0, 1.0)",
     sep="")
model<-mirt.model(s)
mod2d <- mirt(resp, model)
##
s<-paste("F1=1-40,
     PRIOR = (1-40, a1, lnorm, 0.0, 1.0)",
     sep="")
model<-mirt.model(s)
mod1d <- mirt(resp, model)

coef(mod2d,simplify=TRUE,IRTpars=TRUE)
coef(mod1d,simplify=TRUE,IRTpars=TRUE)

mod2d
mod1d
plot(mod2d)
plot(mod1d)

mod2d@Fit$AIC
mod1d@Fit$AIC


