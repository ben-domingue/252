df<-irw::irw_fetch("gilbert_meta_2")
##getting post-test theta
resp<-irw::irw_long2resp(df)
m<-mirt::mirt(resp[,-1],1,'Rasch')
th<-mirt::fscores(m)
##putting data together
z<-data.frame(id=resp[,1],th=th[,1])
z2<-df[,c("id","std_baseline","treat")]
z2<-z2[!duplicated(z2$id),]
z<-merge(z,z2)
##
z$std_baseline<-as.numeric(z$std_baseline)
##aggregate level
m0<-lm(th~treat,z)
m1<-lm(th~treat+std_baseline,z)
m2<-lm(th~treat*std_baseline,z)
##response level
m1a<-lme4::glmer(resp~1+treat+(1|item)+(1|id),df,family='binomial')
m2a<-lme4::glmer(resp~1+treat+std_baseline+(1|item)+(1|id),df,family='binomial')
m3a<-lme4::glmer(resp~1+treat*std_baseline+(1|item)+(1|id),df,family='binomial')
m4a<-lme4::glmer(resp~1+treat*std_baseline+(1+treat|item)+(1|id),df,family='binomial')

lapply(list(m2a,m3a,m4a),function(x) summary(x)$coef)
