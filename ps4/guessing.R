df<-irw::irw_fetch("roar_lexical")
resp<-irw::irw_long2resp(df)
resp$id<-NULL

m1<-mirt::mirt(resp,1,'Rasch')
m2<-mirt::mirt(resp,1,'Rasch',guess=0.5)
