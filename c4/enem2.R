df<-irw::irw_fetch("enem_2013_1mil_lc")
df<-df[df$booklet==175,]
set.seed(8675309)
ids<-sample(unique(df$id),5000)
df<-df[df$id %in% ids,]
resp<-irw::irw_long2resp(df)
resp$id<-NULL

library(mirt)
mods<-list()
mods$`1pl`<-mirt(resp,1,'Rasch')
mods$`2pl`<-mirt(resp,1,'2PL')
mods$`3pl`<-mirt(resp,1,'3PL')

