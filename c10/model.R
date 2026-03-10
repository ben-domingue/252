resp<-irw::irw_long2resp(df)
resp<-resp[,-1]
nc<-apply(resp,2,function(x) length(unique(x[!is.na(x)])))
mods<-ifelse(nc==2,'3PL','gpcmIRT')

m<-mirt::mirt(resp,1,itemtype=mods)
