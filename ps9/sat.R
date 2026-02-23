library(lme4)

##ROAR
df<-irw::irw_fetch("roar_lexical")
df$lrt<-log(df$rt)
roar<-df
m.roar<-glmer(resp~(1|item)+(1|id)+lrt,roar,family='binomial')

##pisa
df<-irw::irw_fetch("pisa2018_read")
df<-df[df$CNT=="ESP",] ##doing this from madrid!
f<-function(x) length(unique(x[!is.na(x)]))
nc<-by(df$resp,df$item,f)
df<-df[df$item %in% names(nc)[nc==2],]
##downsample
ids<-sample(unique(df$id),2000)
df<-df[df$id %in% ids,]
df$lrt<-log(df$rt)
df<-df[is.finite(df$lrt),]
pisa<-df
m.pisa<-glmer(resp~(1|item)+(1|id)+lrt,pisa,family='binomial')


##alt code if you have problems with pisa:
table_obj <- irw:::.fetch_redivis_table("pisa2018_read")
table_obj$download("pisa2018_read.csv")
library(data.table)
df <- fread("pisa2018_read.csv")
