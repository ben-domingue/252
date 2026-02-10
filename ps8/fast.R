xx<-irw::irw_fetch("ffm_CSN")

df<-xx
##cutting it down
ids<-sample(unique(df$id),10000)
df<-df[df$id %in% ids,]
df$resp<-as.numeric(df$resp)
df$rt<-as.numeric(df$rt)

df$slow<-ifelse(df$rt>2,1,0)
z<-df[,c("id","item","slow")]
names(z)[3]<-'resp'
##
df<-df[df$slow==1,]
df<-df[,c("id","item","resp")]
##reversing
resp<-irw::irw_long2resp(df)
m<-mirt::mirt(resp[,-1],1,'graded')
co<-mirt::coef(m,simplify=TRUE)$item
flippers<-rownames(co)[co[,1]<0]
df$resp<-ifelse(df$item %in% flippers,6-df$resp,df$resp)
df$resp<-ifelse(df$resp>2,1,0)
##
z$node<-0
df$node<-1
df<-data.frame(rbind(df,z))

library(lme4)
m<-lmer(resp~item+item:node+(1+node|id),df)

