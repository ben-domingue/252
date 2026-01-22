source("/home/bdomingu/Dropbox/projects/ardp/src/00funs.R")

df<-irw::irw_fetch("gilbert_meta_1")
resp<-irw::irw_long2resp(df)
df<-df[,c("id","treat")]
df<-merge(resp,df)
df$id<-NULL
group<-group<-df$treat
df$treat<-NULL

groupdiff(df,group,g0=0,group.key=0)
