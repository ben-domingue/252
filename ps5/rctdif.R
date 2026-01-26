
##getting the data ready
df<-irw::irw_fetch('gilbert_meta_20')
resp<-irw::irw_long2resp(df)

##assessing gender-related dif
library(difR)
gr<-df[,c("id","treat")]
gr<-gr[!duplicated(gr$id),]
index<-match(gr$id,resp$id)
gr<-gr[index,]
difLogistic(resp[,-1],type='udif',group=gr$treat,focal.name=1)
