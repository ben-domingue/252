enem<-irw::irw_fetch("enem_2013_1mil_lc")
df<-enem[enem$booklet==175,]
set.seed(8675309)
ids<-sample(unique(df$id),5000)
df<-df[df$id %in% ids,]
resp<-irw::irw_long2resp(df)
resp$id<-NULL
resp.all<-resp

library(mirt)
m.all<-mirt(resp,1,'Rasch')

#####################
ncommon<-5

groups<-sample(1:5,nrow(resp),replace=TRUE)
forms<-sample(1:5,ncol(resp),replace=TRUE)
anchors<-sample(names(resp),ncommon)

L<-list()
for (i in 1:5) {
    x<-resp[groups==i,]
    items<-names(resp)[forms==i]
    items<-unique(c(items,anchors))
    for (j in 1:ncol(resp)) if (!(names(resp)[j] %in% items)) x[,j]<-NA
    L[[i]]<-x
}
resp<-data.frame(do.call("rbind",L))
colMeans(is.na(resp))

m<-mirt(resp,1,'Rasch')

plot(coef(m,IRTpars=TRUE,simplify=TRUE)$items,
     coef(m.all,IRTpars=TRUE,simplify=TRUE)$items)
abline(0,1)

