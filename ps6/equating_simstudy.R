b<-rnorm(50)
th<-rnorm(200)
k<-outer(th,b,'-')
p<-1/(1+exp(-k))
resp<-p
for (i in 1:ncol(resp)) resp[,i]<-rbinom(nrow(resp),1,p[,i])

##design 1
gr1<-sample(1:3,length(th),replace=TRUE)
forms1<-list(1:20,21:40,c(16:25,41:50))

##design 2
gr2<-sample(1:4,length(th),replace=TRUE)
forms2<-list(1:20,11:30,21:40,31:50)

##design 3
gr3<-1:length(th)
forms3<-list()
for (i in 1:length(gr)) forms3[[i]]<-sample(1:ncol(resp),20,replace=FALSE)

f<-function(gr,forms,resp) {
    resp.obs<-resp
    for (i in 1:length(gr)) {
        obs<-forms[[gr[i]]]
        test<-1:ncol(resp) %in% obs
        resp.obs[i,]<-ifelse(!test,NA,resp[i,])
    }
    resp.obs
}
ro1<-f(gr1,forms1,resp=resp)
ro2<-f(gr2,forms2,resp=resp)
ro3<-f(gr3,forms3,resp=resp)
L<-list(ro1,ro2,ro3)

f<-function(resp,b) {
    resp<-data.frame(resp)
    names(resp)<-paste("item",1:ncol(resp),sep='')
    m<-mirt::mirt(resp,1,'Rasch')
    cor(b,mirt::coef(m,IRTpars=TRUE,simplify=TRUE)$items[,2])
}
lapply(L,f,b=b)
