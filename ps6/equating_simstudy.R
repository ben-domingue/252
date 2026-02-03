simfun<-function(N=150,b=NULL) {
    if (is.null(b)) b<-rnorm(50,sd=.7)
    th<-rnorm(N)
    k<-outer(th,b,'-')
    p<-1/(1+exp(-k))
    resp<-p
    for (i in 1:ncol(resp)) resp[,i]<-rbinom(nrow(resp),1,p[,i])
    
    ##design 1
    forms1<-list(1:15,6:20,11:25,16:30,21:35,26:40,31:45,36:50)
    gr1<-sample(1:length(forms1),length(th),replace=TRUE)
    
    ##design 2
    forms2<-list(1:15,11:25,21:35,31:45,c(1:5,41:60))
    gr2<-sample(1:length(forms2),length(th),replace=TRUE)
    
    ##design 3
    gr3<-1:length(th)
    forms3<-list()
    for (i in 1:length(gr3)) forms3[[i]]<-sample(1:ncol(resp),15,replace=FALSE)
    
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
    sapply(L,f,b=b)
}
s<-list()
for (i in 1:10) s[[i]]<-simfun()
s<-do.call("rbind",s)
boxplot(s)
