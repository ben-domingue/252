## b<-rnorm(20)
## b<-matrix(b,10,2)
## b<-round(b,2)
## dump("b","")
b <-
structure(c(0.13, -0.48, -0.44, 0.46, 
-0.685, -1.45, 0.565, -1.02, -0.02, 
-0.935, 1.1, -0.478, 
-0.706, -0.5, -1.62, -1.16, 
-2.18, -1.34, -0.288, 
-0.467), dim = c(10L, 2L))
th<-rnorm(1000)

simdata<-function(b,th) {
    x<-list()
    for (i in 1:nrow(b)) {
        b.item<-b[i,]
        den<-1+exp(th-b.item[1])+exp(th-b.item[1]+th-b.item[2])
        p0<-1/den
        p1<-exp(th-b.item[1])/den
        p2<-exp(th-b.item[1]+th-b.item[2])/den
        pr<-cbind(p0,p1,p2)
        resp<-numeric()
        for (j in 1:length(th)) resp[j]<-which(rmultinom(1,1,pr[j,])==1)
        x[[i]]<-resp
    }
    x<-data.frame(do.call("cbind",x))
    names(x)<-paste("item",1:ncol(x))
    x
}
resp<-simdata(b,th)-1 ##subtracting 1 to get things 0/1/2
m<-mirt::mirt(resp,1,'Rasch')
co<-mirt::coef(m,IRTpars=TRUE,simplify=TRUE)$item
par(mfrow=c(1,2))
plot(b[,1],co[,2])
plot(b[,2],co[,3])

##expected resopnse functxion
i1<-mirt::extract.item(m,1)
th<-seq(-4,4,length.out=1000)
ei<-mirt::expected.item(i1,th)
plot(th,ei)
