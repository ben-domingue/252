tab<-list()

getpred<-function(m,resp,tmp) {
    ##get predictions from m2
    th<-fscores(m)
    z<-data.frame(id=resp$id,th=th[,1])
    z<-merge(tmp,z)
    co<-coef(m,simplify=TRUE,IRTpars=TRUE)$items
    co<-data.frame(item=rownames(co),a=co[,1],b=co[,2],c=co[,3])
    z<-merge(z,co)    
    z$p<-z$c+(1-z$c)/(1+exp(-z$a*(z$th-z$b)))
    z[,c("id","item","p")]
}

f<-function(df,holdout=TRUE,
            m1.name="Rasch",
            m2.name="2PL") {
    library(mirt)
    N<-nrow(df)
    df$test<-rbinom(N,1,.2)
    ##fit models in training data (df0)
    if (holdout) df0<-df[df$test==0,] else df0<-df
    resp<-irw::irw_long2resp(df0)
    m1<-mirt(resp[,-1],1,m1.name)
    m2<-mirt(resp[,-1],1,m2.name)
    ##now let's go back to test data
    if (holdout) df1<-df[df$test==1,] else df1<-df
    df1$item<-paste("item_",df1$item,sep='')
    tmp<-df1[,c("id","item","resp")]
    ##get predictions 
    p1<-getpred(m1,resp,tmp)
    p2<-getpred(m2,resp,tmp)
    ##arrange everything
    names(p1)[3]<-'p1'
    names(p2)[3]<-'p2'
    pred<-merge(tmp,p1)
    pred<-merge(pred,p2)
    rmse<-function(x,y) sqrt(mean((x-y)^2))
    r1<-rmse(pred$p1,pred$resp)
    r2<-rmse(pred$p2,pred$resp)
    c(r1,r2)
}

######################################################
##Overfitting example
sim_rasch <- function(n.items, n.people, mean.item.diff = 0) {
  N <- n.items * n.people
  th <- matrix(rnorm(n.people), n.people, n.items, byrow = FALSE)
  diff <- matrix(rnorm(n.items, mean = mean.item.diff), n.people, n.items, byrow = TRUE)
  kern <- exp(th - diff)
  pr <- kern/(1 + kern)
  test <- matrix(runif(N), n.people, n.items)
  resp <- ifelse(pr > test, 1, 0)
  colnames(resp) <- paste("i.", 1:ncol(resp),sep='')
  resp <- resp[rowSums(resp) != 0 & rowSums(resp) != n.items, ] # can you figure out why this might be necessary?
  resp
}
set.seed(1020)
resp<-sim_rasch(10,1000)
id<-1:nrow(resp)
L<-list()
for (i in 1:ncol(resp)) L[[i]]<-data.frame(id=id,item=colnames(resp)[i],resp=resp[,i])
df<-data.frame(do.call("rbind",L))

tab$sim_3pl<-f(df,m2.name="3PL",holdout=TRUE)
tab$sim_3pl_overfit<-f(df,m2.name="3PL",holdout=FALSE)

######################################################
df<-irw::irw_fetch('gilbert_meta_2') 
resp<-irw::irw_long2resp(df)
resp$id<-NULL
summary(colMeans(resp,na.rm=TRUE))
tab$gilbert2<-f(df)
df<-irw::irw_fetch('gilbert_meta_14') 
resp<-irw::irw_long2resp(df)
resp$id<-NULL
summary(colMeans(resp,na.rm=TRUE))
tab$gilbert14<-f(df)

tab$gilbert2_3pl<-f(irw::irw_fetch('gilbert_meta_2'),m2.name="3PL",holdout=TRUE)
tab$gilbert2_3pl_overfit<-f(irw::irw_fetch('gilbert_meta_2'),m2.name="3PL",holdout=FALSE)

##if we look at the rmses for the rasch model, which data is better described by the rasch?
##if we look at the change from 1pl to 2pl, which model change results in bigger improvement? 


z<-do.call("rbind",tab)
z*10000
