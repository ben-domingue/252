##see ?mirt::simdata
a <- matrix(rep(1,10))
diffs <- t(apply(matrix(runif(10*2, -.25, 1), 10), 1, cumsum))
diffs <- -(diffs - rowMeans(diffs))
d <- diffs + rnorm(10)
dat <- mirt::simdata(a, d, 250, itemtype = 'graded')

id<-1:nrow(dat)
L<-list()
for (i in 1:ncol(dat)) L[[i]]<-data.frame(id=id,item=colnames(dat)[i],resp=dat[,i])
df<-data.frame(do.call("rbind",L))

##Let's fit a tree where we first split 0/12 and then 1/2.
xx<-df
L<-list()

##split 1
df<-xx
df$resp2<-ifelse(df$resp==0,0,1)
df$split<-'s1'
L$split1<-df

##split 2
df<-xx
df<-df[df$resp>0,]
df$resp2<-ifelse(df$resp==2,1,0)
df$split<-'s2'
L$split2<-df

df<-data.frame(do.call("rbind",L))

library(lme4)
m1<-glmer(resp2~0+item:split+(0+split|id),df,family='binomial')
m2<-glmer(resp2~0+item:split+(1|id),df,family='binomial')
m3<-mirt::mirt(dat,1,'Rasch')

mirt::coef(m3,IRTpars=TRUE,simplify=TRUE)$items
