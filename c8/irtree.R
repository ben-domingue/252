##
load("ffm_AGR.Rdata")
##df<-irw::irw_fetch(""offlinefriend_bigfive.Rdata"")
df<-df[!is.na(df$resp),]
ids<-sample(unique(df$id),10000)
df<-df[df$id %in% ids,]

agr<-df ## we will create some new versions
L<-list()

##first split: 3 or something else
df<-agr
df$resp2<-ifelse(df$resp==3,0,1)
df$split<-'s1'
L$split1<-df

##second split: are you low or high on the construct
df<-agr
df<-df[df$resp!=3,]
df$resp2<-ifelse(df$resp %in% 4:5,1,0)
df$split<-'s2'
L$split2<-df

##third split: are you extreme?
df<-agr
df<-df[df$resp!=3,]
df$resp2<-ifelse(df$resp %in% c(1,5),1,0)
df$split<-'s3'
L$split3<-df

df<-data.frame(do.call("rbind",L))
library(lme4)
m1<-lmer(resp2~0+item:split+(0+split|id),df)




x<-irw::long2resp(df)
x$id<-NULL



##let's first have a model wherein you choose high/low and then it is all whether or not you prefer extreme responses
L<-list()
for (i in 1:ncol(x)) {
    r1<-ifelse(x[,i]<3,0,1) #this is low/high indicator
    r2<-ifelse(x[,i] %in% c(1,5),1,0) #note this, weird!
    df1<-data.frame(id=1:nrow(x),item=paste('item',i),node='n1',resp=r1)
    df2<-data.frame(id=1:nrow(x),item=paste('item',i),node='n2',resp=r2)
    L[[i]]<-rbind(df1,df2)
}
df<-data.frame(do.call("rbind",L))
##
library(lme4)
m3 <- lmer(resp ~ 0 + item:node + (0 + node | id), 
                                        #family = binomial, #let's use linear probability model to save time
           data = df)
m3
