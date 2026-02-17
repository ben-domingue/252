out<-list()
##
for (fn in c("AGR","CSN","EST","EXT","OPN")) {
    print(fn)
    df<-irw::irw_fetch(""offlinefriend_bigfive.Rdata"")
    
    df<-df[!is.na(df$resp),]
    ids<-sample(unique(df$id),10000)
    df<-df[df$id %in% ids,]
    
    resp<-irw::irw_long2resp(df)
    m0<-mirt::mirt(resp[,-1],1,'graded')
    co<-mirt::coef(m0,simplify=TRUE)$item
    flippers<-rownames(co)[co[,1]<0]
    df$resp<-ifelse(df$item %in% flippers,6-df$resp,df$resp)
    ##df$resp<-ifelse(df$resp>2,1,0)
    
    
    xxx<-df ## we will create some new versions
    L<-list()
    
    ##first split: 3 or something else
    df<-xxx
    df$resp2<-ifelse(df$resp==3,0,1)
    df$split<-'s1'
    L$split1<-df
    
    ##second split: high or low
    df<-xxx
    df<-df[df$resp!=3,]
    df$resp2<-ifelse(df$resp %in% 4:5,1,0)
    df$split<-'s2'
    L$split2<-df
    
    df<-data.frame(do.call("rbind",L))
    library(lme4)
    m1<-lmer(resp2~0+item:split+(1|id),df)
    m2<-lmer(resp2~0+item:split+(0+split|id),df)
    z<-VarCorr(m2)
    
    out[[fn]]<-c(AIC(m1),AIC(m2),BIC(m1),BIC(m2),attr(z$id,'correlation')[1,2])
}

tab<-do.call("rbind",out)
tab[,2]<tab[,1]
tab[,4]<tab[,3]




