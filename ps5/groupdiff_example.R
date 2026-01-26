## - Take data from group 1 and estimate item parameters. 
## - Sample some responses from group 2 that will serve as test data. 
## - Fit model in the remaining training data from group 2. 
## - Make predictions in the group 2 test data using item parameters derived from both groups (where abilities are generated using the group 2 training data). 
## - Compute the RMSE between predictions and responses. 

makeresponse<-function(x,
                       remove.nonvarying.items=TRUE,
                       remove.allNA.rows=TRUE
                       ) {
    ##make IR matrix
    nms<-unique(x$item)
    if (all(nms %in% 1:length(nms))) x$item<-paste("item_",x$item,sep='')
    ##make response matrix
    id<-unique(x$id)
    L<-split(x,x$item)
    out<-list()
    for (i in 1:length(L)) {
        z<-L[[i]]
        index<-match(z$id,id)
        resp<-rep(NA,length(id))
        resp[index]<-z$resp
        out[[i]]<-resp
    }
    resp<-do.call("cbind",out)
    resp<-data.frame(resp)
    names(resp)<-names(L)
    resp$id<-id
    if (remove.nonvarying.items) {
        nr<-apply(resp,2,function(x) length(table(x)))
        resp<-resp[,nr>1]
    }
    if (remove.allNA.rows) resp<-resp[rowSums(!is.na(resp))>1,]
    resp
}
groupdiff<-function(df,group,
                    g0=0, #guessing parameter
                    group.key=1 #group=group.key will be group 1
                    ) {
    library(imv)
    library(mirt)
    library(irw)
    ##fitting model in group 1
    ni<-ncol(df)
    s<-paste("F=1-",ni,"\nPRIOR = (1-",ni,", a1, lnorm, 0.0, 0.3)",
             sep="") 
    model<-mirt.model(s)
    df1<-df[group==group.key,]
    mod.ins<-mirt(df1,model,itemtype="2PL",method="EM",technical=list(NCYCLES=10000),guess=g0) 
    ##get test data in shape
    df2<-df[group!=group.key,]
    L<-list()
    for (i in 1:ncol(df2)) L[[i]]<-data.frame(id=rownames(df2),item=names(df2)[i],resp=df2[,i])
    df2l<-data.frame(do.call("rbind",L))
    df2l$train<-rbinom(nrow(df2l),1,.75)
    df2.tr<-makeresponse(df2l[df2l$train==1,],remove.nonvarying.items=FALSE)
    id<-df2.tr$id    
    ##predictions based on group 1
    df2.tr$id<-NULL
    th<-fscores(mod.ins,response.pattern=df2.tr)
    L<-list()
    for (i in 1:ncol(df2.tr)) {
        mm<-extract.item(mod.ins,names(df2.tr)[i])
        p<-probtrace(mm,th[,1])[,2]
        L[[i]]<-data.frame(id=id,item=names(df2.tr)[i],pr=p)
    }
    pr1<-data.frame(do.call("rbind",L))
    pred<-merge(df2l[df2l$train==0,],pr1) ##only thing you need
    ##fit model in group 2
    m0<-mirt(df2.tr,model,itemtype="2PL",method="EM",technical=list(NCYCLES=10000),guess=g0) 
    ##predictions in group 2
    th<-fscores(m0,response.pattern=df2.tr)
    L<-list()
    for (i in 1:ncol(df2.tr)) {
        mm<-extract.item(m0,names(df2.tr)[i])
        p<-probtrace(mm,th[,1])[,2]
        L[[i]]<-data.frame(id=id,item=names(df2.tr)[i],p2=p)
    }
    pr1<-data.frame(do.call("rbind",L))
    ##
    pred<-merge(pred,pr1) ##only thing you need
    pred<-pred[!is.na(pred$resp),]
    ##imv::imv.binary(pred$resp,pred$pr,pred$p2)
    c(sqrt(mean((pred$resp-pred$pr)^2)),sqrt(mean((pred$resp-pred$p2)^2)))
}

df<-irw::irw_fetch('gilbert_meta_20')
resp<-irw::irw_long2resp(df)
df<-df[,c("id","treat")]
df<-merge(resp,df)
df$id<-NULL
group<-df$treat
df$treat<-NULL
##
groupdiff(df,group,g0=0,group.key=0)

group2<-sample(group,replace=TRUE)
groupdiff(df,group2,g0=0,group.key=0)
