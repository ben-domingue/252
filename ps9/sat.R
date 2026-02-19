######################################
##Block A
hf<-irw::irw_fetch("imps2025_hf")
table(hf$block,df$item)
df<-hf[hf$block=="mixed test",]
df$id<-paste(df$id,df$time) ##we are treating people at different times at different points
##second definition [did shape change]
L<-split(df,df$id)
f<-function(x) {
    z<-x[,c("trial_num","stim_shape")]
    z$trial_num<-z$trial_num+1
    names(z)[2]<-"last_shape"
    y<-merge(x,z)
    y
}
L<-lapply(L,f)
df2<-data.frame(do.call("rbind",L))
df2$switch<-ifelse(df2$last_shape!=df2$stim_shape,1,0)
df<-df2

df<-df[df$time_limit==1.25,]
df<-df[df$rt<1,]
m2<-glmer(resp~switch+item+(1|id),df2,family='binomial')
df$lrt<-log(df$rt)
df<-df[is.finite(df$lrt),]
m2time<-glmer(resp~switch+item+(1|id)+lrt,df,family='binomial')


######################################
##Block B
df<-irw::irw_fetch("roar_lexical")
df$lrt<-log(df$rt)
m<-glmer(resp~(1|item)+(1|id)+lrt,df,family='binomial')

##pisa
df<-irw::irw_fetch("pisa2018_read")
df<-df[df$CNT=="ESP",] ##doing this from madrid!
f<-function(x) length(unique(x[!is.na(x)]))
nc<-by(df$resp,df$item,f)
df<-df[df$item %in% names(nc)[nc==2],]
df$lrt<-log(df$rt)
df<-df[is.finite(df$lrt),]
m<-lmer(resp~(1|item)+(1|id)+lrt,df)

##maybe a good example but need to help them interpret this coefficient
