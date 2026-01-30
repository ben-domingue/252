df<-irw::irw_fetch("imps2025_hf")
table(df$block,df$item)
df<-df[df$block=="mixed test",]
df$id<-paste(df$id,df$time) ##we are treating people at different times at different points

##first definition of item
##df$item<-paste(df$stim_shape,df$stim_side)

##use lmer models
library(lme4)
m1<-glmer(resp~item+(1|id),df,family='binomial')
##can they see change between fall and spring?
th<-ranef(m1)$id
id<-strsplit(rownames(th)," PLUS ")
id<-data.frame(do.call("rbind",id))
names(id)<-c("id","time")
id$th<-th[,1]
z1<-id[id$time=="Fall",]
z2<-id[id$time=="Spring",]
names(z1)[3]<-"fall"
names(z2)[3]<-"spring"
z<-merge(z1[,-2],z2[,-2])
summary(z[,3]-z[,2])
cor(z[,-1])

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

m2<-glmer(resp~switch+item+(1|id),df2,family='binomial')
