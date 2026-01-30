gr<-irw::irw_fetch("florida_twins_grit")
grit_items<-irw::irw_itemtext("florida_twins_grit")
z<-grit_items[,c("item","item_text")]
z[!duplicated(z[,1]),]
##to reverse
##  2 qgrit8  I have difficulty maintaining my focus on projects that take more than a few m…
##  7 qgrit7  I often set a goal but later choose to pursue a different one.                 
##  8 qgrit3  My interests change from year to year.                                         
## 10 qgrit5  I have been obsessed with a certain idea or project for a short time but later…
## 11 qgrit11 I become very interested in new pursuits every few months.                     
## 12 qgrit2  New ideas and projects sometimes distract me from previous ones.     
items<-paste("qgrit",c(8,7,3,5,11,2),sep='')
gr$resp<-ifelse(gr$item %in% items,6-gr$resp,gr$resp)
gr.resp<-irw::irw_long2resp(gr)
psych::alpha(gr.resp[,-1])
psych::alpha(gr.resp[,-1],check.keys=TRUE) ##note 11??

grit_items[,c("resp","option_text")] ##i am taking 'very much like me', 'mostly like me', and 'somewaht like me' and making them 1s. 
gr$resp<-ifelse(gr$resp<=3,1,0)
gr.resp<-irw::irw_long2resp(gr)
psych::alpha(gr.resp[,-1])
psych::alpha(gr.resp[,-1],check.keys=TRUE) ##note 11??

##############################################################
dw<-irw::irw_fetch("florida_twins_dweck")
dweck_items<-irw::irw_itemtext("florida_twins_dweck")
z<-dweck_items[,c("item","item_text")]
z[!duplicated(z[,1]),]
##to reverse
## 1 qdweckt1 You have a certain amount of intelligence, and you can’t really do much to cha…
## 2 qdweckt2 Your intelligence is something about you that you can’t change very much.      
## 4 qdweckt6 You can learn new things, but you can’t really change your basic intelligence. 
## 5 qdweckt4 To be honest, you can’t really change how intelligent you are.                 
items<-paste("qdweckt",c(1,2,4,6),sep='')
dw$resp<-ifelse(dw$item %in% items,7-dw$resp,dw$resp)
dw.resp<-irw::irw_long2resp(dw)
psych::alpha(dw.resp[,-1])
psych::alpha(dw.resp[,-1],check.keys=TRUE) ##note 11??

dweck_items[,c("resp","option_text")] ##i am taking strongly agree, agree, and mostly agree and making them 1s
dw$resp<-ifelse(dw$resp<=3,1,0)
dw.resp<-irw::irw_long2resp(dw)
psych::alpha(dw.resp[,-1])
psych::alpha(dw.resp[,-1],check.keys=TRUE)

##############################################################
x1<-data.frame(id=dw.resp$id,dw=rowMeans(dw.resp[,-1],na.rm=TRUE))
x2<-data.frame(id=gr.resp$id,gr=rowMeans(gr.resp[,-1],na.rm=TRUE))
x<-merge(x1,x2)
cor(x[,-1])
ids<-x$id ##just those who did both surveys

##############################################################

df<-data.frame(rbind(dw,gr))
df<-df[df$id %in% ids,]
table(df$item)
resp<-irw::irw_long2resp(df)
resp$id<-NULL

library(mirt)

s<-paste("F1=1-20,
     F2=1-20,
     PRIOR = (1-20, a1, lnorm, 0.0, 1.0),
     PRIOR = (1-20, a2, lnorm, 0.0, 1.0)",
     sep="")
model<-mirt.model(s)
m2 <- mirt(resp, model)

coef(m2,simplify=TRUE,IRTpars=TRUE)
