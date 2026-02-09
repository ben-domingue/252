df<-irw::irw_fetch("frac20")
library(lme4)
df$id<-paste('id',df$id,sep='') ##will help to track ids

##A
##lme4 style model
m1<-lmer(resp~0+(1|id)+item,df)
resp<-irw::irw_long2resp(df)
m1.irt<-mirt::mirt(resp[,-1],1,'Rasch')

fe<-fixef(m1)
names(fe)<-gsub("itemitem","item",names(fe))
co<-mirt::coef(m1.irt,simplify=TRUE)$items[,2]
z<-merge(fe,co,by=0)
plot(z[,-1]) #very stylish!
abline(0,1) #but they depend on different identification assumptions

##B
##now let's focus on the Q matrix components
nms<-paste("Qmatrix__",1:8,sep='')
fm<-paste("resp~0+(1|id)+",paste(nms,collapse="+"))
m2<-lmer(as.formula(fm),df)
loadings<-t(apply(df[df$id==170,nms],1,paste,collapse='')) ##170 is arbitrary
table(loadings)

##C
##now a CDM approach
library(GDINA)
##make qmatrix
qm<-df[,grep("^Qm",names(df))]
qm<-as.data.frame(qm)
qm$item<-df$item
qm<-qm[!duplicated(qm$item),]
##ensure order is ok
if (!all(qm$item==names(resp)[-1])) {
    stop()
} else {
    qm$item<-NULL
    print("all is well")
}

m3 <- GDINA(resp[,-1],qm,model="DINA") #you'll need to install GDINA

##comparing
coef(m3) #these show the probability of a correct response for a given pattern of skills. you can see the A part of DINA in the fact that the probabilities are constant for patterns that are not all 1s
z<-personparm(m3) #here we have hard calls for whether a person has each of the 8 skills
##hard to know how to contrast theta estimates and skills but one question we can ask is about the relationship between skill acquisition and ability. 
##specifically let's look at ability estimates and average number of skills
th.irt<-mirt::fscores(m1.irt)[,1]
th.lmer<-ranef(m1)[[1]]
person<-data.frame(id=resp$id,th=th.irt,sum.skill=rowSums(z))
tmp<-data.frame(id=rownames(th.lmer),th.lmer=th.lmer[,1])
person<-merge(person,tmp)

plot(person[,-1])
cor(person[,-1])

##now let's take another perspective: what if we modeled having a specific skill as a function of theta
z<-personparm(m3)
z<-data.frame(z)
z$id<-resp$id
z<-merge(z,person)
xl<-range(person$th)
plot(NULL,xlim=xl,ylim=0:1,xlab='theta',ylab='Pr(skill=1)')
for (i in 1:8) {
    fm<-paste("A",i,"~th",sep='')
    m<-glm(formula(fm),z,family='binomial')
    xx<-seq(xl[1],xl[2],length.out=1000)
    co<-coef(m)
    yy<-1/(1+exp(-(co[1]+co[2]*xx)))
    lines(xx,yy)
}
##What do you make of this figure?

##D
##fit
AIC(m1)
AIC(m2)
AIC(m3)
