##levante
remotes::install_github("levante-framework/rlevante")
##get trials.csv via export here: https://stanford.redivis.com/datasets/68kn-csrddrz5x/tables/ztnm-414g6eryz

df<-read.csv("levante_trials.csv")
df<-df[df$task_id=="vocab",]
df<-df[df$dataset=="pilot_mpieva_de_main",]
x<-df[,c("run_id","item_uid","correct")]
names(x)<-c("id","item","resp")
x$resp<-ifelse(x$resp=="true",1,0)
resp<-irw::irw_long2resp(x)

resp$id<-NULL
cm<-colMeans(resp,na.rm=TRUE)
resp<-resp[,cm>.05 & cm<.95]

m1<-mirt::mirt(resp,1,'Rasch')
m2<-mirt::mirt(resp,1,'Rasch',guess=0.5)


##LDT data
## df<-irw::irw_fetch("roar_lexical")
## resp<-irw::irw_long2resp(df)
## resp$id<-NULL

## m1<-mirt::mirt(resp,1,'Rasch')
## m2<-mirt::mirt(resp,1,'Rasch',guess=0.5)
