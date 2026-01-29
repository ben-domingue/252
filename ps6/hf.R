df<-irw::irw_fetch("imps2025_hf")
df<-df[df$block=="mixed test",]

##first definition
df$item1<-paste(df$resp_side,df$stim_side)
##second definition
df[,c("id","trial_num","
