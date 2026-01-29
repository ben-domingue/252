dweck_items<-irw::irw_itemtext("florida_twins_dweck")
grit_items<-irw::irw_itemtext("florida_twins_grit")

dw<-irw::irw_fetch("florida_twins_dweck")
gr<-irw::irw_fetch("florida_twins_grit")
dw.resp<-irw::irw_long2resp(dw)
gr.resp<-irw::irw_long2resp(gr)

library(psych)
alpha(dw.resp[,-1])
alpha(gr.resp[,-1])

df<-data.frame(rbind(dw,gr))
table(df$item)
