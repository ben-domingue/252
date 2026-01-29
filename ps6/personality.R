dw<-irw::irw_fetch("florida_twins_dweck")
gr<-irw::irw_fetch("florida_twins_grit")

df<-data.frame(rbind(dw,gr))
table(df$item)
