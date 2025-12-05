library(psych)## will require psych package, use install.packages("psych") if not available

getalpha<-function(tabnm) {
    df<-irw::irw_fetch(tabnm)
    resp <- irwpkg::irw_long2resp(df) ##reformat long data to wide
    resp$id <- NULL
    c(tabnm,psych::alpha(resp)$total[[1]])
}

tabnames<-c("gilbert_meta_2","chess_lnirt","dd_rotation","andrich_mudfold","science_ltm")
alphavals<-lapply(tabnames,getalpha)
alphavals
