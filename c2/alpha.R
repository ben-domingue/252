df<-irw::irw_fetch("gilbert_meta_2") ##fetch data from the IRW
df$item <- paste("item_", df$item, sep = "")
resp <- irwpkg::irw_long2resp(df) ##reformat long data to wide
resp$id <- NULL
psych::alpha(resp) ##compute cronbach's alpha
