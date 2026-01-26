##all possible tables with item text and dichotomous responses
## t1<-irw::irw_list_itemtext_tables()
## t2<-irw::irw_filter(n_categories=2)
## tabs<-intersect(t1,t2)
## for (nm in tabs) print(irw::irw_info(nm))


##getting the data ready
df<-irw::irw_fetch("gilbert_meta_11")
items<-irw::irw_itemtext("gilbert_meta_11")
df<-merge(items,df)
unique(df$item_text)
resp<-irw::irw_long2resp(df)

##assessing gender-related dif
f<-function(varname='cov_male') {
    library(difR)
    gr<-df[,c("id",varname)]
    names(gr)[2]<-'difvar'
    gr<-gr[!duplicated(gr$id),]
    index<-match(gr$id,resp$id)
    gr<-gr[index,]
    difLogistic(resp[,-1],type='udif',group=gr$difvar,focal.name=1)
}
f('cov_male')

