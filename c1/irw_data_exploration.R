df<-irw::irw_fetch("chess_lnirt")
head(df) #note df is in 'long' form
resp<-irw::irw_long2resp(df,id_density_threshold = NULL) #wide form

## basic counts
dim(df)
nrow(resp)
ncol(resp)-1 #note first column of resp is the respondent id
f<-function(x) {
    n<-length(x)
    na<-sum(is.na(x))/n
    ncat<-length(unique(x[!is.na(x)]))
    c(na,ncat)
}
apply(resp[,-1],2,f)

## Correlation of item responses
mm<-apply(resp[,-1],2,mean,na.rm=TRUE)
hist(mm,xlim=c(0,1))

## Distribution of sum scores
ss<-rowSums(resp[,-1],na.rm=TRUE)
hist(ss)

## Item/total correlations
corr<-numeric()
for (i in 2:ncol(resp)) corr[i]<-cor(ss,resp[,i],use='p')
hist(corr)

