library(psych)## will require psych package, use install.packages("psych") if not available

df<-irw::irw_fetch('lessr_mach4') ##data from an assessment of Machiavellianism

resp <- irw::irw_long2resp(df) ##reformat long data to wide
resp$id <- NULL
psych::alpha(resp) ##what is the reliability estimate?

## Note this warning:
##                 Some items were negatively correlated with the first principal component and probably 
## should be reversed.  

##Let's take two different perspectives on this:
## Item-item correlations
cormat<-cor(resp)
cormat[1:5,1:5] #consider items 1 (or 2) and 4.  responses are 0-6 with 6 being strongly agree
#m01: Never tell anyone the real reason you did something unless it is useful to do so
#m02: The best way to handle people is to tell them what they want to hear
#m03: One should take action only when sure it is morally right
#m04: Most people are basically good and kind

##A more conceptual perspective. https://cran.r-project.org/web/packages/lessR/refman/lessR.html#dataMach4
psych::alpha(resp,check.keys=TRUE) ##much higher alpha but not necessarily right (we've automated the reverse coding but no guarantees....)


###########################################
##let's look at a bunch of alpha values
getalpha<-function(tabnm) {
    df<-irw::irw_fetch(tabnm)
    resp <- irw::irw_long2resp(df) ##reformat long data to wide
    resp$id <- NULL
    c(tabnm,psych::alpha(resp)$total[[1]])
}

tabnames<-c("gilbert_meta_2","chess_lnirt","dd_rotation","andrich_mudfold","science_ltm")
alphavals<-lapply(tabnames,getalpha)
alphavals

