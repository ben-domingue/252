df<-irw::irw_fetch("chess_lnirt")
resp<-irw::irw_long2resp(df)
elo<-df[!duplicated(df$id),]
elo<-elo[,c("id","cov_elo")]
x<-merge(elo,resp)
x$id<-NULL

##Let's look at an example item
m<-glm(item_Y17~cov_elo,x,family='binomial')
summary(m)$coef
##             Estimate Std. Error z value Pr(>|z|)
## (Intercept)    -0.51       0.13    -3.9  0.00011
## cov_elo         0.35       0.13     2.6  0.00863
##Recall the theory: we suspect higher ELO respondents are more likely to get items correct. That is the case for this item (as indicated by the 0.35>0 coefficient for cov_elo)! Now, unlike with linear regression, it isn't so easy to interpret this. Let me walk you through that and then also show you how to get something perhaps more intuitive.

## The idea is that exponentiating coefficients will give you odds ratios. So we have
exp(0.35) ## roughly 1.4
## This means that OR(x=1)/OR(x=0) should be about 1.4. Let's see:

##probability that y=1 when x=0:
1/(1+exp(-(0.51+0.35*0))) ##0.62
##probability that y=0 when x=0:
1-0.62 ##0.38
## so OR(x=0)=0.62/0.38

##probabilty that y=1 when x=1
1/(1+exp(-(0.51+0.35*1))) ##0.7
##probability that y=0 when x=0:
1-0.7 ##0.3
## so OR(x=1)=0.7/0.3

(0.7/0.3)/(0.62/0.38)
##huzzah!

##but thinking in odds ratios isn't always straightforward. i like to look at behavior in the outcome's metric. we can do that here using predict:
x0<-seq(min(x$cov_elo),max(x$cov_elo),length.out=200)
y0<-predict(m,data.frame(cov_elo=x0),
            type='response') ##this is important
plot(x0,y0,type='l',ylim=0:1)
##So for this item, a low-ELO respondent has a 20% probability of success whereas a high-ability respondent has a 60% probability of success.

##for fun, let's look at another item. Try to redo the above with item_Y18 and see how it varies! 




