##raw data: ##https://github.com/DomSamangy/NBA_Shots_04_23

## years<-2004:2024
## L<-list()
## for (y in years) {
##     print(y)
##     x<-read.csv(paste0("NBA_",y,"_Shots.csv"))
##     id<-x$PLAYER_ID
##     item<-'trial'
##     resp<-ifelse(x$EVENT_TYPE=="Made Shot",1,0)
##     lx<-x$LOC_X
##     ly<-x$LOC_Y
##     dist<-sqrt(lx^2+ly^2)
##     by(dist,x$SHOT_TYPE,summary)
##     threept<-ifelse(x$SHOT_TYPE=="3PT Field Goal",1,0)
##     ##game date
##     year<-x$SEASON_1
##     date<-as.numeric(as.POSIXct(x$GAME_DATE, format="%m-%d-%Y"))
##     ##
##     quarter<-x$QUARTER
##     mins<-as.numeric(x$MINS_LEFT+x$SECS_LEFT/60)
##     gameclock<-12*(quarter-1)+12-mins
##     ##
##     L[[as.character(y)]]<-data.frame(id=id,resp=resp,trial_locx=lx,trial_locy=ly,trial_three=threept,
##                    date=date,year=year,
##                    gameclock=gameclock
##                    )
## }
## df<-data.frame(do.call("rbind",L))
## df<-df[df$gameclock<=48,]
## save(df,file="nbashots_samangy.Rdata")

load("nbashots_samangy.Rdata")
df$dist<-sqrt(df$trial_locx^2+df$trial_locy^2)
x<-df[df$trial_three==1 & df$dist>15,]
ii<-sample(1:nrow(x),10000)
plot(x$trial_locx[ii],x$trial_locy[ii])
tab<-table(x$id)
tab<-tab[tab>100]
x<-x[x$id %in% names(tab),]

library(lme4)
m<-lmer(resp~(1|id),x)
th<-ranef(m)$id
th<-th[order(th[,1]),,drop=FALSE]
m<-by(x$dist,x$id,mean)
th<-merge(th,data.frame(id=names(m),dist=as.numeric(m)),by.x=0,by.y=1)
names(th)<-c("id","th","dist")
th<-th[order(th$th,decreasing=TRUE),]
cols<-ifelse(th$id %in% th$id[1:100],'red','gray')
plot(th$dist,th$th,col=cols,pch=19)
m<-loess(th~dist,th)
points(m$x,fitted(m))

h<-head(th$id,100)
t<-tail(th$id,100)

##distance
f<-function(g,...) {
    y<-x[x$id %in% g,]
    lines(density(y$dist),...)
    }
plot(NULL,xlim=c(20,40),ylim=c(0,.2))
f(h,col='red')
f(t,col='blue')

##time
f<-function(g,...) {
    y<-x[x$id %in% g,]
    lines(density(y$gameclock),...)
    }
plot(NULL,xlim=c(0,48),ylim=c(0,.04))
f(h,col='red')
f(t,col='blue')

##time
f<-function(g,...) {
    y<-x[x$id %in% g,]
    L<-list()
    for (d in seq(.1,2,by=.1)) {
        z<-y[abs(y$gameclock-d)<=0.05,]
        L[[as.character(d)]]<-c(d,mean(z$resp))
    }
    lines(do.call("rbind",L),...)
}
plot(NULL,xlim=c(0,2),ylim=c(0,1))
f(h,col='red')
f(t,col='blue')
