# This is a revised version of the code to estimate excess deaths after COVID-19 emergency started using a semiparametric model.
#
rm(list=ls())
library(stringr)
library(gridExtra)
library(grid)
library(lubridate) #make_date
library(tidyverse) #already includes ggplot
library(mgcv) #gam
#library(MASS) #mvnorm, make package available later since `select' clashes with dplyr select

setwd("C:/Users/prame/Desktop/research/covid19")
death_cert<-read.table("State_Custom_Data200911.csv",sep=",",header=T) #data from https://gis.cdc.gov/grasp/fluview/mortality.html
all_pop<-read.table("nst-est2019-01.csv",sep=",",header=T) #https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html


##### weeks reflects the completed week for which data is available. For example, week 14 of 2020 refers to deaths until 4/13/2020

death_cert$TOTAL.DEATHS<-as.numeric(gsub(",", "", death_cert$TOTAL.DEATHS))#remove ,
death_cert$NUM.PNEUMONIA.DEATHS<-as.numeric(gsub(",", "", death_cert$NUM.PNEUMONIA.DEATHS))#remove ,
death_cert$NUM.INFLUENZA.DEATHS<-as.numeric(gsub(",", "", death_cert$NUM.INFLUENZA.DEATHS))#remove ,

#all_pop$State<-as.character(gsub(".", "", all_pop$State))#remove .
all_pop$Population<-as.numeric(gsub(",", "", all_pop$Population))#remove ,


death_cert2<-arrange(death_cert, SEASON)

date.rm<-17 #last 4 weeks removed from data set; unreliability. However, keeping original time frame of analysis - Until May 9
#state<-"California"; state2<-".California"
#state<-"Colorado"; state2<-".Colorado"
#state<-"Connecticut"; state2<-".Connecticut" 
#state<-"Florida"; state2<-".Florida"
#state<-"Illinois"; state2<-".Illinois"
#state<-"Indiana"; state2<-".Indiana"
#state<-"Louisiana"; state2<-".Louisiana"
#state<-"Massachusetts"; state2<-".Massachusetts"
#state<-"Michigan"; state2<-".Michigan"
#state<-"New Jersey"; state2<-".New Jersey"
#state<-"New York"; state2<-".New York"
#state<-"Pennsylvania"; state2<-".Pennsylvania"
#state<-"Washington"; state2<-".Washington"


nyc<-death_cert2%>%filter(SUB.AREA=="New York City")
ny<-death_cert2%>%filter(SUB.AREA==state)
lastweek<-ny$WEEK[nrow(ny)] #since it changes everytime I update the data
ny$year<-c(rep(2015,times=13),rep(2016,times=52),rep(2017,times=52),rep(2018,times=52),rep(2019,times=52),rep(2020,times=lastweek))

ny<-ny[1:(nrow(ny)-date.rm),] #last two weeks are not reliable
#CDC website states week 16 ends on April 18. Need to match that
ny$date<-as.Date(paste(ny$year, ny$WEEK, 6, sep="-"), "%Y-%U-%u")-7
who<-which(is.na(ny$date)==T) 
ny$date[who]<-c(ymd("2015-12-26"),ymd("2018-12-29"),ymd("2019-12-28"))
nyc<-nyc[1:(nrow(nyc)-date.rm),]


#### 2020 population chosen automatically below
# https://www.usapopulation.org/new-york-population/ 20060000 but this number does not make sense compared to other years (https://worldpopulationreview.com/states)
if(state=="New York"){
  pop2020<- 19440000
} else if(state=="California"){
  pop2020<- 39937489
} else if(state=="Colorado"){
  pop2020<- 5845526
} else if(state=="Connecticut"){
  pop2020<- 3563077
} else if(state=="Florida"){
  pop2020<- 21992985
} else if(state=="Illinois"){
  pop2020<- 12659682
} else if(state=="Indiana"){
  pop2020<- 6745350
} else if(state=="Louisiana"){
  pop2020<- 4645184
} else if(state=="Massachusetts"){
  pop2020<- 6976597
} else if(state=="Michigan"){
  pop2020<- 10045029
} else if(state=="New Jersey"){
  pop2020<- 8936574
} else if(state=="Pennsylvania"){
  pop2020<- 12820878 
} else if(state=="Washington"){
  pop2020<- 7797095
}

#### start date must be chosen with care. Significant excess deaths may have started even before the official first count. On the other hand
#### excess deaths may be too low at the beginning of the official counts, reducing the estimated coefficient.
#### This may be exarcerbated with the provisional counts being lower than actual deaths. We will choose start date based on inflection points on mortality charts
 

if(state=="New York"){
  start.date<-ymd("2020-03-21")
} else if(state=="California"){
  start.date<-ymd("2020-03-21")
} else if(state=="Colorado"){
  start.date<-ymd("2020-03-21")
} else if(state=="Connecticut"){
  start.date<-ymd("2020-03-21") 
} else if(state=="Florida"){
  start.date<-ymd("2020-03-28") 
} else if(state=="Illinois"){
  start.date<-ymd("2020-03-21")
} else if(state=="Indiana"){
  start.date<-ymd("2020-03-28")
}else if(state=="Pennsylvania"){
  start.date<-ymd("2020-03-28")
} else if(state=="Massachusetts"){
  start.date<-ymd("2020-03-28") 
} else if(state=="Michigan"){
  start.date<-ymd("2020-03-21") 
} else if(state=="New Jersey"){
  start.date<-ymd("2020-03-21")
} else if(state=="Louisiana"){
  start.date<-ymd("2020-03-21") 
} else if(state=="Pennsylvania"){
  start.date<-ymd("2020-03-21") 
} else if(state=="Washington"){
  start.date<-ymd("2020-02-29")
}




if(state=="New York"){
 p0<-ggplot() + 
  geom_line(data = ny, aes(x = 1:nrow(ny), y = TOTAL.DEATHS), color = "blue") +
  geom_line(data = nyc, aes(x = 1:nrow(ny), y = TOTAL.DEATHS), color = "red") +
  xlab('Time') +
  ylab('Mortality')


p0b<-ggplot() + 
  geom_line(data = ny, aes(x = 1:nrow(ny), y = NUM.PNEUMONIA.DEATHS), color = "blue") +
  geom_line(data = nyc, aes(x = 1:nrow(ny), y = NUM.PNEUMONIA.DEATHS), color = "red") +
  xlab('Time') +
  ylab('Pneumonia Mortality')

p0c<-ggplot() + 
  geom_line(data = ny, aes(x = 1:nrow(ny), y = NUM.INFLUENZA.DEATHS), color = "blue") +
  geom_line(data = nyc, aes(x = 1:nrow(ny), y = NUM.INFLUENZA.DEATHS), color = "red") +
  xlab('Time') +
  ylab('Influenza Mortality')

}

tail(ny$TOTAL.DEATHS)
tail(nyc$TOTAL.DEATHS) # it appears that NYC deaths are not a subset of NY state deaths since the pandemic in NYC made deaths higher than NY state.

tail(ny$NUM.PNEUMONIA.DEATHS)
tail(nyc$NUM.PNEUMONIA.DEATHS)

tail(ny$NUM.INFLUENZA.DEATHS)
tail(nyc$NUM.INFLUENZA.DEATHS)

# Correction by adding

if(state=="New York"){
ny$TOTAL.DEATHS<-ny$TOTAL.DEATHS+nyc$TOTAL.DEATHS
ny$NUM.PNEUMONIA.DEATHS<-ny$NUM.PNEUMONIA.DEATHS+nyc$NUM.PNEUMONIA.DEATHS
ny$NUM.INFLUENZA.DEATHS<-ny$NUM.INFLUENZA.DEATHS+nyc$NUM.INFLUENZA.DEATHS
}

#death_cert2<-death_cert2%>%mutate(date=seq(as.Date("2015/9/28"), as.Date("2020/3/23"), "weeks"))

pop<-all_pop%>%filter(State==state2)


tmp <- bind_rows(data.frame(date = make_date(year = 2010:2019, month = 7, day = 2), 
                            pop = pop$Population),
           data.frame(date = make_date(year=2020, 
                                             month=7, 
                                             day = 2),
                            pop = pop2020)) 
tmp <- approx(tmp$date, tmp$pop, xout=ny$date, rule = 2)
predicted_pop <- data.frame(date = tmp$x, pop = tmp$y)


ny<-tbl_df(ny)
ny <- ny %>% left_join(predicted_pop, by = "date") %>%
  mutate(rate = TOTAL.DEATHS/pop*52*1000)

p1 <- qplot(date, rate, data=ny, geom = "line")+
scale_x_date(date_labels = "%b %y", date_breaks = "1 months") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  #ggtitle("Mortality rate") + 
  ylab("Mortality Rate for State") + xlab("Date")
p2 <- qplot(date, pop, data=predicted_pop, geom = "line")+
scale_x_date(date_labels = "%b %y", date_breaks = "1 months") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  ylab("State Population") + xlab("Date")
grid.arrange(p2, p1, ncol=2)

#popandMortalityrate (after expanding screen in R)





dn<-1:nrow(ny) #index of time
#mf<-ifelse(ny$year==2020&ny$WEEK>=9&ny$WEEK<10,1,0)#after first covid19 death
#mm<-ifelse(ny$year==2020&ny$WEEK>=10&ny$WEEK<=lastweek,1,0)
#ma<-ifelse(ny$year==2020&ny$WEEK==13,1,0)


wm<-ifelse(ny$date>=start.date,1,0)#ifelse(ny$year==2020&ny$WEEK>=12&ny$WEEK<=lastweek,1,0) #week 9 for US, 12 for NY


### stay at home order would decrease traffic accident deaths, dates of orders by state found at: http://covid19.healthdata.org/united-states-of-america/new-york
### NY: 3/22. HOWEVER, there are less than one fatal traffic accident death a day in NY. Most accidental deaths are due to poisoning.


############## Poisson model
lpop<-log(ny$pop)


Time<-(dn - min(dn))/(max(dn) - min(dn))
#yr<-Time # this way, the year progress is measured continuosly and we can use np regression
yr<-as.factor(ny$year)

#chose k=10 because models for states was generally below k=10. Moreover, if too high, large jumps in covid cases (e.g. NY) may incorrectly pass as weekly effects
fit_n0 <- gam(TOTAL.DEATHS ~ s(WEEK,k=10,bs="cc")+yr+wm,offset=lpop,family=poisson,method="REML",dat=ny)
summary(fit_n0)
pfit_n0 <- gam(NUM.PNEUMONIA.DEATHS ~ s(WEEK,k=10,bs="cc")+yr+wm,offset=lpop,family=poisson,method="REML",dat=ny)
summary(pfit_n0)


#fit_n3 <- gam(TOTAL.DEATHS ~ s(WEEK,k=10,bs="cc")+wm+yr,offset=lpop,family=poisson,method="REML")
#summary(fit_n3) #linear yearly effect is reasonable?


out <- summary(fit_n0)
 exp(out$p.coeff[2:7]-1.96*out$se[2:7])
 exp(out$p.coeff[2:7]+1.96*out$se[2:7])





#resid.ssq <- sum(residuals(fit_n,type="pearson")^2)  ## sum of squares of Pearson resids
#resid.df <- nrow(ny)-length(coef(fit_n))        ## estimated resid df (N-p)
#resid.ssq/resid.df #some overdispersion it seems
gam(resid(fit_n0)~s(WEEK,k=40,bs="cs"),gamma=1.4,data=ny) #looks fine to me

#Overdispersed Poisson that should be considered
fit_n <- gam(TOTAL.DEATHS ~ s(WEEK,k=10,bs="cc")+yr+wm,offset=lpop,family=quasipoisson,method="REML",dat=ny)
#sum(residuals(fit_n, type = "pearson")^2) / df.residual(fit_n) 
pfit_n <- gam(NUM.PNEUMONIA.DEATHS ~ s(WEEK,k=10,bs="cc")+yr+wm,offset=lpop,family=quasipoisson,method="REML",dat=ny)
#sum(residuals(pfit_n, type = "pearson")^2) / df.residual(pfit_n)

ifit_n <- gam(NUM.INFLUENZA.DEATHS ~ s(WEEK,k=10,bs="cc")+yr+wm,offset=lpop,family=quasipoisson,method="REML",dat=ny)

# negative binomial (sticking to this model would require changing code below to determine excess counts
fit_nb <- gam(TOTAL.DEATHS ~ s(WEEK,k=10,bs="cc")+yr+wm,offset=lpop,family=nb(),method="REML",dat=ny)
sum(residuals(fit_nb, type = "pearson")^2) / df.residual(fit_nb)
pfit_nb <- gam(NUM.INFLUENZA.DEATHS ~ s(WEEK,k=10,bs="cc")+yr+wm,offset=lpop,family=nb(),method="REML",dat=ny)
sum(residuals(pfit_nb, type = "pearson")^2) / df.residual(pfit_nb)

ifit_nb <- gam(NUM.PNEUMONIA.DEATHS ~ s(WEEK,k=10,bs="cc")+yr+wm,offset=lpop,family=nb(),method="REML",dat=ny)
sum(residuals(ifit_nb, type = "pearson")^2) / df.residual(ifit_nb)

#fit_nm <- gam(death_cert$deaths ~ s(doy,k=10,bs="cc")+ms+mo+mn+yr,offset=lpop,family=poisson,method="REML") 
#anova(fit_nm,fit_n,test="Chisq") #rejects choosing simpler model
acf(residuals(fit_n,type="pearson"))
pacf(residuals(fit_n,type="pearson"))
plot(fit_n,all.terms=T)
gam.check(fit_n) #k is fine
gam(resid(fit_n)~s(yr,k=40),gamma=1.4,data=ny) # I think k is fine


exp(coef(fit_n)[2:5]) # Residents had this number times the risk of dying in March compared to pre-emergency
cor(ny$rate,fit_n$fitted)^2 

# the adaptive fit below is not better than fit_n
#fit_adnm <- gam(death_cert$deaths ~ ms+mo+mn+md+yr+s(doy,k=10,m=4,bs="ad"),offset=lpop,family=poisson,method="REML") 
# summary(fit_adnm)

#### quick look at fit of model, versus how model would look had HM not occurred
pn <- data.frame(WEEK=ny$WEEK,wm=rep(0,length(ny$WEEK)),yr=yr)
ilink <- family(fit_n)$linkinv
no_hm<- exp(lpop)*ilink(predict(fit_n,pn))
plot(ny$TOTAL.DEATHS)
lines(no_hm,col="blue")
lines(fitted(fit_n),col="red")

#approximate 95% point-wise CI
plot(fit_n, shade = TRUE, seWithMean = TRUE, residuals = TRUE, pch = 16, cex = 0.8)


#point-wise CIs
check <- ny %>% filter(date >=start.date)
fits<-fit_n$fitted[(length(fit_n$fitted)-(nrow(check)-1)):length(fit_n$fitted)]
ilink <- family(fit_n)$linkinv
ses<-ilink(predict(fit_n,se=TRUE)$se)
ses<-ses[(length(fit_n$fitted)-(nrow(check)-1)):length(fit_n$fitted)]
tmp <- ny %>% filter(date>=start.date) %>%
  mutate(fit = fits,
         se_fit = ses)

p3<-tmp%>%ggplot(aes(date, TOTAL.DEATHS)) +
  geom_point(alpha=0.5) +
  geom_ribbon(aes(date, ymin = fit - 1.96*se_fit, ymax = fit + 1.96*se_fit), fill="blue", alpha = 0.25) +
  geom_line(aes(date, fit), col = "blue") +
  scale_x_date(date_labels = "%b %y", date_breaks = "1 months") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  #ggtitle("Mortality") + 
  ylab("Weekly Mortality") + xlab("Date")
  #geom_hline(yintercept = 0, lty = 2)






#BETTER ALTERNATIVE
who<-which(ny$date >=start.date)
pd <- data.frame(WEEK=ny$WEEK,wm=wm,yr=yr)
Zp <- predict(fit_n,pd,type="lpmatrix") ## map coefs to fitted curves
pZp<-predict(pfit_n,pd,type="lpmatrix")
iZp<-predict(ifit_n,pd,type="lpmatrix")
pd2 <- data.frame(WEEK=ny$WEEK,wm=rep(0,length(n)),yr=yr)
Zp2 <- predict(fit_n,pd2,type="lpmatrix") ## map coefs to fitted curves
pZp2 <- predict(pfit_n,pd2,type="lpmatrix") ## map coefs to fitted curves
iZp2 <- predict(ifit_n,pd2,type="lpmatrix") ## map coefs to fitted curves
pd<-pd[who,];pd2<-pd2[who,]
Zp<-Zp[who,];Zp2<-Zp2[who,]
pZp<-pZp[who,];pZp2<-pZp2[who,]
iZp<-iZp[who,];iZp2<-iZp2[who,]
spop<-ny$pop[who]
gamma <- coef(fit_n);Vb <- vcov(fit_n) ## posterior mean and cov of coefs
pgamma <- coef(pfit_n);pVb <- vcov(pfit_n) ## posterior mean and cov of coefs
igamma <- coef(ifit_n);iVb <- vcov(ifit_n)
n <- 10000
library(MASS) #mvnorm
set.seed(944)
gr <- mvrnorm(n,gamma,Vb) ## simulate n rep coef vectors from post.
pgr <- mvrnorm(n,pgamma,pVb)
igr <- mvrnorm(n,igamma,iVb)


pred.all <- matrix(NA,n,nrow(Zp))
ppred.all <- matrix(NA,n,nrow(pZp))
ipred.all <- matrix(NA,n,nrow(iZp))

ilink <- family(fit_n)$linkinv
pilink <- family(pfit_n)$linkinv
for (i in 1:n) { ## loop to get diff for each sim
pred.all[i,] <- spop*ilink(Zp%*%gr[i,])  ## curve for this replicate
ppred.all[i,] <- spop*pilink(pZp%*%pgr[i,])
ipred.all[i,] <- spop*pilink(iZp%*%igr[i,])
}

pred.all_ci<-apply(pred.all, 2,quantile,probs=c(0.025,.975) )
ppred.all_ci<-apply(ppred.all, 2,quantile,probs=c(0.025,.975) )
ipred.all_ci<-apply(ipred.all, 2,quantile,probs=c(0.025,.975) )

p3S<-tmp%>%ggplot(aes(date, TOTAL.DEATHS),data=ny) +
  geom_point(alpha=0.5) +
  geom_ribbon(aes(date, ymin = pred.all_ci[1,], ymax = pred.all_ci[2,]), fill="red", alpha = 0.25) +
  geom_line(aes(date, fit), col = "blue") +
  scale_x_date(date_labels = "%b %y", date_breaks = "1 months") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  #ggtitle("Mortality") + 
  ylab("Weekly Mortality") + xlab("Date")
  #geom_hline(yintercept = 0, lty = 2)
# saved as fitperformance.png #AJE paper fill="grey44", and col="black"

### walter
p3S<-ggplot(aes(date, TOTAL.DEATHS),data=ny) +
  geom_point(alpha=0.5)+ 
 # geom_ribbon(aes(date, ymin = pred.all_ci[1,], ymax = pred.all_ci[2,]), fill="red", alpha = 0.25) +
 geom_line(aes(date, fit_n$fitted.values), col = "blue") + #fit
  scale_x_date(date_labels = "%b %y", date_breaks = "1 months") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ylab("Weekly Mortality") + xlab("Date")
p3S
###

##### CUMULATIVE EXCESS DEATHS
who<-which(ny$date >=start.date) #&ny$date <=ymd("2020-04-13"))
WEEKs<-ny$WEEK[who]
wms<-wm[who]
yrs<-yr[who]
pds<-data.frame(WEEK=WEEKs,wm=wms,yr=yrs)

Zps <- exp(lpop[who])*ilink(predict(fit_n,pds)) #contrast total cumsum(Zps)[103]to sum(fitted(fit_n)[(994):(994+102)])
pZps <- exp(lpop[who])*pilink(predict(pfit_n,pds))
iZps <- exp(lpop[who])*pilink(predict(ifit_n,pds))
pd2s <- data.frame(WEEK=WEEKs,wm=rep(0,length(who)),
yr=yrs) #Emergency never happened conditions
Zp2s <- exp(lpop[who])*ilink(predict(fit_n,pd2s)) #predicted daily deaths if HM never happened #changed to account for displacement (replaced lpop2)
pZp2s <- exp(lpop[who])*pilink(predict(pfit_n,pd2s))
iZp2s <- exp(lpop[who])*pilink(predict(ifit_n,pd2s))
all_excs<-Zps-Zp2s
pall_excs<-pZps-pZp2s
iall_excs<-iZps-iZp2s
sum(all_excs)
sum(pall_excs)
sum(iall_excs)


total_exc_c<-cumsum(all_excs)
ptotal_exc_c<-cumsum(pall_excs)
itotal_exc_c<-cumsum(iall_excs)




##### Cumulative CIs
who<-which(ny$date >=start.date)
pd <- data.frame(WEEK=ny$WEEK,wm=wm,yr=yr)
Zp <- predict(fit_n,pd,type="lpmatrix") ## map coefs to fitted curves
pZp <- predict(pfit_n,pd,type="lpmatrix") ## map coefs to fitted curves
iZp <- predict(ifit_n,pd,type="lpmatrix")
pd2 <- data.frame(WEEK=ny$WEEK,wm=rep(0,length(n)),yr=yr)
Zp2 <- predict(fit_n,pd2,type="lpmatrix") ## map coefs to fitted curves
pZp2 <- predict(pfit_n,pd2,type="lpmatrix")
iZp2 <- predict(ifit_n,pd2,type="lpmatrix")
pd<-pd[who,];pd2<-pd2[who,]
Zp<-Zp[who,];Zp2<-Zp2[who,]
pZp<-pZp[who,];pZp2<-pZp2[who,]
iZp<-iZp[who,];iZp2<-iZp2[who,]
spop<-ny$pop[who]

gamma <- coef(fit_n);Vb <- vcov(fit_n) ## posterior mean and cov of coefs
pgamma <- coef(pfit_n);pVb <- vcov(pfit_n) ## posterior mean and cov of coefs
igamma <- coef(ifit_n);iVb <- vcov(ifit_n) ## posterior mean and cov of coefs
n <- 10000 #bringing up to 100000 change CIs little from 10000 but not too long to run
library(MASS) #mvnorm
set.seed(944)
gr <- mvrnorm(n,gamma,Vb) ## simulate n rep coef vectors from post.
set.seed(1944)
pgr <- mvrnorm(n,pgamma,pVb)
set.seed(2944)
igr <- mvrnorm(n,igamma,iVb)
a.range <- matrix(NA,n,nrow(Zp))
pa.range <- matrix(NA,n,nrow(pZp))
ia.range <- matrix(NA,n,nrow(iZp))
cum1 <- rep(NA, n);pcum1 <- rep(NA, n);icum1 <- rep(NA, n)
for (i in 1:n) { ## loop to get diff for each sim
    pred.a <- spop*ilink(Zp%*%gr[i,])  ## curve for this replicate
    pred.a2 <- spop*ilink(Zp2%*%gr[i,]) #Ignore spop2, because otherwise excess will always be declining as pop decreases (too strong an assumption)
    ppred.a <- spop*pilink(pZp%*%pgr[i,])  
    ppred.a2 <- spop*pilink(pZp2%*%pgr[i,])
    ipred.a <- spop*pilink(iZp%*%igr[i,])  
    ipred.a2 <- spop*pilink(iZp2%*%igr[i,]) 
    a.range[i,] <- pred.a-pred.a2 ## range for this curve
    pa.range[i,] <- ppred.a-ppred.a2
    ia.range[i,] <- ipred.a-ipred.a2
    cum1[i]<-sum(pred.a)-sum(pred.a2)
    pcum1[i]<-sum(ppred.a)-sum(ppred.a2)
    icum1[i]<-sum(ipred.a)-sum(ipred.a2)
}

excess<-apply(a.range, 2,quantile,probs=c(0.025,.975) )
ci1 <- quantile(cum1, c(.025,.975)) #CI of excess all-cause deaths until May 9th
 pexcess<-apply(pa.range, 2,quantile,probs=c(0.025,.975) )
pci1 <- quantile(pcum1, c(.025,.975)) #CI of excess pneumonia deaths until May 9th
iexcess<-apply(ia.range, 2,quantile,probs=c(0.025,.975) )
ici1 <- quantile(icum1, c(.025,.975)) #CI of excess influenza deaths until May 9th


### by subtracting official covid deaths from ci1 I get CI for excess deaths above official covid death toll


#q<-ncol(excess)-length(total_exc_c)
#total_exc_c<-c(total_exc_c,rep(last(total_exc_c),q))
excess2<-excess

tmp %>%
  filter(date >=start.date) %>%
  mutate(l_cdf = cumsum(excess2[1,]), 
         u_cdf =cumsum(excess2[2,]),raw_cdf = total_exc_c) %>%
  ggplot() +
  geom_step(aes(date, l_cdf)) +
geom_step(aes(date, u_cdf)) +
geom_step(aes(date, raw_cdf),col="red") +
  scale_x_date(date_labels = "%b %y", date_breaks = "1 months") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  #ggtitle("Excess deaths by date") + 
  ylab("Cumulative Excess Deaths") +xlab("Date")+
  geom_hline(yintercept = 64, lty=2, col="grey")
#cumulative_excess_deaths
