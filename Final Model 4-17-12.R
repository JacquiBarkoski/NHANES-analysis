###Compiled Group Code#####
library(foreign)
library(stats)
library(splines)
library(epicalc)
library(nlme)
library(mgcv)
library(survey)


setwd('/Users/Muna/Desktop/UMich/EHS675 Env Epid/project')
knh<-read.xport("knh.xport")
getwd()
names(knh)
names(knh)<-tolower(names(knh))
names(knh)
save(knh, file='knh.rda')
load('knh.rda')

##entire set without 4 missing datasets##
knh2<-na.omit(knh) 
attach(knh2)

###Creating Pulse pressure variable###
knh2$pp<-sbp-dbp
summ(knh2$pp)
pp<-numeric(knh2$pp)

#recoding smoking variables: 1=never, 2=former, 3=current#
knh2$smk2 [smk==1]<-3
knh2$smk2 [smk==2]<-2
knh2$smk2 [smk==3]<-1
attach(knh2)
names(knh2)
summ(smk2)
tab1(smk2)



####specifying design effect#####

#full dataset#
cdsn<-svydesign(id=~psu, strata=~kstrata, weights=~wt_hm, data=knh2, nest=T)
options(survey.lonely.psu="certainty")


dim(knh2)
knh2a<-knh2[-c(761),] ###for some reason the numbers don't match but by deleting 761 we are actually deleting point 763 (outlier)##
dim(knh2a)
knh2a[760:765,]
knh2[760:765,]

cdsn0<-svydesign(id=~psu, strata=~kstrata, weights=~wt_hm, data=knh2a, nest=T)
options(survey.lonely.psu="certainty")

####################Diastolic BP####################
attach(knh2a)

dbp.logsvyfinal0<-svyglm(dbp~log(bcd)+age+I(age^2)+factor(sex)+waist+factor(income)+ factor(alcohol)+factor(smk2), cdsn0)
summary(dbp.logsvyfinal0)
par(mfrow=c(2,2))
plot(dbp.logsvyfinal0)

fit.dbp.logsvyfinal0<-predict(dbp.logsvyfinal0, type="terms", se.fit=T)
fit.dbp.logsvyfinal0$fit[1:10,]

lo.dbp.logsvyfinal0<-fit.dbp.logsvyfinal0$fit[,1]-1.96*fit.dbp.logsvyfinal0$se.fit[,1]
hi.dbp.logsvyfinal0<-fit.dbp.logsvyfinal0$fit[,1]+1.96*fit.dbp.logsvyfinal0$se.fit[,1]

summary(lo.dbp.logsvyfinal0)
summary(hi.dbp.logsvyfinal0)

o<-order(bcd)
plot(bcd[o], fit.dbp.logsvyfinal0$fit[,1][o], type="l", ylim=c(-5.1,3.1),
ylab="Changes in DBP", xlab="Blood Cadmium")
lines(bcd[o], lo.dbp.logsvyfinal0[o], lty=2, col=2)
lines(bcd[o], hi.dbp.logsvyfinal0[o], lty=2, col=2)

res.dbp.logsvyfinal0<-residuals(dbp.logsvyfinal0)
fitted.dbp.logsvyfinal0<-fitted(dbp.logsvyfinal0)
hist(residuals(dbp.logsvyfinal0),nclass=20)

dbp.logsvyfinal<-exp((log(2))*summary(dbp.logsvyfinal0)$coef[2,1])
lo95.dbp.logsvyfinal<- exp((log(2))*(summary(dbp.logsvyfinal0)$coef[2,1]-1.96*summary(dbp.logsvyfinal0)$coef[2,2]))
hi95.dbp.logsvyfinal<- exp((log(2))*(summary(dbp.logsvyfinal0)$coef[2,1]+1.96*summary(dbp.logsvyfinal0)$coef[2,2]))
dbp.logsvyfinal
hi95.dbp.logsvyfinal
lo95.dbp.logsvyfinal

###################Systolic BP#######################
sbp.logsvyfinal0<-svyglm(sbp~log(bcd)+age+I(age^2)+factor(sex)+waist+factor(income)+ factor(alcohol)+factor(smk2), cdsn0)
summary(sbp.logsvyfinal0)
par(mfrow=c(2,2))
plot(sbp.logsvyfinal0)

fit.sbp.logsvyfinal0<-predict(sbp.logsvyfinal0, type="terms", se.fit=T)
fit.sbp.logsvyfinal0$fit[1:10,]

lo.sbp.logsvyfinal0<-fit.sbp.logsvyfinal0$fit[,1]-1.96*fit.sbp.logsvyfinal0$se.fit[,1]
hi.sbp.logsvyfinal0<-fit.sbp.logsvyfinal0$fit[,1]+1.96*fit.sbp.logsvyfinal0$se.fit[,1]

summary(lo.sbp.logsvyfinal0)
summary(hi.sbp.logsvyfinal0)

o<-order(bcd)
plot(bcd[o], fit.sbp.logsvyfinal0$fit[,1][o], type="l", ylim=c(-6.6,4.0),
ylab="Changes in SBP", xlab="Blood Cadmium")
lines(bcd[o], lo.sbp.logsvyfinal0[o], lty=2, col=2)
lines(bcd[o], hi.sbp.logsvyfinal0[o], lty=2, col=2)

res.sbp.logsvyfinal0<-residuals(sbp.logsvyfinal0)
fitted.sbp.logsvyfinal0<-fitted(sbp.logsvyfinal0)
hist(residuals(sbp.logsvyfinal0),nclass=20)

sbp.logsvyfinal<-exp((log(2))*summary(sbp.logsvyfinal0)$coef[2,1])
lo95.sbp.logsvyfinal<- exp((log(2))*(summary(sbp.logsvyfinal0)$coef[2,1]-1.96*summary(sbp.logsvyfinal0)$coef[2,2]))
hi95.sbp.logsvyfinal<- exp((log(2))*(summary(sbp.logsvyfinal0)$coef[2,1]+1.96*summary(sbp.logsvyfinal0)$coef[2,2]))
sbp.logsvyfinal
hi95.sbp.logsvyfinal
lo95.sbp.logsvyfinal

#########################Pulse Pressure#####################
pp.logsvyfinal0<-svyglm(pp~log(bcd)+age+I(age^2)+factor(sex)+bmi+factor(income)+ factor(alcohol)+factor(smk2), cdsn0)
summary(pp.logsvyfinal0)
par(mfrow=c(2,2))
plot(pp.logsvyfinal0)

fit.pp.logsvyfinal0<-predict(pp.logsvyfinal, type="terms", se.fit=T)
fit.pp.logsvyfinal0$fit[1:10,]

lo.pp.logsvyfinal0<-fit.pp.logsvyfinal0$fit[,1]-1.96*fit.pp.logsvyfinal0$se.fit[,1]
hi.pp.logsvyfinal0<-fit.pp.logsvyfinal0$fit[,1]+1.96*fit.pp.logsvyfinal0$se.fit[,1]

summary(lo.pp.logsvyfinal0)
summary(hi.pp.logsvyfinal0)

o<-order(bcd)
plot(bcd[o], fit.pp.logsvyfinal0$fit[,1][o], type="l", ylim=c(-2.9,2.0),
ylab="Changes in PP", xlab="Blood Cadmium")
lines(bcd[o], lo.pp.logsvyfinal0[o], lty=2, col=2)
lines(bcd[o], hi.pp.logsvyfinal0[o], lty=2, col=2)

res.pp.logsvyfinal0<-residuals(pp.logsvyfinal0)
fitted.pp.logsvyfinal0<-fitted(pp.logsvyfinal0)
hist(residuals(pp.logsvyfinal0),nclass=20)

pp.logsvyfinal<-exp((log(2))*summary(pp.logsvyfinal0)$coef[2,1])
lo95.pp.logsvyfinal<- exp((log(2))*(summary(pp.logsvyfinal0)$coef[2,1]-1.96*summary(pp.logsvyfinal0)$coef[2,2]))
hi95.pp.logsvyfinal<- exp((log(2))*(summary(pp.logsvyfinal0)$coef[2,1]+1.96*summary(pp.logsvyfinal0)$coef[2,2]))
pp.logsvyfinal
hi95.pp.logsvyfinal
lo95.pp.logsvyfinal

#######################Effect Modification################
knh.men<-subset(knh2a, sex==1)
knh.women<-subset(knh2a, sex==0)
knh.never<-subset(knh2a, smk==1)
knh.former<-subset(knh2a, smk==2)
knh.current<-subset(knh2a, smk==3)

#survey for MEN and Womendataset#
cdsn.men<-svydesign(id=~psu, strata=~kstrata, weights=~wt_hm, data=knh.men, nest=T)
cdsn.women<-svydesign(id=~psu, strata=~kstrata, weights=~wt_hm, data=knh.women, nest=T)
options(survey.lonely.psu="certainty")

#### specify the design effect for Smoking status####
cdsn.never<-svydesign(id=~psu, strata=~kstrata, weights=~wt_hm, data=knh.never, nest=T)
cdsn.former<-svydesign(id=~psu, strata=~kstrata, weights=~wt_hm, data=knh.former, nest=T)
cdsn.current<-svydesign(id=~psu, strata=~kstrata, weights=~wt_hm, data=knh.current, nest=T)
options(survey.lonely.psu="certainty")


