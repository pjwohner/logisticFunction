#########################winter telemetry data##########################
#load data
rusties <- read.table("E:/PJW_CLOUD_mar1_2016/RUBL/WINTER_PATCH_USE/ALL_YEARS_wl_pec.txt", header=TRUE, 
  sep="", na.strings="NA", dec=".", strip.white=TRUE)
attach(rusties)
names(rusties)
x <- rusties[15:30]
y <- rusties[15:30]
cor(x, y)
summary(rusties)
rusties

#############ALL YEARS combined#################################################################
library("nnet")
library(AICcmodavg)
library(SDMTools)
library(ROCR)
library(lme4)
names(rusties)#
response<-wl_pec
#test all individual variables to see which are best with random variable bird nested in site nested in year
	
	pmod01<-lmer(response~ AGE+(1 | year/site/bird),REML = FALSE)
	pmod02<-lmer(response~ log(doy)+(1 | year/site/bird),REML = FALSE)
	pmod03<-lmer(response~ sunrise+(1 | year/site/bird),REML = FALSE)
	pmod04<-lmer(response~ precipho+(1 | year/site/bird),REML = FALSE)
	pmod05<-lmer(response~ precipfor2d+(1 | year/site/bird),REML = FALSE)
	pmod06<-lmer(response~ precipfor24h+(1 | year/site/bird),REML = FALSE)
	pmod07<-lmer(response~ tempfor2d+(1 | year/site/bird),REML = FALSE)
	pmod08<-lmer(response~ tempho+(1 | year/site/bird),REML = FALSE)
	pmod09<-lmer(response~ dailymax+(1 | year/site/bird),REML = FALSE)
	pmod10<-lmer(response~ dailymin+(1 | year/site/bird),REML = FALSE)
	pmod11<-lmer(response~ doy+(1 | year/site/bird),REML = FALSE)
	pmod12<-lmer(response~ p2temp3+(1 | year/site/bird),REML = FALSE)
	pmod13<-lmer(response~ sex+(1 | year/site/bird),REML = FALSE)
	pmod14<-lmer(response~ pprev3d+(1 | year/site/bird),REML = FALSE)
	pnull<-lmer(response~ 1+(1 | year/site/bird),REML = FALSE)

#Create list of all the models
	lrModels<- (list(pmod01, pmod02, pmod03, pmod04, pmod05,pmod06,pmod07,pmod08, pmod09,pmod10,
 	pmod11, pmod12,pmod13, pmod14, pnull))

	lrNames <- c("Mod01", "Mod02", "Mod03", "Mod04", "Mod05","Mod06","Mod07","Mod08", "Mod09","Mod10",
	"Mod11","Mod12","Mod13","Mod14","Null")
##model selection table based on AICc
	aictab(cand.set = lrModels, modnames = lrNames)
	aicWt<-aictab(cand.set=lrModels, modnames=lrNames, sort=TRUE, c.hat=1)
	aicWt
#now test combinations of the top variables
response<-wl
	
	pmod01<-lmer(response~ precipho+doy+(1 | year/site/bird),REML = FALSE)
	pmod02<-lmer(response~ precipho+precipfor24h+(1 | year/site/bird),REML = FALSE)
	pmod03<-lmer(response~ precipho+tempho+(1 | year/site/bird),REML = FALSE)
	pmod04<-lmer(response~ precipho+dailymin+(1 | year/site/bird),REML = FALSE)
	pmod05<-lmer(response~ precipho+precipfor24h+tempho+(1 | year/site/bird),REML = FALSE)
	pmod06<-lmer(response~ precipho+precipfor24h+doy+(1 | year/site/bird),REML = FALSE)
	pmod07<-lmer(response~ precipho+precipfor24h+dailymin+(1 | year/site/bird),REML = FALSE)
	pmod08<-lmer(response~ precipfor24h+doy+(1 | year/site/bird),REML = FALSE)
	pmod09<-lmer(response~ precipfor24h+tempho+(1 | year/site/bird),REML = FALSE)
	pmod10<-lmer(response~ precipfor24h+dailymin+(1 | year/site/bird),REML = FALSE)
	pmod11<-lmer(response~ precipho+(1 | year/site/bird),REML = FALSE)
	pmod12<-lmer(response~ precipfor24h+(1 | year/site/bird),REML = FALSE)
	pmod13<-lmer(response~ tempho+(1 | year/site/bird),REML = FALSE)
	pmod14<-lmer(response~ dailymin+(1 | year/site/bird),REML = FALSE)
	pmod15<-lmer(response~ doy+(1 | year/site/bird),REML = FALSE)

pnull<-lmer(response~ 1+(1 | year/site/bird),REML = FALSE)

#Create list of all the models
	lrModels<- (list(pmod01, pmod02, pmod03, pmod04, pmod05,pmod06,pmod07, pmod08, pmod09,pmod10,pmod11, pmod12, pmod13,pmod14,pmod15,pnull))

	lrNames <- c("Mod01", "Mod02", "Mod03", "Mod04", "Mod05","Mod06","Mod07","Mod08", "Mod09","Mod10","Mod11","Mod12","Mod13","Mod14","Mod15",
"Null")
##model selection table based on AICc
	aictab(cand.set = lrModels, modnames = lrNames)
	aicWt<-aictab(cand.set=lrModels, modnames=lrNames, sort=TRUE, c.hat=1)
	aicWt

summary(pmod09) 
#confidence set of models given AICc values. Multiple methods, look up help
#if you want to show all models, change level to 1
confset(cand.set=lrModels, modnames=lrNames, level=0.95) 
#change to level=1 to see all models


#Step 4, get parameter estimates for top model or do model averaging
#can calculate model average for each parameter of candidate set using 
#or when the top model has all the weight, just use the estimates of that model
#the exclude statement is you have to exclude the interactions in the interaction models
#parm is the parameter of interest
	modavg(cand.set=lrModels, parm="precipfor24h", modnames=lrNames, conf.level = 0.95,
	second.ord = TRUE, nobs = NULL, exclude = list("sex:precipho", "sex:precipho"), warn = TRUE,
	uncond.se = "revised")



plot(habitat,pprev3d,pch=16,ylab="pprev3d",xlab="habitat")
confint(pmod82)
#############################################################################################################
#negative estimate scaled up 10 units
var1<-exp(-0.031*10)
var1
p3<-1/var1
p3

#negative estimate
var2<-exp(-0.2)
var2

#positive estimate 
var3<-exp(0.013)
var3

#positive estimate scaled up 10 units
var4<-exp(0.013*10)
var4



######################Frequency analysis################################
count<-matrix(c(191,103,416,24,148,348),nrow=3)
count
chisq.test(count)
chisq.test(count)$expected


count<-matrix(c(402,313,277,285),nrow=2)
count
chisq.test(count)
chisq.test(count)$expected

chisq.test(c(402,313,277,285),p=c(0.25,0.25,0.25,0.25))

###################modeling differences in variables between months and years########

model<-aov(bar24~factor,data=rusties)
summary(model)
TukeyHSD(model)

tapply(bar24,factor,mean)


###############################2012 use a logistic binomial regression##########################################
install.packages(c("AICcmodavg","SDMTools","ROCR"), dependencies=TRUE, repos="http://cran.cnr.berkeley.edu/")
library(AICcmodavg)
library(SDMTools)
library(ROCR)
#these are candidate models these are glm, 
mod01<- glm(habitat5 ~ pprev3d+tempho+bar24, family=binomial(link="logit"), RUBL, na.action=na.pass)
mod01

summary(mod01)
confint(mod01)

tempho<-exp(0.0614*10)
tempho


bar24<-exp(1.91997)
bar24


pprev3d<-exp(0.4958)
pprev3d


##########################FOR 2011 data#############################################
library("nnet")
library(AICcmodavg)
library(SDMTools)
library(ROCR)
fit1 <- multinom(habitat5 ~pprev3d+tempho+p2temp3+precipfor2d,data=rusties)
##########################################################################
#find best model based on AIC.
library(AICcmodavg)


freq <- read.table("C:/RUSTY_BLACKBIRD/DISSERTATION_CHAPTERS/Winter_telemetry/response.txt", header=TRUE, 
  sep="", na.strings="NA", dec=".", strip.white=TRUE)
attach(freq)
names(freq)

glm.model = glm(freq ~ Class * Age * Sex * Survived, data=ti, family=poisson)

#do some plots first
#IF FIRST VARIABLE IS CATEGORICAL, YOU GET A BOX plot
#horizontal line is median
#bottom and top are 25th and 75th percentiles
###################pecan proportions#########################################
par (mfrow=c(2,2))	

response<-log(p_pecan)

plot(DAY[siteyear=="CORAA"],response[siteyear=="CORAA"],pch=16,ylab="Pecan",xlab="",
xlim = range(0,80, na.rm = TRUE),ylim = range(-5,0.4, na.rm = TRUE))

abline(lm(p_pecan[siteyear=="CORAA"]~DAY[siteyear=="CORAA"]),lty=2,col="black")
text(25,0.38,"CORA2009",pos =2)

plot(DAY[siteyear=="CORAB"],p_pecan[siteyear=="CORAB"],pch=16,ylab="",xlab="",
xlim = range(0,80, na.rm = TRUE),ylim = range(0,0.4, na.rm = TRUE))
abline(lm(p_pecan[siteyear=="CORAB"]~DAY[siteyear=="CORAB"]),lty=2,col="black")
text(25,0.38,"CORA2010",pos =2)

plot(DAY[siteyear=="CORAC"],p_pecan[siteyear=="CORAC"],pch=16,ylab="Pecan",xlab="DAY",
xlim = range(0,80, na.rm = TRUE),ylim = range(0,0.4, na.rm = TRUE))
abline(lm(p_pecan[siteyear=="CORAC"]~DAY[siteyear=="CORAC"]),lty=2,col="black")
text(25,0.38,"CORA2011",pos =2)

plot(DAY[siteyear=="ATHEC"],p_pecan[siteyear=="ATHEC"],pch=16,ylab="",xlab="DAY",
xlim = range(0,80, na.rm = TRUE),ylim = range(0,0.4, na.rm = TRUE))
abline(lm(p_pecan[siteyear=="ATHEC"]~DAY[siteyear=="ATHEC"]),lty=2,col="black")
text(25,0.38,"ATHE2011",pos =2)
names(rusty)
##############worm proportions###########################
par (mfrow=c(3,2))	
plot(DOY[SITE=="CORA"],p_worm[SITE=="CORA"],pch=16,ylab="Worm",xlab="",
xlim = range(1,100, na.rm = TRUE),ylim = range(0,0.7, na.rm = TRUE))
abline(lm(p_worm[SITE=="CORA"]~DOY[SITE=="CORA"]),lty=2,col="black")
text(20,0.65,"CORA",pos =2)

plot(DOY[SITE=="ATHE"],p_worm[SITE=="ATHE"],pch=16,ylab="",xlab="",
xlim = range(1,100, na.rm = TRUE),ylim = range(0,0.7, na.rm = TRUE))
abline(lm(p_worm[SITE=="ATHE"]~DOY[SITE=="ATHE"]),lty=2,col="black")
text(20,0.65,"ATHE",pos =2)

plot(DOY[SITE=="CORA"],p_troph[SITE=="CORA"],pch=16,ylab="Troph",xlab="DAY",
xlim = range(1,100, na.rm = TRUE),ylim = range(0,0.7, na.rm = TRUE))
abline(lm(p_troph[SITE=="CORA"]~DOY[SITE=="CORA"]),lty=2,col="black")
text(20,0.65,"CORA",pos =2)

plot(DOY[SITE=="ATHE"],p_troph[SITE=="ATHE"],pch=16,ylab="",xlab="DAY",
xlim = range(1,100, na.rm = TRUE),ylim = range(0,0.7, na.rm = TRUE))
abline(lm(p_troph[SITE=="ATHE"]~DOY[SITE=="ATHE"]),lty=2,col="black")
text(20,0.65,"ATHE",pos =2)

plot(DOY[SITE=="CORA"],p_mast[SITE=="CORA"],pch=16,ylab="Mast",xlab="DAY",
xlim = range(1,100, na.rm = TRUE),ylim = range(0,0.7, na.rm = TRUE))
abline(lm(p_mast[SITE=="CORA"]~DOY[SITE=="CORA"]),lty=2,col="black")
text(20,0.65,"CORA",pos =2)

plot(DOY[SITE=="ATHE"],p_mast[SITE=="ATHE"],pch=16,ylab="",xlab="DAY",
xlim = range(1,100, na.rm = TRUE),ylim = range(0,0.7, na.rm = TRUE))
abline(lm(p_mast[SITE=="ATHE"]~DOY[SITE=="ATHE"]),lty=2,col="black")
text(20,0.65,"ATHE",pos =2)


