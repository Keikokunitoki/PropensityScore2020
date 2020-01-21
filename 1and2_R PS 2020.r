
#################### LAB 1 PScore basics, rough translation from SAS to R ###################

#if you don't yet have these packages, be sure to install first!
#install.packages("sas7bdat")
library(sas7bdat)
datab <- read.sas7bdat(file.choose()) #select data file from whereever you have stored it
names(datab)<-tolower(names(datab)) 

str(datab,give.attr=FALSE) #lists the variables in the dataset

apply(datab,2,FUN=summary, nmiss) #shows the min, max, means, and # of missing

# Some data cleaning steps...

datab$age70<-as.numeric(datab$age>70)

datab$sumbart90<-as.numeric(datab$sumbartel>=90)

datab$rankpre[datab$rankpre>=4]<-5

datab$timeintcat<-as.numeric(datab$timeint>=4)

datab$residentq<-cut(datab$residents,breaks=quantile(datab$residents),labels=1:4,include.lowest=TRUE)

datab[,c("age5","living","sex","referral","paresis","transport","vigilanz","ward","rankpre")] <- lapply(datab[,c("age5","living","sex","referral","paresis","transport","vigilanz","ward","rankpre")],FUN=function (x) as.factor(x))


###CRUDE ASSOCIATION

# Basic 2x2 Table between treatment and outcome
table(datab$tpa,datab$death)

# Basic logistic model
mod<-glm(death~tpa,data=datab,family="binomial")
summary(mod)
exp(mod$coefficients)
exp(confint(mod))

# To get c stat, use lrm function in rms package instead of glm (install rms first if you don't yet have it)
#install.packages("rms")
library(rms)
modc<-lrm(death~tpa,data=datab)
modc
exp(modc$coefficients)
exp(coef(modc)[2]) # We care about tpa estimate, (same as above, glm)
exp(confint.default(modc))

# install.packages("pROC") #another method to get C stat from glm
library(pROC)
auc(roc(mod$y,predict(mod,type="response")))



#MOVING TO A MULTIVARIABLE MODEL

# Age-5yr categories-adjusted outcome model
modage5<-glm(death~tpa+age5,data=datab,family="binomial")
summary(modage5)
exp(modage5$coefficients)
exp(coef(modage5)[2]) # We care about tpa estimate
exp(confint(modage5)) # get 95% CIs

# To get c stat, use lrm function in rms package instead of glm
modage5c<-lrm(death~tpa+age5,data=datab)
modage5c
exp(modage5c$coefficients)
exp(coef(modage5c)[2]) # We care about tpa estimate, (same as above, glm)
exp(confint.default(modage5c))

# TK's OPTION FOR A TRADITIONAL MULTIPLE-ADJUSTED LOGISTIC REGRESSION MODEL
modtk<-glm(death~tpa+age5+afib+aphasia+cardiac+sex+htn+hyperchol+icu+living+rankpre+residentq+referral+paresis+prevstroke+sumbart90+transport+vigilanz+ward+timeintcat+year+timeintcat:year+age70:year,data=datab,family="binomial")
summary(modtk)
exp(coef(modtk)[2]) # We care about tpa estimate
exp(confint.default(modtk))


# install.packages("pROC") #another method to get C stat from glm
library(pROC)
auc(roc(modtk$y,predict(modtk,type="response")))

# Harrell's rms lrm function won't work here, because doesn't allow for interaction terms without the main terms in the model (age70 is the problem here)



###################################################### LAB 2

# PROPENSITY SCORE MODEL EXAMPLE
psmod<-glm(tpa~age5+afib+aphasia+cardiac+sex+htn+hyperchol+icu+living+rankpre+residentq+referral+paresis+prevstroke+sumbart90+transport+vigilanz+ward+timeintcat+year+timeintcat:year+age70:year,data=datab,family="binomial")
summary(psmod)
datab$pscore[as.numeric(names(fitted(psmod)))] <- fitted(psmod)

# some basics statistics
summary(datab$pscore)

# basics statistics by tpa
options(scipen=9)
aggregate(pscore~tpa,data=datab,FUN=summary)

# graphing propscore distributions in treated vs untreated
require(ggplot2)
ggplot(datab,aes(x=pscore,group=factor(tpa),fill=factor(tpa)))+geom_density(alpha=0.3)