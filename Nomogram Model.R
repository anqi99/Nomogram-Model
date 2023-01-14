
getwd()
setwd("D:/R work")
mydata <- read_csv("Relevance.csv")
mydata<-na.omit(mydata)
View(mydata)
names(mydata)
str(mydata)
head(mydata)
summary(mydata)
summary(mydata)
mydata$Sampling_range <-   
         factor(mydata$Sampling_range,  
         levels = c(0,1,2,3),  
         labels = c("foot_of_a_hill", "northern_side","sunny_side","hilltop")) 

str(mydata) 
table(mydata$Sampling_range)
set.seed(123)
trianandvad<- createDataPartition(y=mydata$ID,p=0.70,list=FALSE)
train <- mydata[trianandvad, ]
vadi<-mydata[-trianandvad,] 
write.csv(train, "train.csv")
write.csv(vadi, "vadi.csv")
dev = mydata[mydata$Dataset==1,]
vad = mydata[mydata$Dataset==0,]
M1<-glm(Tea_polyphenol==1~ mydata$course_of_disease,data=mydata,family=binomial)
M1
summary(M1)
cbind(coef= coef(M1),confint(M1))
exp(cbind(OR= coef(M1),confint(M1)))
M2<-glm(Tea_polyphenol==1~ mydata$hypertension,data=mydata,family=binomial)
M2
summary(M2)
M3<-glm(Tea_polyphenol==1~ mydata$Hyperlipidemia,data=mydata,family=binomial)
M3
summary(M3)
MM<-glm(Tea_polyphenol==1~ mydata$Hyperlipidemia+mydata$hypertension+
          mydata$course_of_disease,data=mydata,family=binomial)
MM
summary(MM)
cbind(coef= coef(MM),confint(MM))
exp(cbind(OR= coef(MM),confint(MM)))
uni_glm_model<-function(x){
  FML<-as.formula(paste0("Tea_polyphenol==1~",x))
  glm1<-glm(FML,data = dev,family = binomial)
  glm2<-summary(glm1)
  OR<-round(exp(coef(glm1)),2)
  SE<-round(glm2$coefficients[,2],3)  
  CI2.5<-round(exp(coef(glm1)-1.96*SE),2)
  CI97.5<-round(exp(coef(glm1)+1.96*SE),2)
  CI<-paste0(CI2.5,'-',CI97.5)
  B<-round(glm2$coefficients[,1],3)
  Z<-round(glm2$coefficients[,3],3)
  P<-round(glm2$coefficients[,4],3)
  uni_glm_model<-data.frame('characteristics'=x,
                            'B'=B,
                            'SE'=SE,
                            'OR'=OR,
                            'CI'=CI,
                            'Z' =Z,
                            'P'=P)[-1,]
  
  return(uni_glm_model)
}
variable.names<-colnames(dev)[c(4:26)]
variable.names
uni_glm<-lapply(variable.names,uni_glm_model)
uni_glm
uni_glm<-ldply(uni_glm,data.frame)
uni_glm
View(uni_glm).
write.csv(uni_glm, "uni.csv")
uni_glm1 <- uni_glm[uni_glm$P<= 0.05,]
uni_glm1
uni_glm2 <- uni_glm[uni_glm$P<= 0.1,]
uni_glm2
uni_glm$characteristics[uni_glm$P<= 0.05]
write.csv(uni_glm1, "p5.csv")
write.csv(uni_glm2, "p10.csv")
fml<- as.formula(paste0('Tea_polyphenol==1~',paste0(uni_glm$characteristics[uni_glm$P<0.05],collapse = '+'))) #P<0.05也是可以的
fml
fml<-as.formula(Tea_polyphenol == 1 ~ Altitude + Organic_matter + N + P)
fml
modelA<-glm(fml,data = dev,family=binomial)
modelA 
summary(modelA) 
modelX<-glm(Tea_polyphenol~1,data = dev,family=binomial)
modelX
modelB<-step(modelX,scope=list(upper=~ Altitude + Organic_matter + N + P,
                  lower=~1),data = dev,family=binomial,direction ="forward")
summary(modelB)
modelC<-step(modelA,direction ="backward")
summary(modelC)
modelD<-step(modelA,direction = "both")
summary(modelD)
cbind(coef=coef(modelD),confint(modelD))
exp(cbind(OR=coef(modelD),confint(modelD)))
AIC(modelA,modelB,modelC,modelD)
anova(modelA,modelB,test = "Chisq")
anova(modelA,modelC,test = "Chisq")
anova(modelA,modelD,test = "Chisq")
anova(modelB,modelC,test = "Chisq")
anova(modelB,modelD,test = "Chisq")
anova(modelC,modelD,test = "Chisq")
modelD<-step(modelX,scope=list(upper=~ Altitude + Organic_matter + N + P,
                               lower=~1),data = dev,family=binomial,direction ="forward")
modelD<-step(modelA,direction = "both")
modelD
glm3<-summary(modelD)
glm3
glm3$coefficients
OR<-round(exp(glm3$coefficients[,1]),2)
OR
SE<-round(glm3$coefficients[,2],3)
CI2.5<-round(exp(coef(modelD)-1.96*SE),2)
CI97.5<-round(exp(coef(modelD)+1.96*SE),2)
CI<-paste0(CI2.5,'-',CI97.5)
B<-round(glm3$coefficients[,1],3)
Z<-round(glm3$coefficients[,3],3)
P<-round(glm3$coefficients[,4],3)
mlogit<-data.frame( 
  'B'=B,
  'SE'=SE,
  'OR'=OR,
  'CI'=CI,
  'Z' =Z,
  'P'=P)[-1,]   #-1是指删除常数项
mlogit
View(mlogit)
names(mydata)
fml
multinames<-as.character(colnames(mydata)[c(17,19,4)])
multinames
mlogit<-data.frame('characteristics'=multinames,mlogit)
mlogit
View(mlogit)
write.csv(mlogit, "multi.csv")
final<-merge.data.frame(uni_glm,mlogit,by='characteristics',all = T,sort = T)
View(final)
write.csv(final, "final.csv")
fml8<-as.formula(Tea_polyphenol == 1 ~ Altitude + Organic_matter + N + P)
fml7<-as.formula(Tea_polyphenol == 1 ~ Altitude + Organic_matter  + P)
model8<-glm(fml8,data = dev,family = binomial(logit))
model7<-glm(fml7,data = dev,family = binomial(logit))
dev$predmodel8<- predict(newdata=dev,model8,"response")
dev$predmodel7<- predict(newdata=dev,model7,"response")
View(dev)
vad$predmodel8<- predict(newdata=vad,model8,"response")
vad$predmodel7<- predict(newdata=vad,model7,"response")
View(vad)
library(pROC)
devmodelA <- roc(Tea_polyphenol~predmodel8, data = dev,smooth=F) 
devmodelA
round(auc(devmodelA),3)
round(ci(auc(devmodelA)),3)
plot(devmodelA, print.auc=TRUE, print.thres=TRUE,main = "ROC CURVE", 
     col= "blue",print.thres.col="blue",identity.col="blue",
     identity.lty=1,identity.lwd=1)
devmodelB <- roc(Tea_polyphenol~predmodel7, data = dev,smooth=F)
round(auc(devmodelB),3)
round(ci(auc(devmodelB)),3)
plot(devmodelB, print.auc=TRUE, print.thres=TRUE,main = "ROC CURVE", col= "red",print.thres.col="red",identity.col="red",
     identity.lty=1,identity.lwd=1)
vadmodelB <- roc(Tea_polyphenol~predmodel7, data = vad,smooth=F)
plot(vadmodelB, print.auc=TRUE, print.thres=TRUE,main = "ROC CURVE", col= "red",print.thres.col="red",identity.col="red",
     identity.lty=1,identity.lwd=1)
devroc1 <- plot.roc(dev$Tea_polyphenol, dev$predmodel8, main="dev ROC", percent=TRUE, col="1")
devroc2 <- lines.roc(dev$Tea_polyphenol, dev$predmodel7, percent=TRUE, col="2")
legend("bottomright", legend=c("devmodel8", "devmodel7"), col=c("1", "2"), lwd=2)
vadroc1 <- plot.roc(vad$Tea_polyphenol, vad$predmodel8, main="vad ROC", percent=TRUE, col="1")
vadroc2 <- lines.roc(vad$Tea_polyphenol, vad$predmodel7, percent=TRUE, col="2")
legend("bottomright", legend=c("vadmodelA", "vadmodelB"), col=c("1", "2"), lwd=2)
roc.test(devroc1,devroc2)
roc.test(vadroc1,vadroc2)
val.prob(dev$predmodel8,dev$Tea_polyphenol)
val.prob(dev$predmodel7,dev$Tea_polyphenol)
val.prob(vad$predmodel8,vad$Tea_polyphenol)
val.prob(vad$predmodel7,vad$Tea_polyphenol)
source("HLtest.R") 
hl.ext2(dev$predmodel8,dev$Tea_polyphenol)
hl.ext2(dev$predmodel7,dev$Tea_polyphenol)
source("HLtest.R")
hl.ext2(vad$predmodel8,vad$Tea_polyphenol)
hl.ext2(vad$predmodel7,vad$Tea_polyphenol)
install.packages("riskRegression")
library(riskRegression)
formula<-Storage_year == 1 ~ Altitude + Organic_matter  + P
fit1<-glm(formula,data=dev,family = binomial())
xb<-Score(list("fit"=fit1),formula=Storage_year~1,
          null.model=FALSE,
          plots=c("calibration","ROC"),
          metrics=c("auc","brier"),
          B=1000,M=50,
          data=dev)
plotCalibration(xb,col="red")
fit2<-glm(fit1,data=vad,family = binomial())
xb<-Score(list("fit"=fit1),formula=Storage_year~1,
          null.model=FALSE,
          plots=c("calibration","ROC"),
          metrics=c("auc","brier"),
          B=1000,M=50,
          data=vad)
plotCalibration(xb,col="red")
mydata <- read_csv("Relevance.csv")
mydata<-na.omit(mydata)
dev = mydata[mydata$Dataset==1,]
vad = mydata[mydata$Dataset==0,]
install.packages("rmda")
library(rmda)
model_1<-decision_curve(Tea_polyphenol == 1 ~ Altitude + Organic_matter + N + P,
                        data = dev,
                        family = binomial(logit),
                        thresholds = seq(0,1,by=0.01),
                        confidence.intervals = 0.95,
                        study.design = 'case-control',
                        population.prevalence =0.5)
model_2<-decision_curve(Tea_polyphenol == 1 ~ Altitude + Organic_matter + P,
                        data = dev,
                        family = binomial(logit),
                        thresholds = seq(0,1,by=0.01),
                        confidence.intervals = 0.95,
                        study.design = 'case-control',
                        population.prevalence =0.5)
plot_decision_curve(model_1,curve.names = c('model'),
                    xlim = c(0,0.8),
                    cost.benefit.axis = FALSE,
                    col = c('red'),
                    confidence.intervals = FALSE,
                    standardize = FALSE)
plot_decision_curve(model_2,curve.names = c('model'),
                    xlim = c(0,0.8),
                    cost.benefit.axis = FALSE,
                    col = c('green'),
                    confidence.intervals = FALSE,  
                    standardize = FALSE)
fml8<-as.formula(Tea_polyphenol == 1 ~ Altitude + Organic_matter + N + P)
fml7<-as.formula(Tea_polyphenol == 1 ~ Altitude + Organic_matter  + P)
model8<-glm(fml8,data = dev,family = binomial(logit))
model7<-glm(fml7,data = dev,family = binomial(logit))
dev$predmodel8<- predict(newdata=dev,model8,"response")
dev$predmodel7<- predict(newdata=dev,model7,"response")
vad$predmodel8<- predict(newdata=vad,model8,"response")
vad$predmodel7<- predict(newdata=vad,model7,"response")
vadmodel8 <- decision_curve(Storage_year~predmodel8,
                             data = vad,
                             fitted.risk = TRUE, 
                             thresholds = seq(0, .9, by = .05),
                             bootstraps = 20000) 
plot_decision_curve(vadmodel8,curve.names = c('model_1'),
                    legend.position = "topright",
                    confidence.intervals = FALSE,    
                    standardize = FALSE) 
plot_decision_curve(vadmodel8,curve.names = c('model_1'),
                    legend.position = "topright",
                    confidence.intervals = TRUE,  
                    standardize = FALSE) 
vadmodel7 <- decision_curve(Storage_year~predmodel7,
                             data = vad,
                             fitted.risk = TRUE, 
                             thresholds = seq(0, .9, by = .05),
                             bootstraps = 2000) 
plot_decision_curve(vadmodel7, legend.position = "topright",
                    confidence.intervals = FALSE,
                    standardize = FALSE)  
plot_decision_curve( list(vadmodel8, vadmodel7), 
                     curve.names = c("model_1", "model_2"),
                     col = c("blue", "red"), 
                     confidence.intervals = FALSE,  #remove confidence intervals
                     cost.benefit.axis = FALSE, #remove cost benefit axis
                     legend.position = "topright") #remove the legend "bottomright" "topright" "none"
mydata <- read_csv("Relevance.csv")
dev = mydata[mydata$Dataset==1,]
vad = mydata[mydata$Dataset==0,]
source("dca.R")
library(rms)
library(foreign)
library(nricens)
modelA<-glm(Tea_polyphenol ~ Altitude + Organic_matter + N + P, data = dev, family = binomial(link="logit"),x=TRUE)
summary(modelA)
dev$predmodelA<- predict(newdata=dev,modelA,"response")
vad$predmodelA<- predict(newdata=vad,modelA,"response")
View(vad)
modelD <- glm(Tea_polyphenol  ~ Altitude + Organic_matter  + P, data = dev, family = binomial(link="logit"),x=TRUE)
summary(modelD)
dev$predmodelD<- predict(newdata=dev,modelD,"response")
vad$predmodelD<- predict(newdata=vad,modelD,"response")
View(dev)
dev<-as.data.frame(dev)
dca(data=dev, outcome="Tea_polyphenol",
    predictors=c("predmodelA","predmodelD"),
    smooth="TRUE", probability=c("TRUE","TRUE"),
    xstop=1) 
vad<-as.data.frame(vad)
dca(data=vad, outcome="Tea_polyphenol", 
    predictors=c("predmodelA","predmodelD"),
    smooth="TRUE", probability=c("TRUE","TRUE"),
    xstop=1) 
dca(data=vad, outcome="Tea_polyphenol", 
    predictors=c("predmodelA","predmodelD","TC"),
    smooth="TRUE", probability=c("TRUE","TRUE","TRUE"),
    xstop=1) 
library(MASS)
data.set <- birthwt
View(data.set)
model = glm(low ~ age + lwt, family=binomial(link="logit"), data=data.set)
data.set$predlow = predict(model, type="response")
dca(data=data.set, outcome="low", predictors="predlow", smooth="TRUE", xstop=0.50)               
dca(data=data.set, outcome="low", predictors=c("age", "lwt"), probability=c("FALSE", "FALSE"))   
dca(data=data.set, outcome="low", predictors="age", smooth="TRUE", xstop=0.50, probability="FALSE", intervention="TRUE")   
logodds_Brown = 0.75*(famhistory)+0.26*(age)-17.5 
data.set$phat_Brown = exp(logodds_Brown)/(1+exp(logodds_Brown)) 
dca(data=data.set, outcome="cancer", predictors="phat_Brown", xstop=0.35) 
mydata <- read_csv("Relevance.csv")
mydata<-na.omit(mydata)
dev = mydata[mydata$Dataset==1,]
vad = mydata[mydata$Dataset==0,]
library(rms)
ddist <- datadist(dev)
options(datadist='ddist')
modelA2 <- lrm(Tea_polyphenol == 1 ~ Altitude + Organic_matter + N + P,data=dev)
modelB2 <- lrm(Tea_polyphenol == 1 ~ Altitude + Organic_matter  + P,data=dev)
nomomodelA <- nomogram(modelA2,lp=F, 
                       fun=function(x)1/(1+exp(-x)),
                       fun.at=seq(0.1,1,by=0.1),
                       funlabel="Diagnostic possibility")
nomomodelB <- nomogram(modelB2,lp=F, 
                       fun=function(x)1/(1+exp(-x)),
                       fun.at=seq(0.1,1,by=0.1),
                       funlabel="Diagnostic possibility")
plot(nomomodelA)
plot(nomomodelB)
library(readr)
mydata <- read_csv("Relevance.csv")
mydata<-na.omit(mydata)
dev = mydata[mydata$Dataset==1,]
ddist <- datadist(dev)
options(datadist='ddist')
dev<-na.omit(dev)
dev<-as.data.frame(dev)
library(shiny)
library(DynNom)
library(magrittr)
modelD<-glm(Tea_polyphenol  ~ Altitude + Organic_matter  + P,data=dev,family = binomial)
DynNom(modelD,DNtitle="Nomogram",DNxlab="probability",data = dev)
setwd("D:/R work")
mydata <- read_csv("Relevance.csv")
mydata<-na.omit(mydata)
dev = mydata[mydata$Dataset==1,]
library(psych)
describe(dev)
str(dev)
View(dev)
