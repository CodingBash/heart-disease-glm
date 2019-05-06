library(foreign)
library(ggplot2)
library(MASS)
library(Hmisc)
library(reshape2)
library(VGAM)
library(sure)
library(ordinal)
library(brglm2)
library("nnet")
source('~/Git-Projects/heart-disease-glm/utility.R')

setwd("C:/Users/bbecerra/Documents/Git-Projects/heart-disease-glm")
"
  Retrieved data
"
cleveland <- formatUnprocessedClevelandData()
processed.cleveland <- formatProcessedClevelandData()


### Re-level variables and variable creation
processed.cleveland$num <- relevel(factor(processed.cleveland$num), ref = 1) # Reference: No Heart Disease 
processed.cleveland$binary_num <- with(processed.cleveland, 1*(num != 0)) # BINARY RESPONSE VARIABLE: If any level of heart disease present (num > 0), set to 1
processed.cleveland$sex <- relevel(factor(processed.cleveland$sex), ref = 1) # Reference : Female
processed.cleveland$cp <- relevel(factor(processed.cleveland$cp), ref = 4) # Reference: asymptomatic chest pain
processed.cleveland$fbs <- relevel(factor(processed.cleveland$fbs), ref = 1) # Refernce: fasting blood sugar < 120 mg/dl
processed.cleveland$restecg <- relevel(factor(processed.cleveland$restecg), ref = 1) # Reference: Normal Rest ECG
processed.cleveland$exang <- relevel(factor(processed.cleveland$exang), ref = 1) # Reference: No exercise angina
processed.cleveland$slope <- relevel(factor(processed.cleveland$slope), ref = 2) # Reference: Flat
processed.cleveland$ca <- relevel(factor(processed.cleveland$ca), ref = 1) # Reference: 0 major vessels
processed.cleveland$thal <- relevel(factor(processed.cleveland$thal), ref = 1) # Reference: Normal

"
  Binary logistic regression

  - predictive performance metrics will be compared between this model and the best nominal/ordinal model

"
res.binomial <- glm(binary_num~ age + sex + cp + trestbps + chol + fbs + restecg + thalach + exang + oldpeak + slope + ca + thal,family=binomial(link="logit"),data=processed.cleveland)
summary(res.binomial)

"
  Baseline-categorical Logit Model
"
res.multinom <- multinom(factor(num)~age+factor(sex) + factor(cp) + trestbps + chol + factor(fbs) + factor(restecg) + thalach + factor(exang) + oldpeak + factor(slope) + factor(ca) + factor(thal),data=processed.cleveland)
summary(res.multinom)
logLik(res.multinom)

"
 Proportional odds model
"
res.polr=polr(factor(num)~age+factor(sex) + factor(cp) + trestbps + chol + factor(fbs) + factor(restecg) + thalach + factor(exang) + oldpeak + factor(slope) + factor(ca) + factor(thal),data=processed.cleveland)
summary(res.polr)
coefTable(res.polr)
res.polr <- backwardsSelection(res.polr)


"
  Cumulative logit model
"
res.clm=clm(factor(num)~age+factor(sex) + factor(cp) + trestbps + chol + factor(fbs) + factor(restecg) + thalach + factor(exang) + oldpeak + factor(slope) + factor(ca) + factor(thal), data=processed.cleveland)
summary(res.clm)
(res.clm.ctable <- coef(summary(res.clm)))

"
  Adjacent Categories Model
"
res.bracl <- bracl(factor(num)~age+factor(sex) + factor(cp) + trestbps + chol + factor(fbs) + factor(restecg) + thalach + factor(exang) + oldpeak + factor(slope) + factor(ca) + factor(thal), data = processed.cleveland,
      parallel = TRUE, type = "ML")
summary(res.bracl)







# Issues fitting cratio
#res.cratio <- vglm(cbind(age, factor(sex), factor(cp), trestbps, chol, factor(fbs), factor(restecg),  thalach, factor(exang), oldpeak, factor(slope), factor(ca), factor(thal)) ~ factor(num),
#                                 family=cratio(reverse=FALSE, parallel=TRUE), data=processed.cleveland)


# JUST IMPORTED TO BRGLM2 MODEL, BEGIN TO MODEL THE ADJACENT CATEGORICAL MODEL
# REMEMBER, MOST OF THE INSIGHTS SHOULD BE ON INTERPRETATION
# CONSIDER DOING PREDICTIN
