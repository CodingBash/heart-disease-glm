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
library(boot)
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

# Formulas
binomial.full.model <- binary_num ~ age + sex + cp + trestbps + chol + fbs + restecg + thalach + exang + oldpeak + slope + ca + thal
ordinal.full.model <- num ~ age + sex + cp + trestbps + chol + fbs + restecg + thalach + exang + oldpeak + slope + ca + thal

"
  Exploratory analysis
"
processed.cleveland.positive <- processed.cleveland[processed.cleveland$binary_num == 1,]
processed.cleveland.negative <- processed.cleveland[processed.cleveland$binary_num == 0,]

hist(processed.cleveland.positive$age, col=rgb(1,0,0,0.5))
hist(processed.cleveland.negative$age, col=rgb(0,0,1,0.5), add=T)
box()

hist(processed.cleveland.positive$trestbps, col=rgb(1,0,0,0.5))
hist(processed.cleveland.negative$trestbps, col=rgb(0,0,1,0.5), add=T)
box()

hist(processed.cleveland.positive$chol, col=rgb(1,0,0,0.5))
hist(processed.cleveland.negative$chol, col=rgb(0,0,1,0.5), add=T)
box()

hist(processed.cleveland.positive$thalach, col=rgb(1,0,0,0.5))
hist(processed.cleveland.negative$thalach, col=rgb(0,0,1,0.5), add=T)
box()

hist(processed.cleveland.positive$oldpeak, col=rgb(1,0,0,0.5))
hist(processed.cleveland.negative$oldpeak, col=rgb(0,0,1,0.5), add=T)
box()

"
  Binary logistic regression

  - predictive performance metrics will be compared between this model and the best nominal/ordinal model

  INTERPRETATION
    - 

"
res.binomial.full <- glm(binomial.full.model, family=binomial(link="logit"),data=processed.cleveland)
res.binomial.none <- glm(binary_num ~ 1,family=binomial(link="logit"),data=processed.cleveland)
res.binomial.backward <- backwardsSelection(res.binomial.full)
res.binomial.forward <- forwardsSelection(res.binomial.none)

summary(res.binomial.full)
summary(res.binomial.backward)
summary(res.binomial.forward)

formula(res.binomial.full)
formula(res.binomial.backward)
formula(res.binomial.forward)

res.binomial.backward.residuals.std <- rstandard(res.binomial.backward)

exp(res.binomial.backward$coefficients) # INTERPRETATION: Odds of getting heart disease

"
  Baseline-categorical Logit Model
"
res.multinom.full <- multinom(ordinal.full.model, data=processed.cleveland)
summary(res.multinom.full)
res.multinom.full.backward <- backwardsSelection(res.multinom.full)
res.multinom.full.backward.residuals <- resid(res.multinom.full.backward)


exp(summary(res.multinom.full.backward)$coefficients) # INTERPRETATION: Odds of getting severity of heart disease relative to getting no heart disease

"
 Proportional odds model
"
res.polr.full <- polr(ordinal.full.model, data=processed.cleveland)
summary(res.polr.full)
res.polr.full.backward <- backwardsSelection(res.polr.full)
res.polr.full.backward.residuals <- resids(res.polr.full.backward)

exp(res.polr.full.backward$coefficients) # INTERPRETATION: Odds of following into a heart disease severity or lower

"
  IGNORE: Cumulative logit model
"
#res.clm.full <- clm(ordinal.full.model, data=processed.cleveland)
#summary(res.clm.full)
#res.clm.full.residuals <- resids(res.clm.full)
#(res.clm.ctable <- coef(summary(res.clm)))


"
  Adjacent Categories Model
    - poor AIC
"
res.bracl.full <- bracl(ordinal.full.model, data = processed.cleveland, parallel = TRUE, type = "ML")
summary(res.bracl.full)
exp(summary(res.bracl.full)$coefficients[,1]) # INTERPRETATION: Odds of getting one severity over the lower one 





# Issues fitting cratio
#res.cratio <- vglm(cbind(age, factor(sex), factor(cp), trestbps, chol, factor(fbs), factor(restecg),  thalach, factor(exang), oldpeak, factor(slope), factor(ca), factor(thal)) ~ factor(num),
#                                 family=cratio(reverse=FALSE, parallel=TRUE), data=processed.cleveland)


# JUST IMPORTED TO BRGLM2 MODEL, BEGIN TO MODEL THE ADJACENT CATEGORICAL MODEL
# REMEMBER, MOST OF THE INSIGHTS SHOULD BE ON INTERPRETATION
# CONSIDER DOING PREDICTIN
