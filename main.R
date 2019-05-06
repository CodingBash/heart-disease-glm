library(foreign)
library(ggplot2)
library(MASS)
library(Hmisc)
library(reshape2)
library(VGAM)
library(sure)
library(ordinal)
library(brglm2)


setwd("C:/Users/bbecerra/Documents/Git-Projects/heart-disease-glm")


formatProcessedClevelandData <- function() {
  processed.cleveland <- read.csv("data/processed.cleveland.data")
  colnames(processed.cleveland) <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal", "num")
  processed.cleveland <- as.data.frame(apply(processed.cleveland, 2, as.numeric))
  
  processed.cleveland <- as.data.frame(sapply(processed.cleveland,function(cols) {
    if(is.numeric(cols)) ifelse(is.na(cols),median(cols,na.rm=T),cols) else cols}))
  
  return(processed.cleveland)
}

  
# TODO
formatUnprocessedClevelandData <- function(){
  cleveland <- readLines("data/cleveland.data")
  
  
  matrix <- matro
  for(line in cleveland){
    print(strsplit(line, " "))
  }
  
  
  data <- list()
  curr_row <- list()
  lapply(cleveland, function(line){
    line_split <- strsplit(line, " ")
    if(line_split.contains("name")){
      line_split.remove("name")
      curr_row.append(line_split)
      data.append(curr_row)
      curr_row <- list()
    } else {
      curr_row.append(curr_row)
    }
  })
}

exploratoryAnalysis <- function(processed.cleveland) {
  # Description of what the data looks like
  head(processed.cleveland)
  
  # Histogram of the variables
  hist(processed.cleveland)
  
  # Summary statistics of dataframe
  summary(processed.cleveland)
  
}

backwardsSelection <- function(model){
  model <- res.polr
  backwards <- step(model,trace=0)
  summary(backwards)
  formula(backwards)
  (ctable <- coef(backwards))
  
  
  ## calculate and store p valuesfitted(res.polr)
  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  ## combined table
  (ctable <- cbind(ctable, "p value" = p))
  return(backwards)
}

backwardsSelection <- function(model){
  model <- res.polr
  backwards <- step(model,direction = "backwards", trace=0)
  return(backwards)
}

forwardsSelection <- function(model){
  model <- res.polr
  forwards <- step(model,direction = "forwards", trace=0)
  return(forwards)
}

coefTable <- function (model) {
  (ctable <- coef(summary(model)))
  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  (ctable <- cbind(ctable, "p value" = p))
  (ci <- confint(model, type = "Wald"))
  return(ctable)  
}

residDiagnostics <- function(resids.std) {
  qqnorm(resids.std, 
         ylab="Standardized Residuals", 
         xlab="Normal Scores", 
         main="Residuals QQ")
  hist(resids.std)
  plot(main="Residual Plot", resids.std)
}

calcStandardResiduals <- function(model){
  model.residuals <- resids(model)
  model.residuals.std <- (model.residuals - mean(model.residuals))/ sd(model.residuals)
  return(model.residuals.std)
}

"
  Retrieved data
"
cleveland <- formatUnprocessedClevelandData()
processed.cleveland <- formatProcessedClevelandData()

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
