library(foreign)
library(ggplot2)
library(MASS)
library(Hmisc)
library(reshape2)
library(VGAM)
library(sure)
library(ordinal)
library(brglm2)
library(tidyverse)
# library(ordinal)

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
  backwards <- step(model,trace=0)
  summary(backwards)
  formula(backwards)
  return(backwards)
}

residualDiagnostics <- function ()

cleveland <- formatUnprocessedClevelandData()
processed.cleveland <- formatProcessedClevelandData()
res.polr=polr(factor(num)~age+factor(sex) + factor(cp) + trestbps + chol + factor(fbs) + factor(restecg) + thalach + factor(exang) + oldpeak + factor(slope) + factor(ca) + factor(thal),data=processed.cleveland)
summary(res.polr)
(ctable <- coef(summary(res.polr)))

backwardsSelection(res.polr)

## calculate and store p valuesfitted(res.polr)
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(res.polr)) # default method gives profiled CIs
residuals.polr <- resids(res.polr)
residuals.polr.std <- (residuals.polr - mean(residuals.polr))/ sd(residuals.polr)

qqnorm(residuals.polr.std, 
         ylab="Standardized Residuals", 
         xlab="Normal Scores", 
         main="Old Faithful Eruptions") 
hist(residuals.polr)
plot(residuals.polr)



res.clm=clm(factor(num)~age+factor(sex) + factor(cp) + trestbps + chol + factor(fbs) + factor(restecg) + thalach + factor(exang) + oldpeak + factor(slope) + factor(ca) + factor(thal), data=processed.cleveland)
summary(res.clm)
(res.clm.ctable <- coef(summary(res.clm)))

backwardsSelection(res.clm)





res.bracl <- bracl(factor(num)~age+factor(sex) + factor(cp) + trestbps + chol + factor(fbs) + factor(restecg) + thalach + factor(exang) + oldpeak + factor(slope) + factor(ca) + factor(thal), data = processed.cleveland,
      parallel = TRUE, type = "ML")
summary(res.bracl)
(ctable.bracl <- coef(summary(res.bracl)))

backwardsSelection(res.bracl)

# Issues fitting cratio
#res.cratio <- vglm(cbind(age, factor(sex), factor(cp), trestbps, chol, factor(fbs), factor(restecg),  thalach, factor(exang), oldpeak, factor(slope), factor(ca), factor(thal)) ~ factor(num),
#                                 family=cratio(reverse=FALSE, parallel=TRUE), data=processed.cleveland)


# JUST IMPORTED TO BRGLM2 MODEL, BEGIN TO MODEL THE ADJACENT CATEGORICAL MODEL
# REMEMBER, MOST OF THE INSIGHTS SHOULD BE ON INTERPRETATION
# CONSIDER DOING PREDICTIN
