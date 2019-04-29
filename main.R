require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
require(ordinal)

library(foreign)
library(ggplot2)
library(MASS)
library(Hmisc)
library(reshape2)
library(VGAM)
library(sure)

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

cleveland <- formatUnprocessedClevelandData()
processed.cleveland <- formatProcessedClevelandData()
res.polr=polr(factor(num)~age+factor(sex) + factor(cp) + trestbps + chol + factor(fbs) + factor(restecg) + thalach + factor(exang) + oldpeak + factor(slope) + factor(ca) + factor(thal),data=processed.cleveland)
summary(res.polr)
(ctable <- coef(summary(res.polr)))

## calculate and store p valuesfitted(res.polr)
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(res.polr)) # default method gives profiled CIs
resids(res.polr)




fit <- vglm(cbind(age, sex, cp, trestbps, chol, fbs, restecg, thalach, exang, oldpeak, slope, ca, thal) ~ num, family=cumulative(parallel=TRUE), data=processed.cleveland)
res.polr=polr(factor(num)~age+factor(sex) + factor(cp) + trestbps + chol + factor(fbs) + factor(restecg) + thalach + factor(exang) + oldpeak + factor(slope) + factor(ca) + factor(thal),weights=frequency,data=car)
summary(res.polr)
fitted(res.polr)
res.clm=clm(factor(response)~factor(age)+factor(sex),weights=frequency,data=car)
summary(res.clm)
fitted(res.clm)
confint(res.clm, type = "Wald")