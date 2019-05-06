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
  backwards <- step(model,direction = "backward", trace=0)
  return(backwards)
}

forwardsSelection <- function(model){
  forwards <- step(model,direction = "forward", trace=0)
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