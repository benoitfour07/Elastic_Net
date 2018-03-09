#############################################################
##                                                         ##
##   elastic.R                                             ##
##                                                         ##
##               Author: Benoit Four                       ##
##                                                         ##
##                                     Date: March 2018    ##
##                                                         ##
#############################################################

# workspace cleanup
rm(list=ls())

# libraries
library(glmnet)
library(caTools)
library(caret)
library(Metrics)

#Load data
setwd("A:\\00_Personal\\Four_Benoit\\R\\POC-ElasticNet")
data <- read.csv2("seve.csv",sep=",")
BI <- subset(data, data$INC_MODEL_BI!=0)

##################### MAIN FUNCTIONS #############################

###########CUT AND CAP FUNCTIONS #############

cap_inc <- function(data,amount){
  data$INC <- as.numeric(data$INC)
  data$INC <- pmin(amount,data$INC)
}

cut_inc <- function(data,amount){
  data$INC <- as.numeric(data$INC)
  data$INC <- pmin(amount,data$INC)
  data <- subset(data, data$INC!=amount)
}

########### 10 fold validation with random variable #############

#We suppose the dataset is already filtered, cutted/capped
predict_elnet <- function(dataf,alpha){
  
  l = nrow(dataf)
  indice <- runif(l)*10 
  indice <- trunc(indice)
  dataf <- data.frame(dataf,indice)
  dataf <- dataf[order(indice),]
  #BI$indice <- NULL
  dataf_pred=data.frame()
    for (i in 0:9){
  
      dataf_train <- subset(dataf, dataf$indice!=i)
      dataf_train_factors <- model.matrix(INC~PER_LicenseColor+PER_DriverClause+LOB+BEH_Usage,data=dataf_train) #turn qualitative variables into dummy variables
      x <-as.matrix(data.frame(dataf_train$PER_InsAge,dataf_train$CLA_BM_Num,dataf_train_factors))
      y=dataf_train$INC
      y <- as.numeric(y)
      fit = cv.glmnet(x,y,family="gaussian",alpha=0,intercept = FALSE)
  
      dataf_test <- subset(dataf, dataf$indice==i)
      dataf_test_pred_factors <- model.matrix(INC~PER_LicenseColor+PER_DriverClause+LOB+BEH_Usage,data=dataf_test) #turn qualitative variables into dummy variables
      x1 <-as.matrix(data.frame(dataf_test$PER_InsAge,dataf_test$CLA_BM_Num,dataf_test_pred_factors))
      inc_pred = predict(fit,x1)
      inc_pred = cbind(inc_pred,dataf_test$POL_NUM)
      dataf_pred =rbind(dataf_pred,inc_pred)
    }

  dataf_real_inc = as.numeric(BI$INC)
  dataf_real_pol_number = dataf$POL_NUM
  dataf_pred =cbind(dataf_pred,dataf_real_inc,dataf_real_pol_number)
  colnames(dataf_pred)[1] <- "BI_pred_inc"
  colnames(dataf_pred)[2] <- "BI_pred_pol_number"
  return(dataf_pred)
}

#test
BI <- subset(data, data$INC_MODEL_BI!=0)
C = predict_elnet(BI,0.5)
rmse(C[1],C[3])


#################### TEST / VALIDATION PART #####################
BI <- subset(data, data$INC_MODEL_BI!=0)
l = nrow(BI)
indice <- runif(l)*10 
indice <- trunc(indice)
BI <- data.frame(BI,indice)
BI <- BI[order(indice),]
#BI$indice <- NULL

  BI_train <- subset(BI, BI$indice!=i)
  BI_train_factors <- model.matrix(INC~PER_LicenseColor+PER_DriverClause+LOB+BEH_Usage,data=BI_train) #turn qualitative variables into dummy variables
  x <-as.matrix(data.frame(BI_train$PER_InsAge,BI_train$CLA_BM_Num,BI_train_factors))
  y=BI_train$INC
  y <- as.numeric(y)
  fit = cv.glmnet(x,y,family="gaussian",alpha=0,intercept = FALSE)
  
  BI_test <- subset(BI, BI$indice==i)
  BI_test_pred_factors <- model.matrix(INC~PER_LicenseColor+PER_DriverClause+LOB+BEH_Usage,data=BI_test) #turn qualitative variables into dummy variables
  x1 <-as.matrix(data.frame(BI_test$PER_InsAge,BI_test$CLA_BM_Num,BI_test_pred_factors))
  inc_pred = predict(fit,x1)
  inc_pred = cbind(inc_pred,BI_test$POL_NUM)
  BI_pred =rbind(BI_pred,inc_pred)


BI_real_inc = as.numeric(BI$INC)
BI_real_pol_number = BI$POL_NUM
BI_pred =cbind(BI_pred,BI_real_inc,BI_real_pol_number)
colnames(BI_pred)[1] <- "BI_pred_inc"
colnames(BI_pred)[2] <- "BI_pred_pol_number"
rmse(BI_pred[1],BI_pred[3])

BI <- subset(BI, BI$INC_MODEL_BI!=0)

BI_train_factors <- model.matrix(INC~PER_LicenseColor+PER_DriverClause+LOB+BEH_Usage,data=BI) #turn qualitative variables into dummy variables
