####################################################################
##                                                                ##
##   elastic.R                                                    ##
##                                                                ##
##                                                                ##
##                                                                ##
##                                            Date: March 2018    ##
##                                                                ##
####################################################################

####################################################################
##################### DATA LOADING AND PREP ########################
####################################################################


####################### workspace cleanup###########################
rm(list=ls())

####################### libraries ##################################
library(glmnet)
library(caTools)
library(caret)
library(Metrics)
library(data.table)
library(dplyr)
library(devtools)
library(makedummies)
library(ineq)
#install.packages("devtools")
#devtools::install_github("toshi-ara/makedummies")


####################### Load data ##################################

setwd("A:\\00_Personal\\Four_Benoit\\R\\POC-ElasticNet")
MAIN_DIR <- "//srvhfile1/ShareFiles/Actuarial/DD/Tech_Pricing/working_2018/sample_code"
DATA_DIR <- "//srvhsas3/DD/temporary/tech_pricing/data_prep_rm_201801"

####################### define scope ###############################

TARGET_LOB = "AUTO" # AUTO, BIKE, MOPED
TARGET_RES = "INC_MODEL_PD" # BI, PD, PA, PI, ODdrive, ODother, FBI, FPD, LE

###################### read data (time-consuming) ##################
if(FALSE){
  data_org <- fread(paste(DATA_DIR, "seve.csv", sep="/"), sep=",") %>%
    dplyr:: filter(LOB == TARGET_LOB & VAL_Rand_Model != "Validation")
}

data_org <- fread(paste(DATA_DIR, "seve.csv", sep="/"), sep=",") %>%
  dplyr:: filter(LOB == TARGET_LOB & VAL_Rand_Model != "Validation")

###################### data prep ##################################

data <- mutate_(data_org, INC=TARGET_RES) #replace INC by INC_MODEL_PD then filter by INC > 0 to filter on PD cover
data <- filter(data, INC > 0)
source(paste(MAIN_DIR, "1_data_prep.R", sep="/"))
summary(data)
str(data)

PD <- data

#################### generate dummy variables ######################
PD <- makedummies(PD, basal_level = FALSE)
PD <- select_(PD, c('-starts_with("res")')) # delete strange variables 
str(PD)

#######################################################################
########################## MAIN FUNCTIONS #############################
#######################################################################


################################ GINI ################################# 

Gini <- function(observed, predicted){
  n = length(observed)
  i = 1:n
  y = observed[order(predicted)]
  
  G = 1 / (n - 1) * (n + 1 - 2 * ( sum( (n + 1 - i) * y )  ) / sum(y) )
  return(G)
}


normalized_gini <- function(observed, predicted){
  Gini(observed, predicted) / Gini(observed,observed)
}  

####################CUT AND CAP FUNCTIONS #############################

PD_test <- PD
summary(PD_test$INC)
A = filter(PD_test, INC < 1000000)
summary(PD_test$INC)
cap_PD = PD_test
cap_PD$INC <- replace(cap_PD$INC, cap_PD$INC > 400000*1,400000*1)


############################ CAP KPI ################################
# Make sure there's no indice column before
PD$indice <- NULL
PD$indice1 <- NULL
PD$indice.2 <- NULL
#Random 10 cross validation
l = nrow(PD)
indice <- runif(l)*10 
indice <- trunc(indice)
PD <- data.frame(PD,indice)
PD <- PD[order(indice),]
#Create discrepancy
data_capped <- PD
data_capped$INC <- replace(PD$INC, PD$INC > amount,amount)
discrepancy = (PD$INC - data_capped$INC)/length(PD$INC)

#CAP Function 1: For 1 threshold return KPI indicators
cap_predict_elnet <- function(PD,alpha_value,amount){
  
  pred_inc=matrix()
  pred_coeff=matrix()
  
  for (i in 0:9){
    #Filtering on k fold + Capping on the amount
    data_train <- subset(PD, PD$indice!=i)
    data_train$INC <- replace(data_train$INC, data_train$INC > amount,amount)
    R_VAR=data_train$INC
    E_VAR <- as.matrix(data_train[,!(colnames(data_train) %in% c("RESPONSE","INC","indice","indice.1","indice.2"))])
    
    cv <- cv.glmnet(
      x=E_VAR,
      y=log(R_VAR),
      family="gaussian",
      alpha=alpha_value,
      nfolds=10,
      type.measure="deviance",
      keep=TRUE
    ) 
    
    data_test <- subset(PD, PD$indice==i)
    data_test$INC <- NULL
    E_PRED <- as.matrix(data_test[,!(colnames(data_test) %in% c("RESPONSE","INC","indice","indice.1","indice.2"))])
    pred_coeff=rbind(pred_coeff,coef(cv))
    pred_inc = rbind(pred_inc,exp(predict(cv,E_PRED)))
    
  }
  
  coeff <- pred_coeff
  inc <- pred_inc[!is.na(pred_inc)]
  rmse <- rmse(pred_inc[!is.na(pred_inc)]+discrepancy,PD$INC)
  gini <- Gini(PD$INC,pred_inc[!is.na(pred_inc)])
  ngini <- Gini(PD$INC, pred_inc[!is.na(pred_inc)]) / Gini(PD$INC,PD$INC) 
  newList <- list("coeff" = coeff,"inc" = inc, "rmse" = rmse, "gini" = gini, "ngini" = ngini)
  return(newList)
}

#Test
l = cap_predict_elnet(PD,0,2000000)
l$coeff
l$gini
l$ngini

#CAP Function 2: Loop on threshold list, return list of KPI parameters to be plooted
cap_loop_elnet <- function(PD,alpha_value){
  list_rmse = list()
  list_gini = list()
  list_ngini = list()
  for (i in list(4,8,12,13,14,15,16,17,18,19,20,21,22,23,25,30,40,80,120)){ 
    p = cap_predict_elnet(PD,alpha_value,100000*i)
    list_rmse = rbind(list_rmse,p$rmse)
    list_gini = rbind(list_gini,p$gini)
    list_ngini = rbind(list_ngini,p$ngini)
  }
  result <- list("rmseL" = list_rmse,"giniL" = list_gini, "nginiL" = list_ngini)
  return(result)
}  

#Test CAP for alpha = 1
PD_cap_alpha1 = cap_loop_elnet(PD,1)
PD_cap_alpha1
h1 = as.data.frame(PD_cap_alpha1)
h1$rmseL <- vapply(h1$rmseL, paste, collapse = ", ", character(1L))
h1$giniL <- vapply(h1$giniL, paste, collapse = ", ", character(1L))
h1$nginiL <- vapply(h1$nginiL, paste, collapse = ", ", character(1L))
write.csv2(h1,file = "PD_cap_alpha1")

#Test CAP for alpha = 0
PD_cap_alpha0 = cap_loop_elnet(PD,0)
PD_cap_alpha0
h0 = as.data.frame(PD_cap_alpha0)
h0$rmseL <- vapply(h0$rmseL, paste, collapse = ", ", character(1L))
h0$giniL <- vapply(h0$giniL, paste, collapse = ", ", character(1L))
h0$nginiL <- vapply(h0$nginiL, paste, collapse = ", ", character(1L))
write.csv2(h0,file = "PD_cap_alpha0")


#Test CAP for alpha = 0.5
PD_cap_alpha05 = cap_loop_elnet(PD,0.5)
PD_cap_alpha05
d = as.data.frame(PD_cap_alpha05)
d$rmseL <- vapply(d$rmseL, paste, collapse = ", ", character(1L))
d$giniL <- vapply(d$giniL, paste, collapse = ", ", character(1L))
d$nginiL <- vapply(d$nginiL, paste, collapse = ", ", character(1L))
write.csv2(d,file = "PD_cap_alpha05")


############################# CUT KPI #################################
#CUT Function 1: For 1 threshold return KPI indicators
cut_predict_elnet <- function(PD,alpha_value,amount){
  
  pred_inc=matrix()
  pred_coeff=matrix()
  diff=matrix()
  diff=as.matrix(filter(PD, PD$INC > amount)$INC)
  additionnal_term = (sum(diff[!is.na(diff)])/nrow(PD))
  
  for (i in 0:9){
    #Filtering on k fold + Capping on the amount
    data_train <- subset(PD, PD$indice!=i)
    data_train <- filter(data_train, INC < amount)
    R_VAR=data_train$INC
    E_VAR <- as.matrix(data_train[,!(colnames(data_train) %in% c("RESPONSE","INC","indice","indice.1","indice.2"))])
    
    cv <- cv.glmnet(
      x=E_VAR,
      y=log(R_VAR),
      family="gaussian",
      alpha=alpha_value,
      nfolds=10,
      type.measure="deviance",
      keep=TRUE
    ) 
    
    data_test <- subset(PD, PD$indice==i)
    data_test$INC <- NULL
    E_PRED <- as.matrix(data_test[,!(colnames(data_test) %in% c("RESPONSE","INC","indice","indice.1","indice.2"))])
    pred_coeff=rbind(pred_coeff,coef(cv))
    pred_inc = rbind(pred_inc,exp(predict(cv,E_PRED)))
    
  }
  
  coeff <- pred_coeff
  inc <- pred_inc[!is.na(pred_inc)]
  rmse <- rmse(pred_inc[!is.na(pred_inc)]+additionnal_term,PD$INC)
  gini <- Gini(PD$INC,pred_inc[!is.na(pred_inc)])
  ngini <- Gini(PD$INC, pred_inc[!is.na(pred_inc)]) / Gini(PD$INC,PD$INC) 
  newList <- list("coeff" = coeff,"inc" = inc, "rmse" = rmse, "gini" = gini, "ngini" = ngini)
  return(newList)
}

#Test 
PD_cut_test = cut_predict_elnet(PD,1,2000000)
PD_cut_test$coeff
PD_cut_test$gini
PD_cut_test$rmse

#CUT Function 2: Loop on threshold list, return list of KPI parameters to be plooted
cut_loop_elnet <- function(PD,alpha_value){
  list_rmse = list()
  list_gini = list()
  list_ngini = list()
  for (i in list(4,8,12,13,14,15,16,17,18,19,20,21,22,23,25,30,40,80,120)){ 
    p = cut_predict_elnet(PD,alpha_value,100000*i)
    list_rmse = rbind(list_rmse,p$rmse)
    list_gini = rbind(list_gini,p$gini)
    list_ngini = rbind(list_ngini,p$ngini)
  }
  result <- list("rmseL" = list_rmse,"giniL" = list_gini, "nginiL" = list_ngini)
  return(result)
} 

#Test CAP for alpha = 1
PD_cut_alpha1 = cut_loop_elnet(PD,1)
PD_cut_alpha1
g1 = as.data.frame(PD_cut_alpha1)
g1$rmseL <- vapply(g1$rmseL, paste, collapse = ", ", character(1L))
g1$giniL <- vapply(g1$giniL, paste, collapse = ", ", character(1L))
g1$nginiL <- vapply(g1$nginiL, paste, collapse = ", ", character(1L))
write.csv2(g1,file = "PD_cut_alpha1")

#Test CAP for alpha = 0
PD_cut_alpha0 = cut_loop_elnet(PD,0)
PD_cut_alpha0
g0 = as.data.frame(PD_cut_alpha0)
g0$rmseL <- vapply(g0$rmseL, paste, collapse = ", ", character(1L))
g0$giniL <- vapply(g0$giniL, paste, collapse = ", ", character(1L))
g0$nginiL <- vapply(g0$nginiL, paste, collapse = ", ", character(1L))
write.csv2(g0,file = "PD_cut_alpha0")

#Test CAP for alpha = 0.5
PD_cut_alpha05 = cut_loop_elnet(PD,0.5)
PD_cut_alpha05
g05 = as.data.frame(PD_cut_alpha05)
g05$rmseL <- vapply(g05$rmseL, paste, collapse = ", ", character(1L))
g05$giniL <- vapply(g05$giniL, paste, collapse = ", ", character(1L))
g05$nginiL <- vapply(g05$nginiL, paste, collapse = ", ", character(1L))
write.csv2(g05,file = "PD_cut_alpha05")


####################### TEST / VALIDATION PART ########################


