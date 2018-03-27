##CAP Function 1: For 1 threshold return KPI indicators
cap_predict_elnet <- function(PD,alpha_value,threshold){
  
  #Declare outputs
  pred_inc=matrix()
  pred_coeff=matrix()
  disc <- 0 
  #Random 10 cross validation
  l = nrow(PD)
  indice <- runif(l)*10 
  indice <- trunc(indice)
  PD <- data.frame(PD,indice)
  PD <- PD[order(indice),]
  
  for (i in 0:9){
    #Filtering on k fold + Capping on the threshold
    data_train <- subset(PD, PD$indice!=i)
    data_train$INC <- replace(data_train$INC, data_train$INC > threshold,threshold)
    R_VAR=data_train$INC
    E_VAR <- as.matrix(data_train[,!(colnames(data_train) %in% c("INC","indice","indice.1","indice.2"))])
    
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
    E_PRED <- as.matrix(data_test[,!(colnames(data_test) %in% c("INC","indice","indice.1","indice.2"))])
    pred_coeff=rbind(pred_coeff,coef(cv))
    pred_inc = rbind(pred_inc,exp(predict(cv,E_PRED)))
    
  }
  #Discrepancy for rmse
  disc <- sum(PD$INC < threshold, 0, PD$INC - threshold) / length(PD)
  coeff <- pred_coeff
  inc <- pred_inc[!is.na(pred_inc)]
  rmse <- rmse(PD$INC,pred_inc[!is.na(pred_inc)]+disc)
  gini <- Gini(PD$INC,pred_inc[!is.na(pred_inc)])
  ngini <- Gini(PD$INC, pred_inc[!is.na(pred_inc)]) / Gini(PD$INC,PD$INC) 
  newList <- list("coeff" = coeff,"inc" = inc, "rmse" = rmse, "gini" = gini, "ngini" = ngini)
  return(newList)
}

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

#Test 
h = cap_loop_elnet(PD,1)

#CAP Function 3: For 1 threshold return list of max min average values of KPI (discrepancy to be updated)
cap_confidence_interval_elnet <- function(PD,alpha_value,threshold){
  
  #Declare temp varaible
  G = matrix()
  NG = matrix()
  R = matrix()
  pred_inc=matrix()
  disc <- 0
  #Random 10-fold cross validation
  l = nrow(PD)
  indice <- runif(l)*10 
  indice <- trunc(indice)
  PD <- data.frame(PD,indice)
  PD <- PD[order(indice),]
  
  for (i in 0:9){
    #Fit the model
    data_train <- subset(PD, PD$indice!=i)
    data_train$INC <- replace(data_train$INC, data_train$INC > threshold,threshold)
    R_VAR = data_train$INC
    E_VAR <- as.matrix(data_train[,!(colnames(data_train) %in% c("INC","indice","indice.1","indice.2"))])
    
    cv <- cv.glmnet(
      x=E_VAR,
      y=log(R_VAR),
      family="gaussian",
      alpha=alpha_value,
      nfolds=10,
      type.measure="deviance",
      keep=TRUE
    ) 
    
    #Predict
    data_test <- subset(PD, PD$indice==i)
    T_VAR <- data_test$INC
    disc <- sum(ifelse(data_test$INC < threhold, 0, data_test$INC - threshold))
    data_test$INC <- NULL
    E_PRED <- as.matrix(data_test[,!(colnames(data_test) %in% c("INC","indice","indice.1","indice.2"))])
    gi = Gini(T_VAR$INC,exp(predict(cv,E_PRED)))
    ngi = Gini(T_VAR$INC, exp(predict(cv,E_PRED))) / Gini(T_VAR$INC,T_VAR$INC)
    ri = rmse(T_VAR$INC, disc+exp(predict(cv,E_PRED)))
    NG = rbind(NG,ngi)
    G = rbind(G,gi)
    R = rbind(R,ri)
    pred_inc = rbind(pred_inc,exp(predict(cv,E_PRED)))
    
  }
  
  #Outputs
  discrepancy <- sum(ifelse(PD$INC < threhold, 0, PD$INC - threshold))/length(PD)
  rmse <- rmse(PD$INC, pred_inc[!is.na(pred_inc)]+discrepancy)
  gini <- Gini(PD$INC,pred_inc[!is.na(pred_inc)])
  ngini <- Gini(PD$INC, pred_inc[!is.na(pred_inc)]) / Gini(PD$INC,PD$INC)
  gini_max <- sum(tail(sort(G),3))/3
  gini_min <- sum(head(sort(G),3))/3
  ngini_max <- sum(tail(sort(NG),3))/3
  ngini_min <- sum(head(sort(NG),3))/3
  ri_max <- sum(tail(sort(R),3))/3
  ri_min <- sum(head(sort(R),3))/3
  
  newList <- list("gini" = gini, "gini_max" = gini_max,"gini_min" = gini_min, "ngini" = ngini, "ngini_max" = ngini_max,"ngini_min" = ngini_min, "rmse" = rmse, "ri_max" = ri_max, "ri_min" = ri_min)
  return(newList)
}

#CAP Function 4: Loop on threshold to get confidence interval
cap_confidence_interval_loop_elnet <- function(PD,alpha_value){
  list_rmse = list()
  list_gini = list()
  list_ngini = list()
  list_rmse_min = list()
  list_gini_min = list()
  list_ngini_min = list()
  list_rmse_max = list()
  list_gini_max  = list()
  list_ngini_max  = list()
  for (i in list(4,8,12,16,20,21,22,23,25,30,40,80,120)){ 
    p = cap_confidence_interval_elnet(PD,alpha_value,100000*i)
    list_rmse = rbind(list_rmse,p$rmse)
    list_gini = rbind(list_gini,p$gini)
    list_ngini = rbind(list_ngini,p$ngini)
    list_rmse_min = rbind(list_rmse_min,p$ri_min)
    list_gini_min = rbind(list_gini_min,p$gini_min)
    list_ngini_min = rbind(list_ngini_min,p$ngini_min)
    list_rmse_max = rbind(list_rmse_max,p$ri_max)
    list_gini_max = rbind(list_gini_max,p$gini_max)
    list_ngini_max = rbind(list_ngini_max,p$ngini_max)
  }
  result <- list("rmseL" = list_rmse, "rmse_min" = list_rmse_min, "rmse_max" = list_rmse_max, "giniL" = list_gini, "list_gini_max" = list_gini_max, "list_gini_min" = list_gini_min, "nginiL" = list_ngini, "list_ngini_max" = list_ngini_max, "list_ngini_min" = list_ngini_min)
  return(result)
} 

#Test
test_confidence_005 = cap_confidence_interval_loop_elnet(PD,0.05)
test_confidence_005
test_confidence_005 = as.data.frame(test_confidence_005)
test_confidence_005$rmseL <- vapply(test_confidence_005$rmseL, paste, collapse = ", ", character(1L))
test_confidence_005$giniL <- vapply(test_confidence_005$giniL, paste, collapse = ", ", character(1L))
test_confidence_005$nginiL <- vapply(test_confidence_005$nginiL, paste, collapse = ", ", character(1L))
test_confidence_005$rmse_min <- vapply(test_confidence_005$rmse_min, paste, collapse = ", ", character(1L))
test_confidence_005$list_gini_max <- vapply(test_confidence_005$list_gini_max, paste, collapse = ", ", character(1L))
test_confidence_005$list_ngini_max <- vapply(test_confidence_005$list_ngini_max, paste, collapse = ", ", character(1L))
test_confidence_005$rmse_max <- vapply(test_confidence_005$rmse_max, paste, collapse = ", ", character(1L))
test_confidence_005$list_gini_min <- vapply(test_confidence_005$list_gini_min, paste, collapse = ", ", character(1L))
test_confidence_005$list_ngini_min <- vapply(test_confidence_005$list_ngini_min, paste, collapse = ", ", character(1L))
write.csv2(test_confidence_005,file = "PD_cap_confidence_interval_alpha005")