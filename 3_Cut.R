############################# CUT KPI (out of scope) #################################
#CUT Function 1: For 1 threshold return KPI indicators
cut_predict_elnet <- function(PD,alpha_value,threshold){
  
  #Declare temp variables
  pred_inc=matrix()
  pred_coeff=matrix()
  #Random 10 cross validation
  l = nrow(PD)
  indice <- runif(l)*10 
  indice <- trunc(indice)
  PD <- data.frame(PD,indice)
  PD <- PD[order(indice),]
  #discrepancy
  diff=matrix()
  diff=as.matrix(filter(PD, PD$INC > threshold)$INC)
  discrepancy = (sum(diff[!is.na(diff)])/nrow(PD))
  
  for (i in 0:9){
    #Fitting a model
    data_train <- subset(PD, PD$indice!=i)
    data_train <- filter(data_train, INC < threshold)
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
    
    #Predict
    data_test <- subset(PD, PD$indice==i)
    data_test$INC <- NULL
    E_PRED <- as.matrix(data_test[,!(colnames(data_test) %in% c("INC","indice","indice.1","indice.2"))])
    pred_coeff=rbind(pred_coeff,coef(cv))
    pred_inc = rbind(pred_inc,exp(predict(cv,E_PRED)))
    
  }
  
  #Outputs
  coeff <- pred_coeff
  inc <- pred_inc[!is.na(pred_inc)]
  rmse <- rmse(PD$INC,pred_inc[!is.na(pred_inc)]+discrepancy)
  gini <- Gini(PD$INC,pred_inc[!is.na(pred_inc)])
  ngini <- Gini(PD$INC, pred_inc[!is.na(pred_inc)]) / Gini(PD$INC,PD$INC) 
  newList <- list("coeff" = coeff,"inc" = inc, "rmse" = rmse, "gini" = gini, "ngini" = ngini)
  return(newList)
}

#Test 
PD_cut_test = cut_predict_elnet(PD,1,2000000)
unclass(PD_cut_test$coeff)
unclass(PD_cut_test$coeff[2:72])
coeffL = as.data.frame(attr(PD_cut_test$coeff,"Dimnames")[[1]][2:72])
coeffL = cbind(coeffL,as.data.frame(attr(PD_cut_test$coeff,"x")[2:72]))
PD_cut_test$coeff
PD_cut_test$gini
PD_cut_test$rmse

#CUT Function 2: Loop on threshold list, return list of KPI parameters
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


#Test CUT for alpha = 0.05
PD_cut_alpha005 = cut_loop_elnet(PD,0.05)
PD_cut_alpha005
g005 = as.data.frame(PD_cut_alpha005)
g005$rmseL <- vapply(g005$rmseL, paste, collapse = ", ", character(1L))
g005$giniL <- vapply(g005$giniL, paste, collapse = ", ", character(1L))
g005$nginiL <- vapply(g005$nginiL, paste, collapse = ", ", character(1L))
write.csv2(g005,file = "PD_cut_alpha005")

