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
    
    cv.wi <- cv.glmnet(
      x=E_VAR,
      #y=log(R_VAR),
      y=R_VAR,
      family="gaussian",
      alpha=alpha_value,
      nfolds=10,
      type.measure="deviance",
      keep=TRUE
    ) 
    
    cv <- cv.glmnet(
      x=E_VAR,
      #y=log(R_VAR),
      y=R_VAR,
      family="gaussian",
      alpha=alpha_value,
      nfolds=10,
      type.measure="deviance",
      keep=TRUE,
      intercept = FALSE,
      lambda = cv.wi$lambda
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

##################### CAP FUNCTION TO GET VOLATILITY (AGD Method) ##########################
### First method wold be to get the best lambda value, save it for each step of the loop (not here)
### Second method is for each step of the loop to calculte a cv.glmnet lambda and predict (here)

cap_predict_elnet3 <- function(PD,alpha_value,threshold,nb_var){
  #Get optimized lambda value
  E_VAR <- as.matrix(PD[,!(colnames(PD) %in% c("INC","indice","indice.1","indice.2"))])
  R_VAR=replace(PD$INC, PD$INC > threshold,threshold)
  #Running cv.glmnet once out of the loop to initialize coeff dataframe output (assuming we could have a better way)
  gcv <- cv.glmnet(
    x=E_VAR,
    y=log(R_VAR),
    family="gaussian",
    alpha=alpha_value,
    nfolds=3,
    type.measure="deviance",
    keep=TRUE
  )
  #Result Coeff
  Result_coeff = as.data.frame(attr(coef(gcv),"Dimnames")[[1]][1:nb_var])
  temp_coeff = as.data.frame(coef(gcv)[1:nb_var])
  #Declare temp variables
  list_rmse = list()
  list_gini = list()
  list_ngini = list()
  list_lambda = list()
  list_avg_test_before_capped = list()
  list_avg_test_after_capped = list()
  list_avg_prediction = list()
  list_avg_exceed_amount = list()
  #Loop on gini prediction using the same lambda
  l_loop = 80
  for (j in 1:l_loop){
    #New cross validation
    PD$indice <- NULL
    PD$indice1 <- NULL
    PD$indice.2 <- NULL
    l = nrow(PD)
    #set.seed(j) ###### SMOOTH and BIASED the result
    indice <- runif(l)*10
    indice <- trunc(indice)
    PD <- data.frame(PD,indice)
    PD <- PD[order(indice),]
    #Fitting a model with 50% of the dataset
    data_train <- subset(PD, PD$indice %in% c(0,1,2,3,4))
    disc <- sum(ifelse(data_train$INC < threshold,0,data_train$INC-threshold))/length(data_train)
    data_train$INC <- replace(data_train$INC, data_train$INC > threshold,threshold)
    E_VAR <- as.matrix(data_train[,!(colnames(data_train) %in% c("INC","indice","indice.1","indice.2"))])
    R_VAR <- data_train$INC
    cv <- cv.glmnet(
      x=E_VAR,
      y=log(R_VAR),
      family="gaussian",
      alpha=alpha_value,
      nfolds=20,
      type.measure="deviance",
      keep=TRUE
    )
    #Prediction with optimal lambda on test dataset part
    data_test <- subset(PD, PD$indice %in% c(5,6,7,8,9))
    list_avg_test_before_capped = rbind(list_avg_test_before_capped,as.numeric(summary(as.numeric(data_test$INC))["Mean"]))
    data_test_capped <- data_test
    data_test_capped$INC <- replace(data_test_capped$INC, data_test_capped$INC > threshold,threshold)
    list_avg_test_after_capped = rbind(list_avg_test_after_capped,as.numeric(summary(as.numeric(data_test_capped$INC))["Mean"]))
    E_PRED <- as.matrix(data_test[,!(colnames(data_test) %in% c("INC","indice","indice.1","indice.2"))])
    list_avg_prediction = rbind(list_avg_prediction, mean(exp(predict(cv,E_PRED, s="lambda.1se"))))
    list_rmse = rbind(list_rmse, rmse(data_test$INC,exp(predict(cv,E_PRED, s="lambda.1se")) + disc))
    list_gini = rbind(list_gini, Gini(data_test$INC,exp(predict(cv,E_PRED, s="lambda.1se"))))
    list_ngini = rbind(list_ngini, Gini(data_test$INC, exp(predict(cv,E_PRED))) / Gini(data_test$INC,data_test$INC))
    list_lambda = rbind(list_lambda, cv$lambda.1se)
    list_avg_exceed_amount = rbind(list_avg_exceed_amount, disc)
    temp_coeff = cbind(temp_coeff, coef(cv)[1:nb_var])
  }
  #Outputs = KPI average
  rmse = as.numeric(summary(as.numeric(list_rmse))["Mean"])
  gini = as.numeric(summary(as.numeric(list_gini))["Mean"])
  ngini = as.numeric(summary(as.numeric(list_ngini))["Mean"])
  vol_ngini = max(unlist(list_ngini)) - min(unlist(list_ngini))
  se_ngini = sd(as.numeric(list_ngini))/sqrt(length(as.numeric(list_ngini)))
  lambda = as.numeric(summary(as.numeric(list_lambda))["Mean"])
  avg_test_before_capped = as.numeric(summary(as.numeric(list_avg_test_before_capped))["Mean"])
  avg_test_after_capped = as.numeric(summary(as.numeric(list_avg_test_after_capped))["Mean"])
  avg_prediction = as.numeric(summary(as.numeric(list_avg_prediction))["Mean"])
  avg_exceed_amount = as.numeric(summary(as.numeric(list_avg_exceed_amount))["Mean"])
  #Output Coeff Table (mean, sd, na freq)
  temp_coeff[temp_coeff==0.000000e+00] <- NA
  mean_coeff = apply(temp_coeff[,1:(l_loop+1)],1,sum,na.rm=TRUE)/(l_loop+1)
  sd_coeff = apply(temp_coeff[,1:(l_loop+1)],1,sd,na.rm=TRUE)
  NA_freq = apply(is.na(temp_coeff[,1:(l_loop+1)]),1,sum)/(l_loop+1)
  Result_coeff = cbind(Result_coeff, mean_coeff, sd_coeff, NA_freq)
  Result_coeff$mean_coeff[Result_coeff$mean_coeff==0.000000e+00] <- NA
  Result_coeff$NA_freq <- round(Result_coeff$NA_freq,2)
  names(Result_coeff)[1] <- "Variables"
  #Final Output
  newList <- list("rmse" = rmse, "gini" = gini, "ngini" = ngini, "vol_ngini" = vol_ngini, "se_ngini" = se_ngini, "sd_coeff" = sd_coeff, "lambda" = lambda, "avg_test_before_capped" = avg_test_before_capped,@"avg_test_after_capped" = avg_test_after_capped, "avg_prediction" = avg_prediction, "avg_exceed_amount" = avg_exceed_amount, "Result_coeff" = Result_coeff)
  return(newList)
}

cap_loop_elnet3 <- function(PD,nb_var){
  T1<-Sys.time()
  list_rmse = list()
  list_gini = list()
  list_ngini = list()
  list_vol_ngini = list()
  list_se_ngini = list()
  list_lambda = list()
  list_sd = list()
  Coeff = data.frame()
  list_avg_test_before_capped = list()
  list_avg_test_after_capped = list()
  list_avg_prediction = list()
  list_avg_exceed_amount = list()
  threshold_scope <- NULL
  j = 1
  for (i in list (10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)){ #,90,100,110,120,125,130,135,140,145,150,155,160,165,170,185,190,195)){
    p = cap_predict_elnet3(PD, 1, 100000*i,nb_var)
    list_rmse = rbind(list_rmse,p$rmse)
    list_gini = rbind(list_gini,p$gini)
    list_ngini = rbind(list_ngini,p$ngini)
    list_vol_ngini = rbind(list_vol_ngini,p$vol_ngini)
    list_se_ngini = rbind(list_se_ngini,p$se_ngini)
    list_lambda = rbind(list_lambda, p$lambda)
    list_sd = rbind(list_sd, p$sd_coeff)
    list_avg_test_before_capped = rbind(list_avg_test_before_capped, p$avg_test_before_capped)
    list_avg_test_after_capped = rbind(list_avg_test_after_capped, p$avg_test_after_capped)
    list_avg_prediction = rbind(list_avg_prediction, p$avg_prediction)
    list_avg_exceed_amount = rbind(list_avg_exceed_amount, p$avg_exceed_amount)
    temp_coeff = subset(p$Result_coeff, p$Result_coeff$NA_freq < 0.3, select = c(Variables,mean_coeff,sd_coeff,NA_freq))
    temp_coeff = temp_coeff[order(temp_coeff$NA_freq),]
    Coeff = cbind.fill(Coeff,temp_coeff,fill=NA)
    threshold_scope[j] <- 100000*i
    j = j + 1
  }
  T2<-Sys.time()
  Tdiff= difftime(T2, T1)
  result <- list("rmseL" = list_rmse,"giniL" = list_gini, "nginiL" = list_ngini, "vol_giniL" = list_vol_ngini, "se_nginiL" = list_se_ngini, "list_sd" = list_sd, "list_lambda" = list_lambda, "list_avg_test_before_capped" = list_avg_test_before_capped, "list_avg_test_after_capped" = list_avg_test_after_capped, "list_avg_prediction" = list_avg_prediction, "list_avg_exceed_amount" = list_avg_exceed_amount, Coeff, "threshold_scope" = threshold_scope,"Tdiff" = Tdiff)
  return(result)
}
