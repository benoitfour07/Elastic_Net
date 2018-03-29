############################# CUT KPI (out of scope) #################################
#CUT Function 1: For 1 threshold return KPI indicators
cut_predict_elnet3 <- function(PD,alpha_value,threshold,nb_var){
  #Get optimized lambda value
  initialize <- filter(PD_train, INC > threshold)
  E_VAR <- as.matrix(initialize[,!(colnames(initialize) %in% c("INC","indice","indice.1","indice.2"))])
  R_VAR=initialize$INC
  #Running cv.glmnet once out of the loop to initialize coeff dataframe output (assuming we could have a better way)
  gcv <- cv.glmnet(
    x=E_VAR,
    y=log(R_VAR),
    family="gaussian",
    alpha=alpha_value,
    nfolds=10,
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
  list_avg_test_before_cut = list()
  list_avg_test_after_cut = list()
  list_avg_prediction = list()
  list_avg_exceed_amount = list()
  #Loop on gini prediction using the same lambda
  l_loop = 50
  for (j in 1:l_loop){
    #New cross validation
    PD$indice <- NULL
    PD$indice1 <- NULL
    PD$indice.2 <- NULL
    l = nrow(PD)
    #set.seed(j)
    indice <- runif(l)*10
    indice <- trunc(indice)
    PD <- data.frame(PD,indice)
    PD <- PD[order(indice),]
    #Fitting a model with 50% of the dataset
    data_train <- subset(PD, PD$indice %in% c(0,1,2,3,4))
    disc <- sum(filter(data_train, INC > threshold)$INC)/length(data_train)
	  data_train <- filter(data_train, INC < threshold)
    E_VAR <- as.matrix(data_train[,!(colnames(data_train) %in% c("INC","indice","indice.1","indice.2"))])
    R_VAR <- data_train$INC
    cv <- cv.glmnet(
      x=E_VAR,
      y=log(R_VAR),
      family="gaussian",
      alpha=alpha_value,
      nfolds=10,
      type.measure="deviance",
      keep=TRUE
    )
    #Prediction with optimal lambda on test dataset part
    data_test <- subset(PD, PD$indice %in% c(5,6,7,8,9))
    list_avg_test_before_cut = rbind(list_avg_test_before_cut,as.numeric(summary(as.numeric(data_test$INC))["Mean"]))
    list_avg_test_after_cut = rbind(list_avg_test_after_cut,as.numeric(summary(as.numeric(filter(data_train, INC > threshold)$INC))["Mean"]))
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
  avg_test_before_cut = as.numeric(summary(as.numeric(list_avg_test_before_cut))["Mean"])
  avg_test_after_cut = as.numeric(summary(as.numeric(list_avg_test_after_cut))["Mean"])
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
  newList <- list("rmse" = rmse, "gini" = gini, "ngini" = ngini, "vol_ngini" = vol_ngini, "se_ngini" = se_ngini, "sd_coeff" = sd_coeff, "lambda" = lambda, "avg_test_before_cut" = avg_test_before_cut,　"avg_test_after_cut" = avg_test_after_cut, "avg_prediction" = avg_prediction, "avg_exceed_amount" = avg_exceed_amount, "Result_coeff" = Result_coeff)
  return(newList)
}


cut_loop_elnet3 <- function(PD,nb_var){
  T1<-Sys.time()
  list_rmse = list()
  list_gini = list()
  list_ngini = list()
  list_vol_ngini = list()
  list_se_ngini = list()
  list_lambda = list()
  list_sd = list()
  Coeff = data.frame()
  list_avg_test_before_cut = list()
  list_avg_test_after_cut = list()
  list_avg_prediction = list()
  list_avg_exceed_amount = list()
  threshold_scope <- NULL
  j = 1
  for (i in list (10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)){ #,90,100,110,120,125,130,135,140,145,150,155,160,165,170,185,190,195)){
    p = cut_predict_elnet3(PD, 1, 100000*i,nb_var)
    list_rmse = rbind(list_rmse,p$rmse)
    list_gini = rbind(list_gini,p$gini)
    list_ngini = rbind(list_ngini,p$ngini)
    list_vol_ngini = rbind(list_vol_ngini,p$vol_ngini)
    list_se_ngini = rbind(list_se_ngini,p$se_ngini)
    list_lambda = rbind(list_lambda, p$lambda)
    list_sd = rbind(list_sd, p$sd_coeff)
    list_avg_test_before_cut = rbind(list_avg_test_before_cut, p$avg_test_before_cut)
    list_avg_test_after_cut = rbind(list_avg_test_after_cut, p$avg_test_after_cut)
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
  result <- list("rmseL" = list_rmse,"giniL" = list_gini, "nginiL" = list_ngini, "vol_giniL" = list_vol_ngini, "se_nginiL" = list_se_ngini, "list_sd" = list_sd, "list_lambda" = list_lambda, "list_avg_test_before_cut" = list_avg_test_before_cut, "list_avg_test_after_cut" = list_avg_test_after_cut, "list_avg_prediction" = list_avg_prediction, "list_avg_exceed_amount" = list_avg_exceed_amount, Coeff, "threshold_scope" = threshold_scope,"Tdiff" = Tdiff)
  return(result)
}

