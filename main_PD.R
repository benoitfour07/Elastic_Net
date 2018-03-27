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
library(rowr)
#install.packages("devtools")
#devtools::install_github("toshi-ara/makedummies")


####################### Load data ##################################

setwd("A://00_Personal/Four_Benoit/R/POC-ElasticNet")
MAIN_DIR <- "//srvhfile1/ShareFiles/Actuarial/DD/Tech_Pricing/working_2018/sample_code"
DATA_DIR <- "//srvhsas3/DD/temporary/tech_pricing/data_prep_rm_201801"

####################### define scope ###############################

TARGET_LOB = "AUTO" # AUTO, BIKE, MOPED
TARGET_RES_PD <-  "INC_MODEL_PD" + "INC_EXCEED_PD" # BI, PD, PA, PI, ODdrive, ODother, FBI, FPD, LE

###################### read data (time-consuming) ##################
if(FALSE){
  data_org <- fread(paste(DATA_DIR, "seve.csv", sep="/"), sep=",") %>%
    dplyr:: filter(LOB == TARGET_LOB & VAL_Rand_Model != "Validation")
}

data_org <- fread(paste(DATA_DIR, "seve.csv", sep="/"), sep=",") %>%
  dplyr:: filter(LOB == TARGET_LOB & VAL_Rand_Model != "Validation")

###################### data prep ##################################
data <- data_org
data$INC <- data$INC_MODEL_PD + data$INC_EXCEED_PD #replace INC by INC_MODEL_PD then filter by INC > 0 to filter on PD cover
data <- filter(data, INC > 0)
source(paste(MAIN_DIR, "1_data_prep.R", sep="/"))
summary(data$INC)

PD <- data

#################### generate dummy variables ######################
PD <- makedummies(PD, basal_level = FALSE)
PD <- select_(PD, c('-starts_with("res")')) # delete strange variables 
summary(PD$INC)


##################### CAP FUNCTION TO GET VOLATILITY (AGD Method) ##########################
# Same as previous capping function to calculate gini except the sampling is not the same
# We also get and save the optimal lambda to be reused in the loop

# Same as previous capping function to calculate gini except the sampling is not the same
# We also get and save the optimal lambda to be reused in the loop

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
  
  #Loop on gini prediction using the same lambda
  l_loop = 50
  for (j in 1:l_loop){
    #New cross validation
    PD$indice <- NULL
    PD$indice1 <- NULL
    PD$indice.2 <- NULL
    l = nrow(PD)
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
      nfolds=10,
      type.measure="deviance",
      keep=TRUE
    ) 
    
    #Prediction with optimal lambda on test dataset part
    data_test <- subset(PD, PD$indice %in% c(5,6,7,8,9))
    E_PRED <- as.matrix(data_test[,!(colnames(data_test) %in% c("INC","indice","indice.1","indice.2"))])
    list_rmse = rbind(list_rmse, rmse(data_test$INC,exp(predict(cv,E_PRED, s="lambda.1se")) + disc))
    list_gini = rbind(list_gini, Gini(data_test$INC,exp(predict(cv,E_PRED, s="lambda.1se"))))
    list_ngini = rbind(list_ngini, Gini(data_test$INC, exp(predict(cv,E_PRED))) / Gini(data_test$INC,data_test$INC))
    temp_coeff = cbind(temp_coeff, coef(cv)[1:nb_var])
  }
  #Outputs = KPI average
  rmse = as.numeric(summary(as.numeric(list_rmse))["Mean"])
  gini = as.numeric(summary(as.numeric(list_gini))["Mean"])
  ngini = as.numeric(summary(as.numeric(list_ngini))["Mean"])
  vol_ngini = max(unlist(list_ngini)) - min(unlist(list_ngini))
  se_ngini = sd(as.numeric(list_ngini))/sqrt(length(as.numeric(list_ngini)))
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

  newList <- list("rmse" = rmse, "gini" = gini, "ngini" = ngini, "vol_ngini" = vol_ngini, "se_ngini" = se_ngini, "Result_coeff" = Result_coeff)
  return(newList)
}

cap_loop_elnet3 <- function(PD,nb_var){
  T1<-Sys.time() 
  
  list_rmse = list()
  list_gini = list()
  list_ngini = list()
  list_vol_ngini = list()
  list_se_ngini = list()
  Coeff = data.frame()
  threshold_scope <- NULL
  j = 1
  
  for (i in list (10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)){ #,85,90,95,100,105,110,115,120,125,130,135,140,145,150,155,160,165,170,185,190,195)){
    p = cap_predict_elnet3(PD, 0.05, 100000*i,nb_var)
    list_rmse = rbind(list_rmse,p$rmse)
    list_gini = rbind(list_gini,p$gini)
    list_ngini = rbind(list_ngini,p$ngini)
    list_vol_ngini = rbind(list_vol_ngini,p$vol_ngini)
    list_se_ngini = rbind(list_se_ngini,p$se_ngini)
    temp_coeff = subset(p$Result_coeff, p$Result_coeff$NA_freq < 0.3, select = c(Variables,mean_coeff,NA_freq))
    temp_coeff = temp_coeff[order(temp_coeff$NA_freq),]
    Coeff = cbind.fill(Coeff,temp_coeff,fill=NA)
    threshold_scope[j] <- 100000*i
    j = j + 1
  }
  
  T2<-Sys.time()
  Tdiff= difftime(T2, T1)
  result <- list("rmseL" = list_rmse,"giniL" = list_gini, "nginiL" = list_ngini, "vol_giniL" = list_vol_ngini, "se_nginiL" = list_se_ngini, Coeff, "threshold_scope" = threshold_scope, "Tdiff" = Tdiff)
  return(result)
}

# test 1 with nfolds = 10, loop = 50
test_enlnet3 = cap_loop_elnet3(PD,72)
test_enlnet3
write.csv2(test_enlnet3[[6]],file = "PD_Elnet3_1_to_20_Coeff_100Loop")

############################ PLOTTING RESULT ###########################################
#input = cap_loop_elnet2()
test_enlnet3
elnetL = test_enlnet3
threshold_scope <- elnetL$threshold_scope
threshold_scope
ngini.capped <- NULL
for(i in 1:length(elnetL$nginiL)){
  ngini.capped[i] <- elnetL$nginiL[[i]]
}

rmse.capped <- NULL
for(i in 1:length(elnetL$rmseL)){
  rmse.capped[i] <- elnetL$rmseL[[i]]
}
rmse.capped

se_ngini.capped <- NULL
for(i in 1:length(elnetL$se_nginiL)){
  se_ngini.capped[i] <- elnetL$se_nginiL[[i]]
}

min.gini = min(unlist(ngini.capped)) - 0.004
max.gini = max(unlist(ngini.capped)) + 0.004
min.rmse = min(unlist(rmse.capped))
max.rmse = max(unlist(rmse.capped)) 
line = seq(1000000,13000000,by = 500000)
nfile = paste("Norm_GINI_test_enlnet3_PD_alph1_100Loop_1to20M", ".png", sep="")
png(nfile, 1000, 800)

par(mfrow = c(1,1))
ylim = c(min.gini, max.gini)

plot(threshold_scope, ngini.capped, xlab = "Threshold *Millions", ylab = "Normalized GINI", 
     ylim = c(min.gini, max.gini), pch = 3, lwd = 2, xaxt = "n", col = "black")

legend("topright", legend=c("Normalized GINI","ngini + 2*standard_error","ngini - 2*standard_error"), col = c("black","red","blue"), pch = 3, lwd = 2, bty = "n")
lines(threshold_scope,ngini.capped,col = c("black"))
lines(threshold_scope, ngini.capped + 2*se_ngini.capped,col = c("red"),type = "l", lty=3)
lines(threshold_scope, ngini.capped - 2*se_ngini.capped,col = c("blue"),type = "l", lty=3)
axis(1, at=line, labels=paste(line/1000000, "", sep = ""))

dev.off()

nfile = paste("RMSE_PD_alph1_100Loop_1to20M", ".png", sep="")
png(nfile, 1000, 800)
plot(threshold_scope, rmse.capped, xlab = "Threshold *Millions", ylab = "RMSE", 
     ylim = c(min.rmse, max.rmse), pch = 3, lwd = 2, xaxt = "n", col = "black")

legend("topright", legend=c("RMSE"), col = c("black"), pch = 3, lwd = 2, bty = "n")
lines(threshold_scope,rmse.capped,col = c("black"))
axis(1, at=line, labels=paste(line/1000000, "", sep = ""))

dev.off()



par(mfrow = c(1,1))
rmse.capped
rmse.capped_updated <- rmse.capped[5:23]
rmse.capped_updated
min.rmse_updated = min(unlist(rmse.capped_updated))
max.rmse_updated = max(unlist(rmse.capped_updated)) 
threshold_scope_updated <- threshold_scope[5:23]
plot(threshold_scope_updated, rmse.capped_updated, xlab = "Threshold *Millions", ylab = "RMSE", 
     ylim = c(min.rmse_updated, max.rmse_updated), pch = 3, lwd = 2, xaxt = "n", col = "black")

legend("topright", legend=c("RMSE"), col = c("black"), pch = 3, lwd = 2, bty = "n")
lines(threshold_scope_updated,rmse.capped_updated,col = c("black"))
axis(1, at=line, labels=paste(line/1000000, "", sep = ""))

dev.off()

