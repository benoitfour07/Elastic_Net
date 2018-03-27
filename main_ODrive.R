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

setwd("A://00_Personal/Four_Benoit/R/POC-ElasticNet")
MAIN_DIR <- "//srvhfile1/ShareFiles/Actuarial/DD/Tech_Pricing/working_2018/sample_code"
DATA_DIR <- "//srvhsas3/DD/temporary/tech_pricing/data_prep_rm_201801"

####################### define scope ###############################

TARGET_LOB = "AUTO" # AUTO, BIKE, MOPED
TARGET_RES_ODdrive = "INC_MODEL_ODdrive" # BI, PD, PA, PI, ODdrive, ODother, FBI, FPD, LE

###################### read data (time-consuming) ##################
if(FALSE){
  data_org <- fread(paste(DATA_DIR, "seve.csv", sep="/"), sep=",") %>%
    dplyr:: filter(LOB == TARGET_LOB & VAL_Rand_Model != "Validation")
}

data_org <- fread(paste(DATA_DIR, "seve.csv", sep="/"), sep=",") %>%
  dplyr:: filter(LOB == TARGET_LOB & VAL_Rand_Model != "Validation")

###################### data prep ##################################
data <- data_org
data$INC <- data$INC_MODEL_ODdrive + data$INC_EXCEED_ODdrive #replace INC by INC_MODEL_PD then filter by INC > 0 to filter on PD cover
data <- filter(data, INC > 0)
source(paste(MAIN_DIR, "1_data_prep.R", sep="/"))


ODdrive <- data

#################### generate dummy variables ######################
ODdrive <- makedummies(ODdrive, basal_level = FALSE)
ODdrive <- select_(ODdrive, c('-starts_with("res")')) # delete strange variables 
str(ODdrive)
summary(ODdrive$INC)

############### FINAL ODdrive RESULT ##############################

ODrive_final = cap_loop_elnet2(ODdrive,69)
ODrive_final
write.csv2(ODrive_final[[6]],file = "ODdrive_final_1_to_20M_Coeff")



############################ PLOTTING RESULT ###########################################
#input = cap_loop_elnet2()
ODdrive_final
elnetODdriveL = ODrive_final
threshold_scope <- elnetODdriveL$threshold_scope
threshold_scope
ngini.capped <- NULL
for(i in 1:length(elnetODdriveL$nginiL)){
  ngini.capped[i] <- elnetODdriveL$nginiL[[i]]
}

se_ngini.capped <- NULL
for(i in 1:length(elnetODdriveL$se_nginiL)){
  se_ngini.capped[i] <- elnetODdriveL$se_nginiL[[i]]
}

rmse.capped <- NULL
for(i in 1:length(elnetODdriveL$rmseL)){
  rmse.capped[i] <- elnetODdriveL$rmseL[[i]]
}

min.gini = min(unlist(ngini.capped)) - 0.005
max.gini = max(unlist(ngini.capped)) + 0.005
line = seq(1000000,8000000,by = 100000)
nfile = paste("Norm_GINI_ODdrive_alph1_50Loop", ".png", sep="")
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

min.rmse = min(unlist(rmse.capped))
max.rmse = max(unlist(rmse.capped))
nfile = paste("RMSE_ODdrive_alph1_50Loop", ".png", sep="")
png(nfile, 1000, 800)
rmse.capped
par(mfrow = c(1,1))
ylim = c(min.gini, max.gini)

plot(threshold_scope, rmse.capped, xlab = "Threshold *Millions", ylab = "RMSE", 
     ylim = c(min.rmse, max.rmse), pch = 3, lwd = 2, xaxt = "n", col = "black")

legend("topright", legend=c("RMSE"), col = c("black"), pch = 3, lwd = 2, bty = "n")
lines(threshold_scope,rmse.capped,col = c("black"))
axis(1, at=line, labels=paste(line/1000000, "", sep = ""))

dev.off()
