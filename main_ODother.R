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
TARGET_RES_ODother = "INC_MODEL_ODother" # BI, PD, PA, PI, ODdrive, ODother, FBI, FPD, LE

###################### read data (time-consuming) ##################
if(FALSE){
  data_org <- fread(paste(DATA_DIR, "seve.csv", sep="/"), sep=",") %>%
    dplyr:: filter(LOB == TARGET_LOB & VAL_Rand_Model != "Validation")
}

data_org <- fread(paste(DATA_DIR, "seve.csv", sep="/"), sep=",") %>%
  dplyr:: filter(LOB == TARGET_LOB & VAL_Rand_Model != "Validation")

###################### data prep ##################################

data <- data_org
data$INC <- data$INC_MODEL_ODother + data$INC_EXCEED_ODother #replace INC by INC_MODEL_PD then filter by INC > 0 to filter on PD cover
data <- filter(data, INC > 0)
source(paste(MAIN_DIR, "1_data_prep.R", sep="/"))


ODother <- data

#################### generate dummy variables ######################
ODother <- makedummies(ODother, basal_level = FALSE)
ODother <- select_(ODother, c('-starts_with("res")')) # delete strange variables 
str(ODother)
summary(ODother$INC)

############### FINAL ODother RESULT ##############################

ODother_final = cap_loop_elnet2(ODother,69)
ODother_final
write.csv2(ODother_final[[6]],file = "ODother_final_final_1_to_20M_Coeff")




############################ PLOTTING RESULT ###########################################
#input = cap_loop_elnet2()
ODother_final
elnetODotherL = ODother_final
threshold_scope <- elnetODotherL$threshold_scope
threshold_scope
ngini.capped <- NULL
for(i in 1:length(elnetODotherL$nginiL)){
  ngini.capped[i] <- elnetODotherL$nginiL[[i]]
}

se_ngini.capped <- NULL
for(i in 1:length(elnetODotherL$se_nginiL)){
  se_ngini.capped[i] <- elnetODotherL$se_nginiL[[i]]
}

rmse.capped <- NULL
for(i in 1:length(elnetODotherL$rmseL)){
  rmse.capped[i] <- elnetODotherL$rmseL[[i]]
}
rmse.capped

min.gini = min(unlist(ngini.capped)) - 0.005
max.gini = max(unlist(ngini.capped)) + 0.005
line = seq(1000000,5000000,by = 100000)
nfile = paste("Norm_GINI_ODother_alph1_50Loop", ".png", sep="")
png(nfile, 1000, 800)

par(mfrow = c(1,1))
ylim = c(min.gini, max.gini)

plot(threshold_scope, ngini.capped, xlab = "Threshold *100k", ylab = "Normalized GINI", 
     ylim = c(min.gini, max.gini), pch = 3, lwd = 2, xaxt = "n", col = "black")

legend("topright", legend=c("Normalized GINI","ngini + 2*standard_error","ngini - 2*standard_error"), col = c("black","red","blue"), pch = 3, lwd = 2, bty = "n")
lines(threshold_scope,ngini.capped,col = c("black"))
lines(threshold_scope, ngini.capped + 2*se_ngini.capped,col = c("red"),type = "l", lty=3)
lines(threshold_scope, ngini.capped - 2*se_ngini.capped,col = c("blue"),type = "l", lty=3)
axis(1, at=line, labels=paste(line/1000000, "", sep = ""))

dev.off()

min.rmse = min(unlist(rmse.capped))
max.rmse = max(unlist(rmse.capped))
nfile = paste("RMSE_ODother_alph1_50Loop", ".png", sep="")
png(nfile, 1000, 800)
rmse.capped
par(mfrow = c(1,1))
ylim = c(min.gini, max.gini)

plot(threshold_scope, rmse.capped, xlab = "Threshold *100k", ylab = "Normalized GINI", 
     ylim = c(min.rmse, max.rmse), pch = 3, lwd = 2, xaxt = "n", col = "black")

legend("topright", legend=c("RMSE"), col = c("black"), pch = 3, lwd = 2, bty = "n")
lines(threshold_scope,rmse.capped,col = c("black"))
axis(1, at=line, labels=paste(line/1000000, "", sep = ""))

dev.off()

