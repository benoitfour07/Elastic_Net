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
TARGET_RES_PA = "INC_MODEL_PA" # BI, PD, PA, PI, ODdrive, ODother, FBI, FPD, LE

###################### read data (time-consuming) ##################
if(FALSE){
  data_org <- fread(paste(DATA_DIR, "seve.csv", sep="/"), sep=",") %>%
    dplyr:: filter(LOB == TARGET_LOB & VAL_Rand_Model != "Validation")
}

data_org <- fread(paste(DATA_DIR, "seve.csv", sep="/"), sep=",") %>%
  dplyr:: filter(LOB == TARGET_LOB & VAL_Rand_Model != "Validation")

###################### data prep ##################################
data <- data_org
data$INC <- data$INC_MODEL_PA + data$INC_EXCEED_PA #replace INC by INC_MODEL_PD then filter by INC > 0 to filter on PD cover
data <- filter(data, INC > 0)
source(paste(MAIN_DIR, "1_data_prep.R", sep="/"))


PA <- data

#################### generate dummy variables ######################
PA <- makedummies(PA, basal_level = FALSE)
PA <- select_(PA, c('-starts_with("res")')) # delete strange variables 
str(PA)
summary(PA$INC)


############### FINAL PA RESULT ##############################
# 1 = Granular from 1 million to 4 millions
PA_final = cap_loop_elnet2(PA,72)
PA_final
PA_final_500 <- PA_final

#Key amounts coeff tab
PA_coeff_1800000 = cap_predict_elnet2(PA,1,1800000,72)
PA_coeff_1800000
sum(PA_coeff_1800000$Result_coeff$NA_freq < 0.2)


# 2 = Not granular from 1 million to 20 million
PA_final_1_to_20M = cap_loop_elnet2(PA,72)
PA_final_1_to_20M
write.csv2(PA_final_1_to_20M[[6]],file = "PA_final_1_to_20M_Coeff")


############################ PLOTTING RESULT ###########################################
#input = cap_loop_elnet2()
PA_final_1_to_20M
elnetPAL = PA_final_1_to_20M
threshold_scope <- elnetPAL$threshold_scope
threshold_scope
ngini.capped <- NULL
for(i in 1:length(elnetPAL$nginiL)){
  ngini.capped[i] <- elnetPAL$nginiL[[i]]
}

se_ngini.capped <- NULL
for(i in 1:length(elnetPAL$se_nginiL)){
  se_ngini.capped[i] <- elnetPAL$se_nginiL[[i]]
}

rmse.capped <- NULL
for(i in 1:length(elnetPAL$rmseL)){
  rmse.capped[i] <- elnetPAL$rmseL[[i]]
}

min.gini = min(unlist(ngini.capped)) - 0.005
max.gini = max(unlist(ngini.capped)) + 0.005
line = seq(1000000,20000000,by = 500000)
nfile = paste("Norm_GINI_PA_alph1_100Loop_1to20M", ".png", sep="")
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

rmse.capped
min.rmse = min(unlist(rmse.capped))
max.rmse = max(unlist(rmse.capped))
nfile = paste("RMSE_PA_alph1_100Loop_1to20M", ".png", sep="")
png(nfile, 1000, 800)

par(mfrow = c(1,1))


plot(threshold_scope, rmse.capped, xlab = "Threshold *Millions", ylab = "RMSE", 
     ylim = c(min.rmse, max.rmse), pch = 3, lwd = 2, xaxt = "n", col = "black")

legend("topright", legend=c("RMSE"), col = c("black"), pch = 3, lwd = 2, bty = "n")
lines(threshold_scope_updated,rmse.capped_updated,col = c("black"))
axis(1, at=line, labels=paste(line/1000000, "", sep = ""))

dev.off()
