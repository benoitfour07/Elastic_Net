#rm(list = ls(all.names = TRUE)) # Clear all objects
setwd("D:/R_WORK") # set work folder
MAIN_DIR <- "//srvhfile1/ShareFiles/Actuarial/DD/Tech_Pricing/working_2018/sample_code"
DATA_DIR <- "//srvhsas3/DD/temporary/tech_pricing/data_prep_rm_201801"
library(data.table)
library(dplyr)
library(glmnet)



### define scope
TARGET_LOB = "AUTO" # AUTO, BIKE, MOPED


### read data (time-consuming -- run only when necessary)
if(FALSE){
  data_org <- fread(paste(DATA_DIR, "seve.csv", sep="/"), sep=",") %>%
    dplyr:: filter(LOB == TARGET_LOB & VAL_Rand_Model != "Validation")
}



### data prep
data <- data_org
data$INC <- data$INC_MODEL_PD + data$INC_EXCEED_PD # BI, PD, PA, PI, ODdrive, ODother, FBI, FPD, LE
#replace INC by INC_MODEL_PD then filter by INC > 0 to filter on PD cover
data <- filter(data, INC > 0)
source(paste(MAIN_DIR, "1_data_prep.R", sep="/"))
summary(data)
summary(data$INC)

PD <- data


### generate dummy variables
#install.packages("devtools")
#devtools::install_github("toshi-ara/makedummies")
library("makedummies")
PD <- makedummies(PD, basal_level = FALSE)
PD <- select_(PD, c('-starts_with("res")')) # delete strange variables 



### glmnet
R_VAR <- PD$INC
E_VAR <- as.matrix(PD[,!(colnames(PD) %in% c("RESPONSE"))])

cv <- cv.glmnet(
  x=E_VAR,
  y=log(R_VAR),
  family="gaussian",
  alpha=1,
  nfolds=10,
  type.measure="deviance",
  keep=TRUE
)