#rm(list = ls(all.names = TRUE)) # Clear all objects
setwd("D:/R_WORK") # set work folder
MAIN_DIR <- "//srvhfile1/ShareFiles/Actuarial/DD/Tech_Pricing/working_2018/sample_code"
DATA_DIR <- "//srvhsas3/DD/temporary/tech_pricing/data_prep_rm_201801"
library(data.table)
library(dplyr)
library(glmnet)



### define scope
TARGET_LOB = "AUTO" # AUTO, BIKE, MOPED
TARGET_RES = "INC_MODEL_PD" # BI, PD, PA, PI, ODdrive, ODother, FBI, FPD, LE



### read data (time-consuming -- run only when necessary)
if(FALSE){
  data_org <- fread(paste(DATA_DIR, "seve.csv", sep="/"), sep=",") %>%
    dplyr:: filter(LOB == TARGET_LOB & VAL_Rand_Model != "Validation")
}



### data prep
data <- mutate_(data_org, INC=TARGET_RES)
data <- filter(data, INC > 0)
source(paste(MAIN_DIR, "1_data_prep.R", sep="/"))
summary(data)



### generate dummy variables
#install.packages("devtools")
#devtools::install_github("toshi-ara/makedummies")
library("makedummies")
data <- makedummies(data, basal_level = FALSE)
data <- select_(data, c('-starts_with("res")')) # delete strange variables 



### glmnet
R_VAR <- data$INC
E_VAR <- as.matrix(data[,!(colnames(data) %in% c("RESPONSE"))])

cv <- cv.glmnet(
  x=E_VAR,
  y=log(R_VAR),
  family="gaussian",
  alpha=1,
  nfolds=10,
  type.measure="deviance",
  keep=TRUE
)