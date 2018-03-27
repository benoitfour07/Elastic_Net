### feature selection
data <- select(data,
               "INC",
               "VAL_Year",
               "VAL_Semester",
               "PER_LicenseColor",
               "PER_InsGender",
               "PER_InsAge",
               "PER_AgeClause",
               "PER_DriverClause",
               "CLA_BM_Num",
               "CLA_BM_67new",
               "CLA_BM_Swap",
               "CLA_PunishmentPeriod",
               "CLA_LAST_EVT_MAIN4",
               "CLA_HIST_EVT_MAIN4",
               "BEH_Usage",
               "BEH_Mileage",
               "VEH_Type",
               "VEH_Age",
               "VEH_LogPrice_MI",
               "VEH_LatestClassBI",
               "VEH_LatestClassPD",
               "VEH_LatestClassPA",
               "VEH_LatestClassOD",
               "VEH_EngineCapacity2",
               "VEH_Torque",
               "VEH_Power",
               "VEH_Weight",
               "VEH_PowerWeight",
               "VEH_Odometer",
               "VEH_YealyAvgOfDrivenDistance",
               "VEH_Hybrid",
               "VEH_ManufactureCountry",
               "POL_RenewalTimes",
               "POL_Formula",
               "POL_Deduct",
               "POL_Option_PE",
               "POL_Option_FB",
               "POL_Option_FamPlus",
               "POL_Option_LadyPlus",
               "POL_Option_PetPlus",
               "POL_Option_EQ",
               "POL_Option_PDexcess",
               "POL_Limit_PD",
               "POL_Limit_PA",
               "POL_Limit_PI",
               "POL_Limit_LE",
               "POL_MultiCont",
               "POL_InsurerType_B4ADJ",
               "REG_Region",
               "REG_RuralUrban"
)



### grouping granular levels & missing data imputation
data <- mutate(
  data,
  
  CLA_BM_67new = case_when(
    CLA_BM_67new %in% c("6A", "6B", "6C", "6D", "6E") ~ "6new",
    CLA_BM_67new %in% c("7A", "7B", "7C", "7D", "7E") ~ "7new",
    TRUE ~ "OT"
  ),
  
  CLA_PunishmentPeriod = as.integer(case_when(
    CLA_PunishmentPeriod %in% c("6new", "7new") ~ "0",
    TRUE ~ CLA_PunishmentPeriod
  )),
  
  VEH_Type = case_when(
    VEH_Type %in% c("4_Truck_00_05", "5_Truck_05_20", "6_Truck_Small", "7_Truck_Light", "8_CampingCar") ~ "Other",
    TRUE ~ VEH_Type
  ),
  
  VEH_LatestClassBI = as.integer(case_when(
    VEH_LatestClassBI == "X" ~ "4",
    TRUE ~ VEH_LatestClassBI
  )),
  
  VEH_LatestClassPD = as.integer(case_when(
    VEH_LatestClassPD == "X" ~ "4",
    TRUE ~ VEH_LatestClassPD
  )),
  
  VEH_LatestClassPA = as.integer(case_when(
    VEH_LatestClassPA == "X" ~ "4",
    TRUE ~ VEH_LatestClassPA
  )),
  
  VEH_LatestClassOD = as.integer(case_when(
    VEH_LatestClassOD == "X" ~ "4",
    TRUE ~ VEH_LatestClassOD
  )),
  
  VEH_EngineCapacity2 = case_when(
    is.na(VEH_EngineCapacity2) ~ as.integer(median(VEH_EngineCapacity2, na.rm=TRUE)),
    TRUE ~ VEH_EngineCapacity2
  ),
  
  VEH_Torque = case_when(
    is.na(VEH_Torque) ~ as.integer(median(VEH_Torque, na.rm=TRUE)),
    TRUE ~ VEH_Torque
  ),
  
  VEH_Power = case_when(
    is.na(VEH_Power) ~ as.integer(median(VEH_Power, na.rm=TRUE)),
    TRUE ~ VEH_Power
  ),
  
  VEH_Weight = case_when(
    is.na(VEH_Weight) ~ as.integer(median(VEH_Weight, na.rm=TRUE)),
    TRUE ~ VEH_Weight
  ),
  
  VEH_PowerWeight = case_when(
    is.na(VEH_PowerWeight) ~ as.double(median(VEH_PowerWeight, na.rm=TRUE)),
    TRUE ~ VEH_PowerWeight
  ),
  
  VEH_YealyAvgOfDrivenDistance = case_when(
    is.na(VEH_YealyAvgOfDrivenDistance) ~ as.integer(median(VEH_YealyAvgOfDrivenDistance, na.rm=TRUE)),
    TRUE ~ VEH_YealyAvgOfDrivenDistance
  ),
  
  VEH_ManufactureCountry = case_when(
    VEH_ManufactureCountry %in% c("Japan", "Germany", "Sweden", "France", "USA") ~ VEH_ManufactureCountry,
    TRUE ~ "Other"
  ),
  
  POL_Deduct = case_when(
    POL_Deduct == "30pct" ~ "050_100",
    TRUE ~ POL_Deduct
  ),
  
  POL_Limit_PD = case_when(
    POL_Limit_PD == 10000000 ~ "10M",
    POL_Limit_PD == 20000000 ~ "20M",
    TRUE ~ "INF"
  ),
  
  POL_Limit_PA = case_when(
    POL_Limit_PA == 5000000 ~ "05M",
    POL_Limit_PA == 10000000 ~ "10M",
    POL_Limit_PA == 15000000 ~ "15M",
    POL_Limit_PA == 20000000 ~ "20M",
    POL_Limit_PA == -1 ~ "INF",
    TRUE ~ "NoPA"
  ),
  
  POL_Limit_PI = case_when(
    POL_Limit_PI == 30000000 ~ "30_40M",
    POL_Limit_PI == 40000000 ~ "30_40M",
    POL_Limit_PI == 50000000 ~ "50_60M",
    POL_Limit_PI == 60000000 ~ "50_60M",
    POL_Limit_PI == 70000000 ~ "70_80M",
    POL_Limit_PI == 80000000 ~ "70_80M",
    POL_Limit_PI == 90000000 ~ "90_100M",
    POL_Limit_PI == 100000000 ~ "90_100M",
    POL_Limit_PI == -1 ~ "INF",
    TRUE ~ "NoPI"
  ),
  
  POL_Limit_LE = case_when(
    POL_Limit_LE == 3000000 ~ "3M",
    TRUE ~ "NoLE"
  ),
  
  VEH_Odometer =  case_when(
    is.na(VEH_Odometer) ~ as.integer(median(VEH_Odometer, na.rm=TRUE)),
    TRUE ~ VEH_Odometer
  )
  
)



### convert all character columns to factors 
data <- mutate_if(data, sapply(data, is.character), as.factor)



### modify class
data  <- transform(data, VAL_Year = as.factor(VAL_Year))