setwd("~/Documents/BoF2020_Cruise/Raw_Data/Cage/OPC/OPC_Data/xlsx")
library(tidyverse)
library(readxl)
library(ggpubr)

# Read in all OPC excel data files
OPC_Files = lapply(Sys.glob("OPC*.xlsx"), read_excel)

# Format/trim data frames
for (x in 1:length(OPC_Files)) {
  OPC_Files[[x]] = OPC_Files[[x]][, c(2, 4, 12:17)]
  
  colnames(OPC_Files[[x]])[1] = "Date"
  OPC_Files[[x]]$Date = format(OPC_Files[[x]]$Date, "%d-%b")
  colnames(OPC_Files[[x]])[2] = "Depth_m"
  OPC_Files[[x]]$Depth_m = -OPC_Files[[x]]$Depth_m
  
  attach(OPC_Files[[x]])
  OPC_Files[[x]] = cbind(OPC_Files[[x]], Bin8 + Bin9 + Bin10 + Bin11 + Bin12 + Bin13)
  detach(OPC_Files[[x]])
  colnames(OPC_Files[[x]])[9] = "Bin_Sum"
}

# Assign each listed data frame a name
for (x in 1:9) {
  OPC_Files[[x]] = cbind(rep(paste0("OPC00", x), times = nrow(OPC_Files[[x]])), OPC_Files[[x]])
  colnames(OPC_Files[[x]])[1] = "Cast"
  assign(paste0("OPC00", x), OPC_Files[[x]])
}

for (x in 10:length(OPC_Files)) {
  OPC_Files[[x]] = cbind(rep(paste0("OPC0", x), times = nrow(OPC_Files[[x]])), OPC_Files[[x]])
  colnames(OPC_Files[[x]])[1] = "Cast"
  assign(paste0("OPC0", x), OPC_Files[[x]])
}


OPC001 = OPC001[249:342, ]
OPC002 = OPC002[119:223, ]
OPC003 = OPC003[301:490, ]
OPC004 = OPC004[253:543, ]
OPC005 = OPC005[169:449, ]
OPC006 = OPC006[358:546, ] # date was off in excel sheet, may need to be reconverted
OPC007 = OPC007[253:411, ]
OPC008 = OPC008[339:561, ]
OPC009 = OPC009[289:645, ]
OPC010 = OPC010[191:481, ]
OPC011 = OPC011[166:473, ]
OPC012 = OPC012[207:566, ]
OPC013 = OPC013[172:437, ]
OPC014 = OPC014[173:467, ]
OPC015 = OPC015[, ] # skipped, depths are wonky; ask whoever converted it for deets
OPC016 = OPC016[133:396, ]
OPC017 = OPC017[133:411, ]
OPC018 = OPC018[163:390, ]
OPC019 = OPC019[156:408, ]
OPC020 = OPC020[178:490, ]
OPC021 = OPC021[155:400, ]
OPC022 = OPC022[230:490, ]
OPC023 = OPC023[165:404, ]
OPC024 = OPC024[277:533, ]
OPC025 = OPC025[133:380, ]
OPC026 = OPC026[122:371, ]
OPC027 = OPC027[215:520, ]
OPC028 = OPC028[123:456, ]
OPC029 = OPC029[125:512, ]
OPC030 = OPC030[180:477, ]
OPC031 = OPC031[111:471, ]
OPC032 = OPC032[138:406, ]
OPC032 = OPC032[139:406, ]
OPC033 = OPC033[198:376, ]
OPC034 = OPC034[137:345, ]
OPC035 = OPC035[148:332, ]
OPC036 = OPC036[189:405, ]
OPC037 = OPC037[149:309, ]
OPC038 = OPC038[142:294, ]

OPC_OB_Data = rbind(OPC003, OPC004, OPC005, OPC006, 
                    OPC007, OPC008, OPC009, OPC010, OPC011, OPC012,
                    OPC033, OPC034, OPC035, OPC036, OPC037, OPC038)
write.csv(OPC_OB_Data, file = "~/Documents/BoF2020_Cruise/Processed_Data/Cage/OPC_OB_Data.csv")

OPC_GMB_Data = rbind(OPC013, OPC014, OPC015, OPC016, OPC017, OPC018,
                     OPC019, OPC020, OPC021, OPC022, OPC023, OPC024, OPC025, OPC026,
                     OPC027, OPC028, OPC029, OPC030, OPC031, OPC032)
write.csv(OPC_OB_Data, file = "~/Documents/BoF2020_Cruise/Processed_Data/Cage/OPC_GMB_Data.csv")
