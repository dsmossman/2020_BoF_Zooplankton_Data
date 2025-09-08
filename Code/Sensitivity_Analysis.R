#####
# Setup

rm(list = ls())

setwd("C:/Users/dmossman/Box/2022 MSc Thesis Work/")

library(tidyverse)
library(R.matlab)
library(ggpubr)
library(R.utils)
library(pracma)
library(modelr)

processed_dir = 'Processed_Data/'
sourceDirectory('Code/Hansen Zooplankton Code and Sample Data/src',
                modifiedOnly = F)
figure_dir = 'Visuals/'

#####
# Read in multinet files (tows and Sv values)

MultiData = read_csv("Raw_Data/Spreadsheets/Sept2020_Cruise_Leg_2_Multinet_Log.csv",
                     show_col_types = F)
MultiData = MultiData[c(2:5, 7, 9, 11, 12, 14, 15, 17:22), c(2, 4, 5:16)]
names(MultiData) = c(
  "Tow",
  "Date",
  "StartTime",
  "EndTime",
  "StartLat",
  "StartLong",
  "EndLat",
  "EndLong",
  "Unlockdbarr",
  "Net1dbarr",
  "Net2dbarr",
  "Net3dbarr",
  "Net4dbarr",
  "Net5dbarr"
)

load(paste0(processed_dir, "Net_Data_With_Sv.rda"))

multi_filenames = list.files(paste0(processed_dir, 'Multinet'),
                             pattern = "^Modeled_Sv_[0-9]",
                             full.names = TRUE)
multi_ldf = lapply(multi_filenames, read.table)

remove_outliers = function(x, var) {
  qnt = quantile(unlist(x[var]), probs = c(.25, .75), na.rm = T)
  H = 1.5 * IQR(unlist(x[var]), na.rm = T)
  x %>% filter(!(x[var] <= (qnt[1] - H) | x[var] >= (qnt[2] + H)))
}

basin_list = rbind(t(t(rep("OB", times = 25))), t(t(rep("GMB", times = 40))), t(t(rep("OB", times = 15))))

dB_lower = seq(13.7, 18.7, 0.1)
dB_span = seq(0.5, 5, 0.5)

#####
# Read in Matlab integration files

# Top level of Integration list is tow (2-3 tows per day)

# Within each tow, [[1]] is Sv in linear space, [[2]] is SvDiff in log space,
# and [[3]] is nets; first three are 130kHz(-200kHz), next three are 200kHz(-455kHz), etc

# Within each net, [[1]] is Sv in linear space, [[2]] is SvDiff, and [[3]] is MaskedSv; first
# three are 130kHz(-200kHz), next three are 200kHz(-455kHz), etc

# Vector of dates
dates = c("19", "20", "21", "24", "25", "26")

# Load in each day
for (i in 1:length(dates)) {
  filename = paste0(
    "/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data/",
    dates[i],
    "Sept_Masked_Data.mat"
  )
  assign(paste0("Sept", dates[i], "_Integration"),
         readMat(filename)$Integration)
  rm(filename)
}

# Human-readable names for sublists

DFs = sort(names(which(sapply(.GlobalEnv, is.array))))
DFs = DFs[2:7]

sublist = rep(c("FullSv", "SvDiff", "Net"), times = 5)
for (i in 1:length(sublist)) {
  sublist[i] = paste0(sublist[i], ceiling(i / 3))
}

subsublist = rep(c("NetSv", "NetSvDiff", "NetMaskedSv"), times = 4)
for (i in 1:length(subsublist)) {
  subsublist[i] = paste0(subsublist[i], ceiling(i / 3))
}

for (i in 1:length(DFs)) {
  temp = get(DFs[i])
  for (j in 1:length(temp)) {
    names(temp)[j] = paste0(rownames(temp), j)
    
    for (k in 1:length(temp[[j]])) {
      names(temp[[j]])[k] = sublist[k]
      if (k %% 3 == 0) {
        for (m in 1:length(temp[[j]][[k]])) {
          names(temp[[j]][[k]])[m] = subsublist[m]
        }
      }
    }
  }
  assign(DFs[i], temp)
  rm(temp)
}

#####
# Sensitivity analysis loop - multinet Sv

# Testing various values of lower bound of dB window first
# For each value, I have to:
# - recreate the masked Sv matrix
# - integrate over the masked Sv matrix
# - correlate observed masked Sv with predicted Sv from corresponding multinet tow
# (just correlate the 200kHz to 455kHz band as that seems to be best for large copepods)

r_squared = data.frame()
RMSE = data.frame()

for (a in 1:length(dB_lower)) {
  for (b in 1:length(dB_span)) {
    for (i in 1:length(DFs)) {
      # for each day
      integration = get(DFs[i])
      tows = list()
      
      for (j in 1:length(integration)) {
        # for each tow
        tows[[j]] = list()
        
        for (k in seq(3, 15, by = 3)) {
          # for each net
          
          tows[[j]][[k / 3]] = list()
          
          # only interested in second frequency difference (455kHz - 200kHz)
          tows[[j]][[k / 3]][[1]] = dB_lower[a] < 10 * log10(integration[[j]][[k]][[5]]) &
            (dB_lower[a] + dB_span[b]) > 10 * log10(integration[[j]][[k]][[5]])
          
          # multiply 455kHz Sv by masking matrix and get rid of 0s
          
          integration[[j]][[k]][[6]] = integration[[j]][[k]][[7]] * tows[[j]][[k / 3]][[1]]
          integration[[j]][[k]][[6]][integration[[j]][[k]][[6]] == 0] = NaN
          integration[[j]][[k]][[6]][is.na(integration[[j]][[k]][[6]]) == "TRUE"] = NaN
          
        }
      }
      
      assign(DFs[i], integration)
    }
    
    # Integrate into tables
    
    for (i in 1:length(DFs)) {
      # for each day
      X = get(DFs[i])
      
      netintervals = data.frame(MultiData[MultiData$Date == paste0(dates[i], '-Sep'), 10:14],
                                zeros(nrow(MultiData[MultiData$Date == paste0(dates[i], '-Sep'),]), 1))
      names(netintervals)[6] = "Surface"
      
      for (j in 1:length(X)) {
        # for each tow
        Y = data.frame()
        for (k in seq(3, 15, by = 3)) {
          #for each net
          # get net interval
          interval = netintervals[j, k / 3] - netintervals[j, k / 3 + 1]
          
          # sum echoes
          Y[k / 3, 1] = abs(sum(X[[j]][[k]][[6]], na.rm = TRUE)) / interval
          colnames(Y) = MultiData[MultiData$Date == paste0(dates[i], '-Sep'),]$Tow[j]
        }
        rownames(Y) = c("Net1", "Net2", "Net3", "Net4", "Net5")
        assign(paste0(
          "Integrated_Sv_",
          dates[i],
          "_Sept_2020_multi",
          colnames(Y)
        ),
        Y)
      }
    }
    
    Pattern1 = grep("Integrated", names(.GlobalEnv), value = TRUE)
    Pattern1 = sort(Pattern1)
    echo_masked_ldf = do.call("list", mget(Pattern1))
    
    # Correlation with multinet Sv
    
    temp1 = c()
    temp2 = c()
    
    for (i in 1:length(multi_ldf)) {
      temp1 = rbind(temp1, t(multi_ldf[[i]][3, ]))
      temp2 = rbind(temp2, t(t(echo_masked_ldf[[i]][, ])))
    }
    
    correlation_masked_sv = data.frame(
      basin_list,
      rep(levels(Net_Data$station), each = 5),
      rep(c("455kHz-200kHz"),
          each = length(multi_ldf) * 5),
      rep(colnames(multi_ldf[[1]]), times = length(multi_ldf)),
      NA,
      10 * log10(abs(temp1)),
      10 * log10(abs(temp2))
    )
    
    colnames(correlation_masked_sv) = c(
      "Basin",
      "Station",
      "Frequency_Diff",
      "Net",
      "Community_Comp",
      "Multi_Sv",
      "Echo_Masked_Sv"
    )
    correlation_masked_sv[correlation_masked_sv$Echo_Masked_Sv == -Inf, 7] = NA
    
    for (i in 1:nrow(correlation_masked_sv)) {
      stn = correlation_masked_sv$Station[i]
      netnum = substr(correlation_masked_sv$Net[i], 4, 4)
      
      correlation_masked_sv$Community_Comp[i] = Net_Data$community_comp[which(Net_Data$station == stn &
                                                                                Net_Data$net == netnum)][1]
    }
    
    correlation_masked_sv = correlation_masked_sv %>%
      do(remove_outliers(., "Echo_Masked_Sv"))
    
    models = correlation_masked_sv %>% do(model = summary(lm(Multi_Sv ~ Echo_Masked_Sv, data = .)))
      
    r_squared = rbind(r_squared, c(models$model[[1]][["r.squared"]], dB_lower[a], dB_span[b]))
    
    models = correlation_masked_sv %>% do(model = (lm(Multi_Sv ~ Echo_Masked_Sv, data = .)))
      
      RMSE = rbind(RMSE, c(rmse(models$model[[1]], data = correlation_masked_sv), 
                      dB_lower[a], 
                      dB_span[b]))
  }
}

names(r_squared) = c("Value","dB_Lower","dB_Span")
names(RMSE) = c("Value","dB_Lower","dB_Span")

r_squared$dB_Span = as.factor(as.character(r_squared$dB_Span))
RMSE$dB_Span = as.factor(as.character(RMSE$dB_Span))

save(r_squared,file = paste0(processed_dir,"sensitivity_r_squared_values.rda"))
save(RMSE,file = paste0(processed_dir,"sensitivity_RMSE_values.rda"))

#####
# R-squared and RMSE graphs

load(paste0(processed_dir,"sensitivity_r_squared_values.rda"))
load(paste0(processed_dir,"sensitivity_RMSE_values.rda"))

ggplot(data = RMSE, aes(x = dB_Lower, y = Value, group = dB_Span, color=dB_Span)) +
  geom_line() +
  labs(x = "dB Window Lower Bound",
         y = "RMSE (Sv(net) vs Sv(echo))",
         color = "dB Window Span")

ggsave(filename = paste0(figure_dir,'Sensitivity_RMSE_Values.png'),scale=2)

ggplot(data = r_squared, aes(x = dB_Lower, y = Value, group = dB_Span, color=dB_Span)) +
  geom_line() +
  labs(x = "dB Window Lower Bound",
       y = "r^2 (Sv(net) vs Sv(echo))",
       color = "dB Window Span")

ggsave(filename = paste0(figure_dir, 'Sensitivity_r_squared_Values.png'),scale=2)

RMSE[which(RMSE$Value == min(RMSE$Value)),]
r_squared[which(r_squared$Value == max(r_squared$Value)),]

#####
# Sensitivity analysis loop - cfin Sv
# Second verse same as the first, only use cfin Sv instead of full net Sv
# fold this into above loop at some point

cfin_filenames = list.files(paste0(processed_dir, 'Multinet/CFin'), pattern="^Modeled", full.names=TRUE)
cfin_ldf = lapply(cfin_filenames, read.table)

r_squared_cfin = data.frame()
RMSE_cfin = data.frame()

for (a in 1:length(dB_lower)) {
  for (b in 1:length(dB_span)) {
    for (i in 1:length(DFs)) {
      # for each day
      integration = get(DFs[i])
      tows = list()
      
      for (j in 1:length(integration)) {
        # for each tow
        tows[[j]] = list()
        
        for (k in seq(3, 15, by = 3)) {
          # for each net
          
          tows[[j]][[k / 3]] = list()
          
          # only interested in second frequency difference (200kHz - 455kHz)
          tows[[j]][[k / 3]][[1]] = dB_lower[a] < 10 * log10(abs(integration[[j]][[k]][[5]])) &
            (dB_lower[a] + dB_span[b]) > 10 * log10(abs(integration[[j]][[k]][[5]]))
          
          # multiply 455kHz Sv by masking matrix and get rid of 0s
          
          integration[[j]][[k]][[6]] = integration[[j]][[k]][[7]] * tows[[j]][[k /
                                                                                 3]][[1]]
          integration[[j]][[k]][[6]][integration[[j]][[k]][[6]] == 0] = NaN
          integration[[j]][[k]][[6]][is.na(integration[[j]][[k]][[6]]) == "TRUE"] = NaN
          
        }
      }
      
      assign(DFs[i], integration)
    }
    
    # Integrate into tables
    
    for (i in 1:length(DFs)) {
      # for each day
      X = get(DFs[i])
      
      netintervals = data.frame(MultiData[MultiData$Date == paste0(dates[i], '-Sep'), 10:14],
                                zeros(nrow(MultiData[MultiData$Date == paste0(dates[i], '-Sep'),]), 1))
      names(netintervals)[6] = "Surface"
      
      for (j in 1:length(X)) {
        # for each tow
        Y = data.frame()
        for (k in seq(3, 15, by = 3)) {
          #for each net
          # get net interval
          interval = netintervals[j, k / 3] - netintervals[j, k / 3 + 1]
          
          # average echoes
          Y[k / 3, 1] = abs(sum(X[[j]][[k]][[6]], na.rm = TRUE)) / interval
          colnames(Y) = MultiData[MultiData$Date == paste0(dates[i], '-Sep'),]$Tow[j]
        }
        rownames(Y) = c("Net1", "Net2", "Net3", "Net4", "Net5")
        assign(paste0(
          "Integrated_Sv_",
          dates[i],
          "_Sept_2020_multi",
          colnames(Y)
        ),
        Y)
      }
    }
    
    Pattern1 = sort(grep("Integrated", names(.GlobalEnv), value = TRUE))
    echo_masked_ldf = do.call("list", mget(Pattern1))
    
    # Correlation with cfin Sv
    
    temp1 = c()
    temp2 = c()
    
    for (i in 1:length(multi_ldf)) {
      temp1 = rbind(temp1, t(cfin_ldf[[i]][3, ]))
      temp2 = rbind(temp2, t(t(echo_masked_ldf[[i]][, ])))
    }
    
    correlation_masked_sv = data.frame(
      basin_list,
      rep(levels(Net_Data$station), each = 5),
      rep(c("455kHz-200kHz"),
          each = length(multi_ldf) * 5),
      rep(colnames(cfin_ldf[[1]]), times = length(cfin_ldf)),
      NA,
      10 * log10(temp1),
      10 * log10(temp2)
    )
    
    colnames(correlation_masked_sv) = c(
      "Basin",
      "Station",
      "Frequency_Diff",
      "Net",
      "Community_Comp",
      "Cfin_Sv",
      "Echo_Masked_Sv"
    )
    correlation_masked_sv[correlation_masked_sv$Echo_Masked_Sv == -Inf, 7] = NA
    
    for (i in 1:nrow(correlation_masked_sv)) {
      stn = correlation_masked_sv$Station[i]
      netnum = substr(correlation_masked_sv$Net[i], 4, 4)
      
      correlation_masked_sv$Community_Comp[i] = Net_Data$community_comp[which(Net_Data$station == stn &
                                                                                Net_Data$net == netnum)][1]
    }
    
    correlation_masked_sv = correlation_masked_sv %>%
      do(remove_outliers(., "Echo_Masked_Sv"))
    
    # print(paste0(dB_lower[a], "-", dB_lower[a] + dB_span[b]))
    
    models = correlation_masked_sv %>% do(model = summary(lm(Cfin_Sv ~ Echo_Masked_Sv, data = .)))
    
    r_squared_cfin = rbind(r_squared_cfin, c(models$model[[1]][["r.squared"]], dB_lower[a], dB_span[b]))
    
    models = correlation_masked_sv %>% do(model = (lm(Cfin_Sv ~ Echo_Masked_Sv, data = .)))
    
    RMSE_cfin = rbind(RMSE_cfin, c(rmse(models$model[[1]], data = correlation_masked_sv), 
                         dB_lower[a], 
                         dB_span[b]))
    
    # print(paste0(models[[1]][i], " RMSE_cfin: ", round(
    #   RMSE_cfin(models$model[[i]], data = correlation_masked_sv),
    #   digits = 2
    # )))
  }
}

names(r_squared_cfin) = c("Value","dB_Lower","dB_Span")
names(RMSE_cfin) = c("Value","dB_Lower","dB_Span")

r_squared_cfin$dB_Span = as.factor(as.character(r_squared_cfin$dB_Span))
RMSE_cfin$dB_Span = as.factor(as.character(RMSE_cfin$dB_Span))

save(r_squared_cfin,file = paste0(processed_dir,"sensitivity_r_squared_cfin_values.rda"))
save(RMSE_cfin,file = paste0(processed_dir,"sensitivity_RMSE_cfin_values.rda"))

#####
# R-squared and RMSE graphs

load(paste0(processed_dir,"sensitivity_r_squared_cfin_values.rda"))
load(paste0(processed_dir,"sensitivity_RMSE_cfin_values.rda"))


ggplot(data = RMSE_cfin, aes(x = dB_Lower, y = Value, group=dB_Span, color=dB_Span)) +
  geom_line() +
  labs(x = "dB Window Lower Bound",
       y = "RMSE (Sv(net) vs Sv(cfin))",
       color = "dB Window Span")

ggsave(filename = paste0(figure_dir,'Sensitivity_RMSE_Values_Cfin.png'),scale=2)

ggplot(data = r_squared_cfin, aes(x = dB_Lower, y = Value, group=dB_Span, color=dB_Span)) +
  geom_line() +
  labs(x = "dB Window Lower Bound",
       y = "r^2 (Sv(net) vs Sv(cfin))",
       color = "dB Window Span")

ggsave(filename = paste0(figure_dir,'Sensitivity_r_squared_Values_Cfin.png'),scale=2)

RMSE_cfin[which(RMSE_cfin$Value == min(RMSE_cfin$Value)),]
r_squared_cfin[which(r_squared_cfin$Value == max(r_squared_cfin$Value)),]
