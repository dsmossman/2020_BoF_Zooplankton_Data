# Last updated: 28 Aug 2023

#####
rm(list = ls())

setwd("C:/Users/Delphine/Documents/2020_BoF_Zooplankton_Data/")

## Libraries

library(tidyverse)
library(oce)
library(ocedata)
library(R.utils)
library(readxl)
library(rstatix)
library(emmeans)
library(modelr)
library(ggpubr)
library(pracma)
library(lmodel2)

## Project Structure
sourceDirectory('Code/Hansen Zooplankton Code and Sample Data/src',
                modifiedOnly = F)

function_dir = 'Code/Hansen Zooplankton Code and Sample Data/src/'
processed_dir = 'Processed_Data/'
data_dir = 'Raw_Data/'
figure_dir = 'Visuals/'
report_dir = getwd()

#####

## LOADING IN FILES

load(paste0(processed_dir, "Net_Data_With_Sv.rda"))

# Full echosounder Sv/Sv difference measurements

echo_filenames = list.files(paste0(processed_dir, 'Glider'), pattern="^Integrated_Sv_[0-9]{2}_", full.names=TRUE)
echo_ldf = lapply(echo_filenames, function(x) read.table(x, header = TRUE, row.names = 1))

echo_masked_filenames = list.files(paste0(processed_dir, 'Glider'), pattern="^Integrated_Sv_Masked_[0-9]{2}", full.names=TRUE)
echo_masked_ldf = lapply(echo_masked_filenames, function(x) read.table(x, header = TRUE, row.names = 1))

# Full multinet Sv/Sv diff models

multi_filenames = list.files(paste0(processed_dir, 'Multinet'), pattern="^Modeled_Sv_[0-9]", full.names=TRUE)
multi_ldf = lapply(multi_filenames, read.table)

remove_outliers = function(x,var) {
  qnt = quantile(unlist(x[var]), probs=c(.25, .75), na.rm = T)
  H = 1.5 * IQR(unlist(x[var]), na.rm = T)
  x = x %>% filter(!(x[var] <= (qnt[1] - H) | x[var] >= (qnt[2] + H)))
  return(x)
}

#####

## CORRELATION

require(plyr)
corfun<-function(x, y) {
  corr=(cor.test(x, y,
                 alternative="two.sided", method="pearson"))
}
detach("package:plyr")

## Correlating Multi_Sv and Echo_Sv

# frequencies: 1 = 130 kHz, 2 = 200 kHz, 3 = 455 kHz, 4 = 769 kHz
# frequency differences: 1 = 200 - 130 kHz, 2 = 455 - 200 kHz, 3 = 769 - 455 kHz

temp1 = c()
temp2 = c()

for(j in 1:4) { # for each frequency
  for (i in 1:length(multi_ldf)) { # for each multinet tow
    temp1 = rbind(temp1, t(multi_ldf[[i]][j, ])) # frequency change line
    temp2 = rbind(temp2, t(echo_ldf[[i]][j, ])) # frequency change line
  }
}

basin_list = rbind(t(t(rep("OB", times = 25))), t(t(rep("GMB", times = 40))), t(t(rep("OB", times = 15))))

correlation_sv = data.frame(
  rep(basin_list, times = 4),
  rep(rep(levels(Net_Data$station), each = 5), times = 4),
  rep(c("130kHz", "200kHz", "455kHz", "769kHz"), each = length(multi_ldf) * 5),
  rep(colnames(multi_ldf[[1]]), times = length(multi_ldf) * 4),
  NA,
  10 * log10(temp1),
  10 * log10(temp2)
)
#rm(temp1, temp2)
colnames(correlation_sv) = c("Basin", "Station", "Frequency", "Net", "Community_Comp", "Multi_Sv", "Echo_Sv")
#correlation_sv$Frequency = as.factor(correlation_sv$Frequency)

for(i in 1:nrow(correlation_sv)) {
  stn = correlation_sv$Station[i]
  netnum = substr(correlation_sv$Net[i],4,4)
  
  correlation_sv$Community_Comp[i] = Net_Data$community_comp[which(Net_Data$station == stn & Net_Data$net == netnum)][1]
}

# scatterplot
echo_plot = ggplot(data = correlation_sv, aes(x = Echo_Sv, y = Multi_Sv, shape = Frequency, color = Frequency)) +
  geom_point(na.rm = TRUE, aes(group = Frequency)) +
  geom_abline(slope = 1, intercept= 0) +
  scale_color_viridis_d(begin = 0.2, end = 0.9) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() + 
  labs(x = "Observed Sv (dB)", y = "Expected Sv (full sample) (dB)", shape = "Frequencies", color = "Frequencies")

ggsave(filename = paste0(figure_dir, "Multi_Echo_Sv_Correlation.png"),scale=2)

# Correlation by groups

require(plyr)
ddply(correlation_sv, .(Basin), summarise,
      z=corfun(Multi_Sv,Echo_Sv)$statistic,
      pval=corfun(Multi_Sv,Echo_Sv)$p.value,
      r2=(corfun(Multi_Sv,Echo_Sv)$estimate)^2,
      alt=corfun(Multi_Sv,Echo_Sv)$alternative
)
ddply(correlation_sv, .(Basin, Frequency), summarise,
      z=corfun(Multi_Sv,Echo_Sv)$statistic,
      pval=corfun(Multi_Sv,Echo_Sv)$p.value,
      r2=(corfun(Multi_Sv,Echo_Sv)$estimate)^2,
      alt=corfun(Multi_Sv,Echo_Sv)$alternative
)
ddply(correlation_sv, .(Community_Comp), summarise,
      z=corfun(Multi_Sv,Echo_Sv)$statistic,
      pval=corfun(Multi_Sv,Echo_Sv)$p.value,
      r2=(corfun(Multi_Sv,Echo_Sv)$estimate)^2,
      alt=corfun(Multi_Sv,Echo_Sv)$alternative
)
ddply(correlation_sv, .(Frequency, Community_Comp), summarise,
      z=corfun(Multi_Sv,Echo_Sv)$statistic,
      pval=corfun(Multi_Sv,Echo_Sv)$p.value,
      r2=(corfun(Multi_Sv,Echo_Sv)$estimate)^2,
      alt=corfun(Multi_Sv,Echo_Sv)$alternative
)
ddply(correlation_sv, .(Frequency), summarise,
      z=corfun(Multi_Sv,Echo_Sv)$statistic,
      pval=corfun(Multi_Sv,Echo_Sv)$p.value,
      r2=(corfun(Multi_Sv,Echo_Sv)$estimate)^2,
      alt=corfun(Multi_Sv,Echo_Sv)$alternative
)

values1 = ddply(correlation_sv, .(Frequency, Community_Comp), summarise,
                z=corfun(Multi_Sv,Echo_Sv)$statistic,
                pval=corfun(Multi_Sv,Echo_Sv)$p.value,
                r2=(corfun(Multi_Sv,Echo_Sv)$estimate)^2,
                alt=corfun(Multi_Sv,Echo_Sv)$alternative
)

detach("package:plyr")



#####
## Correlating Multi_Sv and Echo_Masked_Sv

temp3 = c()
temp4 = c()

for(j in 1:4) {
  for (i in 1:length(multi_ldf)) {
    temp3 = rbind(temp3, t(multi_ldf[[i]][j, ])) # frequency change line
    if(j != 1) {
      temp4 = rbind(temp4, t(echo_masked_ldf[[i]][j - 1, ])) # frequency change line
    }
  }
}
temp4 = as.matrix(c(temp2[1:80], temp4))


correlation_masked_sv = data.frame(
  rep(basin_list, times = 4),
  rep(rep(levels(Net_Data$station), each = 5), times = 4),
  rep(
    c("130kHz", "200kHz", "455kHz", "769kHz"),
    each = length(multi_ldf) * 5
  ),
  rep(colnames(multi_ldf[[1]]), times = length(multi_ldf) * 4),
  NA,
  10 * log10(temp3),
  10 * log10(temp4)
)
#rm(temp1, temp2)
colnames(correlation_masked_sv) = c("Basin", "Station", "Frequency", "Net", "Community_Comp", "Multi_Sv", "Echo_Masked_Sv")
correlation_masked_sv[correlation_masked_sv$Echo_Masked_Sv == -Inf,7] = NA

for(i in 1:nrow(correlation_masked_sv)) {
  stn = correlation_masked_sv$Station[i]
  netnum = substr(correlation_masked_sv$Net[i],4,4)
  
  correlation_masked_sv$Community_Comp[i] = Net_Data$community_comp[which(Net_Data$station == stn & Net_Data$net == netnum)][1]
}

correlation_masked_sv = correlation_masked_sv %>%
  group_by(Frequency) %>%
  do(remove_outliers(.,"Echo_Masked_Sv")) %>%
  do(remove_outliers(.,"Echo_Masked_Sv"))

echo_masked_plot = ggplot(data = correlation_masked_sv, aes(x = Echo_Masked_Sv, y = Multi_Sv, shape = Frequency, color = Frequency)) +
  geom_point(na.rm = TRUE, aes(group = Frequency),size=1) +
  geom_abline(slope = 1, intercept= 0) +
  scale_color_viridis_d(begin = 0.2, end = 0.9) +
  geom_smooth(method = "lm", se = FALSE) +
  # stat_regline_equation(label.x = c(-70, -74, -72, -69),
  #   label.y = c(-125, -117, -103, -89),
  #   aes(label = ..rr.label..),
  #   show.legend = F) +
  theme_bw() + theme(text = element_text(size = 16)) +
  geom_text(inherit.aes = F, x = -35, y = -57, label = "Full Model", color = "black", fontface = "bold", size = 6) +
  coord_cartesian(xlim = c(-95,-25), ylim = c(-140, -55)) +
  theme_bw() + theme(text = element_text(size = 16)) +
  labs(x = "Masked Sv(echo) (dB)", y = "Sv(full) (dB)", shape = "Frequency", color = "Frequency")

ggsave(filename = paste0(figure_dir, "Multi_Echo_Sv_Masked_Correlation.png"),scale=2)

# Correlation by groups
require(plyr)
ddply(correlation_masked_sv, .(Basin), summarise,
      z=corfun(Multi_Sv,Echo_Masked_Sv)$statistic,
      pval=corfun(Multi_Sv,Echo_Masked_Sv)$p.value,
      r2=(corfun(Multi_Sv,Echo_Masked_Sv)$estimate)^2,
      alt=corfun(Multi_Sv,Echo_Masked_Sv)$alternative
)
ddply(correlation_masked_sv, .(Basin, Frequency), summarise,
      z=corfun(Multi_Sv,Echo_Masked_Sv)$statistic,
      pval=corfun(Multi_Sv,Echo_Masked_Sv)$p.value,
      r2=(corfun(Multi_Sv,Echo_Masked_Sv)$estimate)^2,
      alt=corfun(Multi_Sv,Echo_Masked_Sv)$alternative
)
ddply(correlation_masked_sv, .(Community_Comp), summarise,
      z=corfun(Multi_Sv,Echo_Masked_Sv)$statistic,
      pval=corfun(Multi_Sv,Echo_Masked_Sv)$p.value,
      r2=(corfun(Multi_Sv,Echo_Masked_Sv)$estimate)^2,
      alt=corfun(Multi_Sv,Echo_Masked_Sv)$alternative
)
ddply(correlation_masked_sv, .(Frequency, Community_Comp), summarise,
      z=corfun(Multi_Sv,Echo_Masked_Sv)$statistic,
      pval=corfun(Multi_Sv,Echo_Masked_Sv)$p.value,
      r2=(corfun(Multi_Sv,Echo_Masked_Sv)$estimate)^2,
      alt=corfun(Multi_Sv,Echo_Masked_Sv)$alternative
)

ddply(correlation_masked_sv, .(Frequency), summarise,
      z=corfun(Multi_Sv,Echo_Masked_Sv)$statistic,
      pval=corfun(Multi_Sv,Echo_Masked_Sv)$p.value,
      r2=(corfun(Multi_Sv,Echo_Masked_Sv)$estimate)^2,
      alt=corfun(Multi_Sv,Echo_Masked_Sv)$alternative
)

values3 = ddply(correlation_masked_sv, .(Frequency, Community_Comp), summarise,
                z=corfun(Multi_Sv,Echo_Masked_Sv)$statistic,
                pval=corfun(Multi_Sv,Echo_Masked_Sv)$p.value,
                r2=(corfun(Multi_Sv,Echo_Masked_Sv)$estimate)^2,
                alt=corfun(Multi_Sv,Echo_Masked_Sv)$alternative
)

detach("package:plyr")

result = manova(cbind(Multi_Sv, Echo_Masked_Sv) ~ Frequency * Basin,
                data = correlation_masked_sv)
summary(result)
(summary.aov(result)[[" Response Multi_Sv"]])
(summary.aov(result)[[" Response Echo_Masked_Sv"]])

models = correlation_masked_sv %>% group_by(Frequency) %>% do(model = summary(lm(Multi_Sv ~ Echo_Masked_Sv, data = .)))

for(i in 1:length(models$model)){
  print(paste0(models[[1]][i], ": "))
  print(round(models$model[[i]][["coefficients"]], digits = 2))
  print(paste0("R-squared: ", round(models$model[[i]][["r.squared"]], digits = 2)))
  print(paste0("p-value: ", pf(models$model[[i]][["fstatistic"]][["value"]], 
                               models$model[[i]][["fstatistic"]][["numdf"]], 
                               models$model[[i]][["fstatistic"]][["dendf"]], 
                               lower.tail = FALSE)))
}

models = correlation_masked_sv %>% group_by(Community_Comp, Frequency) %>% do(model = (lm(Multi_Sv ~ Echo_Masked_Sv, data = .)))

for(i in 1:length(models$model)) {
  x = residuals(models$model[[i]])
  print(paste0(models[[1]][i], " RMSE: ", round(sqrt(mean(x^2,na.rm=T)), digits = 2)))
}

#####

# From Kim: "I would start with comparing the C fin Sv and total copepod Sv to the 455-200 kHz Sv."

# Copepod_Sv includes ALL COPEPODS (large and small)

# Data loading
copepod_filenames = list.files(paste0(processed_dir, 'Multinet/Copepods'), pattern="^Modeled", full.names=TRUE)
copepod_ldf = lapply(copepod_filenames, read.table)

# Correlation
temp5 = c()
temp6 = c()

# frequencies: 1 = 130 kHz, 2 = 200 kHz, 3 = 455 kHz, 4 = 769 kHz
# frequency differences: 1 = 200 - 130 kHz, 2 = 455 - 200 kHz, 3 = 769 - 455 kHz

for(j in 1:4) {
  for (i in 1:length(copepod_ldf)) {
    temp5 = rbind(temp5, t(copepod_ldf[[i]][j, ])) # frequency change line
    if(j != 1) {
    temp6 = rbind(temp6, t(echo_masked_ldf[[i]][j - 1, ])) # frequency change line
    }
  }
}
temp6 = as.matrix(c(temp2[1:80], temp6))

correlation_copepod = data.frame(rep(basin_list, times = 4),
  rep(rep(levels(Net_Data$station), each = 5), times = 4),
  rep(c("130kHz", "200kHz","455kHz","769kHz"), each = length(copepod_ldf) * 5),
  rep(colnames(copepod_ldf[[1]]), times = length(copepod_ldf) * 4),
  NA,
  10 * log10(temp5),
  10 * log10(temp6),
  10 * log10(temp2))
# rm(temp1, temp2, temp3)"
colnames(correlation_copepod) = c("Basin", "Station", "Frequency", "Net", "Community_Comp", "Copepod_Sv", "Echo_Masked_Sv", "Echo_Sv")
correlation_copepod[correlation_copepod$Echo_Masked_Sv == -Inf,7] = NA

for(i in 1:nrow(correlation_copepod)) {
  stn = correlation_copepod$Station[i]
  netnum = substr(correlation_copepod$Net[i],4,4)
  
  correlation_copepod$Community_Comp[i] = Net_Data$community_comp[which(Net_Data$station == stn & Net_Data$net == netnum)][1]
}

correlation_copepod = correlation_copepod %>%
  group_by(Frequency) %>%
  do(remove_outliers(.,"Echo_Masked_Sv")) %>%
  do(remove_outliers(.,"Echo_Masked_Sv"))

# Plots

copepod_plot = ggplot(data = correlation_copepod, aes(x = Echo_Masked_Sv, y = Copepod_Sv, shape = Frequency, color = Frequency)) +
  geom_point(na.rm = TRUE, aes(group = Frequency),size=1) +
  geom_abline(slope = 1, intercept = 0) +
  scale_color_viridis_d(begin = 0.2, end = 0.9) +
  geom_smooth(method = "lm", se = FALSE) +
  # stat_regline_equation(label.x = c(-74, -74, -72, -70),
  #                       label.y = c(-120, -111, -103, -92),
  #                       aes(label = ..rr.label..),
  #                       show.legend = F) +
  geom_text(inherit.aes = F, x = -40, y = -87, label = "Copepod Model", color = "black", fontface = "bold", size = 6) +
  coord_cartesian(xlim = c(-95,-25), ylim = c(-140, -85)) +
  theme_bw() + theme(text = element_text(size = 16)) +
  labs(x = "Masked Sv(echo) (dB)", y = "Sv(cop) (dB)", shape = "Frequency", color = "Frequency")

ggsave(filename = paste0(figure_dir, "Copepod_Echo_Sv_Masked_Correlation_Freq_Only.png"),scale=2)

# Correlation by groups
require(plyr)
ddply(correlation_copepod, .(Basin), summarise,
      z=corfun(Copepod_Sv,Echo_Masked_Sv)$statistic,
      pval=corfun(Copepod_Sv,Echo_Masked_Sv)$p.value,
      r2=(corfun(Copepod_Sv,Echo_Masked_Sv)$estimate)^2,
      alt=corfun(Copepod_Sv,Echo_Masked_Sv)$alternative
)
ddply(correlation_copepod, .(Frequency), summarise,
      z=corfun(Copepod_Sv,Echo_Masked_Sv)$statistic,
      pval=corfun(Copepod_Sv,Echo_Masked_Sv)$p.value,
      r2=(corfun(Copepod_Sv,Echo_Masked_Sv)$estimate)^2,
      alt=corfun(Copepod_Sv,Echo_Masked_Sv)$alternative
)
ddply(correlation_copepod, .(Basin, Frequency), summarise,
      z=corfun(Copepod_Sv,Echo_Masked_Sv)$statistic,
      pval=corfun(Copepod_Sv,Echo_Masked_Sv)$p.value,
      r2=(corfun(Copepod_Sv,Echo_Masked_Sv)$estimate)^2,
      alt=corfun(Copepod_Sv,Echo_Masked_Sv)$alternative
)
ddply(correlation_copepod, .(Community_Comp), summarise,
      z=corfun(Copepod_Sv,Echo_Masked_Sv)$statistic,
      pval=corfun(Copepod_Sv,Echo_Masked_Sv)$p.value,
      r2=(corfun(Copepod_Sv,Echo_Masked_Sv)$estimate)^2,
      alt=corfun(Copepod_Sv,Echo_Masked_Sv)$alternative
)
ddply(correlation_copepod, .(Frequency, Community_Comp), summarise,
      z=corfun(Copepod_Sv,Echo_Masked_Sv)$statistic,
      pval=corfun(Copepod_Sv,Echo_Masked_Sv)$p.value,
      r2=(corfun(Copepod_Sv,Echo_Masked_Sv)$estimate)^2,
      alt=corfun(Copepod_Sv,Echo_Masked_Sv)$alternative
)
detach("package:plyr")

result = manova(cbind(Copepod_Sv, Echo_Masked_Sv) ~ Frequency * Community_Comp,
                data = correlation_copepod)
summary(result)
(summary.aov(result)[[" Response Copepod_Sv"]])
(summary.aov(result)[[" Response Echo_Masked_Sv"]])

models = correlation_copepod %>% group_by(Frequency) %>% do(model = summary(lm(Copepod_Sv ~ Echo_Sv, data = .)))

for(i in 1:length(models$model)){
  print(paste0(models[[1]][i], ": "))
  print(round(models$model[[i]][["coefficients"]], digits = 2))
  print(paste0("R-squared: ", signif(models$model[[i]][["r.squared"]], digits = 2)))
  print(paste0("p-value: ", signif(pf(models$model[[i]][["fstatistic"]][["value"]], 
     models$model[[i]][["fstatistic"]][["numdf"]], 
     models$model[[i]][["fstatistic"]][["dendf"]], 
     lower.tail = FALSE), digits = 2)))
}

models = correlation_copepod %>% group_by(Frequency) %>% do(model = (lm(Copepod_Sv ~ Echo_Sv, data = .)))

for(i in 1:length(models$model)) {
  x = residuals(models$model[[i]])
  print(paste0(models[[1]][i], " RMSE: ", round(sqrt(mean(x^2,na.rm=T)), digits = 2)))
}

#####

cfin_filenames = list.files(paste0(processed_dir, 'Multinet/CFin'), pattern="^Modeled", full.names=TRUE)
cfin_ldf = lapply(cfin_filenames, read.table)


# Correlation
temp9 = c()
temp10 = c()

# frequencies: 1 = 130 kHz, 2 = 200 kHz, 3 = 455 kHz, 4 = 769 kHz
# frequency differences: 1 = 200 - 130 kHz, 2 = 455 - 200 kHz, 3 = 769 - 455 kHz

for(j in 1:4) {
  for (i in 1:length(cfin_ldf)) {
    temp9 = rbind(temp9, t(cfin_ldf[[i]][j, ])) # frequency change line
    if(j != 1) {
      temp10 = rbind(temp10, t(echo_masked_ldf[[i]][j - 1, ])) # frequency change line
    }
  }
}
temp10 = as.matrix(c(temp2[1:80], temp10))

correlation_cfin = data.frame(rep(basin_list, times = 4),
  rep(rep(levels(Net_Data$station), each = 5), times = 4),
  rep(c("130kHz","200kHz","455kHz","769kHz"), each = length(cfin_ldf) * 5),
  rep(colnames(cfin_ldf[[1]]), times = length(cfin_ldf) * 4),
  NA,
  10 * log10(temp9),
  10 * log10(temp10))
# rm(temp1, temp2, temp3)
colnames(correlation_cfin) = c("Basin", "Station", "Frequency", "Net", "Community_Comp", "Cfin_Sv", "Echo_Masked_Sv")
correlation_cfin[correlation_cfin$Echo_Masked_Sv == -Inf,7] = NA

for(i in 1:nrow(correlation_cfin)) {
  stn = correlation_cfin$Station[i]
  netnum = substr(correlation_cfin$Net[i],4,4)
  
  correlation_cfin$Community_Comp[i] = Net_Data$community_comp[which(Net_Data$station == stn & Net_Data$net == netnum)][1]
}

correlation_cfin = correlation_cfin %>%
  group_by(Frequency) %>%
  do(remove_outliers(.,"Echo_Masked_Sv")) %>%
  do(remove_outliers(.,"Echo_Masked_Sv"))
# Plot

cfin_plot = ggplot(data = correlation_cfin, aes(x = Echo_Masked_Sv, y = Cfin_Sv, shape = Frequency, color = Frequency)) +
  geom_point(na.rm = TRUE, aes(group = Frequency), size = 1) +
  geom_abline(slope = 1, intercept = 0) +
  scale_color_viridis_d(begin = 0.2, end = 0.9) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  theme_bw() + theme(text = element_text(size = 16)) +
  # stat_regline_equation(label.x = c(-70, -71, -79, -65),
  #                       label.y = c(-120, -111, -90, -75),
  #                       aes(label = after_stat(eq.label)),
  #                       show.legend = F) +
  # geom_abline(intercept = -9.43,slope=1, linewidth = 1) +
  # geom_text(aes(x=-50,y=-65,label="y = -9.43 + x"),color="black") +
  geom_text(inherit.aes = F, x = -37, y = -65, label = "C. Fin Model", color = "black", fontface = "bold", size = 6) +
  coord_cartesian(xlim = c(-95,-25), ylim = c(-125, -65)) +
  theme_bw() + theme(text = element_text(size = 16)) +
  labs(x = "Masked Sv(echo) (dB)", y = "Sv(cfin) (dB)", shape = "Frequency", color = "Frequency")

ggsave(filename = paste0(figure_dir, "Cfin_Echo_Sv_Masked_Correlation_Freq_Only.png"),scale=2)

# Correlation by frequency difference and net
require(plyr)
ddply(correlation_cfin, .(Basin), summarise,
      z=corfun(Cfin_Sv,Echo_Masked_Sv)$statistic,
      pval=corfun(Cfin_Sv,Echo_Masked_Sv)$p.value,
      r2=(corfun(Cfin_Sv,Echo_Masked_Sv)$estimate)^2,
      alt=corfun(Cfin_Sv,Echo_Masked_Sv)$alternative
)
ddply(correlation_cfin, .(Frequency), summarise,
      z=corfun(Cfin_Sv,Echo_Masked_Sv)$statistic,
      pval=corfun(Cfin_Sv,Echo_Masked_Sv)$p.value,
      r2=(corfun(Cfin_Sv,Echo_Masked_Sv)$estimate)^2,
      alt=corfun(Cfin_Sv,Echo_Masked_Sv)$alternative
)
ddply(correlation_cfin, .(Basin, Frequency), summarise,
      z=corfun(Cfin_Sv,Echo_Masked_Sv)$statistic,
      pval=corfun(Cfin_Sv,Echo_Masked_Sv)$p.value,
      r2=(corfun(Cfin_Sv,Echo_Masked_Sv)$estimate)^2,
      alt=corfun(Cfin_Sv,Echo_Masked_Sv)$alternative
)
ddply(correlation_cfin, .(Community_Comp), summarise,
      z=corfun(Cfin_Sv,Echo_Masked_Sv)$statistic,
      pval=corfun(Cfin_Sv,Echo_Masked_Sv)$p.value,
      r2=(corfun(Cfin_Sv,Echo_Masked_Sv)$estimate)^2,
      alt=corfun(Cfin_Sv,Echo_Masked_Sv)$alternative
)
ddply(correlation_cfin, .(Frequency, Community_Comp), summarise,
      z=corfun(Cfin_Sv,Echo_Masked_Sv)$statistic,
      pval=corfun(Cfin_Sv,Echo_Masked_Sv)$p.value,
      r2=(corfun(Cfin_Sv,Echo_Masked_Sv)$estimate)^2,
      alt=corfun(Cfin_Sv,Echo_Masked_Sv)$alternative
)
detach("package:plyr")

result = manova(cbind(Cfin_Sv, Echo_Masked_Sv) ~ Frequency * Basin,
                data = correlation_cfin)
summary(result)
(summary.aov(result)[[" Response Cfin_Sv"]])
(summary.aov(result)[[" Response Echo_Masked_Sv"]])

models = correlation_cfin %>% 
  group_by(Frequency) %>%
  filter(Echo_Masked_Sv > median(Echo_Masked_Sv)) %>%
  do(model = summary(lm(Cfin_Sv ~ Echo_Masked_Sv, data = .)))

for(i in 1:length(models$model)){
  print(paste0(models[[1]][i], ": "))
  print(round(models$model[[i]][["coefficients"]], digits = 2))
  print(paste0("R-squared: ", signif(models$model[[i]][["r.squared"]], digits = 2)))
  print(paste0("p-value: ", signif(pf(models$model[[i]][["fstatistic"]][["value"]], 
                                      models$model[[i]][["fstatistic"]][["numdf"]], 
                                      models$model[[i]][["fstatistic"]][["dendf"]], 
                                      lower.tail = FALSE), digits = 2)))
}

models = correlation_cfin %>% 
  ungroup() %>%
  group_by(Frequency) %>% 
  filter(Echo_Masked_Sv > median(Echo_Masked_Sv)) %>%
  do(model = (lm(Cfin_Sv ~ Echo_Masked_Sv, data = .))) %>%
  ungroup()

for(i in 1:length(models$model)) {
  x = residuals(models$model[[i]])
  print(paste0(models[[1]][i], " RMSE: ", round(sqrt(mean(x^2,na.rm=T)), digits = 2)))
}

# Potentially force linear model slope to be 1, in order to deal with the OLS...
# weirdness? If slope = 1, then intercept = mean(y - x)
#####
## Residual plots

plot(fitted(models[[2]][[1]]),resid(models[[2]][[1]]))
abline(0,0)

plot(fitted(models[[2]][[2]]),resid(models[[2]][[2]]))
abline(0,0)

plot(fitted(models[[2]][[3]]),resid(models[[2]][[3]]))
abline(0,0)

plot(fitted(models[[2]][[4]]),resid(models[[2]][[4]]))
abline(0,0)

test = data.frame(Echo_Masked_Sv = correlation_cfin$Echo_Masked_Sv[correlation_cfin$Frequency == "455kHz"],
                  Observed = correlation_cfin$Cfin_Sv[correlation_cfin$Frequency == "455kHz"])
test = test[!is.na(test$Observed),]
# test = test[-56,]
# test = test %>% filter(Echo_Masked_Sv < -75.07882)

intercept = mean(test$Observed - test$Echo_Masked_Sv)

test$Predicted = intercept + test$Echo_Masked_Sv
test$Residuals = test$Observed - test$Predicted

test_mean = mean(test$Observed,na.rm=T)

test_SS_reg = sum((test$Predicted - test_mean)^2)
test_SS_err = sum((test$Observed - test$Predicted)^2)
test_SS_total = test_SS_reg + test_SS_err

1 - test_SS_err/test_SS_total

test_f = test_SS_reg/(test_SS_err/(69-1-1))
pf(test_f, 
   1, 
   60, 
   lower.tail = FALSE) # p-value for slope=1 model

sqrt(mean(test$Residuals^2, na.rm = TRUE))

#####

full_plot = ggarrange(echo_masked_plot, copepod_plot, cfin_plot, ncol=1, common.legend = T, legend = "right", labels = "AUTO") +
  theme(plot.background = element_rect(fill="white",color="white"))

ggsave(full_plot, filename = paste0(figure_dir, 'Sv_Correlation_Plot_Full.png'),width=6,height=10,units="in")

#####
## Varying correlation model for Sv(echo) and Sv(cfin)

cfin_plot_vary = ggplot(data = correlation_cfin[correlation_cfin$Frequency == "455kHz",], aes(x = Echo_Masked_Sv, y = Cfin_Sv)) +
  geom_point(color = "#35b779") +
  # All echo data
  geom_smooth(method = "lm", se = FALSE, linewidth = 1, color = "#35b779", aes(group = "All Echo Data", linetype = "All Echo Data")) +
  # Slope forced to 1
  geom_segment(x=-84.48111,y=(intercept-84.48111),
               xend=-51.01692,yend=(intercept-51.01692),
               color="#35b779",linewidth=1,aes(group="Slope=1",linetype="Slope=1"),
               inherit.aes = F) +
  # Top 50% of echo data
  geom_segment(x=-84.48111,y=(-62.75 - 0.4 * 84.48111),
               xend=-51.01692,yend=(-62.75 - 0.4 * 51.01692),
               color="#35b779",linewidth=1,aes(group="Top 50% of Echo Data",linetype="Top 50% of Echo Data"),
               inherit.aes=F) +
  geom_abline(aes(slope = 1, intercept = 0, group = "y=x", linetype = "y=x"), color="black") +
  scale_linetype_manual("455 kHz Equation",values = c("All Echo Data"=1, "Slope=1"=4,"Top 50% of Echo Data"=3,"y=x"=1),
                        breaks = c("All Echo Data", "Top 50% of Echo Data","Slope=1","y=x")) +
  coord_cartesian(xlim = c(-85,-50), ylim = c(-110, -75)) +
  labs(x = "Masked Sv(echo) (dB)", y = "Sv(cfin) (dB)") +
  theme_bw()
ggsave(filename = paste0(figure_dir, "Cfin_Echo_Sv_Masked_Correlation_Varying_Corr_Eq.png"),scale=2)

#####
## The Small Copepods Problem

small_copepods = c("Acartia hudsonica", "Acartia longiremis", "Acartia sp.", "Acartia tonsa",
                   "Centropages hamatus", "Centropages typicus",
                   "Metridia longa", "Metridia lucens", "Metridia sp.",
                   "Microcalanus spp.",
                   "Microsetella norvegica",
                   "Oithona atlantica", "Oithona similis",
                   "Paracalanus spp.",
                   "Pseudocalanus spp.",
                   "Temora longicornis");

large_copepods = c("Calanus finmarchicus", "Calanus glacialis", "Calanus hyperboreus", "Calanus sp.",
                   "Paraeuchaeta norvegica", "Paraeuchaeta sp.",
                   "Pleuromamma robusta");

Copepod_Data = Net_Data[which(Net_Data$taxa %in% small_copepods | Net_Data$taxa %in% large_copepods),]
# Copepod_Data = Copepod_Data[which(Copepod_Data$weighted_concentration > 3000),]

# ggplot(data = Copepod_Data, aes(x = net, y = biomass)) +
#   geom_bar(stat = "identity", aes(fill = taxa)) +
#   facet_wrap(~station)
# 
# ggsave(paste0(figure_dir, "/Copepod_Biomass_By_Station.png"))
# 
# correlation_copepod_test = correlation_copepod[which(correlation_copepod$Frequency != "200kHz-130kHz"),]
# correlation_copepod_test$Station = as.numeric(correlation_copepod_test$Station)
# 
# ggplot(data = correlation_copepod_test, aes(x = Echo_Diff_Sv, y = Copepod_Sv, shape = Net, color = Frequency)) +
#   geom_point(na.rm = TRUE, aes(group = Frequency)) +
#   geom_abline(slope = 1, intercept= 0) +
#   facet_wrap(~Station, scales = "free_y")
# 
# ggsave(paste0(figure_dir, "/Copepod_Echo_Sv_Diff_Correlation_By_Station.png"))

# ANCOVA on copepod biomass with Echo_Sv and Echo_Diff_Sv at high frequency/frequency diffs
# Echo_Sv and Echo_Diff_Sv are the response variables; Copepod_Biomass is the covariate;
# Frequency/Frequency and Community_Comp are the grouping variables

# Is Copepod_Biomass still log-normally distributed? Yes

biomass_correlation = correlation_sv[which(correlation_sv$Frequency != "130kHz"),]
biomass_correlation$Multi_Sv = NA
colnames(biomass_correlation)[6] = "Log10_Copepod_Biomass"
biomass_correlation$Log10_Total_Biomass = NA
biomass_correlation$Log10_Cfin_Biomass = NA

copepod_biomass = c()
total_biomass = c()
cfin_biomass = c()
copepod_concentration = c()
total_concentration = c()
cfin_concentration = c()
for(i in seq_along(levels(Copepod_Data$station))) {
  # define station
  stn = levels(Copepod_Data$station)[i]
  
  # subset the data
  tmp = subset(Copepod_Data, Copepod_Data$station == stn)
  tmp = droplevels(tmp)
  
  # for each net
  for(j in seq_along(levels(tmp$net))) {
    # define net
    netnum = levels(tmp$net)[j]
    
    # subset the data again
    tmp2 = subset(tmp, tmp$net == netnum)
    tmp2 = droplevels(tmp2)
    
    # get the copepod biomass in the net
    copepod_biomass = rbind(copepod_biomass, sum(tmp2$biomass, na.rm = TRUE))
    copepod_concentration = rbind(copepod_concentration,sum(tmp2$concentration, na.rm=TRUE))
  }
  
  stn = levels(Net_Data$station)[i]
  
  # subset the data
  tmp = subset(Net_Data, Net_Data$station == stn)
  tmp = droplevels(tmp)
  
  # for each net
  for(j in seq_along(levels(tmp$net))) {
    # define net
    netnum = levels(tmp$net)[j]
    
    # subset the data again
    tmp2 = subset(tmp, tmp$net == netnum)
    tmp2 = droplevels(tmp2)
    
    # get the copepod biomass in the net
    total_biomass = rbind(total_biomass, sum(tmp2$biomass, na.rm = TRUE))
    total_concentration = rbind(total_concentration, sum(tmp2$concentration, na.rm=TRUE))
  }
  
  stn = levels(Copepod_Data$station)[i]
  
  # subset the data
  tmp = subset(Copepod_Data, (Copepod_Data$station == stn & Copepod_Data$taxa == "Calanus finmarchicus"))
  tmp = droplevels(tmp)
  
  # for each net
  for(j in 1:5) {
    # define net
    netnum = levels(tmp$net)[j]
    
    # subset the data again
    tmp2 = subset(tmp, tmp$net == netnum)
    
    # get the copepod biomass in the net
    cfin_biomass = rbind(cfin_biomass, sum(tmp2$biomass, na.rm = TRUE))
    cfin_concentration = rbind(cfin_concentration, sum(tmp2$concentration, na.rm=TRUE))
  }
}

biomass_correlation$Log10_Copepod_Biomass = rep(copepod_biomass, times = 3)
biomass_correlation$Log10_Total_Biomass = rep(total_biomass, times = 3)
biomass_correlation$Log10_Cfin_Biomass = rep(total_biomass, times = 3)
biomass_correlation$Log10_Copepod_Biomass = log10(biomass_correlation$Log10_Copepod_Biomass)
biomass_correlation$Log10_Total_Biomass = log10(biomass_correlation$Log10_Total_Biomass)
biomass_correlation$Log10_Cfin_Biomass = log10(biomass_correlation$Log10_Cfin_Biomass)
biomass_correlation$Log10_Copepod_Concentration = rep(copepod_concentration, times = 3)
biomass_correlation$Log10_Total_Concentration = rep(total_concentration, times = 3)
biomass_correlation$Log10_Cfin_Concentration = rep(total_concentration, times = 3)
biomass_correlation$Log10_Copepod_Concentration = log10(biomass_correlation$Log10_Copepod_Concentration)
biomass_correlation$Log10_Total_Concentration = log10(biomass_correlation$Log10_Total_Concentration)
biomass_correlation$Log10_Cfin_Concentration = log10(biomass_correlation$Log10_Cfin_Concentration)

biomass_correlation$Frequency = as.factor(biomass_correlation$Frequency)
biomass_correlation$Community_Comp = as.factor(biomass_correlation$Community_Comp)

## Test ANCOVA assumptions
# Linearity within groups

ggplot(data = biomass_correlation, aes(x = Log10_Copepod_Biomass, y = Echo_Sv)) + 
  geom_point() +
  geom_smooth(span = 0.9) +
  facet_wrap(~Community_Comp + Frequency)

# Result: low biomass not linear, others are

# Homogeneity of regression slopes

biomass_correlation %>% anova_test(Echo_Sv ~ Log10_Copepod_Biomass * Frequency * Community_Comp)

# Result: no significant interaction between covariate and grouping variables

# Normality of residuals

model = lm(Echo_Sv ~ Log10_Copepod_Biomass + Frequency*Community_Comp, data = biomass_correlation)
model_metrics = augment(model)
shapiro_test(model_metrics$.resid)

# Result: residuals are normal (p = 0.0924)

# Homogeneity of variances

levene_test(data = model_metrics, .resid ~ Frequency*Community_Comp)

# Result: variances are NOT approximately homogeneous (p = 0.00364)

# Outliers

model_metrics %>% filter(abs(.std.resid) > 3) %>% as.data.frame()

# Result: two outliers where |standard deviation of residual| > 3

# ANCOVA computation

res_aov = biomass_correlation %>% anova_test(Echo_Sv ~ Log10_Copepod_Biomass * Frequency * Community_Comp)
get_anova_table(res_aov)

# Result: after adjustment for copepod biomass, there was a statistically significant effect
# of frequency and community composition on Echo_Sv, but not of (log10 of) copepod biomass or the interaction
# between any of them
# This indicates that Echo_Sv is dependent on Frequency and Community_Comp, but not
# (the log10 of) Copepod_Biomass or the interactions

ggplot(data = biomass_correlation, aes(y = Log10_Copepod_Biomass, x = Echo_Sv, colour = Frequency)) +
  geom_point(na.rm = TRUE) +
  scale_color_viridis_d(begin = 0.2, end = 0.9) +
  geom_smooth(method = "lm", se = FALSE, show.legend = F) +
  stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
                        show.legend = F) +
  theme_bw()

ggsave(paste0(figure_dir, "/Copepod_Biomass_Echo_Sv_Correlation_.png"),scale=2)

ggplot(data = biomass_correlation, aes(y = Log10_Total_Biomass, x = Echo_Sv, colour = Frequency)) +
  geom_point(na.rm = TRUE) +
  scale_color_viridis_d(begin = 0.2, end = 0.9) +
  geom_smooth(method = "lm", se = FALSE, show.legend = F) +
  stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
                        show.legend = F) +
  theme_bw()

ggsave(paste0(figure_dir, "/Total_Biomass_Echo_Sv_Correlation_.png"),scale=2)

require(plyr)
ddply(biomass_correlation, .(Net), summarise,
      z=corfun(Log10_Copepod_Biomass,Echo_Sv)$statistic,
      pval=corfun(Log10_Copepod_Biomass,Echo_Sv)$p.value,
      r2=(corfun(Log10_Copepod_Biomass,Echo_Sv)$estimate)^2,
      alt=corfun(Log10_Copepod_Biomass,Echo_Sv)$alternative
)
ddply(biomass_correlation, .(Basin), summarise,
      z=corfun(Log10_Copepod_Biomass,Echo_Sv)$statistic,
      pval=corfun(Log10_Copepod_Biomass,Echo_Sv)$p.value,
      r2=(corfun(Log10_Copepod_Biomass,Echo_Sv)$estimate)^2,
      alt=corfun(Log10_Copepod_Biomass,Echo_Sv)$alternative
)
ddply(biomass_correlation, .(Station), summarise,
      z=corfun(Log10_Copepod_Biomass,Echo_Sv)$statistic,
      pval=corfun(Log10_Copepod_Biomass,Echo_Sv)$p.value,
      r2=(corfun(Log10_Copepod_Biomass,Echo_Sv)$estimate)^2,
      alt=corfun(Log10_Copepod_Biomass,Echo_Sv)$alternative
)
ddply(biomass_correlation, .(Frequency, Community_Comp), summarise,
      z=corfun(Log10_Copepod_Biomass,Echo_Sv)$statistic,
      pval=corfun(Log10_Copepod_Biomass,Echo_Sv)$p.value,
      r2=(corfun(Log10_Copepod_Biomass,Echo_Sv)$estimate)^2,
      alt=corfun(Log10_Copepod_Biomass,Echo_Sv)$alternative
)
ddply(biomass_correlation, .(Frequency), summarise,
      z=corfun(Log10_Copepod_Biomass,Echo_Sv)$statistic,
      pval=corfun(Log10_Copepod_Biomass,Echo_Sv)$p.value,
      r2=(corfun(Log10_Copepod_Biomass,Echo_Sv)$estimate)^2,
      alt=corfun(Log10_Copepod_Biomass,Echo_Sv)$alternative
)
detach("package:plyr")


#####

biomass_masked_correlation = data.frame(
  rep(basin_list, times = 4),
  rep(rep(levels(Net_Data$station), each = 5), times = 4),
  rep(
    c("130kHz", "200kHz-130kHz", "455kHz-200kHz", "769kHz-455kHz"),
    each = length(multi_ldf) * 5
  ),
  rep(colnames(multi_ldf[[1]]), times = length(multi_ldf) * 4),
  NA,
  10 * log10(temp3),
  10 * log10(temp4)
)
#rm(temp1, temp2)
colnames(biomass_masked_correlation) = c("Basin", "Station", "Frequency", "Net", "Community_Comp", "Multi_Sv", "Echo_Masked_Sv")
biomass_masked_correlation[biomass_masked_correlation$Echo_Masked_Sv == -Inf,7] = NA

for(i in 1:nrow(biomass_masked_correlation)) {
  stn = biomass_masked_correlation$Station[i]
  netnum = substr(biomass_masked_correlation$Net[i],4,4)
  
  biomass_masked_correlation$Community_Comp[i] = Net_Data$community_comp[which(Net_Data$station == stn & Net_Data$net == netnum)][1]
}

biomass_masked_correlation$Log10_Copepod_Biomass = NA
biomass_masked_correlation$Log10_Total_Biomass = NA
biomass_masked_correlation$Log10_Cfin_Biomass = NA

biomass_masked_correlation$Log10_Copepod_Biomass = log10(rep(copepod_biomass, times = 4))
biomass_masked_correlation$Log10_Total_Biomass = log10(rep(total_biomass, times = 4))
biomass_masked_correlation$Log10_Cfin_Biomass = log10(rep(cfin_biomass, times = 4))
biomass_masked_correlation[biomass_masked_correlation$Log10_Cfin_Biomass == -Inf, 10] = NA

biomass_masked_correlation$Log10_Copepod_Concentration = NA
biomass_masked_correlation$Log10_Total_Concentration = NA
biomass_masked_correlation$Log10_Cfin_Concentration = NA

biomass_masked_correlation$Log10_Copepod_Concentration = log10(rep(copepod_concentration, times = 4))
biomass_masked_correlation$Log10_Total_Concentration = log10(rep(total_concentration, times = 4))
biomass_masked_correlation$Log10_Cfin_Concentration = log10(rep(cfin_concentration, times = 4))
biomass_masked_correlation[biomass_masked_correlation$Log10_Cfin_Concentration == -Inf, 10] = NA

biomass_masked_correlation = biomass_masked_correlation %>%
  group_by(Frequency) %>%
  do(remove_outliers(.,"Echo_Masked_Sv"))

biomass_masked_correlation$Frequency = as.factor(biomass_masked_correlation$Frequency)
biomass_masked_correlation$Community_Comp = as.factor(biomass_masked_correlation$Community_Comp)



## Test ANCOVA assumptions
# Linearity within groups

ggplot(data = biomass_masked_correlation, aes(x = Log10_Copepod_Biomass, y = Echo_Masked_Sv)) + 
  geom_point() +
  geom_smooth(span = 0.9) +
  facet_wrap(~Frequency + Community_Comp)

# Result: low biomass not approximately linear

# Homogeneity of regression slopes

biomass_masked_correlation %>% ungroup() %>% anova_test(Echo_Masked_Sv ~ Log10_Copepod_Biomass * Community_Comp * Frequency)

# Result: some significant interaction between covariate and grouping variables

# Normality of residuals

model = lm(Echo_Masked_Sv ~ Log10_Copepod_Biomass + Frequency*Community_Comp, data = biomass_masked_correlation)
model_metrics = augment(model)
shapiro_test(model_metrics$.resid)

# Result: residuals are  normal (p = 0.425)

# Homogeneity of variances

levene_test(data = model_metrics, .resid ~ Frequency*Community_Comp)

# Result: variances are NOT approximately homogeneous (p = 0.0000358)

# Outliers

model_metrics %>% filter(abs(.std.resid) > 3) %>% as.data.frame()

# Result: 1 outlier abs(.std.resid) > 3

# ANCOVA computation

res_aov = biomass_masked_correlation %>% ungroup() %>% anova_test(Echo_Masked_Sv ~ Log10_Copepod_Biomass * Frequency * Community_Comp)
get_anova_table(res_aov)

# Result: after adjustment for copepod biomass, there was a statistically significant effect
# of frequency difference and community composition on Echo_Sv, but not of (log10 of) copepod biomass or the interaction
# between any of them
# This indicates that Echo_Diff_Sv is dependent on Frequency and Community_Comp, but not
# (the log10 of) Copepod_Biomass or the interactions

plot1 = ggplot(data = biomass_masked_correlation, aes(y = Log10_Total_Biomass, x = Echo_Masked_Sv, colour = Frequency, shape = Frequency)) +
  geom_point(na.rm = TRUE) +
  scale_color_viridis_d(begin = 0.2, end = 0.9) +
  geom_smooth(method = "lm", se = FALSE, show.legend = F) +
  #stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
  #                      show.legend = F) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  labs(x = "Sv(echo) (dB)", y = "Log10 of Total\nBiomass", color = "Differenced Frequencies", shape = "Differenced Frequencies")

ggsave(paste0(figure_dir, "/Total_Biomass_Echo_Masked_Sv_Correlation.png"),scale=2)

plot2 = ggplot(data = biomass_masked_correlation, aes(y = Log10_Copepod_Biomass, x = Echo_Masked_Sv, colour = Frequency, shape = Frequency)) +
  geom_point(na.rm = TRUE) +
  scale_color_viridis_d(begin = 0.2, end = 0.9) +
  geom_smooth(method = "lm", se = FALSE, show.legend = F) +
  #stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
  #                      show.legend = F) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  labs(x = "Sv(echo) (dB)", y = "Log10 of Copepod\nBiomass", color = "Differenced Frequencies", shape = "Differenced Frequencies")

ggsave(paste0(figure_dir, "/Copepod_Biomass_Echo_Masked_Sv_Correlation.png"),scale=2)

plot3 = ggplot(data = biomass_masked_correlation, aes(y = Log10_Cfin_Biomass, x = Echo_Masked_Sv, colour = Frequency, shape = Frequency)) +
  geom_point(na.rm = TRUE) +
  scale_color_viridis_d(begin = 0.2, end = 0.9) +
  geom_smooth(method = "lm", se = FALSE) +
  #stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
  #                      show.legend = F) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  labs(x = "Sv(echo) (dB)", y = "Log10 of C. finmarchicus\nBiomass", color = "Differenced Frequencies", shape = "Differenced Frequencies")

ggsave(paste0(figure_dir, "/Cfin_Biomass_Echo_Masked_Sv_Correlation.png"),scale=2)

plot4 = ggplot(data = biomass_masked_correlation, aes(y = Log10_Total_Concentration, x = Echo_Masked_Sv, colour = Frequency, shape = Frequency)) +
  geom_point(na.rm = TRUE) +
  scale_color_viridis_d(begin = 0.2, end = 0.9) +
  geom_smooth(method = "lm", se = FALSE, show.legend = F) +
  #stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
  #                      show.legend = F) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  labs(x = "Sv(echo) (dB)", y = "Log10 of Total\nConcentration", color = "Differenced Frequencies", shape = "Differenced Frequencies")

ggsave(paste0(figure_dir, "/Total_Concentration_Echo_Masked_Sv_Correlation.png"),scale=2)

plot5 = ggplot(data = biomass_masked_correlation, aes(y = Log10_Copepod_Concentration, x = Echo_Masked_Sv, colour = Frequency, shape = Frequency)) +
  geom_point(na.rm = TRUE) +
  scale_color_viridis_d(begin = 0.2, end = 0.9) +
  geom_smooth(method = "lm", se = FALSE, show.legend = F) +
  #stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
  #                      show.legend = F) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  labs(x = "Sv(echo) (dB)", y = "Log10 of Copepod\nConcentration", color = "Differenced Frequencies", shape = "Differenced Frequencies")

ggsave(paste0(figure_dir, "/Copepod_Concentration_Echo_Masked_Sv_Correlation.png"),scale=2)

plot6 = ggplot(data = biomass_masked_correlation, aes(y = Log10_Cfin_Concentration, x = Echo_Masked_Sv, colour = Frequency, shape = Frequency)) +
  geom_point(na.rm = TRUE) +
  scale_color_viridis_d(begin = 0.2, end = 0.9) +
  geom_smooth(method = "lm", se = FALSE) +
  #stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
  #                      show.legend = F) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  labs(x = "Sv(echo) (dB)", y = "Log10 of C. finmarchicus\nConcentration", color = "Differenced Frequencies", shape = "Differenced Frequencies")

ggsave(paste0(figure_dir, "/Cfin_Concentration_Echo_Masked_Sv_Correlation.png"),scale=2)

#####

ggarrange(plot1, plot4, plot2, plot5, plot3, plot6, ncol=2,nrow=3, common.legend = T, legend = "right",labels = c("A","D","B","E","C","F"))

ggsave(paste0(figure_dir, "/Biomass_Echo_Masked_Sv_Correlations.png"),width=8,height=8,units="in")


ggplot(data = biomass_masked_correlation, aes(y = Log10_Copepod_Biomass, x = Multi_Sv, colour = Frequency)) +
  geom_point(na.rm = TRUE) +
  scale_color_viridis_d(begin = 0.2, end = 0.9) +
  geom_smooth(method = "lm", se = FALSE, show.legend = F) +
  stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
                        show.legend = F) +
  theme_bw()

ggsave(paste0(figure_dir, "/Copepod_Biomass_Multi_Sv_Correlation_.png"))

ggplot(data = biomass_masked_correlation, aes(y = Log10_Total_Biomass, x = Multi_Sv, colour = Frequency)) +
  geom_point(na.rm = TRUE) +
  scale_color_viridis_d(begin = 0.2, end = 0.9) +
  geom_smooth(method = "lm", se = FALSE, show.legend = F) +
  stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
                        show.legend = F) +
  theme_bw()

ggsave(paste0(figure_dir, "/Total_Biomass_Multi_Sv_Correlation_.png"))

ggplot(data = biomass_masked_correlation, aes(y = Log10_Cfin_Biomass, x = Multi_Sv, colour = Frequency)) +
  geom_point(na.rm = TRUE) +
  scale_color_viridis_d(begin = 0.2, end = 0.9) +
  geom_smooth(method = "lm", se = F) +
  stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
                        show.legend = F) +
  theme_bw()

ggsave(paste0(figure_dir, "/Cfin_Biomass_Multi_Sv_Correlation_.png"))



models = biomass_masked_correlation %>% group_by(Frequency) %>% do(
    total_model = summary(lm(Log10_Total_Biomass ~ Echo_Masked_Sv, data = .)),
    copepod_model = summary(lm(Log10_Copepod_Biomass ~ Echo_Masked_Sv, data = .)),
    cfin_model = summary(lm(Log10_Cfin_Biomass ~ Echo_Masked_Sv, data = .))
  )

for(i in 1:length(models[[1]])){
  print(paste0(names(models))[2])
  print(paste0(models[[1]][i], ": "))
  
  print(round(models$total_model[[i]][["coefficients"]], digits = 2))
  print(paste0("R-squared: ", signif(models$total_model[[i]][["r.squared"]], digits = 2)))
  print(paste0("p-value: ", signif(pf(models$total_model[[i]][["fstatistic"]][["value"]], 
                                      models$total_model[[i]][["fstatistic"]][["numdf"]], 
                                      models$total_model[[i]][["fstatistic"]][["dendf"]], 
                                      lower.tail = FALSE), digits = 2)))
  
  print(paste0(names(models))[3])
  print(paste0(models[[1]][i], ": "))
  
  print(round(models$copepod_model[[i]][["coefficients"]], digits = 2))
  print(paste0("R-squared: ", signif(models$copepod_model[[i]][["r.squared"]], digits = 2)))
  print(paste0("p-value: ", signif(pf(models$copepod_model[[i]][["fstatistic"]][["value"]], 
                                      models$copepod_model[[i]][["fstatistic"]][["numdf"]], 
                                      models$copepod_model[[i]][["fstatistic"]][["dendf"]], 
                                      lower.tail = FALSE), digits = 2)))
  
  print(paste0(names(models))[4])
  print(paste0(models[[1]][i], ": "))
  
  print(round(models$cfin_model[[i]][["coefficients"]], digits = 2))
  print(paste0("R-squared: ", signif(models$cfin_model[[i]][["r.squared"]], digits = 2)))
  print(paste0("p-value: ", signif(pf(models$cfin_model[[i]][["fstatistic"]][["value"]], 
                                      models$cfin_model[[i]][["fstatistic"]][["numdf"]], 
                                      models$cfin_model[[i]][["fstatistic"]][["dendf"]], 
                                      lower.tail = FALSE), digits = 2)))
}

models = biomass_masked_correlation %>% group_by(Frequency) %>% do(
  total_model = (lm(Log10_Total_Biomass ~ Echo_Masked_Sv, data = .)),
  copepod_model = (lm(Log10_Copepod_Biomass ~ Echo_Masked_Sv, data = .)),
  cfin_model = (lm(Log10_Cfin_Biomass ~ Echo_Masked_Sv, data = .))
)

for(i in 1:length(models[[1]])) {
  print(paste0(names(models))[2])
  print(paste0(models[[1]][i], " RMSE: ", round(rmse(models$total_model[[i]], data = biomass_masked_correlation), digits = 2)))
  
  print(paste0(names(models))[3])
  print(paste0(models[[1]][i], " RMSE: ", round(rmse(models$copepod_model[[i]], data = biomass_masked_correlation), digits = 2)))
  
  print(paste0(names(models))[4])
  print(paste0(models[[1]][i], " RMSE: ", round(rmse(models$cfin_model[[i]], data = biomass_masked_correlation), digits = 2)))
}

require(plyr)
ddply(biomass_masked_correlation, .(Frequency, Community_Comp), summarise,
      z=corfun(Echo_Masked_Sv,Log10_Copepod_Biomass)$statistic,
      pval=corfun(Echo_Masked_Sv,Log10_Copepod_Biomass)$p.value,
      r2=(corfun(Echo_Masked_Sv,Log10_Copepod_Biomass)$estimate)^2,
      alt=corfun(Echo_Masked_Sv,Log10_Copepod_Biomass)$alternative
)
ddply(biomass_masked_correlation, .(Frequency), summarise,
      z=corfun(Echo_Masked_Sv,Log10_Copepod_Biomass)$statistic,
      pval=corfun(Echo_Masked_Sv,Log10_Copepod_Biomass)$p.value,
      r2=(corfun(Echo_Masked_Sv,Log10_Copepod_Biomass)$estimate)^2,
      alt=corfun(Echo_Masked_Sv,Log10_Copepod_Biomass)$alternative
)
ddply(biomass_masked_correlation, .(Community_Comp), summarise,
      z=corfun(Echo_Masked_Sv,Log10_Copepod_Biomass)$statistic,
      pval=corfun(Echo_Masked_Sv,Log10_Copepod_Biomass)$p.value,
      r2=(corfun(Echo_Masked_Sv,Log10_Copepod_Biomass)$estimate)^2,
      alt=corfun(Echo_Masked_Sv,Log10_Copepod_Biomass)$alternative
)
ddply(biomass_masked_correlation, .(Frequency, Community_Comp), summarise,
      z=corfun(Echo_Masked_Sv,Log10_Total_Biomass)$statistic,
      pval=corfun(Echo_Masked_Sv,Log10_Total_Biomass)$p.value,
      r2=(corfun(Echo_Masked_Sv,Log10_Total_Biomass)$estimate)^2,
      alt=corfun(Echo_Masked_Sv,Log10_Total_Biomass)$alternative
)
ddply(biomass_masked_correlation, .(Frequency), summarise,
      z=corfun(Echo_Masked_Sv,Log10_Total_Biomass)$statistic,
      pval=corfun(Echo_Masked_Sv,Log10_Total_Biomass)$p.value,
      r2=(corfun(Echo_Masked_Sv,Log10_Total_Biomass)$estimate)^2,
      alt=corfun(Echo_Masked_Sv,Log10_Total_Biomass)$alternative
)
ddply(biomass_masked_correlation, .(Community_Comp), summarise,
      z=corfun(Echo_Masked_Sv,Log10_Total_Biomass)$statistic,
      pval=corfun(Echo_Masked_Sv,Log10_Total_Biomass)$p.value,
      r2=(corfun(Echo_Masked_Sv,Log10_Total_Biomass)$estimate)^2,
      alt=corfun(Echo_Masked_Sv,Log10_Total_Biomass)$alternative
)
ddply(biomass_masked_correlation, .(Frequency, Community_Comp), summarise,
      z=corfun(Echo_Masked_Sv,Log10_Cfin_Biomass)$statistic,
      pval=corfun(Echo_Masked_Sv,Log10_Cfin_Biomass)$p.value,
      r2=(corfun(Echo_Masked_Sv,Log10_Cfin_Biomass)$estimate)^2,
      alt=corfun(Echo_Masked_Sv,Log10_Cfin_Biomass)$alternative
)
ddply(biomass_masked_correlation, .(Frequency), summarise,
      z=corfun(Echo_Masked_Sv,Log10_Cfin_Biomass)$statistic,
      pval=corfun(Echo_Masked_Sv,Log10_Cfin_Biomass)$p.value,
      r2=(corfun(Echo_Masked_Sv,Log10_Cfin_Biomass)$estimate)^2,
      alt=corfun(Echo_Masked_Sv,Log10_Cfin_Biomass)$alternative
)
ddply(biomass_masked_correlation, .(Community_Comp), summarise,
      z=corfun(Echo_Masked_Sv,Log10_Cfin_Biomass)$statistic,
      pval=corfun(Echo_Masked_Sv,Log10_Cfin_Biomass)$p.value,
      r2=(corfun(Echo_Masked_Sv,Log10_Cfin_Biomass)$estimate)^2,
      alt=corfun(Echo_Masked_Sv,Log10_Cfin_Biomass)$alternative
)
detach("package:plyr")

######
## The Siphonophore Problem

pneu_filenames = list.files(paste0(processed_dir, 'Multinet/Pneumatophore/'), pattern="^Min_Pneu_", full.names=TRUE)
pneu_ldf = lapply(pneu_filenames, read.table)

# MANOVA
temp11 = c()
temp12 = c()

# frequencies: 1 = 130 kHz, 2 = 200 kHz, 3 = 455 kHz, 4 = 769 kHz
# frequency differences: 1 = 200 - 130 kHz, 2 = 455 - 200 kHz, 3 = 769 - 455 kHz

for(j in 1:4) {
  for (i in 1:length(pneu_ldf)) {
    temp11 = rbind(temp11, t(multi_ldf[[i]][j, ])) # frequency change line
    temp12 = rbind(temp12, t(pneu_ldf[[i]][j, ])) # frequency change line
  }
}

pneu_comparison = data.frame(
  rep(basin_list, times = 4),
  rep(rep(levels(Net_Data$station), each = 5), times = 4),
  rep(c("130kHz", "200kHz", "455kHz", "769kHz"), each = length(multi_ldf) * 5),
  rep(colnames(multi_ldf[[1]]), times = length(multi_ldf) * 4),
  NA,
  10 * log10(temp11),
  10 * log10(temp12),
  10 * log10(temp2)
)
colnames(pneu_comparison) = c("Basin", "Station", "Frequency", "Net", "Community_Comp", 
                              "Multi_Sv", "Min_Pneu_Count_Multi_Sv", "Echo_Sv")

for(i in 1:nrow(pneu_comparison)) {
  stn = pneu_comparison$Station[i]
  netnum = substr(pneu_comparison$Net[i],4,4)
  
  pneu_comparison$Community_Comp[i] = Net_Data$community_comp[which(Net_Data$station == stn & Net_Data$net == netnum)][1]
}

pneu_comparison_masked = pneu_comparison
pneu_comparison_masked$Frequency = rep(c("130kHz", "200kHz-130kHz","455kHz-200kHz","769kHz-455kHz"), each = 80)
pneu_comparison_masked$Echo_Sv[81:320] = 10 * log10(temp4[81:320])
colnames(pneu_comparison_masked) = c("Basin", "Station", "Frequency", "Net", "Community_Comp", 
                                   "Multi_Sv", "Min_Pneu_Count_Multi_Sv", "Echo_Masked_Sv")
pneu_comparison_masked[pneu_comparison_masked$Echo_Masked_Sv == -Inf,8] = NA

result = manova(cbind(Multi_Sv, Min_Pneu_Count_Multi_Sv) ~ Frequency + Net,
                data = pneu_comparison)
summary(result)

# Just because, test the difference in means
wilcox.test(pneu_comparison$Multi_Sv, pneu_comparison$Min_Pneu_Count_Multi_Sv, paired = T)

# Plot
ggplot(data = pneu_comparison, aes(x = Multi_Sv, y = Min_Pneu_Count_Multi_Sv, shape = Net, color = Frequency)) +
  geom_point(na.rm = TRUE, aes(group = Frequency)) +
  geom_abline(slope = 1, intercept= 0)

ggsave(filename = paste0(figure_dir, "Min_Pneu_Count_Comparison.png"))


# Echo_Sv

ggplot(data = pneu_comparison, aes(x = Echo_Sv, y = Min_Pneu_Count_Multi_Sv, shape = Frequency, color = Frequency)) +
  geom_point(na.rm = TRUE, aes(group = Frequency)) +
  geom_abline(slope = 1, intercept= 0) +
  scale_color_viridis_d(begin = 0.2, end = 0.9) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  theme_bw()

ggsave(filename = paste0(figure_dir, "Min_Pneu_Count_Echo_Sv_Correlation.png"))

require(plyr)
ddply(pneu_comparison, .(Basin), summarise,
      z=corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$statistic,
      pval=corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$p.value,
      r2=(corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$estimate)^2,
      alt=corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$alternative
)
ddply(pneu_comparison, .(Basin, Frequency), summarise,
      z=corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$statistic,
      pval=corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$p.value,
      r2=(corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$estimate)^2,
      alt=corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$alternative
)
ddply(pneu_comparison, .(Community_Comp), summarise,
      z=corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$statistic,
      pval=corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$p.value,
      r2=(corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$estimate)^2,
      alt=corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$alternative
)
ddply(pneu_comparison, .(Frequency, Community_Comp), summarise,
      z=corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$statistic,
      pval=corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$p.value,
      r2=(corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$estimate)^2,
      alt=corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$alternative
)

values2 = ddply(pneu_comparison, .(Frequency, Community_Comp), summarise,
                z=corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$statistic,
                pval=corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$p.value,
                r2=(corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$estimate)^2,
                alt=corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$alternative
)
detach("package:plyr")


# Echo_Masked_Sv

ggplot(data = pneu_comparison_masked, aes(x = Echo_Masked_Sv, y = Min_Pneu_Count_Multi_Sv, shape = Frequency, color = Frequency)) +
  geom_point(na.rm = TRUE, aes(group = Frequency)) +
  geom_abline(slope = 1, intercept= 0) +
  scale_color_viridis_d(begin = 0.2, end = 0.9) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  theme_bw()

ggsave(filename = paste0(figure_dir, "Min_Pneu_Count_Echo_Masked_Sv_Correlation.png"))

require(plyr)
ddply(pneu_comparison_masked, .(Basin), summarise,
      z=corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$statistic,
      pval=corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$p.value,
      r2=(corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$estimate)^2,
      alt=corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$alternative
)
ddply(pneu_comparison_masked, .(Basin, Frequency), summarise,
      z=corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$statistic,
      pval=corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$p.value,
      r2=(corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$estimate)^2,
      alt=corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$alternative
)
ddply(pneu_comparison_masked, .(Community_Comp), summarise,
      z=corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$statistic,
      pval=corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$p.value,
      r2=(corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$estimate)^2,
      alt=corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$alternative
)
ddply(pneu_comparison_masked, .(Frequency, Community_Comp), summarise,
      z=corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$statistic,
      pval=corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$p.value,
      r2=(corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$estimate)^2,
      alt=corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$alternative
)

values4 = ddply(pneu_comparison_masked, .(Frequency, Community_Comp), summarise,
                z=corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$statistic,
                pval=corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$p.value,
                r2=(corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$estimate)^2,
                alt=corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$alternative
)
detach("package:plyr")

# Affected Data
# The affected nets will be any nets that had siphonophore parts but did not have pneumatophores
# So go thru Net_Data and find all the nets that fit those criteria
# First, just get all the nets with siphonophore parts (including pneumatophores)

siph = c("Siphonophora",
         "Siphonophora (colony)",
         "Siphonophora (bract)",
         "Siphonophora (nectophore)",
         "Siphonophora (pneumatophore)",
         "Physonectae (nectophore)")

Siph_Data = Net_Data[which(Net_Data$taxa %in% siph),]
Pneu_Data = Siph_Data[which(Siph_Data$taxa == "Siphonophora (pneumatophore)"),]

for(i in 1:nrow(Pneu_Data)) {
  Siph_Data[which(Siph_Data$tow == Pneu_Data$tow[i] & Siph_Data$net == Pneu_Data$net[i]),] = NA
}
Siph_Data = Siph_Data[!(is.na(Siph_Data$tow)),]

Siph_Data %>% group_by(site, station, tow, net) %>% summarise()

pneu_comparison$Affected_By_Min_Pneu_Estimate = "No"

for(i in 1:nrow(Siph_Data)) {
  pneu_comparison$Affected_By_Min_Pneu_Estimate[which(
    pneu_comparison$Station == Siph_Data$station[i] &
      pneu_comparison$Net == paste0("Net", Siph_Data$net[i])
  )] = "Yes"
}

pneu_comparison$Affected_By_Min_Pneu_Estimate = as.factor(pneu_comparison$Affected_By_Min_Pneu_Estimate)
pneu_comparison_masked$Affected_By_Min_Pneu_Estimate = pneu_comparison$Affected_By_Min_Pneu_Estimate[1:320]

require(plyr)
ddply(pneu_comparison, .(Affected_By_Min_Pneu_Estimate), summarise,
      z=corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$statistic,
      pval=corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$p.value,
      r2=(corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$estimate)^2,
      alt=corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$alternative
)
ddply(pneu_comparison, .(Affected_By_Min_Pneu_Estimate, Frequency), summarise,
      z=corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$statistic,
      pval=corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$p.value,
      r2=(corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$estimate)^2,
      alt=corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$alternative
)
ddply(pneu_comparison, .(Affected_By_Min_Pneu_Estimate, Basin), summarise,
      z=corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$statistic,
      pval=corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$p.value,
      r2=(corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$estimate)^2,
      alt=corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$alternative
)
ddply(pneu_comparison, .(Affected_By_Min_Pneu_Estimate, Net), summarise,
      z=corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$statistic,
      pval=corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$p.value,
      r2=(corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$estimate)^2,
      alt=corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$alternative
)
ddply(pneu_comparison, .(Affected_By_Min_Pneu_Estimate, Community_Comp), summarise,
      z=corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$statistic,
      pval=corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$p.value,
      r2=(corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$estimate)^2,
      alt=corfun(Min_Pneu_Count_Multi_Sv,Echo_Sv)$alternative
)
detach("package:plyr")

require(plyr)
ddply(pneu_comparison_masked, .(Affected_By_Min_Pneu_Estimate), summarise,
      z=corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$statistic,
      pval=corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$p.value,
      r2=(corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$estimate)^2,
      alt=corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$alternative
)
ddply(pneu_comparison_masked, .(Affected_By_Min_Pneu_Estimate, Frequency), summarise,
      z=corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$statistic,
      pval=corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$p.value,
      r2=(corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$estimate)^2,
      alt=corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$alternative
)
ddply(pneu_comparison_masked, .(Affected_By_Min_Pneu_Estimate, Basin), summarise,
      z=corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$statistic,
      pval=corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$p.value,
      r2=(corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$estimate)^2,
      alt=corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$alternative
)
ddply(pneu_comparison_masked, .(Affected_By_Min_Pneu_Estimate, Net), summarise,
      z=corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$statistic,
      pval=corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$p.value,
      r2=(corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$estimate)^2,
      alt=corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$alternative
)
ddply(pneu_comparison_masked, .(Affected_By_Min_Pneu_Estimate, Community_Comp), summarise,
      z=corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$statistic,
      pval=corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$p.value,
      r2=(corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$estimate)^2,
      alt=corfun(Min_Pneu_Count_Multi_Sv,Echo_Masked_Sv)$alternative
)
detach("package:plyr")

##

values1 = cbind(rep("Actual_Pneu_Count", times = nrow(values1)), values1)
colnames(values1)[1] = "Pneu_Group"
values2 = cbind(rep("Min_Est_Pneu_Count", times = nrow(values2)), values2)
colnames(values2)[1] = "Pneu_Group"
values3 = cbind(rep("Actual_Pneu_Count", times = nrow(values3)), values3)
colnames(values3)[1] = "Pneu_Group"
values4 = cbind(rep("Min_Est_Pneu_Count", times = nrow(values4)), values4)
colnames(values4)[1] = "Pneu_Group"

Echo_Sv_Pneu_Comparison = rbind(values1, values2)
Echo_Sv_Pneu_Comparison$Pneu_Group = as.factor(Echo_Sv_Pneu_Comparison$Pneu_Group)
Echo_Sv_Pneu_Comparison$Frequency = as.factor(Echo_Sv_Pneu_Comparison$Frequency)
Echo_Sv_Pneu_Comparison$Community_Comp = as.factor(Echo_Sv_Pneu_Comparison$Community_Comp)

Echo_Masked_Sv_Pneu_Comparison = rbind(values3, values4)
Echo_Masked_Sv_Pneu_Comparison$Pneu_Group = as.factor(Echo_Masked_Sv_Pneu_Comparison$Pneu_Group)
Echo_Masked_Sv_Pneu_Comparison$Frequency = as.factor(Echo_Masked_Sv_Pneu_Comparison$Frequency)
Echo_Masked_Sv_Pneu_Comparison$Community_Comp = as.factor(Echo_Masked_Sv_Pneu_Comparison$Community_Comp)

wilcox.test(r2 ~ Pneu_Group, data = Echo_Sv_Pneu_Comparison, paired = T)
wilcox.test(r2 ~ Pneu_Group, data = Echo_Masked_Sv_Pneu_Comparison, paired = T)

Echo_Sv_Pneu_Comparison = Echo_Sv_Pneu_Comparison %>% group_by(Community_Comp)
Echo_Sv_Pneu_Comparison %>% summarise(result = wilcox_test(cor.est ~ Pneu_Group, data = Echo_Sv_Pneu_Comparison, paired = T))
Echo_Sv_Pneu_Comparison = Echo_Sv_Pneu_Comparison %>% group_by(Frequency)
Echo_Sv_Pneu_Comparison %>% summarise(wilcox_test(cor.est ~ Pneu_Group, data = Echo_Sv_Pneu_Comparison, paired = T))

Echo_Masked_Sv_Pneu_Comparison = Echo_Masked_Sv_Pneu_Comparison %>% group_by(Community_Comp)
Echo_Masked_Sv_Pneu_Comparison %>% summarise(result = wilcox_test(r2 ~ Pneu_Group, data = Echo_Masked_Sv_Pneu_Comparison, paired = T))
ungroup(Echo_Masked_Sv_Pneu_Comparison)

Echo_Masked_Sv_Pneu_Comparison = Echo_Masked_Sv_Pneu_Comparison %>% group_by(Frequency)
Echo_Masked_Sv_Pneu_Comparison %>% summarise(wilcox_test(r2 ~ Pneu_Group, data = Echo_Masked_Sv_Pneu_Comparison, paired = T))
ungroup(Echo_Masked_Sv_Pneu_Comparison)

######
## Shallow and deep C. fin concentrations

load(paste0(processed_dir, "Cfin_Stages.rda"))

ggplot(data = stages) +
  geom_histogram(aes(x = concentration, fill = net),
                 colour = "black",
                 bins = 40) +
  scale_fill_manual(values=rev(oceColorsViridis(6))) +
  theme_bw() +
  facet_wrap(~stage)

ggplot(data = stages) +
  geom_histogram(aes(x = log10(concentration), fill = net),
                 colour = "black",
                 bins = 40) +
  scale_fill_manual(values=rev(oceColorsViridis(6))) +
  theme_bw() +
  facet_wrap(~stage)

Cfin_Shallow = stages[which(stages$net == 5 | stages$net == 4),c(1,2,4,5,9,10,13,14)]

# missing net 4 in tows 5, 6, 7, 9, net 5 in tow 22, nets 4 and 5 in tows 23 and 24

addon = data.frame(rep(c(5, 6, 7, 9, 22, 23, 23, 24, 24), each = 6),
                   rep(c(4, 4, 4, 4, 5, 4, 5, 4, 5), each = 6),
                   "OB",
                   rep(c(4, 5, 6, 7, 18, 19, 19, 20, 20), each = 6),
                   rep(c("CI", "CII", "CIII", "CIV", "CV", "CVIF"), times = 9),
                   0,
                   0,
                   0)
colnames(addon) = colnames(Cfin_Shallow)


Cfin_Shallow = rbind(Cfin_Shallow,addon)
Cfin_Shallow = Cfin_Shallow[order(Cfin_Shallow$tow), ]
rownames(Cfin_Shallow) = 1:nrow(Cfin_Shallow)


Cfin_Shallow %>% group_by(stage, site) %>% summarise(median = median(concentration, na.rm = T),
                                                     std = sd(concentration))
Cfin_Shallow %>% group_by(site) %>% summarise(median = median(concentration, na.rm = T),
                                                     std = sd(concentration))


Cfin_Deep = stages[which(stages$net == 3 | stages$net == 2 | stages$net == 1),c(1,2,4,5,9,10,13,14)]

# missing net 3 in tow 9

addon = data.frame(rep(c(9), each = 6),
                   rep(c(3), each = 6),
                   "OB",
                   rep(c(7), each = 6),
                   rep(c("CI", "CII", "CIII", "CIV", "CV", "CVIF"), times = 1),
                   0,
                   0,
                   0)
colnames(addon) = colnames(Cfin_Deep)

Cfin_Deep = rbind(Cfin_Deep,addon)
Cfin_Deep = Cfin_Deep[order(Cfin_Deep$tow), ]
rownames(Cfin_Deep) = 1:nrow(Cfin_Deep)


Cfin_Deep %>% group_by(stage, site) %>% summarise(median = median(concentration, na.rm = T),
                                                  std = sd(concentration))
Cfin_Deep %>% group_by(site) %>% summarise(median = median(concentration, na.rm = T),
                                                  std = sd(concentration))

stages$net = as.character(stages$net)
stages[stages$net == "1", 2] = "Deep"
stages[stages$net == "2", 2] = "Deep"
stages[stages$net == "3", 2] = "Deep"
stages[stages$net == "4", 2] = "Shallow"
stages[stages$net == "5", 2] = "Shallow"
stages$net = as.factor(stages$net)

summary(aov((concentration) ~ site * net, data = stages[stages$stage == "CV",]))

Cfin_Data = Net_Data[which(Net_Data$taxa == "Calanus finmarchicus"),]

Cfin_Data$net = as.character(Cfin_Data$net)
Cfin_Data[Cfin_Data$net == "1", 2] = "Deep"
Cfin_Data[Cfin_Data$net == "2", 2] = "Deep"
Cfin_Data[Cfin_Data$net == "3", 2] = "Deep"
Cfin_Data[Cfin_Data$net == "4", 2] = "Shallow"
Cfin_Data[Cfin_Data$net == "5", 2] = "Shallow"
Cfin_Data$net = as.factor(Cfin_Data$net)

Cfin_Data %>% group_by(site, net) %>% summarise(median = median(concentration, na.rm = T),
                                           std = sd(concentration))

