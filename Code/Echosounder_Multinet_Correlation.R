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

# creating the dataframe of values                         
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

# assigning community composition                         
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

# creating dataframe
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

# assigning community composition                         
for(i in 1:nrow(correlation_masked_sv)) {
  stn = correlation_masked_sv$Station[i]
  netnum = substr(correlation_masked_sv$Net[i],4,4)
  
  correlation_masked_sv$Community_Comp[i] = Net_Data$community_comp[which(Net_Data$station == stn & Net_Data$net == netnum)][1]
}

# removing outliers twice                         
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

# MANOVA to look at effects on calculated and measured Sv                         
result = manova(cbind(Multi_Sv, Echo_Masked_Sv) ~ Frequency * Basin,
                data = correlation_masked_sv)
summary(result)
(summary.aov(result)[[" Response Multi_Sv"]])
(summary.aov(result)[[" Response Echo_Masked_Sv"]])

# linear models by different groups
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

# RMSE for linear models by different groups                         
models = correlation_masked_sv %>% group_by(Frequency) %>% do(model = (lm(Multi_Sv ~ Echo_Masked_Sv, data = .)))

for(i in 1:length(models$model)) {
  x = residuals(models$model[[i]])
  print(paste0(models[[1]][i], " RMSE: ", round(sqrt(mean(x^2,na.rm=T)), digits = 2)))
}

#####

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

# creating dataframe                         
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

# assigning community composition                         
for(i in 1:nrow(correlation_copepod)) {
  stn = correlation_copepod$Station[i]
  netnum = substr(correlation_copepod$Net[i],4,4)
  
  correlation_copepod$Community_Comp[i] = Net_Data$community_comp[which(Net_Data$station == stn & Net_Data$net == netnum)][1]
}

# removing outliers twice                         
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

# MANOVA as above
result = manova(cbind(Copepod_Sv, Echo_Masked_Sv) ~ Frequency * Community_Comp,
                data = correlation_copepod)
summary(result)
(summary.aov(result)[[" Response Copepod_Sv"]])
(summary.aov(result)[[" Response Echo_Masked_Sv"]])

# linear models + RMSE as above                         
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
  # filter(Echo_Masked_Sv > median(Echo_Masked_Sv)) %>%
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

models = correlation_cfin %>% 
  group_by(Frequency) %>%
  filter(Echo_Masked_Sv > median(Echo_Masked_Sv)) %>%
  do(model = summary(lm(Cfin_Sv ~ Echo_Masked_Sv, data = .)))

plot(fitted(models[[2]][[1]]),resid(models[[2]][[1]]))
abline(0,0)

plot(fitted(models[[2]][[2]]),resid(models[[2]][[2]]))
abline(0,0)

plot(fitted(models[[2]][[3]]),resid(models[[2]][[3]]))
abline(0,0)

plot(fitted(models[[2]][[4]]),resid(models[[2]][[4]]))
abline(0,0)

# Determining other values when slope = 1
test = data.frame(Echo_Masked_Sv = correlation_cfin$Echo_Masked_Sv[correlation_cfin$Frequency == "455kHz"],
                  Observed = correlation_cfin$Cfin_Sv[correlation_cfin$Frequency == "455kHz"])
test = test[!is.na(test$Observed),]

intercept = mean(test$Observed - test$Echo_Masked_Sv)

test$Predicted = intercept + test$Echo_Masked_Sv
test$Residuals = test$Observed - test$Predicted

test_mean = mean(test$Observed,na.rm=T)

test_SS_reg = sum((test$Predicted - test_mean)^2)
test_SS_err = sum((test$Observed - test$Predicted)^2)
test_SS_total = test_SS_reg + test_SS_err

test_SS_reg/test_SS_total # r^2 for slope=1

test_f = test_SS_reg/(test_SS_err/(69-1-1))
pf(test_f, 
   1, 
   60, 
   lower.tail = FALSE) # p-value for slope=1 model

sqrt(mean(test$Residuals^2, na.rm = TRUE)) # RMSE for slope=1 model

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

