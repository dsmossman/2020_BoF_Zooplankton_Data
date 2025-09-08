# Last updated: 2 Dec 2022

#####
## Project Structure
rm(list = ls())

setwd("C:/Users/dmossman/Box/2022 MSc Thesis Work/")

## Libraries

library(tidyverse)
library(oce)
library(ocedata)
library(R.utils)
library(rstatix)
library(emmeans)
library(modelr)
library(ggpubr)
library(pracma)
library(rlang)
library(lmtest)

sourceDirectory('Code/Hansen Zooplankton Code and Sample Data/src',
                modifiedOnly = F)

function_dir = 'Code/Hansen Zooplankton Code and Sample Data/src/'
create_dir(function_dir)
cache_dir = 'Processed_Data/'
create_dir(cache_dir)
data_dir = 'Raw_Data/'
create_dir(data_dir)
figure_dir = 'Visuals/'
create_dir(figure_dir)
report_dir = getwd()
create_dir(report_dir)

#####
## Loading Files

load(paste0(cache_dir, "Net_Data_With_Sv.rda"))

multi_filenames = list.files(paste0(cache_dir, 'Multinet'), pattern="^Modeled_Sv_[0-9]", full.names=TRUE)
multi_ldf = lapply(multi_filenames, read.table)

echo_filenames = list.files(paste0(cache_dir, 'Glider'), pattern="^Integrated_Sv_[0-9]{2}_", full.names=TRUE)
echo_ldf = lapply(echo_filenames, function(x) read.table(x, header = TRUE, row.names = 1))

echo_masked_filenames_1 = list.files(paste0(cache_dir, 'Glider/Integrated_Sv_1db_Mask/'), pattern="^Integrated_Sv_Masked", full.names=TRUE)
echo_masked_ldf_1 = lapply(echo_masked_filenames_1, function(x) read.table(x, header = TRUE, row.names = 1))
echo_masked_filenames_5 = list.files(paste0(cache_dir, 'Glider/Integrated_Sv_5db_Mask/'), pattern="^Integrated_Sv_Masked", full.names=TRUE)
echo_masked_ldf_5 = lapply(echo_masked_filenames_5, function(x) read.table(x, header = TRUE, row.names = 1))
echo_masked_filenames_10 = list.files(paste0(cache_dir, 'Glider/Integrated_Sv_10db_Mask/'), pattern="^Integrated_Sv_Masked", full.names=TRUE)
echo_masked_ldf_10 = lapply(echo_masked_filenames_10, function(x) read.table(x, header = TRUE, row.names = 1))

copepod_filenames = list.files(paste0(cache_dir, 'Multinet/Copepods'), pattern="^Modeled", full.names=TRUE)
copepod_ldf = lapply(copepod_filenames, read.table)

cfin_filenames = list.files(paste0(cache_dir, 'Multinet/CFin'), pattern="^Modeled", full.names=TRUE)
cfin_ldf = lapply(cfin_filenames, read.table)

basin_list = rbind(t(t(rep("OB", times = 25))), t(t(rep("GMB", times = 40))), t(t(rep("OB", times = 15))))

remove_outliers = function(x,var) {
  qnt = quantile(unlist(x[var]), probs=c(.25, .75), na.rm = T)
  H = 1.5 * IQR(unlist(x[var]), na.rm = T)
  x %>% filter(!(x[var] <= (qnt[1] - H) | x[var] >= (qnt[2] + H)))
}

#####
## Creating Data Frames

temp1 = c()
temp2 = c()
temp3 = c()
temp4 = c()

for(j in 1:4) {
  for (i in 1:length(multi_ldf)) {
    temp1 = rbind(temp1, t(multi_ldf[[i]][j, ]))
    temp2 = rbind(temp2, t(echo_ldf[[i]][j, ]))
    temp3 = rbind(temp3, t(copepod_ldf[[i]][j, ]))
    temp4 = rbind(temp4, t(cfin_ldf[[i]][j, ]))
  }
}

temp5 = c()

for(j in 2:4) {
  for (i in 1:length(multi_ldf)) {
      temp5 = rbind(temp5, t(echo_masked_ldf_1[[i]][j - 1, ])) # frequency change line
  }
}
temp5 = as.matrix(c(temp2[1:80], temp5))

temp6 = c()

for(j in 2:4) {
  for (i in 1:length(multi_ldf)) {
    temp6 = rbind(temp6, t(echo_masked_ldf_5[[i]][j - 1, ])) # frequency change line
  }
}
temp6 = as.matrix(c(temp2[1:80], temp6))

temp7 = c()

for(j in 2:4) {
  for (i in 1:length(multi_ldf)) {
    temp7 = rbind(temp7, t(echo_masked_ldf_10[[i]][j - 1, ])) # frequency change line
  }
}
temp7 = as.matrix(c(temp2[1:80], temp7))


correlation_masked_sv_1 = data.frame(
  rep(basin_list, times = 4),
  rep(rep(levels(Net_Data$station), each = 5), times = 4),
  rep(
    c("130kHz", "200kHz-130kHz", "455kHz-200kHz", "769kHz-455kHz"),
    each = length(multi_ldf) * 5
  ),
  rep(colnames(multi_ldf[[1]]), times = length(multi_ldf) * 4),
  NA,
  10 * log10(temp1),
  10 * log10(temp3),
  10 * log10(temp4),
  10 * log10(temp5)
)

colnames(correlation_masked_sv_1) = c("Basin", "Station", "Frequency_Diff", "Net", "Community_Comp", "Multi_Sv", "Copepod_Sv","Cfin_Sv", "Echo_Masked_Sv_1")
correlation_masked_sv_1[correlation_masked_sv_1$Echo_Masked_Sv_1 == -Inf,7] = NA

for(i in 1:nrow(correlation_masked_sv_1)) {
  stn = correlation_masked_sv_1$Station[i]
  netnum = substr(correlation_masked_sv_1$Net[i],4,4)
  
  correlation_masked_sv_1$Community_Comp[i] = Net_Data$community_comp[which(Net_Data$station == stn & Net_Data$net == netnum)][1]
}


correlation_masked_sv_5 = data.frame(
  rep(basin_list, times = 4),
  rep(rep(levels(Net_Data$station), each = 5), times = 4),
  rep(
    c("130kHz", "200kHz-130kHz", "455kHz-200kHz", "769kHz-455kHz"),
    each = length(multi_ldf) * 5
  ),
  rep(colnames(multi_ldf[[1]]), times = length(multi_ldf) * 4),
  NA,
  10 * log10(temp1),
  10 * log10(temp3),
  10 * log10(temp4),
  10 * log10(temp6)
)

colnames(correlation_masked_sv_5) = c("Basin", "Station", "Frequency_Diff", "Net", "Community_Comp", "Multi_Sv", "Copepod_Sv","Cfin_Sv", "Echo_Masked_Sv_5")
correlation_masked_sv_5[correlation_masked_sv_5$Echo_Masked_sv_5 == -Inf,7] = NA

for(i in 1:nrow(correlation_masked_sv_5)) {
  stn = correlation_masked_sv_5$Station[i]
  netnum = substr(correlation_masked_sv_5$Net[i],4,4)
  
  correlation_masked_sv_5$Community_Comp[i] = Net_Data$community_comp[which(Net_Data$station == stn & Net_Data$net == netnum)][1]
}


correlation_masked_sv_10 = data.frame(
  rep(basin_list, times = 4),
  rep(rep(levels(Net_Data$station), each = 5), times = 4),
  rep(
    c("130kHz", "200kHz-130kHz", "455kHz-200kHz", "769kHz-455kHz"),
    each = length(multi_ldf) * 5
  ),
  rep(colnames(multi_ldf[[1]]), times = length(multi_ldf) * 4),
  NA,
  10 * log10(temp1),
  10 * log10(temp3),
  10 * log10(temp4),
  10 * log10(temp7)
)

colnames(correlation_masked_sv_10) = c("Basin", "Station", "Frequency_Diff", "Net", "Community_Comp", "Multi_Sv", "Copepod_Sv","Cfin_Sv", "Echo_Masked_Sv_10")
correlation_masked_sv_10[correlation_masked_sv_10$Echo_Masked_sv_10 == -Inf,7] = NA

for(i in 1:nrow(correlation_masked_sv_10)) {
  stn = correlation_masked_sv_10$Station[i]
  netnum = substr(correlation_masked_sv_10$Net[i],4,4)
  
  correlation_masked_sv_10$Community_Comp[i] = Net_Data$community_comp[which(Net_Data$station == stn & Net_Data$net == netnum)][1]
}


rm(temp1,temp2,temp3,temp4,temp5,temp6,temp7)

# ggplot(data = correlation_masked_sv_1) +
#   geom_boxplot(aes(x = Frequency_Diff, y = Echo_Masked_Sv_1))
# ggplot(data = correlation_masked_sv_5) +
#   geom_boxplot(aes(x = Frequency_Diff, y = Echo_Masked_Sv_5))
# ggplot(data = correlation_masked_sv_10) +
#   geom_boxplot(aes(x = Frequency_Diff, y = Echo_Masked_Sv_10))

correlation_masked_sv_1 = correlation_masked_sv_1 %>%
  group_by(Frequency_Diff) %>%
  do(remove_outliers(.,"Echo_Masked_Sv_1"))
correlation_masked_sv_5 = correlation_masked_sv_5 %>%
  group_by(Frequency_Diff) %>%
  do(remove_outliers(.,"Echo_Masked_Sv_5"))
correlation_masked_sv_10 = correlation_masked_sv_10 %>%
  group_by(Frequency_Diff) %>%
  do(remove_outliers(.,"Echo_Masked_Sv_10"))

#####
## Correlation - Multi_Sv

correlation_masked_sv_1$Frequency_Diff = as.factor(correlation_masked_sv_1$Frequency_Diff)

unweighted_models_1 = list()
summaries_1 = list()

for(j in seq_along(levels(correlation_masked_sv_1$Frequency_Diff))) {
  frq = levels(correlation_masked_sv_1$Frequency_Diff)[j]
  
  tmp = subset(correlation_masked_sv_1, correlation_masked_sv_1$Frequency_Diff == frq)
  tmp = droplevels(tmp)
  
  unweighted_model = lm(Multi_Sv ~ Echo_Masked_Sv_1, data = tmp)
  unweighted_models_1[[j]] = unweighted_model

  summaries_1[[j]] = summary(unweighted_model)
}

lapply(unweighted_models_1, function(x) bptest(x))

summaries_1[[3]]
rmse(unweighted_models_1[[3]], data = correlation_masked_sv_1)


correlation_masked_sv_5$Frequency_Diff = as.factor(correlation_masked_sv_5$Frequency_Diff)

unweighted_models_5 = list()
summaries_5 = list()

for(j in seq_along(levels(correlation_masked_sv_5$Frequency_Diff))) {
  frq = levels(correlation_masked_sv_5$Frequency_Diff)[j]
  
  tmp = subset(correlation_masked_sv_5, correlation_masked_sv_5$Frequency_Diff == frq)
  tmp = droplevels(tmp)
  
  unweighted_model = lm(Multi_Sv ~ Echo_Masked_Sv_5, data = tmp)
  unweighted_models_5[[j]] = unweighted_model
  
  summaries_5[[j]] = summary(unweighted_model)
}

lapply(unweighted_models_5, function(x) bptest(x))

summaries_5[[3]]
rmse(unweighted_models_5[[3]], data = correlation_masked_sv_5)


correlation_masked_sv_10$Frequency_Diff = as.factor(correlation_masked_sv_10$Frequency_Diff)

unweighted_models_10 = list()
summaries_10 = list()

for(j in seq_along(levels(correlation_masked_sv_10$Frequency_Diff))) {
  frq = levels(correlation_masked_sv_10$Frequency_Diff)[j]
  
  tmp = subset(correlation_masked_sv_10, correlation_masked_sv_10$Frequency_Diff == frq)
  tmp = droplevels(tmp)
  
  unweighted_model = lm(Multi_Sv ~ Echo_Masked_Sv_10, data = tmp)
  unweighted_models_10[[j]] = unweighted_model
  
  summaries_10[[j]] = summary(unweighted_model)
}

lapply(unweighted_models_10, function(x) bptest(x))

summaries_10[[3]]
rmse(unweighted_models_10[[3]], data = correlation_masked_sv_10)

#####
## Scatterplots - Multi_Sv

echo_masked_plot_1 = 
  ggplot(data = correlation_masked_sv_1, aes(x = Echo_Masked_Sv_1, y = Multi_Sv, shape = Frequency_Diff, color = Frequency_Diff)) +
  geom_point(na.rm = TRUE, aes(group = Frequency_Diff)) +
  geom_abline(slope = 1, intercept= 0) +
  geom_smooth(method = "lm") +
  stat_regline_equation(show.legend = F) +
  scale_color_viridis_d(begin = 0.2, end = 0.9) +
  theme_bw() + theme(text = element_text(size = 16)) +
  labs(x = "Sv(echo) (dB)", y = "Sv(net) (dB)", shape = "Differenced Frequencies", color = "Differenced Frequencies")

# ggsave(filename = paste0(figure_dir, "dB_Window_Correlations/Multi_Echo_Sv_Masked_1dB_Correlation.png"))


echo_masked_plot_5 = 
ggplot(data = correlation_masked_sv_5, aes(x = Echo_Masked_Sv_5, y = Multi_Sv, shape = Frequency_Diff, color = Frequency_Diff)) +
  geom_point(na.rm = TRUE, aes(group = Frequency_Diff)) +
  geom_abline(slope = 1, intercept= 0) +
  scale_color_viridis_d(begin = 0.2, end = 0.9) +
  geom_smooth(method = "lm") +
  stat_regline_equation(show.legend = F) +
  theme_bw() + theme(text = element_text(size = 16)) +
  labs(x = "Sv(echo) (dB)", y = "Sv(net) (dB)", shape = "Differenced Frequencies", color = "Differenced Frequencies")

# ggsave(filename = paste0(figure_dir, "dB_Window_Correlations/Multi_Echo_Sv_Masked_5dB_Correlation.png"))


echo_masked_plot_10 = 
ggplot(data = correlation_masked_sv_10, aes(x = Echo_Masked_Sv_10, y = Multi_Sv, shape = Frequency_Diff, color = Frequency_Diff)) +
  geom_point(na.rm = TRUE, aes(group = Frequency_Diff)) +
  geom_abline(slope = 1, intercept= 0) +
  scale_color_viridis_d(begin = 0.2, end = 0.9) +
  geom_smooth(method = "lm") +
  stat_regline_equation(show.legend = F) +
  theme_bw() + theme(text = element_text(size = 16)) +
  labs(x = "Sv(echo) (dB)", y = "Sv(net) (dB)", shape = "Differenced Frequencies", color = "Differenced Frequencies")

# ggsave(filename = paste0(figure_dir, "dB_Window_Correlations/Multi_Echo_Sv_Masked_10dB_Correlation.png"))


ggarrange(echo_masked_plot_1,echo_masked_plot_5,echo_masked_plot_10,
          labels = c("1 dB","5 dB","10 dB"), label.y = 1.1,
          common.legend = T, legend = "left") +
  theme(plot.margin = margin(1,0,0,0, "cm"))

#####
## Correlation - Cfin_Sv

correlation_masked_sv_1 = correlation_masked_sv_1[which(!is.na(correlation_masked_sv_1$Cfin_Sv)),]

unweighted_models_1 = list()
summaries_1 = list()

for(j in seq_along(levels(correlation_masked_sv_1$Frequency_Diff))) {
  frq = levels(correlation_masked_sv_1$Frequency_Diff)[j]
  
  tmp = subset(correlation_masked_sv_1, correlation_masked_sv_1$Frequency_Diff == frq)
  tmp = droplevels(tmp)
  
  unweighted_model = lm(Cfin_Sv ~ Echo_Masked_Sv_1, data = tmp)
  unweighted_models_1[[j]] = unweighted_model
  
  summaries_1[[j]] = summary(unweighted_model)
}

lapply(unweighted_models_1, function(x) bptest(x))

summaries_1[[3]]
rmse(unweighted_models_1[[3]], data = correlation_masked_sv_1)


correlation_masked_sv_5 = correlation_masked_sv_5[which(!is.na(correlation_masked_sv_5$Cfin_Sv)),]

unweighted_models_5 = list()
summaries_5 = list()

for(j in seq_along(levels(correlation_masked_sv_5$Frequency_Diff))) {
  frq = levels(correlation_masked_sv_5$Frequency_Diff)[j]
  
  tmp = subset(correlation_masked_sv_5, correlation_masked_sv_5$Frequency_Diff == frq)
  tmp = droplevels(tmp)
  
  unweighted_model = lm(Cfin_Sv ~ Echo_Masked_Sv_5, data = tmp)
  unweighted_models_5[[j]] = unweighted_model
  
  summaries_5[[j]] = summary(unweighted_model)
}

lapply(unweighted_models_5, function(x) bptest(x))

summaries_5[[3]]
rmse(unweighted_models_5[[3]], data = correlation_masked_sv_5)


correlation_masked_sv_10 = correlation_masked_sv_10[which(!is.na(correlation_masked_sv_10$Cfin_Sv)),]

unweighted_models_10 = list()
summaries_10 = list()

for(j in seq_along(levels(correlation_masked_sv_10$Frequency_Diff))) {
  frq = levels(correlation_masked_sv_10$Frequency_Diff)[j]
  
  tmp = subset(correlation_masked_sv_10, correlation_masked_sv_10$Frequency_Diff == frq)
  tmp = droplevels(tmp)
  
  unweighted_model = lm(Cfin_Sv ~ Echo_Masked_Sv_10, data = tmp)
  unweighted_models_10[[j]] = unweighted_model
  
  summaries_10[[j]] = summary(unweighted_model)
}

lapply(unweighted_models_10, function(x) bptest(x))

j = 3
frq = levels(correlation_masked_sv_10$Frequency_Diff)[j]

tmp = subset(correlation_masked_sv_10, correlation_masked_sv_10$Frequency_Diff == frq)
tmp = droplevels(tmp)

unweighted_model = lm(Cfin_Sv ~ Echo_Masked_Sv_10, data = tmp)
wts = lm(abs(unweighted_model$residuals) ~ unweighted_model$fitted.values)$fitted.values^2

weighted_model = lm(Cfin_Sv ~ Echo_Masked_Sv_10, data = tmp, weights = (1/wts))

summary(weighted_model)
rmse(weighted_model, data = correlation_masked_sv_10)

#####
## Scatterplots - Cfin_Sv

echo_masked_plot_1 = 
  ggplot(data = correlation_masked_sv_1, aes(x = Echo_Masked_Sv_1, y = Cfin_Sv, shape = Frequency_Diff, color = Frequency_Diff)) +
  geom_point(na.rm = TRUE, aes(group = Frequency_Diff)) +
  geom_abline(slope = 1, intercept= 0) +
  geom_smooth(method = "lm") +
  stat_regline_equation(show.legend = F) +
  scale_color_viridis_d(begin = 0.2, end = 0.9) +
  theme_bw() + theme(text = element_text(size = 16)) +
  labs(x = "Sv(echo) (dB)", y = "Sv(cfin) (dB)", shape = "Differenced Frequencies", color = "Differenced Frequencies")

# ggsave(filename = paste0(figure_dir, "dB_Window_Correlations/Cfin_Echo_Sv_Masked_1dB_Correlation.png"))

echo_masked_plot_5 = 
  ggplot(data = correlation_masked_sv_5, aes(x = Echo_Masked_Sv_5, y = Cfin_Sv, shape = Frequency_Diff, color = Frequency_Diff)) +
  geom_point(na.rm = TRUE, aes(group = Frequency_Diff)) +
  geom_abline(slope = 1, intercept= 0) +
  scale_color_viridis_d(begin = 0.2, end = 0.9) +
  geom_smooth(method = "lm") +
  stat_regline_equation(show.legend = F) +
  theme_bw() + theme(text = element_text(size = 16)) +
  labs(x = "Sv(echo) (dB)", y = "Sv(cfin) (dB)", shape = "Differenced Frequencies", color = "Differenced Frequencies")

# ggsave(filename = paste0(figure_dir, "dB_Window_Correlations/Cfin_Echo_Sv_Masked_5dB_Correlation.png"))


xmat = weighted_model$model[,1:2] %>% do(as.data.frame(model.matrix(~ Echo_Masked_Sv_10, data = .)))
xmat = as.matrix(xmat)
  
conf = data.frame()
eqn = data.frame()

i = 3
frq = levels(correlation_masked_sv_10$Frequency_Diff)[i]

betahat = coef(weighted_model)
sigmahat = vcov(weighted_model)

varcovEYhat = xmat %*% sigmahat %*% t(xmat)
SEline = sqrt(diag(varcovEYhat))

predicted = fitted(weighted_model)
upconf = predicted + 1.96 * SEline
lowconf = predicted - 1.96 * SEline

conf = rbind(conf, cbind(predicted, upconf, lowconf))

eqn = rbind(eqn, round(as.numeric(coef(
  weighted_model
)), digits = 2))
eqn$Equation = paste0("y = ",eqn[,1]," + ",eqn[,2],"x")



echo_masked_plot_10 = 
  ggplot(data = correlation_masked_sv_10, aes(x = Echo_Masked_Sv_10, y = Cfin_Sv, shape = Frequency_Diff, color = Frequency_Diff)) +
  geom_point(na.rm = TRUE, aes(group = Frequency_Diff)) +
  geom_abline(slope = 1, intercept= 0) +
  geom_smooth(method = "lm", show.legend = F) +
  stat_regline_equation(show.legend = F, label.x = c(-40,-40,-40,-40)) +
  scale_color_viridis_d(begin = 0.2, end = 0.9) +
  theme_bw() + theme(text = element_text(size = 16)) +
  labs(x = "Sv(echo) (dB)", y = "Sv(cfin) (dB)", shape = "Differenced Frequencies", color = "Differenced Frequencies") +
  geom_line(data = weighted_model$model[,1:2], aes(y = predicted, shape = NULL, color = NULL), 
            color = "#00BB73", 
            linewidth = 1,
            linetype = "dashed") +
  geom_ribbon(data = conf, inherit.aes = F, aes(x = weighted_model$model[,2], 
                               y = predicted, ymin = conf$lowconf, ymax = conf$upconf),
              alpha = 0.1,
              fill = "red") +
  geom_text(data = eqn, inherit.aes = F, aes(x = -36, y = -90, label = Equation), color = "#00BB73")

# ggsave(filename = paste0(figure_dir, "dB_Window_Correlations/Cfin_Echo_Sv_Masked_10dB_Correlation.png"))


ggarrange(echo_masked_plot_1,echo_masked_plot_5,echo_masked_plot_10,
          labels = c("1 dB","5 dB","10 dB"), label.y = 1.1,
          common.legend = T, legend = "left") +
  theme(plot.margin = margin(1,0,0,0, "cm"))

#####
## Code for WLS formulation and plotting

# Creation
unweighted_models_1 = list()
weighted_models_1 = list()
summaries_1 = list()

for(j in seq_along(levels(correlation_masked_sv_1$Frequency_Diff))) {
  frq = levels(correlation_masked_sv_1$Frequency_Diff)[j]
  
  tmp = subset(correlation_masked_sv_1, correlation_masked_sv_1$Frequency_Diff == frq)
  tmp = droplevels(tmp)
  
  unweighted_model = lm(Multi_Sv ~ Echo_Masked_Sv_1, data = tmp)
  unweighted_models_1[[j]] = unweighted_model
  unweighted_model = augment(unweighted_model)
  wts = sqrt(abs(unweighted_model$.fitted))
  
  weighted_model = lm(Multi_Sv ~ Echo_Masked_Sv_1, data = tmp, weights = 1/wts)
  
  weighted_models_1[[j]] = weighted_model
  summaries_1[[j]] = summary(weighted_model)
}

# Plotting confidence interval
xmat = correlation_masked_sv_1 %>% group_by(Frequency_Diff) %>% do(as.data.frame(model.matrix(~ Echo_Masked_Sv_1, data = .)))
xmat = ungroup(xmat)

correlation_masked_sv_1$Frequency_Diff = as.factor(correlation_masked_sv_1$Frequency_Diff)

conf = data.frame()
eqn = data.frame()

for(i in seq_along(levels(correlation_masked_sv_1$Frequency_Diff))) {
  frq = levels(correlation_masked_sv_1$Frequency_Diff)[i]
  
  tmp = subset(xmat, xmat$Frequency_Diff == frq)
  tmp = droplevels(tmp)
  
  sub_xmat = cbind(flatten_dbl(tmp[,2]),flatten_dbl(tmp[,3]))
  
  betahat = coef(weighted_models_1[[i]])
  sigmahat = vcov(weighted_models_1[[i]])
  
  varcovEYhat = sub_xmat %*% sigmahat %*% t(sub_xmat)
  SEline = sqrt(diag(varcovEYhat))
  
  predicted = predict(weighted_models_1[[i]])
  upconf = predicted + 1.96 * SEline
  lowconf = predicted - 1.96 * SEline
  
  conf = rbind(conf,cbind(predicted,upconf,lowconf))
  
  eqn = rbind(eqn,round(as.numeric(coef(weighted_models_1[[i]])),digits = 2))
}

