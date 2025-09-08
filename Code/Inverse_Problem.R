#####
setwd("C:/Users/dmossman/Box/2022 MSc Thesis Work/")

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

## Project Structure
sourceDirectory('Code/Hansen Zooplankton Code and Sample Data/src',
                modifiedOnly = F)

function_dir = 'Code/Hansen Zooplankton Code and Sample Data/src/'
create_dir(function_dir)
processed_dir = 'Processed_Data/'
create_dir(processed_dir)
data_dir = 'Raw_Data/'
create_dir(data_dir)
figure_dir = 'Visuals/'
create_dir(figure_dir)
report_dir = getwd()
create_dir(report_dir)

remove_outliers = function(x,var) {
  qnt = quantile(unlist(x[var]), probs=c(.25, .75), na.rm = T)
  H = 1.5 * IQR(unlist(x[var]), na.rm = T)
  x %>% filter(!(x[var] <= (qnt[1] - H) | x[var] >= (qnt[2] + H)))
}

## Loading in files

load(paste0(processed_dir, "Net_Data_With_Sv.rda"))
load(paste0(processed_dir, "Sept2020_R_Echosounder_Data.rda"))

# Question mark: Should I be using the non-integrated echosounder data for the inverse problem?
## No, non-integrated masked Sv data minus background noise

# Question mark: Masked or non-masked difference data? Will do both for now

## In Sept2020, the differences are currently linear, so need to transform them to logspace for the calculations
# Use the P and PDiff matrices, as those are the ones that have the background noise subtracted;
# remember that they are averaged into 1m depth bins

for(i in 1:size(Sept2020, 2)) {
  Sept2020[[i]][["PDiff"]][[2]] = 10 * log10(abs(Sept2020[[i]][["PDiff"]][[2]]))
  Sept2020[[i]][["P"]][[3]] = 10 * log10(abs(Sept2020[[i]][["P"]][[3]]))
}

## Also need to get the masked data
# First, set the dB difference windows for 455-200 kHz

dB_Diff_Lower = 15.8
dB_Diff_Upper = 16.3

# Then, go thru each set of 200-455 kHz differences and determine whether they are within the window or not
# Finally, create a masked set of 455 kHz data

for(j in 1:size(Sept2020, 2)) {
  # Masking matrix
  Sept2020[[j]][["Masking"]] = 1 * (dB_Diff_Lower < Sept2020[[j]][["PDiff"]][[2]] & Sept2020[[j]][["PDiff"]][[2]] < dB_Diff_Upper)
  
  # Masked data
  Sept2020[[j]][["455kHz_Masked"]] = Sept2020[[j]][["Masking"]] * Sept2020[[j]][["P"]][[3]]
  Sept2020[[j]][["455kHz_Masked"]][which(Sept2020[[j]][["455kHz_Masked"]] == 0)] = NA
}


## Inverse problem (solving for [Z])
## Sv = TS + 10log10([Z])
## 10log10([Z]) = Sv - TS
## log10([Z]) = (Sv - TS)/10
## [Z] = 10^((Sv - TS)/10)
# Question mark: is this even the right equation? Papers make it seem much more complicated;
# probably something to ask Joe
## This is how Kim does it
## Reiss paper integrates Sv over a certain depth (5-250 m), would that be better?
## Reiss paper also uses the length distribution, is that necessary with something as small as copepods?
## Implicit in this method is that all of the Sv is from the target species, which means it usually ends up
## as an overestimation
## But it is this simple!

# Question mark: is (Sv - TS) in log or linear space?
# Integrated echosounder data is in linear space, TS is in log space
## Keep Sv and TS in log space

echo_masked_biomass = list()
cfin_TS = c(-129.7,	-122.3,	-108.3,	-99.9)

for(i in 1:size(Sept2020, 2)) {
  echo_masked_biomass[[i]] = 10^((Sept2020[[i]][["455kHz_Masked"]] - cfin_TS[3])/10)
}

names(echo_masked_biomass) = c("Sept19", "Sept20", "Sept21", "Sept24", "Sept25", "Sept26", "Sept21_Seafloor_Corrected")

# Question mark: now what? I guess I plot the calculated biomass values against the net values; integrated
# or not integrated?

# First, just want to plot the calculated biomass as a function of depth

for(k in 1:size(echo_masked_biomass, 2)) {
  test = as.data.frame(echo_masked_biomass[[k]])
  test = t(test)
  test[which(is.nan(test))] = NA
  test = test[, colSums(is.na(test)) < nrow(test)]
  
  # x = biomass value, y = depth
  
  test2 = data.frame()
  
  for (i in 1:nrow(test)) {
    for (j in 1:ncol(test)) {
      if (is.na(test[i, j] == TRUE)) {
        next
      } else
        test2 = rbind(test2, c(i + 4, test[i, j]))
      
    }
  }
  
  colnames(test2) = c("Depth", "Biomass")
  test3 = test2 %>% filter(!is_extreme(Biomass))
  
  plot1 = ggplot(data = test3) +
    geom_point(aes(x = Biomass, y = Depth)) +
    scale_y_reverse() +
    theme_bw() +
    labs(
      title = paste0(
        "Concentration Estimates from ",
        names(echo_masked_biomass)[[k]],
        " Echosounder Data"
      ),
      y = "Depth (m)",
      x = expression(paste(
        "Estimated Abundance (individuals/", m ^ 3, ')'
      ))
    )
  
  
  file = paste0(names(echo_masked_biomass)[[k]], "_Biomass_Estimate.png")
  ggsave(
    filename = file,
    path = paste0(figure_dir, "/Biomass_Estimates"),
    width = 8,
    units = "in"
  )
  rm(file)
  
  test4 = data.frame()
  for (i in seq(5, 195, 10)) {
    I = which(test3$Depth >= i & test3$Depth < (i + 10))
    test4 = rbind(test4, mean(test3$Biomass[I]))
  }
  
  names(test4) = "Mean_Biomass"
  test4$Depth_Int = paste0(seq(5, 195, 10), "-", seq(15, 205, 10))
  test4$Depth_Int = factor(test4$Depth_Int, levels = test4$Depth_Int)
  
  plot2 = ggplot(test4) +
    geom_bar(aes(x = Mean_Biomass, y = Depth_Int), stat = "identity") +
    theme_bw() +
    scale_y_discrete(limits = rev) +
    labs(
      title = paste0(
        "Mean Concentration Estimates from ",
        names(echo_masked_biomass)[[k]],
        " Echosounder Data"
      ),
      y = "Depth Interval (m)",
      x = expression(paste(
        "Estimated Mean Abundance (individuals/", m ^ 3, ')'
      ))
    )
  
  file = paste0(names(echo_masked_biomass)[[k]], "_Mean_Biomass_Estimate.png")
  ggsave(
    filename = file,
    path = paste0(figure_dir, "/Biomass_Estimates"),
    width = 8,
    units = "in"
  )
  
}
