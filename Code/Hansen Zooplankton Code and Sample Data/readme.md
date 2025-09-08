# README

## Overview

Here are all the plankton data collected during the 2017 cruise in the Gulf of St Lawrence aboard the R/V Shelagh I. Data were generated using two systems:
* NET: 1/2 meter diameter, 333 micron ring net with SBE37 CTD mounted in-line
* CTD: a single vertical cast of the same SBE37 CTD

## Project structure

### data

#### raw
All raw data for ctd, logs, and net

#### cache
Processed *.rda* files for each sensor

### src
R subroutines for data processing, plotting, etc

### master.R
R script for processing the entire project, and generating plots in 'figures'

### figures
Plots of net contents, ctd cast, etc.

### docs
Various documentation, paperwork, etc for cruise

### pics
Various images
