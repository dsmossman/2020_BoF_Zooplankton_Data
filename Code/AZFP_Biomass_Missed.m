%% Load AZFP data
% Make sure you've run the masking code first!
% Otherwise this code won't work!

clc
clear variables
close all
addpath( genpath('/Users/Delphine/Documents/2020_BoF_Zooplankton_Data/Code/AzfpMatlabToolbox_v18'))
addpath( genpath('/Users/Delphine/Documents/2020_BoF_Zooplankton_Data/Raw_Data'))
addpath( genpath('/Users/Delphine/Documents/2020_BoF_Zooplankton_Data/Processed_Data'))
addpath( genpath('/Users/dmossman/Box/Glider Data/'))

date = input('Enter the numerical day of the data: ','s');

filename = strcat("/Users/Delphine/Documents/2020_BoF_Zooplankton_Data/Processed_Data/",date,"Sept_Masking_Data.mat");
load(filename);
clear filename;

%% Load up data on seafloor depth/echo indices

filename = strcat('/Users/Delphine/Documents/2020_BoF_Zooplankton_Data/Processed_Data/',date,"Sept_2020_Seafloor_Data.mat");
load(filename);
clear filename;

bottom_dep = floor(mean(bott_dep3(3,:),'omitnan'));

%% distance above seafloor, the threshold
d = 5;

%% Get masked data that is below threshold overall

% only interested in 200-455 kHz band

missed_echoes = sum(P(3).masked(:,(bottom_dep-d):bottom_dep),'all','omitnan');
missed_echoes = missed_echoes/sum(P(3).masked,'all','omitnan') * 100;

%% Get masked data that is below threshold by dive

% only interested in 200-455 kHz band

for i = 1:size(Dive,2)
    missed_echoes(i) = sum(Dive(i).P(3).masked_sv(:,(bottom_dep-d):bottom_dep),'all','omitnan');
end

%% Divide masked data below threshold by total masked data in each dive

for j = 1:size(Dive, 2)
    missed_echoes(2,j) = missed_echoes(1,j)/sum(Dive(j).P(3).masked_sv, 'all', 'omitnan') * 100;
end
