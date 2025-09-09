clear variables
close all

addpath( genpath('/Users/delphine/Documents/MATLAB'))
addpath( genpath('/Users/delphine/Documents/BoF2020_Cruise/Visuals'))
addpath( genpath('/Users/delphine/Documents/BoF2020_Cruise/Processed_Data'))

%% Load in the seafloor data

path = '/Users/delphine/Documents/MATLAB/AZFP processed data/';
files = dir(fullfile(path,'*_Seafloor_Data.mat'));

for k = 1:numel(files)
    F = fullfile(path,files(k).name);
    files(k).data = load(F);
    files(k).name = files(k).name(1:end-18);
end
%% Get the median and max values for each day and save them in a structure

Seafloor_Data = struct();
types = ["Glider","Glider","Cage","Cage","Cage","Glider","Cage",...
    "Glider","Glider","Glider","Glider","Glider","Glider","Glider","Glider",];

for i = 1:numel(files)
    Seafloor_Data(i).Date = files(i).name;
    Seafloor_Data(i).Type = types(i);
    Seafloor_Data(i).Sv_Values = files(i).data.bott_sv3;
    Seafloor_Data(i).Sv_Medians = median(files(i).data.bott_sv3,2,'omitnan');
    Seafloor_Data(i).Sv_Maximums = max(files(i).data.bott_sv3,[],2,'omitnan');
end
%% Save file

filename = strcat("/Users/delphine/Documents/MATLAB/AZFP processed data/Seafloor_Data.mat");
save(filename,'Seafloor_Data');
clear filename;
