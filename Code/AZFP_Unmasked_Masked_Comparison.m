%% Load AZFP data
% Make sure you've run the initial processing and differencing code first!
% Otherwise this code won't work!

clear variables
close all
addpath( genpath('/Users/Delphine/Documents/2020_BoF_Zooplankton_Data/Code/AzfpMatlabToolbox_v18'))
addpath( genpath('/Users/Delphine/Documents/2020_BoF_Zooplankton_Data/Raw_Data'))
addpath( genpath('/Users/Delphine/Documents/2020_BoF_Zooplankton_Data/Processed_Data'))
addpath( genpath('/Users/dmossman/Box/Glider Data/'))

date = input('Enter the numerical day of the data: ','s');

filename = strcat("/Users/Delphine/Documents/2020_BoF_Zooplankton_Data/Processed_Data/",date,"Sept_Processed_Data.mat");
load(filename);
clear filename;

filename = strcat("/Users/Delphine/Documents/2020_BoF_Zooplankton_Data/Processed_Data/",date,"Sept_Differencing_Data.mat");
load(filename);
clear filename;
%% Create full matrix of masked Sv

% First trying just whether one single frequency is larger than the other and vice versa

for i = 1:3 % for each pair of frequencies
    P(i).masking(1).mask = (10 * log10(abs(P(i).avg_sv)) > 10 * log10(abs(P(i+1).avg_sv)));
    P(i).masking(2).mask = (10 * log10(abs(P(i).avg_sv)) <= 10 * log10(abs(P(i+1).avg_sv)));
end

figure(2)
subplot(2,2,1)
imagesc([NaN * ones(5, size(P(1).masking(1).mask, 1)); P(1).masking(1).mask']);
colormap('gray');
xlabel('Ping Number')
ylabel('Depth')
title('130kHz > 200kHz')

subplot(2,2,2)
imagesc([NaN * ones(5, size(P(1).masking(2).mask, 1)); P(1).masking(2).mask']);
colormap('gray');
xlabel('Ping Number')
ylabel('Depth')
title('130kHz =< 200kHz')

subplot(2,2,3)
imagesc([NaN * ones(5, size(P(2).masking(1).mask, 1)); P(2).masking(1).mask']);
colormap('gray');
xlabel('Ping Number')
ylabel('Depth')
title('200kHz > 455kHz')

subplot(2,2,4)
imagesc([NaN * ones(5, size(P(2).masking(2).mask, 1)); P(2).masking(2).mask']);
colormap('gray');
xlabel('Ping Number')
ylabel('Depth')
title('200kHz =< 455kHz')

% filename = strcat("/Users/delphine/Documents/BoF2020_Cruise/Visuals/MATLAB Echosounder Figures/",date,"Sept/Simple_Mask_",date,"Sept.png");
% print(gcf,'-dpng',filename,'-r0')
% clear filename;

%%
% First get the dB difference window
% Values below are for copepods between 1.27 and 2.99 mm in length, from
% Joe's spreadsheet

% windows are likely too small; play with these values until the matching
% matrix looks like the patches in the echogram
% 130-200 make 0-7 dB and see if that helps
% ignore 769 kHz for now

% Controlled parameter tuning based on MultiNet data
% Do correlations with windows in 200-455 kHz of 1 dB, 5 dB, 10 dB
dB_Diff_Lower = [7.4, 13.7, 7.8];
% dB_Diff_Lower = [7.4, 16.1, 7.8];
dB_Diff_Upper = [7.5, 14.2, 8.8];
% dB_Diff_Upper = [7.5, 19.6, 8.8];

% is the 455 kHz data "real" or just noise? Calibration issues? Offset or
% dynamic range

% pick a transect, look at the bottom value, see what the values are as a
% pseudo calibration
% if the bottom depth values are off, we will need to do a calibration
% correction; bottom is flat, broad, frequency-independent
% histogram of 1 m above bottom to 2 m below bottom (and right at the bottom)
% for each frequency, see how similar the values are (or how different)
% gives us insight into the sensitivity
% if the dynamic range of the different frequencies is off, this becomes
% trickier

%% Then create the binary filter matrix

% frequency 1 > frequency 2 as a masking matrix, to start (and vice versa)

for i = 1:size(PDiff,2) % for each frequency difference
    % PDiff values are in linear space, dB_Diff values are in log space
    % convert PDiff to log space before creating mask
    PDiff(i).mask = (dB_Diff_Lower(i) < 10 * log10(abs(PDiff(i).avg_sv)) & 10 * log10(abs(PDiff(i).avg_sv)) < dB_Diff_Upper(i));
end

% Then multiply masking matrix by Sv to get masked observed Sv

P(1).masked = P(1).avg_sv;
% 130 kHz is not masked

for i = 1:size(PDiff,2) % for each frequency difference
    P(i+1).masked = P(i+1).avg_sv .* PDiff(i).mask;
    P(i+1).masked(P(i+1).masked == 0) = NaN;
end

%% Create by-dive matrices of masked Sv

for i = 1:size(Dive,2) % for each dive
    for j = 1:4 % for each frequency
        Dive(i).P(j).masked_sv = P(j).masked(Dive(i).Index(1):Dive(i).Index(2),:);
    end
end
%% Plot unmasked and masked 455 kHz only next to each other

figure(1)

% subplot(4,3,1)
% imagesc([NaN * ones(5, size(P(1).avg_sv, 1)); 10 * log10(abs(P(1).avg_sv'))])
% colormap('jet');
% caxis([-80 -40]);
% xlabel('Ping Number')
% ylabel('Depth (m)')
% title('130kHz')

subplot(2,2,1)
imagesc([NaN * ones(5, size(P(2).avg_sv, 1)); 10 * log10(abs(P(2).avg_sv'))],'AlphaData',~isnan([NaN * ones(5, size(P(2).avg_sv, 1)); 10*log10(abs(P(2).avg_sv'))]))
colormap('parula');
caxis([-80 -50]);
xlabel('Ping Number')
ylabel('Depth (m)')
title('200kHz unmasked')

subplot(2,2,3)
imagesc([NaN * ones(5, size(P(3).avg_sv, 1)); 10 * log10(abs(P(3).avg_sv'))],'AlphaData',~isnan([NaN * ones(5, size(P(3).avg_sv, 1)); 10*log10(abs(P(3).avg_sv'))]))
colormap('parula');
caxis([-80 -50]);
xlabel('Ping Number')
ylabel('Depth (m)')
title('455kHz unmasked')

ax1 = subplot(2,2,2);
imagesc([NaN * ones(5, size(PDiff(2).mask, 1)); PDiff(2).mask']);
colormap('gray');
xlabel('Ping Number')
ylabel('Depth')
title('455kHz filtering matrix')

subplot(2,2,4)
imagesc([NaN * ones(5, size(P(3).masked, 1)); 10 * log10(abs(P(3).masked'))],'AlphaData',~isnan([NaN * ones(5, size(P(3).masked, 1)); 10*log10(abs(P(3).masked'))]))
colormap('parula');
caxis([-80 -50]);
xlabel('Ping Number')
ylabel('Depth (m)')
title('455kHz filtered')

colormap(ax1,gray);

h = colorbar;
set(get(h,'label'),'string','Sv (dB scattering per unit volume)');

h.Position(4) = 0.65;
h.Position(1) = .94-h.Position(3);
h.Position(2) = 0.5-h.Position(4)/2;

AddLetters2Plots(gcf,'VShift',-0.03,'Direction','TopDown')

sgtitle(strcat("Decibel Window Used: ",num2str(dB_Diff_Lower(2))," - ",num2str(dB_Diff_Upper(2))));

%% save figure
filename = strcat("/Users/Delphine/Documents/2020_BoF_Zooplankton_Data/Visuals/MATLAB Echosounder Figures/",date,"Sept/Unmasked_Masked_Comparison_",date,"Sept.png");
% filename = strcat("/Users/Delphine/Documents/2020_BoF_Zooplankton_Data/Visuals/MATLAB Echosounder Figures/",...
%     date,...
%     "Sept/Unmasked_Masked_Comparison_10_dB_window_",...
%     date,...
%     "Sept.png");
print(gcf,'-dpng',filename,'-r0')
clear filename
close all
%% save files
filename = strcat("/Users/Delphine/Documents/2020_BoF_Zooplankton_Data/Processed_Data/",date,"Sept_Masking_Data.mat");
save(filename, 'Output', 'P', 'Dive', 'Diff', 'DiveDiff', 'PDiff');