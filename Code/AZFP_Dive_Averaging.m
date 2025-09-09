%% Cleanup section
% Make sure you've run the initial processing/differencing/integration code
% first!

clc
clear variables
close all
addpath( genpath('/Users/dmossman/Box/2022 MSc Thesis Work/Code/AzfpMatlabToolbox_v18'))
addpath( genpath('/Users/dmossman/Box/2022 MSc Thesis Work/Raw_Data'))
addpath( genpath('/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data'))
addpath( genpath('/Users/dmossman/Box/Glider Data/'))

%% Load and format multinet data

% Load the event log CSV
warning('off','MATLAB:table:ModifiedAndSavedVarnames')
MultiData = readtable('/Users/dmossman/Box/2022 MSc Thesis Work/Raw_Data/Spreadsheets/Sept2020_Cruise_Leg_2_Multinet_Log.csv');

% Keep only the relevant tows and variables
MultiData = MultiData([2:5 7 9 11 12 14 15 17:22],[2 4 5:16]);

% Give the variables readable names
MultiData.Properties.VariableNames = ["Tow" "Date" "StartTime" "EndTime"...
    "StartLat" "StartLong" "EndLat" "EndLong" "Unlockdbarr" "Net1dbarr"...
    "Net2dbarr" "Net3dbarr" "Net4dbarr" "Net5dbarr"];

% Convert the lats and longs to strings
MultiData.StartLat = string(MultiData.StartLat);
MultiData.StartLong = string(MultiData.StartLong);
MultiData.EndLat = string(MultiData.EndLat);
MultiData.EndLong = string(MultiData.EndLong);

% Convert the lat/long columns from degrees and minutes to decimal degrees

% the ddm2dd function is based off the one Hansen wrote for R
MultiData.StartLat = ddm2dd(MultiData.StartLat);
MultiData.StartLong = -ddm2dd(MultiData.StartLong);
MultiData.EndLat = ddm2dd(MultiData.EndLat);
MultiData.EndLong = -ddm2dd(MultiData.EndLong);

%% Load Owen Basin data
load("/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data/19Sept_Processed_Data.mat");
load("/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data/19Sept_Differencing_Data.mat");
load("/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data/19Sept_Integration_Data.mat");

Sept19Dive = Dive;
Sept19DiveDiff = DiveDiff;
Sept19Dives = [starttow;endtow];
Sept19P = P;
Sept19PDiff = PDiff;

load("/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data/20Sept_Processed_Data.mat");
load("/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data/20Sept_Differencing_Data.mat");
load("/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data/20Sept_Integration_Data.mat");

Sept20Dive = Dive;
Sept20DiveDiff = DiveDiff;
Sept20Dives = [starttow;endtow];
Sept20P = P;
Sept20PDiff = PDiff;

load("/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data/26Sept_Processed_Data.mat");
load("/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data/26Sept_Differencing_Data.mat");
load("/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data/26Sept_Integration_Data.mat");

Sept26Dive = Dive;
Sept26DiveDiff = DiveDiff;
Sept26Dives = [starttow;endtow];
Sept26P = P;
Sept26PDiff = PDiff;

clear cc Diff Dive DiveDiff endtow Integration netintervals P PDiff StartDive starttow

%% Averaging dive medians

n = 10; % average over every 10 m

% Preallocating space
OBDivesAveraged = NaN * ones(20,8);

for j = 1:4
    temp1 = [];
    temp2 = [];
    temp3 = [];
    
    for k = 1:size(Sept19Dive, 2)
        % concatenate all the medians from each day
        temp1 = [temp1, Sept19Dive(k).P(j).msv'];
    end
    
    for k = 1:size(Sept20Dive, 2)
        temp2 = [temp2, Sept20Dive(k).P(j).msv'];
    end
    
    for k = 1:size(Sept26Dive, 2)
        temp3 = [temp3, Sept26Dive(k).P(j).msv'];
    end
    
    % concatenate all the medians from the basin
    OBDives = [temp1 temp2 temp3];
    
    % get the 10m mean (of the medians) and standard deviation
    OBDivesAveraged(:,j) = abs([arrayfun(@(i)...
        nanmean(OBDives(i:i+n-1,:), 'all'),1:n:length(OBDives)-n+1)';...
        nanmean(OBDives(191:196,:),'all')]);
    OBDivesAveraged(:,j+4) = abs([arrayfun(@(i)...
        nanstd(10*log10(abs(OBDives(i:i+n-1,:))), 1, 'all'),1:n:length(OBDives)-n+1)';...
        nanstd(10*log10(abs(OBDives(191:196,:))), 1, 'all')]);
    
end

%% Plot bar graph

x = categorical([]);
for j = 1:20
    x(j) = strcat(string(10 * (j) - 5), "-", string(10 * (j+1) - 5));
end

figure(1)
sgtitle('Averaged backscatter for dives in Owen Basin')

for k = 1:4
    subplot(2, 2, k)
    barh(x, 10*log10(abs(OBDivesAveraged(:,k))));
    hold on;
    errorbar(10*log10(abs(OBDivesAveraged(:,k))), x, OBDivesAveraged(:,k+4),...
        'horizontal', 'LineStyle', 'none', 'LineWidth', 1, 'color', 'r');
    hold off;
    yticklabels({'5-15',' ','25-35',' ','45-55',' ','65-75',' ','85-95',' ','105-115',' ','125-135',' ','145-155',' ','165-175',' ','185-195',' '});
    xlim([-100 -40]);
    set(gca, 'Ydir', 'reverse');
    set(gca, 'Xdir', 'reverse');
    xlabel('Mean Backscatter Strength (dB)')
    ylabel('Depth Bin (m)')
    title(strcat(num2str(Output(k).Freq), ' kHz'))
    %     if k == 2
    %         rectangle('Position',[-100,9,1,2], 'FaceColor', '#0072BD');
    %         text(-101, 10, 'Net 1', 'clipping', 'off');
    %         rectangle('Position',[-100,7,1,2], 'FaceColor', '#D95319');
    %         text(-101, 8, 'Net 2', 'clipping', 'off');
    %         rectangle('Position',[-100,5,1,2], 'FaceColor', '#EDB120');
    %         text(-101, 6, 'Net 3', 'clipping', 'off');
    %         rectangle('Position',[-100,2,1,3], 'FaceColor', '#7E2F8E');
    %         text(-101, 3.5, 'Net 4', 'clipping', 'off');
    %         rectangle('Position',[-100,0,1,2], 'FaceColor', '#77AC30');
    %         text(-101, 1, 'Net 5', 'clipping', 'off');
    %     end
end

print(gcf,'-dpng','/Users/dmossman/Box/2022 MSc Thesis Work/Visuals/MATLAB Echosounder Figures/OBDivesAveraged.png', '-r0')

close all;

%% Averaging dive difference means

n = 10; % average over every 10 m

% Preallocating space
OBDiveDiffsAveraged = NaN * ones(20,6);

for j = 1:3
    temp1 = [];
    temp2 = [];
    temp3 = [];
    
    for k = 1:size(Sept19DiveDiff, 2)
        temp1 = [temp1, mean(Sept19DiveDiff(k).PDiff(j).sv, 1, 'omitnan')'];
    end
    
    for k = 1:size(Sept20DiveDiff, 2)
        temp2 = [temp2, mean(Sept20DiveDiff(k).PDiff(j).sv, 1, 'omitnan')'];
    end
    
    for k = 1:size(Sept26DiveDiff, 2)
        temp3 = [temp3, mean(Sept26DiveDiff(k).PDiff(j).sv, 1, 'omitnan')'];
    end
    
    OBDiveDiffs = [temp1 temp2 temp3];
    
    OBDiveDiffsAveraged(:,j) = abs([arrayfun(@(i)...
        nanmean(OBDiveDiffs(i:i+n-1,:), 'all'),1:n:length(OBDiveDiffs)-n+1)';...
        nanmean(OBDiveDiffs(191:196,:),'all')]);
    
    OBDiveDiffsAveraged(:,j+3) = abs([arrayfun(@(i)...
        nanstd(10*log10(abs(OBDiveDiffs(i:i+n-1,:))), 1, 'all'),1:n:length(OBDiveDiffs)-n+1)';...
        nanstd(10*log10(abs(OBDiveDiffs(191:196,:))), 1, 'all')]);
end

%% Plot bar graph

figure(2)
sgtitle('Averaged backscatter differences for dives in Owen Basin')

for k = 1:3
    subplot(2, 2, k)
    barh(x, 10*log10(abs(OBDiveDiffsAveraged(:,k))));
    hold on;
    errorbar(10*log10(abs(OBDiveDiffsAveraged(:,k))), x, OBDiveDiffsAveraged(:,k+3),...
        'horizontal', 'LineStyle', 'none', 'LineWidth', 1, 'color', 'r');
    hold off;
    yticklabels({'5-15',' ','25-35',' ','45-55',' ','65-75',' ','85-95',' ','105-115',' ','125-135',' ','145-155',' ','165-175',' ','185-195',' '});
    xlim([5 25]);
    set(gca, 'Ydir', 'reverse');
    xlabel('Mean Backscatter Strength (dB)')
    ylabel('Depth Bin (m)')
    title({strcat(num2str(Output(k+1).Freq), ' kHz - ', strcat(num2str(Output(k).Freq), ' kHz'))})
    if k == 2
        rectangle('Position',[24.5,9,1,2], 'FaceColor', '#0072BD');
        text(26, 10, 'Net 1', 'clipping', 'off');
        rectangle('Position',[24.5,7,1,2], 'FaceColor', '#D95319');
        text(26, 8, 'Net 2', 'clipping', 'off');
        rectangle('Position',[24.5,5,1,2], 'FaceColor', '#EDB120');
        text(26, 6, 'Net 3', 'clipping', 'off');
        rectangle('Position',[24.5,2,1,3], 'FaceColor', '#7E2F8E');
        text(26, 3.5, 'Net 4', 'clipping', 'off');
        rectangle('Position',[24.5,0,1,2], 'FaceColor', '#77AC30');
        text(26, 1, 'Net 5', 'clipping', 'off');
    end
end

print(gcf,'-dpng','/Users/dmossman/Box/2022 MSc Thesis Work/Visuals/MATLAB Echosounder Figures/OBDiveDiffsAveraged.png', '-r0')

close all;
%% Load Grand Manan Basin data
% clear variables

load("/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data/21Sept_Processed_Data.mat");
load("/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data/21Sept_Differencing_Data.mat");
load("/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data/21Sept_Integration_Data.mat");

Sept21Dive = Dive;
Sept21DiveDiff = DiveDiff;
Sept21Dives = [starttow;endtow];
Sept21P = P;
Sept21PDiff = PDiff;

load("/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data/24Sept_Processed_Data.mat");
load("/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data/24Sept_Differencing_Data.mat");
load("/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data/24Sept_Integration_Data.mat");

Sept24Dive = Dive;
Sept24DiveDiff = DiveDiff;
Sept24Dives = [starttow;endtow];
Sept24P = P;
Sept24PDiff = PDiff;

load("/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data/25Sept_Processed_Data.mat");
load("/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data/25Sept_Differencing_Data.mat");
load("/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data/25Sept_Integration_Data.mat");

Sept25Dive = Dive;
Sept25DiveDiff = DiveDiff;
Sept25Dives = [starttow;endtow];
Sept25P = P;
Sept25PDiff = PDiff;

clear cc Diff Dive DiveDiff endtow Integration netintervals P PDiff StartDive starttow

%% Averaging dive medians

n = 10; % average over every 10 m

% Preallocating space
GMBDivesAveraged = NaN * ones(20,8);

for j = 1:4
    temp1 = [];
    temp2 = [];
    temp3 = [];
    
    for k = 1:size(Sept21Dive, 2)
        temp1 = [temp1, Sept21Dive(k).P(j).msv'];
    end
    
    for k = 1:size(Sept24Dive, 2)
        temp2 = [temp2, Sept24Dive(k).P(j).msv'];
    end
    
    for k = 1:size(Sept25Dive, 2)
        temp3 = [temp3, Sept25Dive(k).P(j).msv'];
    end
    
    GMBDives = [temp1 temp2 temp3];
    
    GMBDivesAveraged(:,j) = abs([arrayfun(@(i)...
        nanmean(GMBDives(i:i+n-1,:), 'all'),1:n:length(GMBDives)-n+1)';...
        nanmean(GMBDives(191:196,:),'all')]);
    GMBDivesAveraged(:,j+4) = abs([arrayfun(@(i)...
        nanstd(10*log10(abs(GMBDives(i:i+n-1,:))), 1, 'all'),1:n:length(GMBDives)-n+1)';...
        nanstd(10*log10(abs(GMBDives(191:196,:))), 1, 'all')]);
    
end

%% Plot bar graph

x = categorical([]);
for j = 1:20
    x(j) = strcat(string(10 * (j) - 5), "-", string(10 * (j+1) - 5));
end

figure(3)
sgtitle('Averaged backscatter for dives in Grand Manan Basin')

for k = 1:4
    subplot(2, 2, k)
    barh(x, 10*log10(abs(GMBDivesAveraged(:,k))));
    hold on;
    errorbar(10*log10(abs(GMBDivesAveraged(:,k))), x, GMBDivesAveraged(:,k+4),...
        'horizontal', 'LineStyle', 'none', 'LineWidth', 1, 'color', 'r');
    hold off;
    yticklabels({'5-15',' ','25-35',' ','45-55',' ','65-75',' ','85-95',' ','105-115',' ','125-135',' ','145-155',' ','165-175',' ','185-195',' '});
    xlim([-100 -50]);
    set(gca, 'Ydir', 'reverse');
    set(gca, 'Xdir', 'reverse');
    xlabel('Mean Backscatter Strength (dB)')
    ylabel('Depth Bin (m)')
    title(strcat(num2str(Output(k).Freq), ' kHz'))
    %     if k == 2
    %         rectangle('Position',[-100,9,1,2], 'FaceColor', '#0072BD');
    %         text(-101, 10, 'Net 1', 'clipping', 'off');
    %         rectangle('Position',[-100,7,1,2], 'FaceColor', '#D95319');
    %         text(-101, 8, 'Net 2', 'clipping', 'off');
    %         rectangle('Position',[-100,5,1,2], 'FaceColor', '#EDB120');
    %         text(-101, 6, 'Net 3', 'clipping', 'off');
    %         rectangle('Position',[-100,2,1,3], 'FaceColor', '#7E2F8E');
    %         text(-101, 3.5, 'Net 4', 'clipping', 'off');
    %         rectangle('Position',[-100,0,1,2], 'FaceColor', '#77AC30');
    %         text(-101, 1, 'Net 5', 'clipping', 'off');
    %     end
end

print(gcf,'-dpng','/Users/dmossman/Box/2022 MSc Thesis Work/Visuals/MATLAB Echosounder Figures/GMBDivesAveraged.png', '-r0')

close all;

%% Averaging dive difference means

n = 10; % average over every 10 m

% Preallocating space
GMBDiveDiffsAveraged = NaN * ones(20,6);

for j = 1:3
    temp1 = [];
    temp2 = [];
    temp3 = [];
    
    for k = 1:size(Sept21DiveDiff, 2)
        temp1 = [temp1, mean(Sept21DiveDiff(k).PDiff(j).sv, 1, 'omitnan')'];
    end
    
    for k = 1:size(Sept24DiveDiff, 2)
        temp2 = [temp2, mean(Sept24DiveDiff(k).PDiff(j).sv, 1, 'omitnan')'];
    end
    
    for k = 1:size(Sept25DiveDiff, 2)
        temp3 = [temp3, mean(Sept25DiveDiff(k).PDiff(j).sv, 1, 'omitnan')'];
    end
    
    GMBDiveDiffs = [temp1 temp2 temp3];
    
    GMBDiveDiffsAveraged(:,j) = abs([arrayfun(@(i)...
        nanmean(GMBDiveDiffs(i:i+n-1,:), 'all'),1:n:length(GMBDiveDiffs)-n+1)';...
        nanmean(GMBDiveDiffs(191:196,:),'all')]);
    
    GMBDiveDiffsAveraged(:,j+3) = abs([arrayfun(@(i)...
        nanstd(10*log10(abs(GMBDiveDiffs(i:i+n-1,:))), 1, 'all'),1:n:length(GMBDiveDiffs)-n+1)';...
        nanstd(10*log10(abs(GMBDiveDiffs(191:196,:))), 1, 'all')]);
end

%% Plot bar graph

figure(4)
sgtitle('Averaged backscatter differences for dives in Grand Manan Basin')

for k = 1:3
    subplot(2, 2, k)
    barh(x, 10*log10(abs(GMBDiveDiffsAveraged(:,k))));
    hold on;
    errorbar(10*log10(abs(GMBDiveDiffsAveraged(:,k))), x, GMBDiveDiffsAveraged(:,k+3),...
        'horizontal', 'LineStyle', 'none', 'LineWidth', 1, 'color', 'r');
    hold off;
    yticklabels({'5-15',' ','25-35',' ','45-55',' ','65-75',' ','85-95',' ','105-115',' ','125-135',' ','145-155',' ','165-175',' ','185-195',' '});
    xlim([5 25]);
    set(gca, 'Ydir', 'reverse');
    xlabel('Mean Backscatter Strength (dB)')
    ylabel('Depth Bin (m)')
    title({strcat(num2str(Output(k+1).Freq), ' kHz - ', strcat(num2str(Output(k).Freq), ' kHz'))})
    if k == 2
        rectangle('Position',[24.5,12,1,3], 'FaceColor', '#0072BD');
        text(26, 13.5, 'Net 1', 'clipping', 'off');
        rectangle('Position',[24.5,10,1,2], 'FaceColor', '#D95319');
        text(26, 11, 'Net 2', 'clipping', 'off');
        rectangle('Position',[24.5,7,1,3], 'FaceColor', '#EDB120');
        text(26, 8.5, 'Net 3', 'clipping', 'off');
        rectangle('Position',[24.5,3,1,4], 'FaceColor', '#7E2F8E');
        text(26, 5, 'Net 4', 'clipping', 'off');
        rectangle('Position',[24.5,0,1,3], 'FaceColor', '#77AC30');
        text(26, 1.5, 'Net 5', 'clipping', 'off');
    end
end

print(gcf,'-dpng','/Users/dmossman/Box/2022 MSc Thesis Work/Visuals/MATLAB Echosounder Figures/GMBDiveDiffsAveraged.png', '-r0')

close all;
%% Eight-panel figures

figure(5)

subplot(4,2,1)
imagesc([NaN * ones(5, size(Sept20P(1).avg_sv, 1)); 10*log10(abs(Sept20P(1).avg_sv'))]) % conversion back to decibels + dealing with the fact that we cut off the first 5 m of data
colormap('jet');
caxis([-80 -60]);
xlabel('Ping Number')
ylabel('Depth (m)')
title('0-130 kHz')
h = colorbar('westoutside');
set(get(h,'label'),'string','Sv (dB scattering per unit volume)');
h.Position(4) = 0.15;
h.Position(1) = 0.08-h.Position(3);
h.Position(2) = 0.85-h.Position(4)/2;
h.Position(3) = 0.010;

subplot(4,2,3)
imagesc([NaN * ones(5, size(Sept20PDiff(1).avg_sv, 1)); 10 * log10(abs(Sept20PDiff(1).avg_sv'))]) % averaging procedure line
colormap('jet');
caxis([0 20]);
xlabel('Ping Number')
ylabel('Depth (m)')
title('130-200 kHz')

subplot(4,2,5)
imagesc([NaN * ones(5, size(Sept20PDiff(2).avg_sv, 1)); 10 * log10(abs(Sept20PDiff(2).avg_sv'))]) % averaging procedure line
colormap('jet');
caxis([0 20]);
xlabel('Ping Number')
ylabel('Depth (m)')
title('200-455 kHz')

subplot(4,2,7)
imagesc([NaN * ones(5, size(Sept20PDiff(3).avg_sv, 1)); 10 * log10(abs(Sept20PDiff(3).avg_sv'))]) % averaging procedure line
colormap('jet');
caxis([0 20]);
xlabel('Ping Number')
ylabel('Depth (m)')
title('455-769 kHz')

h = colorbar('southoutside');
set(get(h,'label'),'string','Difference in Sv (dB scattering per unit volume)');
% set(get(h,'location'),'southoutside');

h.Position(4) = 0.01;
h.Position(1) = 0.465-h.Position(3);
h.Position(2) = 0.05-h.Position(4)/2;

for k = 1:4
    subplot(4, 2, 2*k)
    if k == 1
        b = barh(x, 10*log10(abs(OBDivesAveraged(:,k))));
        hold on;
        errorbar(10*log10(abs(OBDivesAveraged(:,k))), x, OBDivesAveraged(:,k+4),...
            'horizontal', 'LineStyle', 'none', 'LineWidth', 1, 'color', 'r');
        hold off;
        yticklabels({'5-15',' ','25-35',' ','45-55',' ','65-75',' ','85-95',' ','105-115',' ','125-135',' ','145-155',' ','165-175',' ','185-195',' '});
        b(1).BaseValue = -100;
        xlim([-100 -40]);
        set(gca, 'Ydir', 'reverse');
        xlabel('Mean Backscatter Strength (dB)')
        ylabel('Depth Bin (m)')
        title('0-130 kHz')
    else
        subplot(4, 2, 2*k)
        barh(x, 10*log10(abs(OBDiveDiffsAveraged(:,k-1))));
        hold on;
        errorbar(10*log10(abs(OBDiveDiffsAveraged(:,k-1))), x, OBDiveDiffsAveraged(:,k+2),...
            'horizontal', 'LineStyle', 'none', 'LineWidth', 1, 'color', 'r');
        hold off;
        yticklabels({'5-15',' ','25-35',' ','45-55',' ','65-75',' ','85-95',' ','105-115',' ','125-135',' ','145-155',' ','165-175',' ','185-195',' '});
        xlim([5 25]);
        set(gca, 'Ydir', 'reverse');
        xlabel('Mean Backscatter Difference (dB)')
        ylabel('Depth Bin (m)')
        title({strcat(num2str(Output(k-1).Freq), '-', strcat(num2str(Output(k).Freq), ' kHz'))})
        if k == 3
            rectangle('Position',[24.5,9,1,2], 'FaceColor', '#0072BD');
            text(26, 10, 'Net 1', 'clipping', 'off');
            rectangle('Position',[24.5,7,1,2], 'FaceColor', '#D95319');
            text(26, 8, 'Net 2', 'clipping', 'off');
            rectangle('Position',[24.5,5,1,2], 'FaceColor', '#EDB120');
            text(26, 6, 'Net 3', 'clipping', 'off');
            rectangle('Position',[24.5,2,1,3], 'FaceColor', '#7E2F8E');
            text(26, 3.5, 'Net 4', 'clipping', 'off');
            rectangle('Position',[24.5,0,1,2], 'FaceColor', '#77AC30');
            text(26, 1, 'Net 5', 'clipping', 'off');
        end
    end
end

AddLetters2Plots(gcf,'VShift',-0.03,'Direction','TopDown')
print(gcf,'-dpng','/Users/dmossman/Box/2022 MSc Thesis Work/Visuals/MATLAB Echosounder Figures/OBEightPanelPlot.png', '-r0')


figure(6)

subplot(4,2,1)
imagesc([NaN * ones(5, size(Sept21P(1).avg_sv, 1)); 10*log10(abs(Sept21P(1).avg_sv'))]) % conversion back to decibels + dealing with the fact that we cut off the first 5 m of data
colormap('jet');
caxis([-80 -60]);
xlabel('Ping Number')
ylabel('Depth (m)')
title('0-130 kHz')
h = colorbar('westoutside');
set(get(h,'label'),'string','Sv (dB scattering per unit volume)');
h.Position(4) = 0.15;
h.Position(1) = 0.08-h.Position(3);
h.Position(2) = 0.85-h.Position(4)/2;
h.Position(3) = 0.010;

subplot(4,2,3)
imagesc([NaN * ones(5, size(Sept21PDiff(1).avg_sv, 1)); 10 * log10(abs(Sept21PDiff(1).avg_sv'))]) % averaging procedure line
colormap('jet');
caxis([0 20]);
xlabel('Ping Number')
ylabel('Depth (m)')
title('130-200 kHz')

subplot(4,2,5)
imagesc([NaN * ones(5, size(Sept21PDiff(2).avg_sv, 1)); 10 * log10(abs(Sept21PDiff(2).avg_sv'))]) % averaging procedure line
colormap('jet');
caxis([0 20]);
xlabel('Ping Number')
ylabel('Depth (m)')
title('200-455 kHz')

subplot(4,2,7)
imagesc([NaN * ones(5, size(Sept21PDiff(3).avg_sv, 1)); 10 * log10(abs(Sept21PDiff(3).avg_sv'))]) % averaging procedure line
colormap('jet');
caxis([0 20]);
xlabel('Ping Number')
ylabel('Depth (m)')
title('455-769 kHz')

h = colorbar('southoutside');
set(get(h,'label'),'string','Difference in Sv (dB scattering per unit volume)');
% set(get(h,'location'),'southoutside');

h.Position(4) = 0.01;
h.Position(1) = 0.465-h.Position(3);
h.Position(2) = 0.05-h.Position(4)/2;

for k = 1:4
    subplot(4, 2, 2*k)
    if k == 1
        b = barh(x, 10*log10(abs(GMBDivesAveraged(:,k))));
        hold on;
        errorbar(10*log10(abs(GMBDivesAveraged(:,k))), x, GMBDivesAveraged(:,k+4),...
            'horizontal', 'LineStyle', 'none', 'LineWidth', 1, 'color', 'r');
        hold off;
        yticklabels({'5-15',' ','25-35',' ','45-55',' ','65-75',' ','85-95',' ','105-115',' ','125-135',' ','145-155',' ','165-175',' ','185-195',' '});
        b(1).BaseValue = -100;
        xlim([-100 -40]);
        set(gca, 'Ydir', 'reverse');
        xlabel('Mean Backscatter Strength (dB)')
        ylabel('Depth Bin (m)')
        title('0-130 kHz')
    else
        subplot(4, 2, 2*k)
        barh(x, 10*log10(abs(GMBDiveDiffsAveraged(:,k-1))));
        hold on;
        errorbar(10*log10(abs(GMBDiveDiffsAveraged(:,k-1))), x, GMBDiveDiffsAveraged(:,k+2),...
            'horizontal', 'LineStyle', 'none', 'LineWidth', 1, 'color', 'r');
        hold off;
        yticklabels({'5-15',' ','25-35',' ','45-55',' ','65-75',' ','85-95',' ','105-115',' ','125-135',' ','145-155',' ','165-175',' ','185-195',' '});
        xlim([5 25]);
        set(gca, 'Ydir', 'reverse');
        xlabel('Mean Backscatter Difference (dB)')
        ylabel('Depth Bin (m)')
        title({strcat(num2str(Output(k-1).Freq), '-', strcat(num2str(Output(k).Freq), ' kHz'))})
        if k == 3
            rectangle('Position',[24.5,12,1,3], 'FaceColor', '#0072BD');
            text(26, 13.5, 'Net 1', 'clipping', 'off');
            rectangle('Position',[24.5,10,1,2], 'FaceColor', '#D95319');
            text(26, 11, 'Net 2', 'clipping', 'off');
            rectangle('Position',[24.5,7,1,3], 'FaceColor', '#EDB120');
            text(26, 8.5, 'Net 3', 'clipping', 'off');
            rectangle('Position',[24.5,3,1,4], 'FaceColor', '#7E2F8E');
            text(26, 5, 'Net 4', 'clipping', 'off');
            rectangle('Position',[24.5,0,1,3], 'FaceColor', '#77AC30');
            text(26, 1.5, 'Net 5', 'clipping', 'off');
        end
    end
end

AddLetters2Plots(gcf,'VShift',-0.03,'Direction','TopDown')
print(gcf,'-dpng','/Users/dmossman/Box/2022 MSc Thesis Work/Visuals/MATLAB Echosounder Figures/GMBEightPanelPlot.png', '-r0')

%% Z-score figure

zscor_xnan = @(x) bsxfun(@rdivide, bsxfun(@minus, x, nanmean(x)), nanstd(x));

OBDivesAveraged(16,1) = NaN; % removing outlier

Z_OB = [zscor_xnan(10*log10(OBDivesAveraged(:,1))),zscor_xnan(10*log10(OBDiveDiffsAveraged(:,1:3)))];
Z_GMB = [zscor_xnan(10*log10(GMBDivesAveraged(:,1))),zscor_xnan(10*log10(GMBDiveDiffsAveraged(:,1:3)))];

figure(7)
subplot(2,1,1)
plot1 = barh(x,Z_OB,'stacked');
plot1(1).FaceColor = '#21a883';
plot1(2).FaceColor = '#29788e';
plot1(3).FaceColor = '#7ad151';
plot1(4).FaceColor = '#450d54';
set(gca, 'Ydir', 'reverse');
xlabel('Z-Score')
ylabel('Depth Bin (m)')
yticklabels({'5-15',' ','25-35',' ','45-55',' ','65-75',' ','85-95',' ','105-115',' ','125-135',' ','145-155',' ','165-175',' ','185-195',' '});
legend({'0-130 kHz','130-200kHz','200-455kHz','455-769kHz'})
title('Owen Basin')

subplot(2,1,2)
plot2 = barh(x,Z_GMB,'stacked');
plot2(1).FaceColor = '#21a883';
plot2(2).FaceColor = '#29788e';
plot2(3).FaceColor = '#7ad151';
plot2(4).FaceColor = '#450d54';
set(gca, 'Ydir', 'reverse');
xlabel('Z-Score')
ylabel('Depth Bin (m)')
yticklabels({'5-15',' ','25-35',' ','45-55',' ','65-75',' ','85-95',' ','105-115',' ','125-135',' ','145-155',' ','165-175',' ','185-195',' '});
legend({'0-130 kHz','130-200kHz','200-455kHz','455-769kHz'})
title('Grand Manan Basin')

AddLetters2Plots(gcf,'VShift',-0.03,'Direction','TopDown')
print(gcf,'-dpng','/Users/dmossman/Box/2022 MSc Thesis Work/Visuals/MATLAB Echosounder Figures/ZScorePlot.png', '-r0')