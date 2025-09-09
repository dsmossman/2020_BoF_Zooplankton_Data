clc
clear variables
close all

addpath( genpath('/Users/dmossman/Box/2022 MSc Thesis Work/Code/AzfpMatlabToolbox_v18'))
addpath( genpath('/Users/dmossman/Box/2022 MSc Thesis Work/Raw_Data'))
addpath( genpath('/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data'))
addpath( genpath('/Users/dmossman/Box/Glider Data/'))

date = input('Enter the numerical day of the data: ','s');

filename = strcat("/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data/",date,"Sept_Processed_Data.mat");
load(filename);
clear filename;

filename = strcat("/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data/",date,"Sept_Differencing_Data.mat");
load(filename);
clear filename;

%%
n = 10;

for j = 1:4
    Dives = [];
    
    for k = 1:size(Dive, 2)
        % concatenate all the medians from each day
        Dives = [Dives, Dive(k).P(j).msv'];
    end
    
    DivesAveraged(:,j) = abs([arrayfun(@(i)...
        nanmean(Dives(i:i+n-1,:), 'all'),1:n:length(Dives)-n+1)';...
        nanmean(Dives(191:196,:),'all')]);
    DivesAveraged(:,j+4) = abs([arrayfun(@(i)...
        nanstd(10*log10(abs(Dives(i:i+n-1,:))), 1, 'all'),1:n:length(Dives)-n+1)';...
        nanstd(10*log10(abs(Dives(191:196,:))), 1, 'all')]);
end


for j = 1:3
    DiveDiffs = [];
    
    for k = 1:size(DiveDiff, 2)
        DiveDiffs = [DiveDiffs, mean(DiveDiff(k).PDiff(j).sv, 1, 'omitnan')'];
    end
    
    
    DiveDiffsAveraged(:,j) = abs([arrayfun(@(i)...
        nanmean(DiveDiffs(i:i+n-1,:), 'all'),1:n:length(DiveDiffs)-n+1)';...
        nanmean(DiveDiffs(191:196,:),'all')]);
    
    DiveDiffsAveraged(:,j+3) = abs([arrayfun(@(i)...
        nanstd(10*log10(abs(DiveDiffs(i:i+n-1,:))), 1, 'all'),1:n:length(DiveDiffs)-n+1)';...
        nanstd(10*log10(abs(DiveDiffs(191:196,:))), 1, 'all')]);
end

%%

x = categorical([]);
for j = 1:20
    x(j) = strcat(string(10 * (j) - 5), "-", string(10 * (j+1) - 5));
end

zscor_xnan = @(x) bsxfun(@rdivide, bsxfun(@minus, x, nanmean(x)), nanstd(x));

% DivesAveraged(16,1) = NaN; %removing outlier for sept 19th

Z = [zscor_xnan(10*log10(DivesAveraged(:,1))),zscor_xnan(10*log10(DiveDiffsAveraged(:,1:3)))];

%%
figure(1)
plot1 = barh(x,Z,'stacked');
plot1(1).FaceColor = '#21a883';
plot1(2).FaceColor = '#29788e';
plot1(3).FaceColor = '#7ad151';
plot1(4).FaceColor = '#450d54';
set(gca, 'Ydir', 'reverse');
xlabel('Z-Score')
ylabel('Depth Bin (m)')
yticklabels({'5-15',' ','25-35',' ','45-55',' ','65-75',' ','85-95',' ','105-115',' ','125-135',' ','145-155',' ','165-175',' ','185-195',' '});
legend({'0-130 kHz','200-130kHz','455-200kHz','769-455kHz'})
title(['Date: ',date,' Sept 2020'])


print(gcf,'-dpng',['/Users/dmossman/Box/2022 MSc Thesis Work/Visuals/MATLAB Echosounder Figures/',date,'Sept_ZScorePlot.png'], '-r0')