%% Load data
% Make sure you've run the initial processing code first! Otherwise this
% part won't work!

clc
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

%% Frequency differencing calculations and plots

% According to Joe, I should perform the differencing on the raw data and
% then redo the averaging
% In addition, decibel differencing is done in the log (decibel) space! Who
% knew!

% All the Sv and Range fields in the Output structure have different
% numbers of columns, so I need to pad the smaller ones to match the
% largest before doing the differencing

% first find the largest Sv matrix
biggest = find(max([size(Output(1).Sv, 2), size(Output(2).Sv, 2),...
    size(Output(3).Sv, 2), size(Output(4).Sv, 2)]));

% then make a pad for the smaller Sv matrices to bring them up to the same
% size as the largest one
for i = 1:4
    if i ~= biggest
        nanpad = nan * ones(size(Output(1).Sv,1), size(Output(biggest).Sv, 2) - size(Output(i).Sv, 2));
    else
        nanpad = [];
    end
    % create temporary structure from the Output data + the nanpad, if it
    % exists, in linear space
    temp(i).raw = [Output(i).Sv, nanpad];
end

% then make a structure of frequency differences
% some of these frequency differences are positive, some are negative
% how do I preserve this when converting from linear to log and back again?
% something to investigate down the line
for j = 1:3
    Diff(j).SvDiff = temp(j+1).raw - temp(j).raw;
end

% remove the temporary structure
% clear temp;

%% Average differences into 1 m depth bins
% Same averaging procedure as for the Output file
dbins = 5:200;

tic
for ii=1:3
    for dd = 1:length(dbins)
        for pp = 1:size(Output(biggest).Depth,1)
            I = find(Output(biggest).Depth(pp,:) > (dbins(dd)-0.5) & Output(biggest).Depth(pp,:) <= (dbins(dd)+0.5) );
            PDiff(ii).avg_sv(pp,dd) = mean(10.^(Diff(ii).SvDiff(pp,I)./10), 'omitnan'); % averages are in the linear space
        end
    end
end
toc

%% Plot figures of frequency differences

figure(1)

subplot(2,2,1)
imagesc([NaN * ones(5, size(P(1).avg_sv, 1)); 10 * log10(abs(P(1).avg_sv'))]) % averaging procedure line
colormap('jet');
caxis([-80 -60]);
xlabel('Ping Number')
ylabel('Depth (m)')
title('130kHz')
h = colorbar;
set(get(h,'label'),'string','Sv (dB scattering per unit volume)');

subplot(2,2,2)
% imagesc([NaN * ones(5, size(PDiff(1).avg_sv, 1)); PDiff(1).avg_sv'])
imagesc([NaN * ones(5, size(PDiff(1).avg_sv, 1)); 10 * log10(abs(PDiff(1).avg_sv'))]) % averaging procedure line
colormap('jet');
caxis([-20 20]);
xlabel('Ping Number')
ylabel('Depth (m)')
title('200kHz minus 130kHz')

subplot(2,2,3)
% imagesc([NaN * ones(5, size(PDiff(2).avg_sv, 1)); PDiff(2).avg_sv'])
imagesc([NaN * ones(5, size(PDiff(2).avg_sv, 1)); 10 * log10(abs(PDiff(2).avg_sv'))]) % averaging procedure line
colormap('jet');
caxis([-20 20]);
xlabel('Ping Number')
ylabel('Depth (m)')
title('455kHz minus 200kHz')

subplot(2,2,4)
% imagesc([NaN * ones(5, size(PDiff(3).avg_sv, 1)); PDiff(3).avg_sv'])
imagesc([NaN * ones(5, size(PDiff(3).avg_sv, 1)); 10 * log10(abs(PDiff(3).avg_sv'))]) % averaging procedure line
colormap('jet');
caxis([-20 20]);
xlabel('Ping Number')
ylabel('Depth (m)')
title('769kHz minus 455kHz')

% single colorbar for all four plots
h = colorbar;
set(get(h,'label'),'string','Difference in Sv (dB scattering per unit volume)');

h.Position(4) = 0.65;
h.Position(1) = .95-h.Position(3);
h.Position(2) = 0.5-h.Position(4)/2;

AddLetters2Plots(gcf,'VShift',-0.04)

% save file
filename = strcat("/Users/Delphine/Documents/2020_BoF_Zooplankton_Data/Visuals/MATLAB Echosounder Figures/",date,"Sept/Frequency_Differences_All_",date,"Sept.png");
print(gcf,'-dpng',filename,'-r0')
clear filename
close;

%% Separate differences into dive profiles

% Does this still work with the new differencing method? Guess we'll find
% out!

for jj=1:3 % Frequency index
    cc = 0;
    for DD = 1:length(StartDive) % for each index where a dive might start
        if DD == length(StartDive) % important for separation of dives/deal with fact that glider comes back up sometimes
            % we need to check if we are at the end of the StartDive
            % vector, because the code changes
            if (size(PDiff(jj).avg_sv,1) - StartDive(DD)) > 250 % at the end of some dives the glider starts coming back up. Avoid that data. Also avoid very short dives
                cc = cc+1;  % Dive count
                % grab the avg_sv values corresponding to the dive
                DiveDiff(cc).PDiff(jj).sv = PDiff(jj).avg_sv(StartDive(DD):end,:);
                DiveDiff(cc).PDiff(jj).msv = mean(DiveDiff(cc).PDiff(jj).sv,1, 'omitnan');
            end
        else % when we are not at the end of the StartDive vector
            if (StartDive(DD+1)-1 - StartDive(DD)) >250
                cc = cc+1; % Dive count
                % grab the avg_sv values corresponding to the dive
                DiveDiff(cc).PDiff(jj).sv = PDiff(jj).avg_sv(StartDive(DD):StartDive(DD+1)-1,:);
                DiveDiff(cc).PDiff(jj).msv = mean(DiveDiff(cc).PDiff(jj).sv,1,'omitnan');
            end
        end
    end
end

% Have to repeat the way Scott did the dives so that Dive and DiveDiff are
% the same size
% Still not sure why that is btw

% for i = 1:3 % for each difference between frequencies
%     for j = 1:cc % for each dive
%         % first get the indices where there is a dive
%         index1 = Dive(j).Index(1);
%         index2 = Dive(j).Index(2);
%         
%         % then pull the corresponding frequency differences from the PDiff
%         % structure and put them into a new structure
%         DiveDiff(j).PDiff(i).sv = PDiff(i).avg_sv(index1:index2,:);
%     end
% end

%% Bin-averaged depth profiles of single frequencies and differences

% Using this to check that my differencing "makes sense"

% First, the bin averaging; we want a line plot that has depth on the y
% axis and Sv on the x axis
% That means we have to take the mean over the pings, i.e. compress all
% the rows into one FOR EACH PROFILE/DIVE because we have temporal and
% spatial differences in the plankton distribution

% Then a line plot of the two frequencies and their difference, in both
% linear and log space

for i = 1:3 % for each difference between frequencies
    % Create a figure
    figure(i)
    for cc=1:size(DiveDiff,2) % for each dive
        % Make a subplot that is 3 cells across and a variable number of
        % cells tall depending on how many dives there were
        subplot(3,ceil(size(DiveDiff,2)/3),cc)
        
        % Convert the first frequency from log to linear space
        m = 10 * real(log10(mean(Dive(cc).P(i).sv,1, 'omitnan')));
        
        % Apply a smoothing filter
        m = smoothdata(m,'movmean',5);
        
        % Plot as a line
        plot(m, dbins, '-r', 'linewidth', 1);
        
        hold on
        
        % Convert the second frequency from log to linear space
        n = 10 * real(log10(mean(Dive(cc).P(i+1).sv,1, 'omitnan')));
        
        % Apply a smoothing filter
        n = smoothdata(n,'movmean',5);
        
        % Plot as a line
        plot(n, dbins, '-b', 'linewidth', 1);
        
        % Grab the difference
        % o = nanmean(DiveDiff(cc).PDiff(i).sv,1);
        o = mean(10 * real(log10(DiveDiff(cc).PDiff(i).sv)),1,'omitnan'); % averaging procedure line
        
        % Apply a smoothing filter
        o = smoothdata(o,'movmean',5);
        
        % Plot as a line
        plot(o, dbins, '-k', 'linewidth', 1);
        
        % Formatting
        set(gca, 'Ydir', 'reverse')
        ylim([0 length(dbins)])
        xlim([-120 40])
        xlabel('Ping-Averaged Sv (dB)')
        ylabel('Depth (m)')
        title(['Dive Number ' num2str(cc)])
    end
    % Add title to entire plot
    sgtitle(['Bin-Averaged Profiles for September ',date]); 
end

% Set a legend for the entire plot
figure(1)
g = legend({'130 kHz','200 kHz','200-130 kHz'}, 'Orientation','horizontal');
set(g,'Position',[0.5,0,0.05,0.05]);
    AddLetters2Plots(gcf,'VShift',-0.04)
print(gcf,'-dpng',['/Users/Delphine/Documents/2020_BoF_Zooplankton_Data/Visuals/MATLAB Echosounder Figures/',date,'Sept/Ping Averaged Profiles/Avg_Profiles_',date,'Sept_1.png'],'-r0')
clear g;

% Set a legend for the entire plot
figure(2)
g = legend({'200 kHz','455 kHz','455-200 kHz'}, 'Orientation','horizontal');
set(g,'Position',[0.5,0,0.05,0.05]);
    AddLetters2Plots(gcf,'VShift',-0.04)
print(gcf,'-dpng',['/Users/Delphine/Documents/2020_BoF_Zooplankton_Data/Visuals/MATLAB Echosounder Figures/',date,'Sept/Ping Averaged Profiles/Avg_Profiles_',date,'Sept_2.png'],'-r0')
clear g;

% Set a legend for the entire plot
figure(3)
g = legend({'455 kHz','769 kHz','769-455 kHz'}, 'Orientation','horizontal');
set(g,'Position',[0.5,0,0.05,0.05]);
    AddLetters2Plots(gcf,'VShift',-0.04)
print(gcf,'-dpng',['/Users/Delphine/Documents/2020_BoF_Zooplankton_Data/Visuals/MATLAB Echosounder Figures/',date,'Sept/Ping Averaged Profiles/Avg_Profiles_',date,'Sept_3.png'],'-r0')
clear g;

close all;

%% Plots of the differences alone
for i = 1:3
    % Create a figure
    figure(i)
    for cc = 1:size(DiveDiff,2) % for each dive
        % Make a subplot that is 3 cells across and a variable number of
        % cells tall depending on how many dives there were
        subplot(3,ceil(size(DiveDiff,2)/3),cc)
        
        % Grab the difference
        % o = nanmean(DiveDiff(cc).PDiff(i).sv,1);
        o = mean(10 * real(log10(real(DiveDiff(cc).PDiff(i).sv))),1,'omitnan'); % averaging procedure line
        
        % Apply a smoothing filter
        o = smoothdata(o,'movmean',5);
        
        % Plot as a line
        plot(o, dbins, '-k', 'linewidth', 1);
        
        % Formatting
        set(gca, 'Ydir', 'reverse')
        ylim([0 length(dbins)])
        xlim([0 20])
        xlabel('Ping-Averaged Sv (dB)')
        ylabel('Depth (m)')
        title(['Dive Number ' num2str(cc)])
    end
    % Add title to entire plot
    sgtitle({['Bin-Averaged Difference Profiles for September ',date]...
        [' and Frequency Difference ', num2str(Output(i+1).Freq),' kHz - ',...
        num2str(Output(i).Freq),' kHz']});
    
    AddLetters2Plots(gcf)
    
    % print(gcf,'-dpng',['/Users/delphine/Documents/BoF2020_Cruise/Visuals/MATLAB Echosounder Figures/',...
    %    date,'Sept/Ping Averaged Profiles/Avg_Difference_Profiles_',date,...
    %    'Sept_',i,'.png'],'-r0')
end

close all;

%% Save variables

filename = strcat("/Users/Delphine/Documents/2020_BoF_Zooplankton_Data/Processed_Data/",date,"Sept_Differencing_Data.mat");
% filename = strcat("/Users/delphine/Documents/BoF2020_Cruise/Processed_Data/",date,"Sept_Differencing_Data_Seafloor_Corrected.mat");
%fields = {'Tx','Ty','T','filename','HourlyAvgTemp','SoundSpeed','N','Range','TiltCorrRange','Sv','TS','seaAbs','Freq','Bins2Avg','Time2Avg','BurstInt','PingPerProfile','NumAcqPings','DataType'};
%Output = rmfield(Output, fields);
%Dive = rmfield(Dive, 'P');
save(filename, 'Output', 'P', 'Dive', 'Diff', 'DiveDiff', 'PDiff');
% save(filename, 'Output', 'P', 'Dive', 'Diff', 'DiveDiff', 'PDiff', '-V6');
clear filename;