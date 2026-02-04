clc
clear variables
close all

addpath( genpath('/Users/Delphine/Documents/2020_BoF_Zooplankton_Data/Code/AzfpMatlabToolbox_v18'))
addpath( genpath('/Users/Delphine/Documents/2020_BoF_Zooplankton_Data/Raw_Data'))
addpath( genpath('/Users/Delphine/Documents/2020_BoF_Zooplankton_Data/Processed_Data'))

%%%%%%%%%%%%%%%%%%%%%%%%%%
% Jan 7, 2021 AZFP Noise Floor Estimate
% Scott Loranger

% Modified Nov 19, 2021
% Delphine Mossman
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Cleanup section, clearing out clutter and making sure data are accessible

%% AZFP Parameters
% Parameter description and the default value (if the value is omitted):
% >> ParametersAZFP;[Output,Par] = ProcessAZFP(Parameters);
%
% Ver 1.3 September 2017
% written by Dave Billenness
% ASL Environmental Sciences Inc.
% 1-6703 Rajpur Place, Victoria, B.C., V8M 1Z5, Canada
% T: +1 (250) 656-0177 ext. 126
% E: dbillenness@aslenv.com
% w: http://www.aslenv.com/
% For any suggestions, comments, questions or collaboration, please contact me.

% FILE LOADING AND AVERAGING:
% Parameters.ProcDir = 0; 1 will prompt for an entire directory to
% process, = 0 will prompt to load individual files in a directory
Parameters.ProcDir = 1;

% Parameters.datafilename = ''; % '' will prompt for hourly AZFP
% file(s) to load, example '16010100.01A'
Parameters.datafilename = '';

% Parameters.xmlfilename = ''; % prompt for XML filename if no XML file exists
% in the directory, example '15101614.XML'
Parameters.xmlfilename = '';

% Parameters.Salinity = 35; % Salinity in psu
Parameters.Salinity = 33;

% Parameters.Bins2Avg = 10; % number of range bins to average
Parameters.Bins2Avg = 1; % 0.45m ->15

% Parameters.Time2Avg = 60; % number of time values to average
Parameters.Time2Avg = 1; %1

% Parameters.Pressure = 50; % in dbars (~ depth of instrument in meters).
% This can be approximate and is used in the soundspeed and absorption calc
Parameters.Pressure = 100;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PLOTTING:
% Parameters.Plot = 1; % show an echogram plot for each channel
Parameters.Plot = 1;

% Parameters.Channel: freq to plot #1-4, default 1
Parameters.Channel = 1;

% Parameters.Value2Plot = 2; 1,2,3,4 = Counts, Sv, TS, Temperature/Tilts, default 2
Parameters.Value2Plot = 2;

% Parameters.NoiseFloor = 10000; % for Sv and Ts plotting only, values
% with counts < NoiseFloor will be set to -150, can use individual values
% for each frequency, ex. 'NoiseFloor',[10000; 11000; 10500; 12500]
% Default = 10000.
% Parameters.NoiseFloor = [10000;11000;11000;11500];
% Parameters.NoiseFloor = [11014;9966;13206;12760]; % 59015
Parameters.NoiseFloor = [12564;9660;13604;10780]; % 59016
% from the manufacturer; 59015/59016 are serial numbers

% Parameters.Orientation = 0 instrument on bottom looking up (range bins), 1 at surface
% looking down (depth bins). This changes the ydir on the echogram plots only. Default is 1.
Parameters.Orientation = 1;

% Parameters.UseTiltCorr = 0; Use the tilt corrected ranges for the echogram plots,
% default 0. Will give a warning if the tilt magnitudes are unreasonable (> 20 deg)
Parameters.UseTiltCorr = 0;

%% My parameters to add
Parameters.Plot = 0;

% do not show an echogram plot for each channel?

%% Load AZFP Data
% Will pull up your file explorer on the current path; first select the
% *folder* with the AZFP data, then select the XML file within that folder
% If there is only one XML file, the code will find it automatically, and
% you will not need to select it
[Output,Par]=ProcessAZFP(Parameters);

% Used when saving plots, since there are different subfolders for each day
% of data; it's up here because the AZFP data is also divided by day, so
% whatever day is being worked on is "fresh" in my memory
date = input('Enter the numerical day of the data: ','s');

% sort everything by date
[~,order] = sort([Output(:).Date],'ascend');

Output(1).Date = Output(1).Date(order);
Output(1).BatteryMain = Output(1).BatteryMain(order);
Output(1).BatteryTx = Output(1).BatteryTx(order);
Output(1).Depth = Output(1).Depth(order);

Output(1).N = Output(1).N(order,:);
Output(2).N = Output(2).N(order,:);
Output(3).N = Output(3).N(order,:);
Output(4).N = Output(4).N(order,:);

Output(1).Sv = Output(1).Sv(order,:);
Output(2).Sv = Output(2).Sv(order,:);
Output(3).Sv = Output(3).Sv(order,:);
Output(4).Sv = Output(4).Sv(order,:);

Output(1).TS = Output(1).TS(order,:);
Output(2).TS = Output(2).TS(order,:);
Output(3).TS = Output(3).TS(order,:);
Output(4).TS = Output(4).TS(order,:);

clear order

% In Output, there are variables tx, ty, t, and tilt-corrected range
% Fsr tx/ty/t are empty??? ASL says that there should be a tilt sensor
% inside the echosounder, so if that is true, code should correct for that
% automatically
% If the glider-mounted echosounder doesn't have tilt, we could possibly
% fill that in with the pitch angle from the glider data?

% Offset based off seafloor echo strength; 455 Khz seafloor is ~3 dB higher
% than others, so subtract 3 db from all the values
Output(3).Sv = Output(3).Sv - 3;

% In the Output file, the rows and columns of different subsets of data
% correspond to different important values
% * The Output Date row number is the number of pings in the entire file
% (1 ping/second)
% * The Output Range column number is the number of range bins
% ** What is a range bin? It refers to the range that the ping was
% "collected" from
% ** In the raw data, these bins are fairly narrow; when taking the average
% sv, they are collated into 1m-wide bins, and that's why the column number
% of the avg_sv variable is smaller than the Output Range column number
% * So, Output Depth is a large matrix of depth values for a given ping
% (row number) and range bin (column number); Output Sv is the same
% row/column format but for Sv values instead of depths

%% Trim Transmit Pulse/Near Field from Sv
% Using 2 m as the calculated Rb for the highest frequency (769 kHz) is ~1 m;
% therefore this should eliminate the near-field data from all four
% frequencies
I = find(Output(1).Range(1,:) <= 2);

for i = 1:4
    Output(i).Sv(:,I) = [];
    Output(i).Range(:,I) = [];
end

% self explanatory, if the ping is within 2 m of the transducer, remove it
% on all frequencies
%% Load Glider Data and correct for depth of glider

% Skip this section if you are working with cage-mounted AZFP data!
% THIS MEANS YOU, GINA!!

% load up the raw datafile downloaded from CEOTR
load('C:\Users\Delphine\Documents\2020_BoF_Zooplankton_Data\Raw_Data\Glider\cabot_20200919_119_delayed_83ef_60ad_f731.mat');
% load('cabot_20220727_158_delayed_83ef_60ad_f731.mat');

% put that data into a nicely named variable
gliderdata = cabot_20200919_119_delayed;
clear cabot_20200919_119_delayed
% gliderdata = cabot_20220727_158_delayed;
% clear cabot_20220727_158_delayed

% change time to same format as in Output file (datenum)
unix_epoch = datenum(1970,1,1,0,0,0);
gliderdata.mtime = gliderdata.time./86400 + unix_epoch;

% find the non-NaN indices of glider depth and get their values + the time
% at which they were recorded
nanindex = find(~isnan(gliderdata.depth));
gdepth = gliderdata.depth(nanindex);
gtime = gliderdata.mtime(nanindex);

% %% get the average glider descent/ascent rate
% 
% for i = 1:length(gdepth)-1
%     gdeltadepth(i) = abs(gdepth(i+1) - gdepth(i));
% end
% gdeltadepth = gdeltadepth';
% gdeltadepth=[0; gdeltadepth];
% 
% for i = 1:length(gtime)-1
%     gdeltatime(i) = abs(datetime(gtime(i+1), 'ConvertFrom','datenum') - datetime(gtime(i), 'ConvertFrom','datenum'));
% end
% gdeltatime = gdeltatime';
% gdeltatime=[0; gdeltatime];
% gdeltatime = seconds(gdeltatime);
% 
% diving_index = find(gdeltatime <= 6);
% gdeltatime = gdeltatime(diving_index);
% gdeltadepth = gdeltadepth(diving_index);
% 
% gdiverate = gdeltadepth ./ gdeltatime;
% mean(gdiverate, 'omitnan');
% 
% LatLon = [gliderdata.latitude gliderdata.longitude NaN*ones(1, length(gliderdata.latitude))'];
% 
% LatLon(1,3) = 0;
% for j = 1:length(LatLon)-1
%     LatLon(j+1,3) = rad2km(distance(LatLon(j,1),LatLon(j,2),LatLon(j+1,1),LatLon(j+1,2)));
% end
% 
% LatLon(:,3) = LatLon(:,3) .* 1000;
% gposi = LatLon(nanindex,:);
% gposi = gposi(diving_index,:);
% 
% gtime2 = gtime(diving_index);
% day_index = find(string(datetime(gtime2, 'ConvertFrom', 'datenum', 'Format', 'dd')) == "20");
% 
% ggroundspeed = gposi(day_index,3) ./ gdeltatime(day_index);
% convvel(mean(ggroundspeed, 'omitnan'), 'm/s', 'kts');

%%
% for each date in the echosounder Output file, create a timeindex entry equal to
% the index # of where the minimum difference between each recorded echosounder time
% and every non-NaN glider time is; this is time-aligning the glider and
% echosounder data

for ii = 1:length(Output(1).Date)
    [~,timeindex(ii)] = min(abs(Output(1).Date(ii) - gtime));
end

% Create a new variable in the echosounder Output structure called Depth,
% and make it equal to the echosounder Range (i.e. transducer ping depth) plus
% the glider depth at that time
% Then make three more Depths of equal value, so you have one per frequency
Output(1).Depth = Output(1).Range(1,:) + gdepth(timeindex);
Output(2).Depth = Output(1).Depth(:,1:size(Output(2).Sv,2));
Output(3).Depth = Output(1).Depth(:,1:size(Output(3).Sv,2));
Output(4).Depth = Output(1).Depth(:,1:size(Output(4).Sv,2));

% Use these lines instead if you are working with cage AZFP data and
% comment out lines 241 to 252
% GINA THIS IS FOR YOU AGAIN!!!!
% Output(1).PingDepth = Output(1).Range(1,:) + Output(1).Depth;
% Output(2).PingDepth = Output(1).PingDepth(:,1:size(Output(2).Sv,2);
% Output(3).PingDepth = Output(1).PingDepth(:,1:size(Output(3).Sv,2);
% Output(4).PingDepth = Output(1).PingDepth(:,1:size(Output(4).Sv,2);
%% Find the indices of each dive

StartDive = find([1;diff(Output(1).Depth(:,1))<0]);
% find indices where depth decreases
% these indices will be used as starting points for searching for actual
% dives

cc = 0; % number of distinct dives to go into the structure

for DD = 1:length(StartDive)-1
    if (StartDive(DD+1)-1 - StartDive(DD)) > 250
        % This if statement checks the "length" of each dive and only keeps
        % dives with > 250 entries
        % This avoids short dives and "false" dives where the glider begins
        % to rise again at the end of a dive
        % Depending on the depth of the area you are in, this value may
        % need to be changed in order to capture the true dives
        cc = cc+1;
        Dive(cc).Index = [StartDive(DD);StartDive(DD+1)-1];
    end
end

% The index-finding code skips over a few data points in between dives,
% which I need to correct for since glider dives are not separated by time;
% they happen one after the other while the instrument is on

% for DD = 1:cc-1
%     Dive(DD).Index(2) = Dive(DD + 1).Index(1);
% end
% Dive(end).Index(2) = length(Output(1).Depth);
%% KD added 31 August 2021: determine a maximum range for each frequency
% Limit this analysis to only samples within a defined range.  Eyeball for
% now but later Scott suggests:
% "...use an informed value for the threshold.
% I would probably use something like the modeled backscatter from X copepods
% per m3 (whatever density makes sense) and exclude any ranges where the noise
% is above 10dB below the modeled backscatter for your target.  Detection is
% usually limited to 10dB above the noise, but it's not a hard and fast rule.
% This way you are only analyzing depths where there is sufficient SNR to
% detect your target."

% To start getting an idea of where the noise floor cutoff range is
figure
for ii=2:50:2000  % can change to plot more or less data.
    scatter(Output(4).Range(1,:),Output(4).Sv(ii,:),'k') % frequency change line
    % plots range vs frequency-dependent Sv
    hold on
    xlabel('range')
    ylabel('Sv')
end

%% KD added 31 August 2021: far field range cut-off
% 130 kHz = 75 m, 200 kHz = 50 m, 455 kHz = 35 m, 769 kHz = 20 m
% these are our "eyeballed" values

cr = [75,50,35,20];
for i = 1:4 % for each frequency
    J = find(Output(i).Range(1,:) >= cr(i)); % find the frequency-specific
    % far field data
    Output(i).Sv(:,J) = []; % eliminate the Sv in the far field
    Output(i).Range(:,J) = []; % eliminate the far field ranges
    % Redo the depth calculation to account for the far field being cut off
    Output(i).Depth = Output(i).Range(1,:) + gdepth(timeindex);
    % Output(i).PingDepth = Output(i).Range(1,:) + Output(1).Depth;
end

%% Find and remove seafloor data from each dive
for k=1:4 % for each frequency
    %for j = 1:cc % for each dive
    % first need to pull out sv and depths for each dive separately, so
    % remove bottom does not delete too much data
    %         sv = Output(k).Sv(Dive(j).Index(1):Dive(j).Index(2),:)';
    %         depth = Output(1).Depth(Dive(j).Index(1):Dive(j).Index(2),:)';
    
    sv = Output(k).Sv';
    depth = Output(k).Depth';
    % depth = Output(1).PingDepth';
    
    % next run the find and remove bottom functions to delete all data
    % at or below the seafloor
    [bott_sv(k,:), bott_dep(k,:), bott_ind(k,:)] = find_bottom(sv, depth);
    sv_nb = removeBottom(sv,bott_sv(k,:),bott_ind(k,:));
    
    % finally, reinsert the data with the seafloor removed into the
    % original Output file
    % Output(k).Sv(Dive(j).Index(1):Dive(j).Index(2),:) = sv_nb';
    
    Output(k).Sv = sv_nb';
    %end
end

% figure
% imagesc(Output(3).Sv')
% test figure to see if it worked

% Note; for some reason, the find bottom code misses some values towards
% the end of the first and last dives of September 19th; cannot figure out
% why
% Kim figured out that changing maxdepinds in find_bottom eliminates this,
% but we are still not sure why it happens at all
% Gotta play around with it

%% Average into 1 m depth bins
dbins = 5:200;

% At each depth (starting at 5 because of the trimming step), for each
% value in the Depth entry of the Output, first find the indices where
% depths are in a 1 m bin (defined as +/-0.5 each entry in dbins) and
% assign those to I

% Next, get the average Sv by raising 10 to the power of the 1m bin depths,
% dividing each of those values by 10, and taking the mean of that entire
% vector

% Convention is to average Sv in linear space; because Output gives Sv in
% decibel space, need to convert back to linear space
% Reduces influence of weak scatterers? Unsure of exact reasoning but it's
% just what's done.
% KD reply: arithmetic operations are always done in linear space and
% although its been explained to me before the explanation is odd.  It is
% convention in the discipline though.

tic
for ii=1:4 % for each frequency
    for dd = 1:length(dbins) % for each depth bin
        for pp = 1:size(Output(ii).Depth,1) % for each ping
            I = find(Output(ii).Depth(pp,:) > (dbins(dd)-0.5) & Output(ii).Depth(pp,:) <= (dbins(dd)+0.5) );
            P(ii).avg_sv(pp,dd) = nanmean(10.^(Output(ii).Sv(pp,I)./10)); % frequency change line
        end
    end
end
toc

% Each avg_sv ends up being a large matrix with each column being a depth
% bin (in 1 m increments)
% and each row being ping number (every 1s)
% Does NOT correspond with horizontal space

%% Organize into separate dives and take the median of all pings at a given depth to create single profiles for each dive

for jj=1:4 % Frequency index
    cc = 0;
    for DD = 1:length(StartDive) % for each index where a dive might start
        if DD == length(StartDive) % important for separation of dives/deal with fact that glider comes back up sometimes
            % we need to check if we are at the end of the StartDive
            % vector, because the code changes
            if (size(P(jj).avg_sv,1) - StartDive(DD)) > 250 % at the end of some dives the glider starts coming back up. Avoid that data. Also avoid very short dives
                cc = cc+1;  % Dive count
                % grab the avg_sv values corresponding to the dive
                Dive(cc).P(jj).sv = P(jj).avg_sv(StartDive(DD):end,:);
                % get the median of the dive avg_sv
                Dive(cc).P(jj).msv = nanmedian(Dive(cc).P(jj).sv,1);
                % use the median not the mean to decrease the influence of high scattering spikes (such as bubbles and fish)
            end
        else % when we are not at the end of the StartDive vector
            if (StartDive(DD+1)-1 - StartDive(DD)) > 250
                cc = cc+1; % Dive count
                % grab the avg_sv values corresponding to the dive
                Dive(cc).P(jj).sv = P(jj).avg_sv(StartDive(DD):StartDive(DD+1)-1,:);
                % get the median of the dive avg_sv
                Dive(cc).P(jj).msv = nanmedian(Dive(cc).P(jj).sv,1);
            end
        end
    end
end

% Note from Delphine: I tried using the DiveIndex that I got before to do
% this step, and it did not work
% It does not take that long to compile, so I've left it as-is

%% Moving average to determine noise floor

% According to Scott, each frequency should have its own noise floor due to
% frequency dependencies, differences in the conditions, etc
% We assume that the minimum Sv in each frequency is equivalent to the
% noise floor for that frequency

d_int = 10;
% depth interval to average over

for i = 1:4 % for each frequency
    % preallocate enough space 
    M(i).AvgSv = nan * ones(size(Dive, 2), length(dbins));
    for f = 1:size(Dive, 2) % for each dive
        % take the mean of d_int Sv values at a time and put them in the M
        % structure
        % any means that include a NaN are set to NaN
        % (need to include the NaNs here for depth window calculations
        % later)
        temp = movmean(Dive(f).P(i).msv, d_int, 'includenan', 'Endpoints','discard');
        M(i).AvgSv(f,1:length(temp)) = temp;

    end
end

% Then find the minimum noise interval for each frequency
for i = 1:4 % for each frequency
    % find the minimum Sv value in the moving average and its index, not
    % counting any NaN values
    [N, index] = min(M(i).AvgSv,[],'all','linear','omitnan');
   
    % raw minimum value
    NoiseFloor(i) = N;
    % dive number for each frequency where the minimum is located
    [D, J] = ind2sub(size(M(i).AvgSv),index);
    divenum(i) = D;
    
    while J >= 187
        J = J - 1;
    end
    
    % depth interval for each frequency where the minimum Sv is located
    DepthWindow(i,:) = dbins(J:J+d_int);
end

% remove the temporary structure
clear temp;

% %% Save daily noise floor
% Preserved for archival purposes
% filename = strcat("/Users/delphine/Documents/MATLAB/Sept_","26th","_Noise_Floor_Data_10m.mat");
% save(filename, 'NoiseFloor_26th', 'DepthWindow_26th', 'divenum_26th', 'frequency_26th');

%% Subtract the noise floor from the avg_sv structures
for  i = 1:4 % for each frequency
    % subtract the frequency-dependent noise floor from avg_sv
    P(i).avg_sv = P(i).avg_sv - NoiseFloor(i);
end

for j = 1:size(Dive, 2) % for each dive
    for k = 1:4 % for each frequency
        % subtract the frequency-dependent noise floor
        Dive(j).P(k).sv = Dive(j).P(k).sv - NoiseFloor(i);
        % recalculate the median
        Dive(j).P(k).msv = nanmedian(Dive(j).P(k).sv,1);
    end
end

%% Save necessary variables

filename = strcat("/Users/Delphine/Documents/2020_BoF_Zooplankton_Data/Processed_Data/",date,"Sept_Processed_Data.mat");
save(filename, 'Output', 'P', 'Dive', 'dbins', 'cc', 'gliderdata', 'StartDive');
clear filename;

%% Bottom depth line cleanup for Sept 21st figure

if date == "21"
    bott_dep(:,1905:1951) = NaN;
    P(1).avg_sv(1905:1951,:) = NaN;
    P(2).avg_sv(1905:1951,:) = NaN;
    P(3).avg_sv(1905:1951,:) = NaN;
    P(4).avg_sv(1905:1951,:) = NaN;

    bott_dep(:,2975:3017) = NaN;
    P(1).avg_sv(2975:3017,:) = NaN;
    P(2).avg_sv(2975:3017,:) = NaN;
    P(3).avg_sv(2975:3017,:) = NaN;
    P(4).avg_sv(2975:3017,:) = NaN;

    bott_dep(:,4094:4133) = NaN;
    P(1).avg_sv(4094:4133,:) = NaN;
    P(2).avg_sv(4094:4133,:) = NaN;
    P(3).avg_sv(4094:4133,:) = NaN;
    P(4).avg_sv(4094:4133,:) = NaN;

    bott_dep(:,5213:5256) = NaN;
    P(1).avg_sv(5213:5256,:) = NaN;
    P(2).avg_sv(5213:5256,:) = NaN;
    P(3).avg_sv(5213:5256,:) = NaN;
    P(4).avg_sv(5213:5256,:) = NaN;

    bott_dep(:,6268:6307) = NaN;
    P(1).avg_sv(6268:6307,:) = NaN;
    P(2).avg_sv(6268:6307,:) = NaN;
    P(3).avg_sv(6268:6307,:) = NaN;
    P(4).avg_sv(6268:6307,:) = NaN;

    bott_dep(:,7284:end) = NaN;
    P(1).avg_sv(7284:end,:) = NaN;
    P(2).avg_sv(7284:end,:) = NaN;
    P(3).avg_sv(7284:end,:) = NaN;
    P(4).avg_sv(7284:end,:) = NaN;
end

%% Create ping by depth figure with color scale corresponding to Sv for all four frequencies
figure(1)

subplot(2,2,1)
% figure
imagesc([NaN * ones(5, size(P(1).avg_sv, 1)); 10*log10(abs(P(1).avg_sv'))],'AlphaData',~isnan([NaN * ones(5, size(P(1).avg_sv, 1)); 10*log10(abs(P(1).avg_sv'))])) % conversion back to decibels + dealing with the fact that we cut off the first 5 m of data
colormap('parula');
caxis([-80 -50]);
% set(gca, 'Xdir', 'reverse');
xlabel('Time')
ylabel('Depth (m)')
ylim([0,200])
title('130 kHz')
xt = get(gca,'XTick');
xtlbl = [];
for i = 1:numel(xt)
    temp = Output(1).Date(xt(i));
    temp = datetime(temp, 'ConvertFrom','datenum','Format','HH:mm');
    temp = char(temp);
    xtlbl = [xtlbl;temp];
end

set(gca, 'XTick',xt, 'XTickLabel',xtlbl, 'XTickLabelRotation',30)
hold on
line(1:length(bott_dep),bott_dep(1,1:length(bott_dep)),'Color','m','LineWidth',1)
hold off

subplot(2,2,2)
imagesc([NaN * ones(5, size(P(2).avg_sv, 1)); 10*log10(abs(P(2).avg_sv'))],'AlphaData',~isnan([NaN * ones(5, size(P(2).avg_sv, 1)); 10*log10(abs(P(2).avg_sv'))]))
colormap('parula');
caxis([-80 -50]);
% set(gca, 'Xdir', 'reverse');
xlabel('Ping Number')
ylabel('Depth (m)')
ylim([0,200])
title('200 kHz')
set(gca, 'XTick',xt, 'XTickLabel',xtlbl, 'XTickLabelRotation',30)
hold on
line(1:length(bott_dep),bott_dep(2,1:length(bott_dep)),'Color','m','LineWidth',1)
hold off

subplot(2,2,3)
imagesc([NaN * ones(5, size(P(3).avg_sv, 1)); 10*log10(abs(P(3).avg_sv'))],'AlphaData',~isnan([NaN * ones(5, size(P(3).avg_sv, 1)); 10*log10(abs(P(3).avg_sv'))])) 
colormap('parula');
caxis([-80 -50]);
% set(gca, 'Xdir', 'reverse');
xlabel('Ping Number')
ylabel('Depth (m)')
ylim([0,200])
title('455 kHz')
set(gca, 'XTick',xt, 'XTickLabel',xtlbl, 'XTickLabelRotation',30)
hold on
line(1:length(bott_dep),bott_dep(3,1:length(bott_dep)),'Color','m','LineWidth',1)
hold off

subplot(2,2,4)
imagesc([NaN * ones(5, size(P(4).avg_sv, 1)); 10*log10(abs(P(4).avg_sv'))],'AlphaData',~isnan([NaN * ones(5, size(P(4).avg_sv, 1)); 10*log10(abs(P(4).avg_sv'))]))
colormap('parula');
caxis([-80 -50]);
% set(gca, 'Xdir', 'reverse');
xlabel('Ping Number')
ylabel('Depth (m)')
ylim([0,200])
title('769 kHz')
set(gca, 'XTick',xt, 'XTickLabel',xtlbl, 'XTickLabelRotation',30)
hold on
line(1:length(bott_dep),bott_dep(4,1:length(bott_dep)),'Color','m','LineWidth',1)
hold off

% single colorbar for all four plots
h = colorbar;
set(get(h,'label'),'string','Sv (dB scattering per unit volume)');

h.Position(4) = 0.65;
h.Position(1) = .94-h.Position(3);
h.Position(2) = 0.5-h.Position(4)/2;

AddLetters2Plots(gcf,'VShift',-0.04)
% save the figure
filename = strcat("/Users/Delphine/Documents/2020_BoF_Zooplankton_Data/Visuals/MATLAB Echosounder Figures/",date,"Sept/Frequencies_All_",date,"Sept.png");
print(gcf,'-dpng',filename,'-r0')

clear filename;
%close

%%
% 
% figure(1)
% 
% subplot(2,1,1)
% imagesc(Output(1).Sv')
% colormap('jet');
% caxis([-110 -0]);
% xlabel('Ping Number', 'FontSize', 16)
% ylabel('Range', 'FontSize', 16)
% 
% subplot(2,1,2)
% imagesc(10*log10(abs(P(1).avg_sv'))) % conversion back to decibels + dealing with the fact that we cut off the first 5 m of data
% colormap('jet');
% caxis([-110 -0]);
% xlabel('Ping Number', 'FontSize', 16)
% ylabel('Depth (m)', 'FontSize', 16)
% 
% h = colorbar;
% set(get(h,'label'),'string','Sv (dB scattering per unit volume)');
% 
% h.Position(4) = 0.65;
% h.Position(1) = .94-h.Position(3);
% h.Position(2) = 0.5-h.Position(4)/2;
% 
% AddLetters2Plots(gcf,'VShift',-0.05,'Direction','TopDown','FontSize',16)
% 
% filename = strcat("/Users/delphine/Documents/BoF2020_Cruise/Visuals/MATLAB Echosounder Figures/",date,"Sept/Output_Unprocessed_and_Processed_",date,"Sept.png");
% print(gcf,'-dpng',filename,'-r0')
% clear filename;
% % close;
% %%
% for cc = 1:size(Dive,2)
%     for dd = 1:4
%         figure
%         subplot(121)
%         imagesc(10*log10(abs(Dive(cc).P(dd).sv')))
%         caxis([-80 -60]);
%         xlabel('Ping Number','FontSize',14)
%         ylabel('Depth (m)','FontSize',14)
%         % title(['Dive Number ' num2str(cc)])
%         title('Example Echogram','FontSize',14)
%         h = colorbar;
%         % set(get(h,'label'),'string','Sv (dB scattering per unit volume)');
%         set(get(h,'label'),'string','Acoustic Scattering (dB per unit volume)');
%         ylim([0 180])
%         
%         subplot(122)
%         plot(10*log10(abs(Dive(cc).P(dd).msv)),dbins,'-k','linewidth',2)
%         set(gca, 'Ydir', 'reverse')
%         ylim([0 180])
%         xlim([-100 -20])
%         
%         filename = strcat("/Users/delphine/Documents/BoF2020_Cruise/Visuals/MATLAB Echosounder Figures/",...
%             date,...
%             "Sept/",date,...
%             "Sep2020_Freq",...
%             string(dd),...
%             "_Dive",...
%             string(cc),...
%             ".png");
%         print(gcf,'-dpng',filename,'-r0')
%         clear filename;
%         close;
%     end
% end
% 
% %% Plot the median Sv profiles to visualize where the active noise floor can
% % be calculated.
% 
% % I don't recall why this was important but it probably is
% 
% for cc=1:size(Dive,2)
%     figure
%     m=plot(10*log10(abs(Dive(cc).P(1).msv)),dbins,'-k','linewidth',2);
%     hold on
%     n=plot(10*log10(abs(Dive(cc).P(2).msv)),dbins,'-r','linewidth',2);
%     o=plot(10*log10(abs(Dive(cc).P(3).msv)),dbins,'-g','linewidth',2);
%     p=plot(10*log10(abs(Dive(cc).P(4).msv)),dbins,'-b','linewidth',2);
%     set(gca, 'Ydir', 'reverse')
%     ylim([0 length(dbins)])
%     xlim([-100 -50])
%     legend([m,n,o,p],{'130 kHz','200 kHz','455 kHz','769 kHz'})
%     % pause(0.5)
%     xlabel('Median Sv (dB)')
%     ylabel('Depth (m)')
%     title(['Dive Number ' num2str(cc)])
% end
% 
% %close all;
% 
% %% Scott's code for a single frequency. Preserved for archival purposes
% for DD = 1:length(StartDive) % for each index where a dive might start
%     
%     if DD == length(StartDive) % important for separation of dives/deal with fact that glider comes back up sometimes
%         if (size(avg_sv,1) - StartDive(DD)) > 250 % at the end of some dives the glider starts coming back up. Avoid that data. Also avoid very short dives
%             cc = cc+1;
%             Dive(cc).sv = avg_sv(StartDive(DD):end,:); % get the pings at each depth
%             Dive(cc).msv = nanmedian(Dive(cc).sv,1); % use the median not the mean to decrease the influence of high scattering spikes (such as bubbles and fish)
%             figure
%             
%             % plotting all Svs
%             subplot(121)
%             imagesc(10*log10(abs(Dive(cc).sv')))
%             title(['Dive Number ' num2str(cc)])
%             caxis([-100 -50])
%             colorbar
%             ylim([0 180])
%             
%             % plotting medians only - without removing noise data?
%             %             subplot(122)
%             %             plot(10*log10(abs(Dive(cc).msv)),dbins,'-k','linewidth',2)
%             %             set(gca, 'Ydir', 'reverse')
%             %             ylim([0 180])
%             %             xlim([-100 -20])
%             %             pause(0.5)
%             
%         end
%     else % when we are not at the end of the StartDive vector
%         if (StartDive(DD+1)-1 - StartDive(DD)) >250
%             cc = cc+1;
%             Dive(cc).sv = avg_sv(StartDive(DD):StartDive(DD+1)-1,:); % indicies of start/end of dive
%             Dive(cc).msv = nanmedian(Dive(cc).sv,1); % median of Dive Svs
%             figure
%             subplot(121)
%             imagesc(10*log10(abs(Dive(cc).sv')))
%             title(['Dive Number ' num2str(cc)])
%             caxis([-100 -50])
%             colorbar
%             ylim([0 180])
%             
%             subplot(122)
%             plot(10*log10(abs(Dive(cc).msv)),dbins,'-k','linewidth',2)
%             set(gca, 'Ydir', 'reverse')
%             ylim([0 180])
%             xlim([-100 -20])
%             pause(0.5)
%             
%         end
%     end
%     
% end


%% Fit a line to get noise floor for whole depth range
% you could also just fit a line to the "gap" where the scattering layer is
% i.e. from 72 to 100 m
% Eyeballed spot where there was not lots of scatterers/real signal; dives
% 8 and 9 have less stuff in the water column
% However, might be frequency dependent; could also verify with the
% multinet data

% Values above the noise floor are "real" data
% Seafloor drives noise floor calculation, this is an issue; remove
% seafloor data first?
% Masking issue is also present

% Should this be done before averaging? Or am I misunderstanding the noise
% floor procedure

% I = find( ( (dbins <=50) | (dbins>=130 & dbins<=140 ) ) & ~isnan(Dive(3).msv) ); % noise floor change line
% nsv = Dive(3).msv(I); % noise floor change line
% ndbins = dbins(I);
% 
% p = polyfit(ndbins,10*log10(abs(nsv)),1);
% fitnSv = polyval(p,dbins);
% 
% 
% for ii = 1:length(Dive)
%     figure(ii)
%     hold on
%     plot(10*log10(abs(Dive(ii).msv)),dbins,'-k','linewidth',2)
%     %plot(fitnSv,dbins,'--r','linewidth',2)
%     xlim([-100 -20])
%     ylim([0 180])
%     set(gca, 'Ydir', 'reverse')
% end

% 19 Sept: eyeballed least scattering between 0 to 72m, and 100 to 150m on
% dive 8
% 20 Sept: eyeballed least scattering between 0 to 70m, and 110 to 130m on
% dive 6
% 21 Sept: eyeballed least scattering between 50 to 90m, and 110 to 130m on
% dive 7
% 24 Sept: eyeballed least scattering between 50 to 70m, and 150 to 165m on
% dive 9
% 25 Sept: eyeballed least scattering between 60 to 90m, and 160 to 175m on
% dive 5
% 26 Sept: eyeballed least scattering between 0 to 50m, and 130 to 140m on
% dive 3
