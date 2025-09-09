%% Load AZFP data
% Make sure you've run the initial processing and differencing code first!
% Otherwise this code won't work!

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

%% Importing and formatting the multinet data

% Load the event log CSV
warning('off','MATLAB:table:ModifiedAndSavedVarnames')
MultiData = readtable('/Users/dmossman/Box/2022 MSc Thesis Work/Raw_Data/Spreadsheets/Sept2020_Cruise_Leg_2_Multinet_Log.csv');

% Keep only the relevant tows and variables
MultiData = MultiData([2:5 7 9 11 12 14 15 17:22],[2 4 5:16]);

% Give the variables readable names
MultiData.Properties.VariableNames = ["Tow" "Date" "StartTime" "EndTime"...
    "StartLat" "StartLong" "EndLat" "EndLong" "Unlockdbarr" "Net1dbarr"...
    "Net2dbarr" "Net3dbarr" "Net4dbarr" "Net5dbarr"];

% Subset the data to only the date we are working with
MultiData = MultiData(strcmp(MultiData.Date, strcat(date,'-Sep')),:);

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

%% Find the gliderdata latitude indices that are between the
% multinet deployment latitudes *on the date being examined*

% Preallocate space for the position indices
posindex = NaN * ones(length(gliderdata.latitude),height(MultiData));

% Get the subset of glider data from the relevant date
subset = string(datetime(gliderdata.mtime, 'ConvertFrom', 'datenum', 'Format', 'dd')) == string(date);
gliderdata2.mtime = gliderdata.mtime(subset);
gliderdata2.latitude = gliderdata.latitude(subset);
gliderdata2.longitude = gliderdata.longitude(subset);

% Locate the latitudes which are in between the start of n and n+1
% multinet tows
for j = 1:height(MultiData)
    if j == height(MultiData)
        if MultiData.StartLat(j) > MultiData.EndLat(j) % traveling south
            temp = find( (gliderdata2.latitude < MultiData.StartLat(j) + 0.002));
            posindex(1:length(temp),j) = temp;
            break
        elseif MultiData.StartLat(j) < MultiData.EndLat(j) % traveling north
            temp = find( (gliderdata2.latitude > MultiData.StartLat(j) - 0.002));
            posindex(1:length(temp),j) = temp;
            break
        end
    elseif MultiData.StartLat(j) > MultiData.EndLat(j) % traveling south
        temp = find( (gliderdata2.latitude < MultiData.StartLat(j) + 0.002...
            & (gliderdata2.latitude > MultiData.StartLat(j+1) - 0.002)) );
        posindex(1:length(temp),j) = temp;
    elseif MultiData.StartLat(j) < MultiData.EndLat(j) % traveling north
        temp = find( (gliderdata2.latitude > MultiData.StartLat(j) - 0.002...
            & (gliderdata2.latitude < MultiData.StartLat(j+1) + 0.002)) );
        posindex(1:length(temp),j) = temp;
    end
end

% Eliminate rows that are all NaNs
posindex(all(isnan(posindex),2),:) = [];

%% Align glider and Output data

% Preallocate space
startindex = NaN * ones(size(posindex,2),1);
endindex = NaN * ones(size(posindex,2),1);

for i = 1:size(posindex,2)
    % First get the times at the same indices as the relevant positions
    gtime2 = gliderdata2.mtime(posindex(~isnan(posindex(:,i)),i));
    
    % Then find the Output Date indices corresponding to the time range
    % where the glider was in the same area as the multinet
    [~,startindex(i)] = min(abs(Output(1).Date - gtime2(1)));
    [~,endindex(i)] = min(abs(Output(1).Date - gtime2(end)));
end

%% Take all 1-m averaged dives closest to the multinet tow

for j = 1:length(startindex) % for each tow
    for i = 1:size(Dive, 2) % for each dive
        % Basically, the code below looks for the overlap between the dive
        % and the multinet tow in terms of their indices in the Output file
        
        % find the indices of the minimum difference between the start index of
        % each dive and the start index of each tow
        temp1(i) = abs(Dive(i).Index(1) - startindex(j));
        [~,starttow(j)] = min(temp1);
        
        % find the indices of the minimum difference between the end index of
        % each dive and the end index of each tow
        temp2(i) = abs(Dive(i).Index(2) - endindex(j));
        [~,endtow(j)] = min(temp2);
    end
    % clear the temporary structures
    clear temp1 temp2
end

for k = 1:4 % for each frequency
    for i = 1:length(starttow) % for each tow
        if starttow(i) == endtow(i) % if there is only one dive that overlaps the multinet tow
            % Take the averaged Sv from that single dive and put it in a
            % new structure to get ready for the integration
            Integration(i).Tow(k).FullSv = Dive(starttow(i)).P(k).sv;
            if k ~= 4
                Integration(i).Tow(k).SvDiff = DiveDiff(starttow(i)).PDiff(k).sv;
            end
        else
            % preallocate space
            Integration(i).Tow(k).FullSv = [];
            Integration(i).Tow(k).SvDiff = [];
            for j = starttow(i):1:endtow(i) % for all the dives
                % Take all the averaged Sv values from the dives and put
                % them in a new structure to get ready for the integration
                Integration(i).Tow(k).FullSv = [Integration(i).Tow(k).FullSv; Dive(j).P(k).sv];
                if k ~= 4
                    Integration(i).Tow(k).SvDiff = [Integration(i).Tow(k).SvDiff; DiveDiff(j).PDiff(k).sv];
                end
            end
        end
    end
end

%% Divide the full dive Svs into net bins

% Each column of the FullSv matrix is a 1m range bin, since I pulled from
% the already-averaged Dive structure, starting at 5 m. So I want to find
% the columns that correspond to each net's interval and integrate the rows
% Net 5's end is always 0 m

% get the net intervals
netintervals = [table2array(MultiData(:,10:14)), zeros(size(table2array(MultiData(:,10:14)),1),1)];

% set up 5 Net columns in Integration structure
for i = 1:size(Integration,2)
    Integration(i).Tow(1).Net = [];
    Integration(i).Tow(5).Net = [];
end

% find the FullSv columns which correspond to the net intervals; inclusive
% on both sides (some overlapping on the ends)

for k = 1:4 % for each frequency
    for i = 1:size(Integration,2) % for each tow
        for j = 1:4 % for nets 1-4
            % Preallocate space
            Integration(i).Tow(j).Net(k).Sv = [];
            % Find the Sv values that are between the net depths
            Integration(i).Tow(j).Net(k).Sv = Integration(i).Tow(k).FullSv(:,...
                (netintervals(i,j+1)-4):(netintervals(i,j)-4));
            if k ~= 4
                Integration(i).Tow(j).Net(k).SvDiff = [];
                Integration(i).Tow(j).Net(k).SvDiff = Integration(i).Tow(k).SvDiff(:,...
                    (netintervals(i,j+1)-4):(netintervals(i,j)-4));
            end
        end
        % account for the fact that net 5's end is 0 m; cannot have an
        % index of 0 in Matlab
        Integration(i).Tow(5).Net(k).Sv = [];
        Integration(i).Tow(5).Net(k).Sv = Integration(i).Tow(k).FullSv(:,...
            1:(netintervals(i,5)-4));
        if k ~= 4
            Integration(i).Tow(5).Net(k).SvDiff = [];
            Integration(i).Tow(5).Net(k).SvDiff = Integration(i).Tow(k).SvDiff(:,...
                1:(netintervals(i,5)-4));
        end
    end
end

%% Integration for each tow, net, and frequency, and put those values somewhere
% A text file for every tow, a column for every net, a row for every
% frequency

for k = 1:size(Integration, 2) % for each tow
    for i = 1:4 % for each frequency
        for j = 1:5 % for each net
            % get the net depth interval from the MultiNet
            interval = netintervals(k,j) - netintervals(k,j+1);
            % Sum the Svs AND DIVIDE BY THE DEPTH INTERVAL FOR AN AVERAGE
            % AAAAAAAA!!!!!!!!!!!
            X(i, j) = abs(sum(Integration(k).Tow(j).Net(i).Sv, 'all', 'omitnan'))/interval;
        end
    end
    
    % Transform X into a table with readable variable names
    X = array2table(X);
    X.Properties.VariableNames = ["Net1" "Net2" "Net3" "Net4" "Net5"];
    X.Properties.RowNames = ["Freq1";"Freq2";"Freq3";"Freq4"];
    
    %     save the file
    filename = strcat("/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data/Glider/",...
        "Integrated_Sv_",...
        string(date),...
        "_Sep_2020_multi",...
        num2str(MultiData.Tow(k),'%03d'),...
        ".txt");
    writetable(X,filename,'Delimiter','tab', 'WriteRowNames',true);
    
    % clear the structure
    clear X;
    clear interval;
end

%% dB Differencing Window
% We need to use the differenced output in order to mask our
% non-differenced frequency to get only the scattering that is consistent
% with certain taxa
% Then we re-integrate the masked frequency over the net intervals

% How to deal with frequency windows changing for different frequency
% differences? For now just focus on 769-455 kHz copepod differencing

% First get the dB difference window
% Values below are for copepods between 1.27 and 2.99 mm in length, from
% Joe's spreadsheet
dB_Diff_Lower = [7.4, 15.8, 7.8]; % input('Enter the lower dB difference bounds: ');
dB_Diff_Upper = [7.5, 16.3, 8.8]; % input('Enter the upper dB difference bound: ');

% Testing different window sizes for middle frequencies

% dB_Diff_Lower(2) = 16;
% dB_Diff_Upper(2) = 26;

% Next, go over the differenced Sv matrices and see whether each value
% falls within the dB difference window

for i = 1:size(Integration,2) % for each tow
    for k = 1:3 % for each frequency difference
        for j = 1:5 % for each net
            % Mask(i).Tow(j).Net(k).Masking = dB_Diff_Lower(k) < Integration(i).Tow(j).Net(k).SvDiff & Integration(i).Tow(j).Net(3).SvDiff < dB_Diff_Upper(k);
            Mask(i).Tow(j).Net(k).Masking = (dB_Diff_Lower(k) < 10 * log10((Integration(i).Tow(j).Net(k).SvDiff))...
                & dB_Diff_Upper(k) > 10 * log10((Integration(i).Tow(j).Net(k).SvDiff)));
            % averaging procedure line
        end
    end
end

% Apply the logical indexing to the matrix of values
for i = 1:size(Integration,2) % for each tow
    for k = 1:3 % for each frequency difference
        for j = 1:5 % for each net
            Integration(i).Tow(j).Net(k).MaskedSv = Integration(i).Tow(j).Net(k).Sv .* Mask(i).Tow(j).Net(k).Masking;
            Integration(i).Tow(j).Net(k).MaskedSv(Integration(i).Tow(j).Net(k).MaskedSv == 0) = NaN;
        end
    end
end

%% Redo the integration with the masked matrix

for k = 1:size(Integration,2) % for each tow
    for i = 1:3 % for each frequency difference
        for j = 1:5 % for each net
            interval = netintervals(k,j) - netintervals(k,j+1);
            
            Y(i, j) = abs(sum(Integration(k).Tow(j).Net(i).MaskedSv, 'all', 'omitnan'))/interval;
        end
    end
    Y = array2table(Y);
    Y.Properties.VariableNames = ["Net1" "Net2" "Net3" "Net4" "Net5"];
    Y.Properties.RowNames = ["Freq2-Freq1", "Freq3-Freq2", "Freq4-Freq3"];
    
    filename = strcat("/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data/Glider/",...
        "Integrated_Sv_Masked_",...
        string(date),...
        "_Sep_2020_multi",...
        num2str(MultiData.Tow(k),'%03d'),...
        ".txt");
    writetable(Y,filename,'Delimiter','tab', 'WriteRowNames',true);
    
    % clear the structure
    clear Y;
    clear interval;
end

%% Save variables

filename = strcat("/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data/",date,"Sept_Integration_Data.mat");
save(filename, 'gliderdata2', 'gtime2', 'netintervals','MultiData','starttow','endtow', 'Integration');
clear filename;

filename = strcat("/Users/dmossman/Box/2022 MSc Thesis Work/Processed_Data/",date,"Sept_Masked_Data.mat");
save(filename, 'Integration', '-V6');
clear filename;