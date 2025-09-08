% centered moving average
% data is the desired data string to average, center is the value at the
% center of the moving average, window is one half of the desired window
% size, series length is the length of the data series you want to average.
%  Note that the resulting series will have 2*window less observations,
%  filled with NaNs on either end.
function [moving_av]=moving_avg(data,center,window,serieslength)

for i=center:serieslength-center   ;
    moving_av(i)=nanmean(data(i-window:i+window));
end
  moving_av((serieslength-center):serieslength) = NaN;
 moving_av(1:center) = NaN;