header=fgetl(opc);
disp(['header is ' header])
disp('do you wish to change it?')
change = input('1 for Yes, 0 for No>>>>>>>')
if change==1;
    modification=1;
    header=input('enter new header>> ')   
end 
if change==0;
    modification=0;
end
[hday,header]=strtok(header);
hday=str2num(hday);
[hmonth,header]=strtok(header);
[hdate,header]=strtok(header);
hdate=str2num(hdate);
[htime,header]=strtok(header,' ');
[hhour,hrest]=strtok(htime,'":');
 hhour=str2num(hhour);
[hminute,hrest]=strtok(hrest,':');
 hminute=str2num(hminute);
 [hsecond,hrest]=strtok(hrest,':');
 hsecond=str2num(hsecond);
 [hyear,header]=strtok(header,' ');
%hyear=str2num(hyear);
datestr(htime,13);
 %% to convert the header month into a number call the m file header

if hmonth=='Jan'
     hmonth=1
 elseif hmonth=='Feb'
     hmonth=2
 elseif hmonth=='Mar'
     hmonth=3
 elseif hmonth=='Apr'
     hmonth=4
 elseif hmonth=='May'
     hmonth=5
 elseif hmonth=='Jun'
     hmonth=6
 elseif hmonth=='Jul'
     hmonth=7
 elseif  hmonth=='Aug'
     hmonth=8
 elseif hmonth=='Sep'
     hmonth=9
 elseif hmonth=='Oct'
     hmonth=10
 elseif hmonth=='Nov'
     hmonth=11
  elseif hmonth=='Dec'
     hmonth=12
 else
    error ('error in Header month')
 end