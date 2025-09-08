%%CTD extract subroutine associated to OPC2
[date,ctd]=strtok(rest,' ');
            [date]=strtok(date,',');  % means date is everything after the "11,"
            [month,date]=strtok(date,'/'); % extract the month, put the rest of the string back in "date"
            month=str2num(month);
            [day,year]=strtok(date,'/');
            day=str2num(day);
            [year]=strtok(year,'/');  
            year=str2num(year);
             [time,ctd]=strtok(ctd,' ');
            [hour,time]=strtok(time,':');
            hour=str2num(hour);
             [minute,time]=strtok(time,':');
             minute=str2num(minute);
            [second]=strtok(time,':');
           second=str2num(second);
           ctdtime=datestr(datenum([year month day hour minute second]),'dd-mmm-yy HH:MM:SS');
           [ctddepth,ctd]=strtok(ctd,' ');
            [ctddepth,ctd]=strtok(ctd,' ');
            ctddepth=str2num(ctddepth);
            ctddepth=ctddepth;
            [temperature,ctd]=strtok(ctd,' ');
             [temperature,ctd]=strtok(ctd,' ');
            temperature=str2num(temperature);
            [salinity,ctd]=strtok(ctd,' ');
            salinity=str2num(salinity);
            [toss,density]=strtok(ctd,' ');
            density=str2num(density);
            
            
          