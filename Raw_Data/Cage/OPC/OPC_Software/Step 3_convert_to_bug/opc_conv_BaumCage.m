%%  REMEMBER TO CHANGE THE BUGS FILE IN THE OTHERWISE LOOP....REMOVED FPRINT%%%

%.m file for reading the OPC convert files (Q261OPC.Txx) and writting them into an 
% output file (Q261OPC.Mxx) that is a matrix that can worked with in matlab
%There is 4 associated .m files (subroutines) :headermonth, bugs, printout and CTD
%For Q261 , the CTD scan rate was 17; the offset was 2.2m. For TUBBS 2 (25/08/01)
% The CTD was not functioning. A TDR was attatched to the TUBBS and the associated
% Minilog file is Bin57708.004 (Q261T12.asc)...clock on minilog has to be corrected)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Header format must be: Sun Jul 14 15:53:22 2019
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Flags (from Parizeau cruise basic program, modified by Taggart during Q261)
%  1: bug ID and size (size bin form 1 to 4096)
%  2: Light attenuance-ID and value (relative usually ~1800)
%  3: Timestamp-ID, every 0.5 sec (to verify)up to 4096 and then reset
%  5: OPC-depth-ID and value (need conversion to meters)
%  6: OPC-temperature and value (need conversion)-Not Used
%  9: OPC-Pitch and value (need conversion)
%  10:OPC-Roll and value (need conversion)
%  11: CTD data stream (from OS200 CTD)
%  14: GPS data stream (form NAV input via RS232) (to verify with TAG if apply to Q261)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear
% to open the text file
filename=input ('enter file name use apostrophes>> ')
% fopen means open a file for reading
opc=fopen(filename);
scan=17;

% disp('Enter the scan rate')
% scan=input('>>>>')
% if scan <=0 
%     disp ('invalid scan rate, enter new scan rate')
%     scan=input('>>>>')
% end
% if scan >99
%     disp ('invalid scan rate, enter new scan rate')
%       scan=input('>>>>')
%  end
lightatt=[];timestamp=[];timestampold=0;elapstimeold=0;opcdate=[];deltat=[], opcdepth=[];opcpitch=[];opcroll=[];position=[];ctd=[];
date=[];day=[];time=[];month=[];year=[];hour=[];minute=[];second=[];ctddepth=[];ctdtime=[];temperature=[];salinity=[];density=[];speed=[];
latdeg=[];longdeg=[];latmin=[];longmin=[];LAT=[];LONG=[];latold=0;longold=0;north=[];linecount=0,date=[];
navtime=[];gpstime2=[];elapsed_dist=[];GPSLAT=[];GPSLON=[];flow_speed=[];

% to get the header and to
%%convert the header month into a number call the .m file headermonth
 headermonth;
 %%% to create the output file
 outputfile=input('enter the ouputfile name, use apostrophes > ')
 %fopen 'w' means open a new file for writing
 fp_out=fopen(outputfile,'w');
counter=0;
bugcount=0;lightcount=0;timecount=0;opcdepthcount=0;opcpitchcount=0;opcrollcount=0;
ctdcount=0;gpscount=0; daycount=0;monthcount=0;date1count=0;yearcount=0;
hourcount=0;temperaturecount=0;

bin1=0;bin2=0;bin3=0;bin4=0;bin5=0;bin6=0;bin7=0;bin8=0;bin9=0;bin10=0;
bin11=0;bin12=0;bin13=0;bin14=0;bin15=0;bin16=0;bin17=0;bin18=0;bin19=0;
bin20=0;bin21=0;bin22=0;bin23=0;bin24=0;bin25=0;bin26=0;bin27=0;bin28=0;bin29=0;
bin30=0;bin31=0;bin32=0;bin33=0;bin34=0;bin35=0;bin36=0;bin37=0;bin38=0;bin39=0;
bin40=0;bin41=0;bin42=0;bin43=0;bin44=0;bin45=0;bin46=0;bin47=0;bin48=0;bin49=0;
bin50=0;bin51=0;bin52=0;bin53=0;bin54=0;bin55=0;bin56=0;bin57=0;bin58=0;bin59=0;
bin60=0;bin61=0;bin62=0;bin63=0;bin64=0
  
% start of case loop. 
line=fgetl(opc);  % fgetl means get each line in the code, starting with the first and moving down
while line ~=-1 %%%-1 means end of file, ~= means is not equal to
linecount=linecount+1;
[flag,rest]=strtok(line,',');
    switch (flag);
        case ('1') 
            value=str2num(rest);   
             bugcount=bugcount+1;  % counter keeps going up every time this case is true, then the value is passed on to 'bugs' to be put in the correct bug bin. very nice coding 
             bugsize(bugcount)=value;
             %% to go to subroutine bugs***
             bugs
         case ('2')
             value=str2num(rest);
            lightcount=lightcount+1;
            lightatt(lightcount)=value;
        case ('3')
            value=str2num(rest);
            timecount=timecount+1;
            timestamp(timecount)=value;
            counter=counter+1;
            difference=timestamp(timecount)-timestampold;
            %% to verify that there is not jumps in timestamp; 
            %%other than the inital and the reset timestamp(0)
                if difference~=1 & timestampold==0;
                    difference=1;
                end
                if difference~=1 & timestampold==4095;
                       difference=1;
                 end 
                    if difference~= 1 & timestampold~=4095;
%                     disp ('timestamp error')
%                     disp ('at line')
%                     disp  ([linecount+1])
%                     keyboard
                end
                elapstime=elapstimeold+0.5;
elapstimeold=elapstime;
timestampold=timestamp(timecount);
 hyear2=str2num(hyear);
 hfracday=hdate+(hhour/24)+(hminute/1440)+(hsecond/86400);
 fracday=elapstime/86400;  % should be 0.5 because the time step is 0.5 seconds.  86400 is the number of seconds in a day
 later=hfracday+fracday;
 oday=floor(later);
 hourlater=(later-oday)*24;
 ohour=floor(hourlater);
minutelater=(hourlater-ohour)*60;
ominute=floor(minutelater);
secondlater=(minutelater-ominute)*60;
    opcdate= datestr(datenum([hyear2 hmonth oday ohour ominute secondlater]),'dd-mmm-yy HH:MM:SS');
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%The printing in the output file is at each flag 3 (timestamp) . All the flag 11 
%14 and 1 (CTD and NAV and bugs) are associates with the preceding flag 3;. Call of the m file printout (fprintsubroutine)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
          if timecount==2
    printout;
     % use the timestamp and header date (hdate) to find the elapsed time
 % (opcdate) and include that string in the file.
          end

        case ('5')
            value=str2num(rest);
            opcdepthcount=opcdepthcount+1;
            opcdepth=(((value/4095*5)*125)-127)*0.70307/1.027*-1;
        case ('9')
            value=str2num(rest);
             opcpitchcount=opcpitchcount+1;
             opcpitch=(value/4095*5*-12)+30;
        case('10')
            value=str2num(rest);
            opcrollcount=opcrollcount+1;
            opcroll=(value/4095*5*-12)+30;
        case('11')
            ctd=rest;
            ctdcount=ctdcount+1;
            % to extract each term of the CTD string and convert to number
               %call of the subroutine ctd%
               if ctdcount==1
               ctd;
               end
               
        case ('13') 
            fgps=rest;
            fgps;
        case('14')
            gpscount=gpscount+1;
            [latdeg,rest]=strtok(rest,' ');
            latdeg=str2num(latdeg);
            [latmin,rest]=strtok(rest,' ');
            [latmin]=strtok(latmin,'N');
            latmin=str2num(latmin);
            format long 
            LAT=latdeg + latmin/60;
            format short ;

            [longdeg,rest]=strtok(rest,' ');
            longdeg=str2num(longdeg);
            [longmin,rest]=strtok(rest,' ');
            [speed,navtime]=strtok(rest,',');
            speed=str2num(speed);
            navtime=strtok(navtime,',')
            
            [longmin]=strtok(longmin,'W');
            longmin=str2num(longmin);
            format long
            LONG=-(longdeg + longmin/60);
            format short;
                north=(LAT-latold);
       case('8')                      
        
        otherwise
%        disp ('flag error')
%          disp ('at line')
%           disp  ([linecount+1]);
%        keyboard
    end; %switch  
line=fgetl(opc);  % get next line in the code
end % while
fclose(fp_out);

 [outputfile]
disp ('has been created')



