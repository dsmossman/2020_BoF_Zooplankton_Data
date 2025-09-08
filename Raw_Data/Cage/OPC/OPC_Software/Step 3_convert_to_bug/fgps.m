
  [toss,fgps]=strtok(fgps,'"');
   [gpstime2,fgps]=strtok(fgps,'"'); % EXPORT
   [toss,fgps]=strtok(fgps,'"');
   [toss,fgps]=strtok(fgps,',');
   [elapsed_dist,fgps]=strtok(fgps,',');
   elapsed_dist=(str2num(elapsed_dist))./100 ; % EXPORT
   [flow_speed,fgps]=strtok(fgps,',');
   flow_speed=(str2num(flow_speed))./100 ; % EXPORT
   [glatdeg,fgps]=strtok(fgps,'"N''°');
    [glatdeg,fgps]=strtok(fgps,'"N''°');
   glatdeg2=str2num(glatdeg);
   [glatmin,fgps]=strtok(fgps,'°''"');
   glatmin2=str2num(glatmin);
   [toss,fgps]=strtok(fgps,' ');
   [glondeg,fgps]=strtok(fgps,'°');
   glondeg2=str2num(glondeg);
 [fgps,toss]=strtok(fgps,'°');
   [glonmin]=strtok(fgps,'"');
   glonmin2=str2num(glonmin);
  GPSLAT=glatdeg2+(glatmin2./60);  % EXPORT
   GPSLON=(glondeg2+(glonmin2./60))*-1;  % EXPORT
            