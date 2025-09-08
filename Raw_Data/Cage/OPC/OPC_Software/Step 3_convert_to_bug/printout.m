%%% Printout in output file subroutine associated with OPC2.m

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% To ouptput the data in the outputfile  at each timestamp (OPC entry)           %% 
% some variables (CTD and GPS input do not have an output for each timestamp   %%
% hence need to create and empty space (nan) for each of the missing input     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fprintf(fp_out,'%u\t',modification);
fprintf(fp_out,'%u\t',counter);
%Header info%%
if isempty(hdate);
    fprintf(fp_out,'nan ');  
end
fprintf(fp_out,'%u\t',hdate);
if isempty(hmonth)
   fprintf(fp_out,'nan ');  
end 
fprintf(fp_out,'%u\t',hmonth);

if isempty(hyear);
    fprintf(fp_out,'nan ');  
end
fprintf(fp_out,'%s\t',hyear)
if isempty(htime);
    fprintf(fp_out,'nan ');  
end
fprintf(fp_out,'%s\t',htime)

% if isempty(hhour);
%     fprintf(fp_out,'nan ');  
% end
% fprintf(fp_out,'%u\t',hhour);
% 
% if isempty(hminute);
%     fprintf(fp_out,'nan ');  
% end
% fprintf(fp_out,'%u\t',hminute);
% 
% if isempty(hsecond);
%     fprintf(fp_out,'nan ');  
% end
%  fprintf(fp_out,'%2.2f\t',hsecond);
 
 %%%OPC entry info%%
 if isempty(timestamp(2));
    fprintf(fp_out,'nan ');  
end
fprintf(fp_out,'%u\t',timestamp(2));
if isempty(elapstime);
     fprintf(fp_out,'nan');
end
    fprintf(fp_out,'%3.1f\t',elapstime);
if isempty(opcdate);
     fprintf(fp_out,'nan');
end
    fprintf(fp_out,'%s\t',opcdate);       
if isempty(lightatt);
    fprintf(fp_out,'nan ');  
end
 fprintf(fp_out,'%u\t',lightatt);
if isempty(opcdepth);
    fprintf(fp_out,'nan ');  
end
fprintf(fp_out,'%3.2f\t',opcdepth);
if isempty(opcpitch);
    fprintf(fp_out,'nan ');  
end   
fprintf(fp_out,'%2.4f\t',opcpitch); 
if isempty(opcroll);
    fprintf(fp_out,'nan ');  
end
fprintf(fp_out,'%2.4f\t',opcroll); 
%%GPS info%%
if isempty(navtime);
    fprintf(fp_out,'nan ');  
end   
fprintf(fp_out,'%s\t',navtime);
if isempty(LAT);
    fprintf(fp_out,'nan ');  
end    
fprintf(fp_out,'%2.5f\t',LAT);

if isempty (LONG);
    fprintf(fp_out,'nan ');
end
fprintf(fp_out,'%2.5f\t',LONG);
if isempty(speed);
    fprintf(fp_out,'nan ');  
end   
fprintf(fp_out,'%2.3f\t',speed);
%% CTD info%%
 
%%  if isempty(day);
%     fprintf(fp_out,'nan ');  
% end
% fprintf(fp_out,'%u\t',day);
% 
% if isempty(month);
%     fprintf(fp_out,'nan ');  
% end
% fprintf(fp_out,'%u\t',month);
% 
% if isempty(year);
%     fprintf(fp_out,'nan ');  
%     
% end
% fprintf(fp_out,'%u\t',year);
%              
% if isempty(hour);
%     fprintf(fp_out,'nan ');
% end
% fprintf(fp_out,'%u\t',hour);
% 
% if isempty(minute);
%     fprintf(fp_out,'nan ');  
% end
% fprintf(fp_out,'%u\t',minute);
% 
% if isempty(second);
%     fprintf(fp_out,'nan ');  
% end
% fprintf(fp_out,'%2.2f\t',second);

 if isempty(ctdtime);
    fprintf(fp_out,'nan');
end
fprintf(fp_out,'%s\t',ctdtime);

if isempty(ctddepth);
    fprintf(fp_out,'nan ');
end
fprintf(fp_out,'%3.2f\t',ctddepth);

if isempty(temperature);
    fprintf(fp_out,'nan ');  
end
 fprintf(fp_out,'%2.3f\t',temperature);
 
 if isempty(salinity);
    fprintf(fp_out,'nan ');  
end
fprintf(fp_out,'%2.3f\t',salinity);

if isempty(density);
    fprintf(fp_out,'nan ');  
end
fprintf(fp_out,'%2.3f\t',density);

%% GPS/FLOWMETER info%%
 if isempty(gpstime2);
    fprintf(fp_out,'nan');
end
fprintf(fp_out,'%s\t',gpstime2);

if isempty(elapsed_dist);
    fprintf(fp_out,'nan ');  
end
 fprintf(fp_out,'%2.3f\t',elapsed_dist);
 
 if isempty(flow_speed);
    fprintf(fp_out,'nan ');  
end
 fprintf(fp_out,'%2.3f\t',flow_speed);
 
  if isempty(GPSLAT);
    fprintf(fp_out,'nan ');  
end
 fprintf(fp_out,'%2.3f\t',GPSLAT);

   if isempty(GPSLON);
    fprintf(fp_out,'nan ');  
end
 fprintf(fp_out,'%2.3f\t',GPSLON);
 
 
if counter==1
fprintf(fp_out,'nan ');    fprintf(fp_out,'nan ');   fprintf(fp_out,'nan ');%3
fprintf(fp_out,'nan ');    fprintf(fp_out,'nan ');   fprintf(fp_out,'nan ');%6
fprintf(fp_out,'nan ');    fprintf(fp_out,'nan ');   fprintf(fp_out,'nan ');%9
fprintf(fp_out,'nan ');    fprintf(fp_out,'nan ');   fprintf(fp_out,'nan ');%12
fprintf(fp_out,'nan ');    fprintf(fp_out,'nan ');   fprintf(fp_out,'nan ');%15
fprintf(fp_out,'nan ');    fprintf(fp_out,'nan ');   fprintf(fp_out,'nan ');%18
fprintf(fp_out,'nan ');    fprintf(fp_out,'nan ');   fprintf(fp_out,'nan ');%21
fprintf(fp_out,'nan ');    fprintf(fp_out,'nan ');   fprintf(fp_out,'nan ');%24
fprintf(fp_out,'nan ');    fprintf(fp_out,'nan ');   fprintf(fp_out,'nan ');%27
fprintf(fp_out,'nan ');    fprintf(fp_out,'nan ');   fprintf(fp_out,'nan ');%30
fprintf(fp_out,'nan ');    fprintf(fp_out,'nan ');   fprintf(fp_out,'nan ');%33
fprintf(fp_out,'nan ');    fprintf(fp_out,'nan ');   fprintf(fp_out,'nan ');%36
fprintf(fp_out,'nan ');    fprintf(fp_out,'nan ');   fprintf(fp_out,'nan ');%39
fprintf(fp_out,'nan ');    fprintf(fp_out,'nan ');   fprintf(fp_out,'nan ');%42
fprintf(fp_out,'nan ');    fprintf(fp_out,'nan ');   fprintf(fp_out,'nan ');%45
fprintf(fp_out,'nan ');    fprintf(fp_out,'nan ');   fprintf(fp_out,'nan ');%48
fprintf(fp_out,'nan ');    fprintf(fp_out,'nan ');   fprintf(fp_out,'nan ');%51
fprintf(fp_out,'nan ');    fprintf(fp_out,'nan ');   fprintf(fp_out,'nan ');%54
fprintf(fp_out,'nan ');    fprintf(fp_out,'nan ');   fprintf(fp_out,'nan ');%57
fprintf(fp_out,'nan ');    fprintf(fp_out,'nan ');   fprintf(fp_out,'nan ');%60
fprintf(fp_out,'nan ');    fprintf(fp_out,'nan ');   fprintf(fp_out,'nan ');%63
fprintf(fp_out,'nan ');    %64
end

if counter > 1;
fprintf(fp_out,'%u\t',bin1);fprintf(fp_out,'%u\t',bin2);fprintf(fp_out,'%u\t',bin3);
fprintf(fp_out,'%u\t',bin4);fprintf(fp_out,'%u\t',bin5);fprintf(fp_out,'%u\t',bin6);
fprintf(fp_out,'%u\t',bin7);fprintf(fp_out,'%u\t',bin8);fprintf(fp_out,'%u\t',bin9);
fprintf(fp_out,'%u\t',bin10);fprintf(fp_out,'%u\t',bin11);fprintf(fp_out,'%u\t',bin12);
fprintf(fp_out,'%u\t',bin13);fprintf(fp_out,'%u\t',bin14);fprintf(fp_out,'%u\t',bin15);
fprintf(fp_out,'%u\t',bin16);fprintf(fp_out,'%u\t',bin17);fprintf(fp_out,'%u\t',bin18);
fprintf(fp_out,'%u\t',bin19);fprintf(fp_out,'%u\t',bin20);fprintf(fp_out,'%u\t',bin21);
fprintf(fp_out,'%u\t',bin22);fprintf(fp_out,'%u\t',bin23);fprintf(fp_out,'%u\t',bin24);
fprintf(fp_out,'%u\t',bin25);fprintf(fp_out,'%u\t',bin26);fprintf(fp_out,'%u\t',bin27);
fprintf(fp_out,'%u\t',bin28);fprintf(fp_out,'%u\t',bin29);fprintf(fp_out,'%u\t',bin30);
fprintf(fp_out,'%u\t',bin31);fprintf(fp_out,'%u\t',bin32);fprintf(fp_out,'%u\t',bin33);
fprintf(fp_out,'%u\t',bin34);fprintf(fp_out,'%u\t',bin35);fprintf(fp_out,'%u\t',bin36);
fprintf(fp_out,'%u\t',bin37);fprintf(fp_out,'%u\t',bin38);fprintf(fp_out,'%u\t',bin39);
fprintf(fp_out,'%u\t',bin40);fprintf(fp_out,'%u\t',bin41);fprintf(fp_out,'%u\t',bin42);
fprintf(fp_out,'%u\t',bin43);fprintf(fp_out,'%u\t',bin44);fprintf(fp_out,'%u\t',bin45);
fprintf(fp_out,'%u\t',bin46);fprintf(fp_out,'%u\t',bin47);fprintf(fp_out,'%u\t',bin48);
fprintf(fp_out,'%u\t',bin49);fprintf(fp_out,'%u\t',bin50);fprintf(fp_out,'%u\t',bin51);
fprintf(fp_out,'%u\t',bin52);fprintf(fp_out,'%u\t',bin53);fprintf(fp_out,'%u\t',bin54);
fprintf(fp_out,'%u\t',bin55);fprintf(fp_out,'%u\t',bin56);fprintf(fp_out,'%u\t',bin57);
fprintf(fp_out,'%u\t',bin58);fprintf(fp_out,'%u\t',bin59);fprintf(fp_out,'%u\t',bin60);
fprintf(fp_out,'%u\t',bin61);fprintf(fp_out,'%u\t',bin62);fprintf(fp_out,'%u\t',bin63);
fprintf(fp_out,'%u\t',bin64);
end
fprintf(fp_out,'\n')%%% important line...returns to a new line
                 
%To set the counts and the values to zero after each new line is printed
%% it is  needed to be removed in the case where the matrix would be created in malab
%% without being ouputed into a ascii file


%timestamp=[];
timecount=0;
lightatt=[];lightcount=0;
opcdepth=[];opcdepthcount=0; 
opcpitch=[];opcpitchcount=0;
opcroll=[];opcrollcount=0;
opcdate=[];

% if ctdcount >1;
%     disp ('more than 2 ctd entry for one line')
%     keyboard
% end
day=[];month=[];year=[];hour=[];minute=[];second=[];ctddepth=[];temperature=[];salinity=[];density=[];speed=[];elapstime=[];
ctdcount=0;ctdtime=[];date=[];navtime=[];


% if gpscount >1;
%     disp ('more than 2 positions for a line')
%     keyboard
% end
latold=LAT;
longold=LONG;
latdeg=[];longdeg=[];latmin=[];longmin=[];
gpscount=0;
LAT=[];LONG=[];
 gpstime2=[]; elapsed_dist=[];flow_speed=[];GPSLAT=[];GPSLON=[];
 
bin1=0;bin2=0;bin3=0;bin4=0;bin5=0;bin6=0;bin7=0;bin8=0;bin9=0;bin10=0;
bin11=0;bin12=0;bin13=0;bin14=0;bin15=0;bin16=0;bin17=0;bin18=0;bin19=0;
bin20=0;bin21=0;bin22=0;bin23=0;bin24=0;bin25=0;bin26=0;bin27=0;bin28=0;bin29=0;
bin30=0;bin31=0;bin32=0;bin33=0;bin34=0;bin35=0;bin36=0;bin37=0;bin38=0;bin39=0;
bin40=0;bin41=0;bin42=0;bin43=0;bin44=0;bin45=0;bin46=0;bin47=0;bin48=0;bin49=0;
bin50=0;bin51=0;bin52=0;bin53=0;bin54=0;bin55=0;bin56=0;bin57=0;bin58=0;bin59=0;
bin60=0;bin61=0;bin62=0;bin63=0;bin64=0;