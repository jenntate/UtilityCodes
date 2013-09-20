%% Script to Processes AdH Sediment and Hydro Output
% (Created for Conowingo AdH Post Processing.  Verfify generic file format) 
% 
%
% Created by Ian Floyd
% 9-Sept-13
% USACE ERDC CHL
%% Create and open input dialog boxes 
% Prompt for increment
prompt={'Enter Time Step Increment (s)'};
name='Input for Conflux Time Output';
numlines=1;
defaultanswer={'86400'};
tempin=inputdlg(prompt,name,numlines,defaultanswer);
increment=char(tempin);
clear name defaultanswer prompt tempin 

%% Read adh conflux file
% Open file
[fn,pn]=uigetfile('*.*','Select AdH Conflux File');
filename=fullfile(pn,fn);
fid=fopen(filename,'rt');
% Read and process header Info
s0=fgetl(fid);
flds=regexp(s0,'\w*','match');
% Read numeric data
data=textscan(fid,'%f %f %f %f %f %f %f %f');
% Convert data and headernames from cell arrays to structure
s=cell2struct(data,flds,2);
numstr=unique(s.STRING);
numgs=unique(s.ITRNS);
clear fn pn fid filename s0 flds data

%% Parse out Adh Water and Sediment Conflux Data by Strings and Grain size
for k=1:length(numstr)
    in=s.STRING==numstr(k);
    for kk=1:length(numgs);
        in1=s.ITRNS==kk;
        time=s.TIME(in&in1);
        tsedflx(:,kk,k)=cumsum(abs(s.TOTALSEDFLUX(in&in1)),1);
        ssedflx(:,kk,k)=cumsum(abs(s.SUSCONFLX(in&in1)),1);
        bsedflx(:,kk,k)=cumsum(abs(s.BEDLOADFLUX(in&in1)),1);
        waterflx(:,k)=abs(s.WATERFLUX(in&in1));
    end
end
clear k kk in in1

%% Convert Conflux to Conmass 
tempTime=[0;time];
dt=diff(tempTime);
 
for k=1:length(numstr)
    for kk=1:length(numgs)
        susmass(:,kk,k)=dt.*ssedflx(:,kk,k);
        bedmass(:,kk,k)=dt.*bsedflx(:,kk,k);
        totmass(:,kk,k)=dt.*tsedflx(:,kk,k);
    end
end
clear k kk

%% Interpolate Data to Desired Time Interval
start=time(1,1);
stop=max(time);
itime=start:str2double(increment):stop;
itime=itime';

% Interpolated Cummulative Sediment in kg
isusmass_kg=interp1(time,susmass,itime,'spline');
ibedmass_kg=interp1(time,bedmass,itime,'spline');
itotmass_kg=interp1(time,totmass,itime,'spline');
iwater=interp1(time,waterflx,itime,'spline'); % cms
gs_total_kg=sum(itotmass_kg,2);
total_kg=horzcat(itotmass_kg,gs_total_kg);
Qw_cfs=iwater.*35.31;
%% Convert from cummulative mass (kg) to kg/day
isusmass_kg_d=diff(isusmass_kg);
ibedmass_kg_d=diff(ibedmass_kg);
itotmass_kg_d=diff(itotmass_kg);
gs_total_kg_d=sum(itotmass_kg_d,2);
temp_kg_d=horzcat(itotmass_kg_d,gs_total_kg_d);

%% Convert from kg to tons (cummulative)
isusmass_tons=isusmass_kg.*0.0011;
ibedmass_tons=ibedmass_kg.*0.0011;
itotmass_tons=itotmass_kg.*0.0011;
gs_total_tons=sum(itotmass_tons,2);
total_tons=horzcat(itotmass_tons,gs_total_tons);

% Convert from cummulative mass(tons) to tons/day
isusmass_tons_d=diff(isusmass_tons);
ibedmass_tons_d=diff(ibedmass_tons);
itotmass_tons_d=diff(itotmass_tons);
gs_total_tons_d=sum(itotmass_tons_d,2);
temp_tons_d=horzcat(itotmass_tons_d,gs_total_tons_d);

%% Created cell array of string and grain size names (String2, String3, Grain size1, etc.)
% String arrays
for k=1:length(numstr)
    strflds(k,:)=cellstr(['String',num2str(numstr(k,:)),':']);
end
% Grain size array
for kk=1:length(numgs)
    gsflds(kk,:)=cellstr(['GrainSize',num2str(numgs(kk,:)),':']);
end
gsflds=[gsflds;'Total:']';
clear k kk

% %% Add Row of Zero Values to tons/day and kg/day
% zrow=zeros(size(gsflds));
% 
% for i=1:length(numstr)
%     total_tons_d(:,:,i)=[zrow;temp_tons_d(:,:,i)];
%     total_kg_d(:,:,i)=[zrow;temp_kg_d(:,:,i)];
% end
% 
% %% Calculate Concentration (mg/L)
% temp=0.0027.*Qw_cfs;
% for j=1:length(numstr)
%     for jj=1:length(numgs)+1
%         conc_mgL(:,jj,j)=total_tons_d(:,jj,j)./temp(:,j);
%     end
% end
% clear j jj
%% Create Unit Extentions
unit1=regexprep(gsflds,':','(cum_tons)');
unit2=regexprep(gsflds,':','(cum_kg)');
unit3=regexprep(gsflds,':','(tons/day)');
unit4=regexprep(gsflds,':','(kg/day)');
unit5=regexprep(gsflds,':','(mg/L)');
unit6=regexprep(strflds,':','(kg)');
unit7=regexprep(strflds,':','(tons)');
unit8=regexprep(strflds,':','(mg_L)');
unit9=regexprep(strflds,':','(cms)')';
unit10=regexprep(strflds,':','(cfs)')';

%% Export Data to Excel File
% Prompt GUI to enter excel file name
prompt={'Enter Desired Excel Filename'};
name='Enter Filename';
xlsfn=inputdlg(prompt,name,numlines);
xlsfn=char(xlsfn);
timehdr={'Time'};
Qwcms='Discharge(cms)';
Qwcfs='Discharge(cfs)';

% Output data in kg
for k=1:length(numstr)
    xlswrite(xlsfn,timehdr,unit6{k},'A1');
    xlswrite(xlsfn,itime,unit6{k},'A2');
    xlswrite(xlsfn,unit4,unit6{k},'B1');
    xlswrite(xlsfn,total_kg_d(:,:,k),unit6{k},'B2');
    xlswrite(xlsfn,unit2,unit6{k},'P1');
    xlswrite(xlsfn,total_kg(:,:,k),unit6{k},'P2');
end

% Output data in tons
for kk=1:length(numstr)
    xlswrite(xlsfn,timehdr,unit7{kk},'A1');
    xlswrite(xlsfn,itime,unit7{kk},'A2');
    xlswrite(xlsfn,unit3,unit7{kk},'B1');
    xlswrite(xlsfn,total_tons_d(:,:,kk),unit7{kk},'B2');
    xlswrite(xlsfn,unit1,unit7{kk},'P1');
    xlswrite(xlsfn,total_tons(:,:,kk),unit7{kk},'P2');
end

% Output data in mg/L
for i=1:length(numstr)
    xlswrite(xlsfn,timehdr,unit8{i},'A1');
    xlswrite(xlsfn,itime,unit8{i},'A2');
    xlswrite(xlsfn,unit5,unit8{i},'B1');
    xlswrite(xlsfn,conc_mgL(:,:,i),unit8{i},'B2');
end

% Output Water Discharge in cms and cfs
xlswrite(xlsfn,unit9,Qwcms,'A1');
xlswrite(xlsfn,iwater,Qwcms,'A2');
xlswrite(xlsfn,unit10,Qwcfs,'A1');
xlswrite(xlsfn,iwater.*35.31,Qwcfs,'A2');



