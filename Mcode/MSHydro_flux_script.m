function whiteditch_flux_script(basename)
% clear

 basename='July8-diversions-2008'
t0='01/01/2008 00:00:00';
units =1;

%% Fluxstrings contain the X and Y coordinate of start and end node
%   Syntax: fluxstring{i}=[xcoord1, ycoord1, xcoord2, ycoord2];
%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Bohemia US
fluxstring{1}=[815730.02268539, 3270728.7472511, 815972.95272729, 3271467.5738518];
string_name{1}='Bohemia U/S';

%% Bohemial DS (below Fort St. Philip)
fluxstring{2}=[851722.0, 3247721.0, 852346.0, 3248361.0];
string_name{2}='Bohemia D/S';

%% Baptiste Calette
fluxstring{3}=[854411.5, 3245770.0, 854733.6, 3245465.9];
string_name{3}='Baptiste Collette';

%% Grand Pass
fluxstring{4}=[854282.2, 3242810.2, 854966.7, 3242606.7];
string_name{4}='Grand Pass';

%% West Bay
fluxstring{5}=[860506.4  3237223.9, 860578.7, 3237031.9];
string_name{5}='West Bay';

%% Cubit's Gap
fluxstring{6}=[862935.7, 3236114.5, 863596.8, 3234828.4];
string_name{6}='Cubits Gap';

%% SW Pass
fluxstring{7}=[863916.8, 3229458.6, 864624.7, 3229229.3];
string_name{7}='SW Pass';

%% South Pass
fluxstring{8}=[864881.6, 3229274.1, 865308.8, 3229475.8];
string_name{8}='South Pass';

%% Pass a Loutre
fluxstring{9}=[865230.6, 3229948.8, 865742.1, 3231242.3];
string_name{9}='Pass a Loutre';

%% Baton Rouge
fluxstring{10}=[672371.366, 3367295.78,673483.504, 3366948.22];
string_name{10}='Baton Rouge'
%%
% determines the time series discharges
data=calculate_fluxes_kcp(basename,fluxstring);

%% writes the data out as an adh formatted _tflx file 
write_adh_formatted_fluxes(basename,data,t0)

%% plots the data as a timeseries and saves
data1=read_tflx_discharges(basename,t0,units);

fid=fopen([basename '_tflx_ts'],'w')
%n=length(string_name)
%t=length(data1.fluxes)
% fprintf(fid,'Time Discharge n%s\n',string_name{i},i=1:n);
%for i=1:t
%  csvwrite(fid,data1.time(i),data1.fluxes(i,:));
%end

formatSpec='%20f';
fprintf(fid,'%20s','Time');
for i=1:length(data1.fluxes(1,:))
    fprintf(fid,'%20s',string_name{i});
    formatSpec=[formatSpec ' %20f'];
end
fprintf(fid,'\n');
formatSpec=[formatSpec '\n'];

temp=data1.time;
for i=1:length(data1.fluxes(1,:))
    temp(i+1,:)=data1.fluxes(:,i);
end

fprintf(fid,formatSpec,temp);


createfigure_Discharge(data1.time,data1.fluxes,basename, string_name);
    save_name=[basename '_discharge'];
    print('-r600','-cmyk','-dtiff',save_name);  
    saveas(gcf,[basename '_discharge'],'fig'); 
    close(gcf);
