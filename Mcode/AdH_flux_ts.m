function AdH_flux_script(basename)
% clear

basename='Sept12-2012bc'
t0='01/01/2012 00:00:00';
units =1;
flag = 1; %1 for hydro, 2 for hydro and con


%% plots the data as a timeseries and saves
data1=read_tflx_discharges(basename,t0,units);
if (flag > 1)
  data2=read_conflux(basename,t0,units);
end
  
  
fid=fopen([basename '_tflx_ts'],'w')
if(flag>1)
  fid2=fopen([basename '_conflx_ts'],'w')
end
  
  %n=length(string_name)
%t=length(data1.fluxes)
% fprintf(fid,'Time Discharge n%s\n',string_name{i},i=1:n);
%for i=1:t
%  csvwrite(fid,data1.time(i),data1.fluxes(i,:));
%end

location{1}='Bohemia U/S';
location{2}='Bohemia D/S';
location{3}='Baptiste Collette';
location{4}='Grand Pass';
location{5}='West Bay';
location{6}='Cubits Gap';
location{7}='Southwest Pass';
location{8}='South Pass';
location{9}='Pass A Loutre';
location{10}='Tarbert 1';
location{11}='Tarbert 2';
location{12}='Tarbert 3';
location{13}='Baton Rouge 1';
location{14}='Baton Rouge 2';
location{15}='Baton Rouge 3';
location{16}='Convent 1';
location{17}='Convent 2';
location{18}='Convent 3';
location{19}='Caernarvon 1';
location{20}='Caernarvon 2';
location{21}='Caernarvon 3';
location{22}='Caernarvon 4';
location{23}='Fort Jackson 1';
location{24}='Fort Jackson 2';
location{25}='Fort Jackson 4';

%Hydro Flux
formatSpec='%20f';
fprintf(fid,'%20s','Time');
for i=1:length(data1.fluxes(1,:))
    fprintf(fid,'%20s',location{i});
    formatSpec=[formatSpec ' %20f'];
end
fprintf(fid,'\n');
formatSpec=[formatSpec '\n'];

temp=data1.time;
for i=1:length(data1.fluxes(1,:))
    temp(i+1,:)=data1.fluxes(:,i);
end

fprintf(fid,formatSpec,temp);


%Constituent Flux
if (flag > 1)
    fprintf(fid2,'%20s','Time');
    for i=1:length(data2.fluxes(1,:))
        fprintf(fid2,'%20s',location{i});
        formatSpec=[formatSpec ' %20f'];
    end
    fprintf(fid2,'\n');
    formatSpec=[formatSpec '\n'];
    
    temp2=data2.time;
    for i=1:length(data2.fluxes(1,:))
        temp2(i+1,:)=data2.tot_flux(:,i);
    end
    
    fprintf(fid2,formatSpec,temp2);
end
