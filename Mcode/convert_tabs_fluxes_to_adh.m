function convert_tabs_fluxes_to_adh(basename,number_strings)
%
%
%
%   subroutine to read tabs flux values and output in adh format
%
%   input parameters:
%
%   basename - basename of the input (assumes a .xls extension) 
%   and output files (_tflx will be added to the end of this basename)
%
%   number_strings - number of strings in the tabs formatted flux file
%
%   example usage:
%
%   convert_tabs_fluxes_to_adh('MTOG_RMA10_3300-4400',6)
%
%

%% reading tabs format

fid=fopen([basename '.xls'],'r');

discharges=zeros(5000, number_strings);
wse=zeros(5000, number_strings);
time=zeros(5000,1);
temp2=zeros(number_strings,1);
temp3=zeros(number_strings,1);
count=1;

while(~feof(fid))

    if(count==1)
      temp1=fscanf(fid,'%f',1);
    else
      if(feof(fid))
        break;
      end
      temp1=fscanf(fid,'%f',1);
    end
    if(feof(fid))
        break;
    end    
    time(count)=temp1;

    for i=1:number_strings
         temp2(i) = fscanf(fid,'%f',1);
         temp3(i) = fscanf(fid,'%f',1);
    end    
    discharges(count,1:number_strings)=temp2(:);
    wse(count,1:number_strings)=temp3(:);
    count=count+1;

end

data.time=time(1:count-1);
data.discharges=discharges(1:count-1, 1:number_strings);
data.wse=wse(1:count-1,1:number_strings);
fclose(fid);

clear fid time discharges i temp1 temp2 temp3 count wse

%% writing adh format

fid=fopen([basename '_tflx'],'w');
dt=data.time(2)-data.time(1);

fprintf(fid,'Time %.10e Flux-Dt %.10e\n',data.time(1)-3300,dt);
for j=1:length(data.discharges(1,:))
   fprintf(fid,'String %2i Flux %4.4e WSE %2.4e WSEMin %2.4e WSEMax %2.4e\n', ...
       j, data.discharges(1,j),data.wse(1,j),data.wse(1,j),data.wse(1,j));
end

for i=2:length(data.time)
    dt=data.time(i)-data.time(i-1);
    fprintf(fid,'Time %.10e Flux-Dt %.10e\n',data.time(i)-3300,dt);
    for j=1:length(data.discharges(i,:))
       fprintf(fid,'String %2i Flux %2.4e WSE %2.4e WSEMin %2.4e WSEMax %2.4e\n', ...
           j, data.discharges(i,j),data.wse(i,j),data.wse(i,j),data.wse(i,j));
    end    
end

fclose(fid);

clear dt data i j fid

