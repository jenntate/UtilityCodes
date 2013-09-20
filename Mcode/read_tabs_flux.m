function data=read_tabs_flux(basename, number_strings)
%
%
%
%   subroutine to read a Tabs formatted flux output from the code
%   tabs2gc.exe 
%
%   input parameters:
%
%   basename - basename of the file with the flux information (assumes a
%   .xls extention
%
%   number_strings - number of strings in the flux output file
%
%   example usage:
%
%   data=read_tabs_flux('MTOG_RMA10_3300-4400',6)
%
%

fid=fopen(basename,'r');

discharges=zeros(10000, number_strings);
time=zeros(10000,1);
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
