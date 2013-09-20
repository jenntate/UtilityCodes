function field=read_noaa_bouy(filename)


%% open the NOAA Bouy formatted data file

fid1=fopen(filename,'rt');

fgetl(fid1);
fgetl(fid1);

%% read each line of data from the file

count=1;

while(~feof(fid1))
    
   % read a line of data
    
   fid=fgets(fid1); 
   
   % determine if the end of the file has been reach and if so exits
   
%   if(feof(fid1))
%       break;
%   end
   
   % reads the time information
   
   year=str2double(fid(1:4));
   month=str2double(fid(6:7));
   day=str2double(fid(9:10));
   hour=str2double(fid(12:13));
   minute=str2double(fid(15:16));
%   second=str2double(fid(14:15));
   second=0;  
   % reads the measured value
   
   field.wd(count)=sscanf(fid,'%*s %*s %*s %*s %*s %f',1);
   field.ws(count)=sscanf(fid,'%*s %*s %*s %*s %*s %*s %f',1);
   % converts the string time to numeric
   
   vector_time=[year month day hour minute second];
   temp=datestr(vector_time);
   field.time(count)=datenum(temp);
   count=count+1;
end

%% closes the USGS formatted file

fclose(fid1);





