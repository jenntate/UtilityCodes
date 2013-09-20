function field=read_davispond(filename)
%
%   Function to read the raw data file 
%
%   INPUT VARIABLES
%
%   filename = filename containing the raw data
%
%   OUTPUT VARIABLES
%
%   field = structure containing the field data time and measurements
%
%   field.time = field data time series values
%
%   field.values = field data measurement values
%
%   Finished 3/12/2010
%

%% opens the raw data file and reads the first two lines

fid3=fopen(filename,'rt');

fgetl(fid3);
fgetl(fid3);
second=0;

%% reads each the raw data file

count=1;
while(~feof(fid3)) 
    
   % reads each line of the raw data file
    
   hold=fgetl(fid3);
   
   % determines if the read has reached the end of the solutino file
   
   if(isequal(hold,''))
       break;
   end
   
   % reads the first string of the line
   
   date=sscanf(hold,'%s',1);
   
   % determines the locations of "/" and the length of the string
   
   set1 = find(date=='/',1,'first');
   set2 = find(date=='/',1,'last');  
   set3 = length(date);
   
   % reads the month value
   
   month=str2double(date(1:set1-1));
   
   % reads the day value
   
   day=str2double(date(set1+1:set2-1));
   
   % reads the year value
   
   year=str2double(date(set2+1:set3));

   % if the year is specified using two values add 2000
   
   if(year < 100)
     year=year+2000;
   end
   
   % read the string containing the time
   
   time=sscanf(hold,'%*s %s',1);
   
   % locate the locations for ":"
   
   loc1=find(time==':',1,'first');
   loc2=find(time==':',1,'last');
   
   % if statement to determine if the seconds are specified
   
   if(loc1 == loc2)
       
     % read the hour value
       
     hour=str2double(time(1:loc1-1));
     
     % read the minute value
     
     minute=str2double(time(loc1+1:(length(time))));
     
   else
     
     % read the hour value
     
     hour=str2double(time(1:loc1-1));
     
     % read the minute value
     
     minute=str2double(time(loc1+1:loc2-1));
     
     % read the seconds value
     
     second=str2double(time(loc2+1:(length(time))));
     
   end
   
   % convert the time to a vector
   
   vector_time=[year month day hour minute second];
   
   % read the measurement value
   
   field.value(count)=sscanf(hold,'%*s %*s %f',1);

   % convert the date vector to a string
   
   temp=datestr(vector_time);
   
   % convert the date string to a number
   
   field.time(count)=datenum(temp);
   count=count+1;
end

%% close the raw data file

fclose(fid3);
