function field=read_caer(filename)

count=1;

fid3=fopen(filename,'rt');

fgetl(fid3);
fgetl(fid3);
while(~feof(fid3)) 
   hold=fgetl(fid3);
   if(isequal(hold,''))
       break;
   end
   year    = sscanf(hold,'%i',1);
   month   = sscanf(hold,'%*i %i',1);
   day     = sscanf(hold,'%*i %*i %i',1);
%   hour    = sscanf(hold,'%*i %*i %*i %i',1)
   minute  = sscanf(hold,'%*i %*i %*i %i',1);
   hour=0.0;
   second=0.0;
%   temp1   = sscanf(hold,'%*i %*i %*i %*i %*i %*f',1)
%   year=str2double(hold(1:4));
%   month=str2double(hold(6:7));
%   day=str2double(hold(9:10));
%   hour=str2double(hold(12:13));
%   minute=str2double(hold(15:16));
%   second=str2double(hold(18:19));
     
   vector_time=[year month day hour minute second];
   field.value(count)=sscanf(hold,'%*i %*i %*i %*i %f',1);
   %field.value(count)
   %field.value(count)=temp1;
   
   %temp=datestr(vector_time')
   field.time(count)=datenum(vector_time);
   count=count+1;
end

fclose(fid3);
