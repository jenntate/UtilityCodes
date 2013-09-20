function field=read_date_time_value(filename)

count=1;

fid3=fopen(filename,'rt');

fgetl(fid3);
fgetl(fid3);

while(~feof(fid3)) 
   hold=fgetl(fid3);
   if(isequal(hold,''))
       break;
   end
   date=sscanf(hold,'%s',1);
   month=sscanf(date,'%i',1);
   day=sscanf(date,'%*i %*1s %i',1);
   year=sscanf(date,'%*i %*1s %*i %*1s %i',1);
   
   time=sscanf(hold,'%*s %s',1);
   loc=find(time==':');
   if(loc(1)==3)
       hour=str2num(time(1:2));
       minute=str2num(time(4:5));
       second=str2num(time(7:8));
   else
       hour=str2num(time(1));
       minute=str2num(time(3:4));
       second=str2num(time(6:7));
   end
   vector_time=[year month day hour minute second];
   field.value(count)=sscanf(hold,'%*s %*s %f',1);
   temp=datestr(vector_time);
   field.time(count)=datenum(temp);
   count=count+1;
end

fclose(fid3);
