function field=read_yyyy_mm_dd_hh_mm_ss_value(filename)

count=1;

fid3=fopen(filename,'rt');

while(~feof(fid3)) 
   year=fscanf(fid3,'%4i',1);
   if(feof(fid3))
       break;
   end   
   month=fscanf(fid3,' %2i',1);
   day=fscanf(fid3,'%2i',1);
   hour=fscanf(fid3,'%2i',1);
   minute=fscanf(fid3,'%2i',1);
   second=fscanf(fid3,'%2i',1);
   if(feof(fid3))
       break;
   end
   hold=fscanf(fid3,'%f',1);
   field.value(count)=hold;
   vector_time=[year month day hour minute second];
   temp=datestr(vector_time);
   field.time(count)=datenum(temp);
   count=count+1;
end

fclose(fid3);
