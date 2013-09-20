function field=read_usgs1(filename)

fid1=fopen(filename,'rt');

compare='a';
while(~strcmp(compare,'DATE'))
    hold=fgetl(fid1);
    compare=hold(1:4);
end

fgetl(fid1);

count=1;

while(~feof(fid1))
   fid=fgets(fid1);  
   if(feof(fid1))
       break;
   end
   date=sscanf(fid,'%s',1);
   set1 = find(date=='/',1,'first');
   set2 = find(date=='/',1,'last');   
   month=str2double(date(1:set1-1));
   day=str2double(date(set1+1:set2-1));
   year=str2double(date(set2+1:length(date)));
   if(year < 100)
     year=year+2000;
   end
   time=sscanf(fid,'%*s %s',1);
   loc1=find(time==':',1,'first');
   loc2=find(time==':',1,'last');
   hour=str2double(time(1:loc1-1));
   minute=str2double(time(loc1+1:loc1+2));
   second=str2double(time(loc2+1:loc2+2));

   field.value(count)=sscanf(fid,'%*s %*s %*s %f',1);
   vector_time=[year month day hour minute second];
   temp=datestr(vector_time);
   field.time(count)=datenum(temp);
   count=count+1;
end

fclose(fid1);
