function field=read_usgs_write1(filename)

%fid1=fopen(filename,'rt');
%fid2=fopen('output.out','wb','n');

%for i=1:20
%    line{i}=fgetl(fid1);
%end

count=1;

%while(~feof(fid1))
%   fid=fgets(fid1);  
%   if(feof(fid1))
%       break;
%   end
%   hold1=sscanf(fid,'%s',1);
%   hold2=sscanf(fid,'%*s %s',1);
%   hold4=sscanf(fid,'%*s %*s %*s %f',1);
%   fprintf(fid2,'%s %s %f\n',hold1, hold2, hold4);
%   field.value=1;
%   field.value(count)=sscanf(fid,'%*s %*s %*s %f',hold1,hold2,hold4);
%end

%fclose(fid2);
%fclose(fid1);

fid3=fopen('out_excel.prn','rt');

while(~feof(fid3))
%   fid=fgets(fid3);  

   year=fscanf(fid3,'%4i',1);
   if(feof(fid3))
       break;
   end   
   month=fscanf(fid3,' %2i',1);
   day=fscanf(fid3,'%2i',1);
   hour=fscanf(fid3,'%2i',1);
   minute=fscanf(fid3,'%2i',1);
   second=fscanf(fid3,'%2i',1);
   field.value(count)=fscanf(fid3,'%f',1);
   vector_time=[year month day hour minute second];
   temp=datestr(vector_time);
   field.time(count)=datenum(temp);
   count=count+1;
%   if(count > 50)
%       break;
%   end
end

fclose(fid3);
