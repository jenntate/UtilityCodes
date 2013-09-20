function field=read_bush_canal_2(filename)




loc=find(filename=='.',1,'first');
tempaaa=filename(1:loc-1);
if exist([tempaaa '.mat'],'file')
    disp('Using pre-read grid')
    load(tempaaa)    
else
    count=1;
    
    fid3=fopen(filename,'rt');
    
    fgetl(fid3);
    fgetl(fid3);
    second = 0.0;
    while(~feof(fid3))
        hold=fgetl(fid3);
        if(isequal(hold,''))
            break;
        end
        %   year    = sscanf(hold,'%i',1)
        %   month   = sscanf(hold,'%*i %i',1)
        %   day     = sscanf(hold,'%*i %*i %i',1)
        %   hour    = sscanf(hold,'%*i %*i %*i %i',1)
        %   minute  = sscanf(hold,'%*i %*i %*i %*i %i',1)
        %   temp1   = sscanf(hold,'%*i %*i %*i %*i %*i %*f',1)
        year=str2double(hold(1:4));
        month=str2double(hold(6:7));
        day=str2double(hold(9:10));
        hour=str2double(hold(12:13));
        minute=str2double(hold(15:16));
        second=str2double(hold(18:19));
        
        vector_time=[year month day hour minute second];
        temp1=sscanf(hold,'%*20c %s',1);
        field.value(count)=str2double(temp1);
        
        temp=datestr(vector_time);
        field.time(count)=datenum(temp);
        count=count+1;
    end
    save(tempaaa,'field')    
    fclose(fid3);
end