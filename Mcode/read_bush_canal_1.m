function field=read_bush_canal_1(filename)




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
    fgetl(fid3);
    fgetl(fid3);
    second=0;
    
    while(~feof(fid3))
        hold=fgetl(fid3);
        if(isequal(hold,''))
            break;
        end
        date=sscanf(hold,'%s',1);
        set1 = find(date=='/',1,'first');
        set2 = find(date=='/',1,'last');
        month=str2double(date(1:set1-1));
        day=str2double(date(set1+1:set2-1));
        year=str2double(date(set2+1:length(date)));
        if(year < 100)
            year=year+2000;
        end
        time=sscanf(hold,'%*s %s',1);
        loc=find(time==':',1,'first');
        hour=str2double(time(1:loc-1));
        minute=str2double(time(loc+1:loc+2));
        
        vector_time=[year month day hour minute second];
        field.value(count)=sscanf(hold,'%*s %*s %f',1);
        temp=datestr(vector_time);
        field.time(count)=datenum(temp);
        count=count+1;
    end
    save(tempaaa,'field')   
    fclose(fid3);
end