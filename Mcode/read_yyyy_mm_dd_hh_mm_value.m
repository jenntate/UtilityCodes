function field=read_yyyy_mm_dd_hh_mm_value(filename)


loc=find(filename=='.',1,'first');
tempaaa=filename(1:loc-1);
if exist([tempaaa '.mat'],'file')
    disp('Using pre-read data')
    load(tempaaa)    
else

    count=1;
    
    fid3=fopen(filename,'rt');
    second = 0.0;
    fgetl(fid3);
    fgetl(fid3);
    while(~feof(fid3))
        hold=fgetl(fid3);
        year   = sscanf(hold,'%i',1);
        if(feof(fid3))
            break;
        end
        month  = sscanf(hold,'%*i %i',1);
        day    = sscanf(hold,'%*i %*i %i',1);
        hour   = sscanf(hold,'%*i %*i %*i %i',1);
        minute = sscanf(hold,'%*i %*i %*i %*i %i',1);
        %   second=fscanf(fid3,'%2i',1);
        if(feof(fid3))
            break;
        end
        hold=sscanf(hold,'%*i %*i %*i %*i %*i %f',1);
        field.value(count)=hold;
        vector_time=[year month day hour minute second];
        temp=datestr(vector_time);
        field.time(count)=datenum(temp);
        count=count+1;
    end
    save(tempaaa,'field')    
    fclose(fid3);
end