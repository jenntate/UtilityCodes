function field=read_data_dcerp(filename)



loc=find(filename=='.',1,'first');
tempaaa=filename(1:loc-1);
if exist([tempaaa '.mat'],'file')
    disp('Using pre-read grid')
    load(tempaaa)    
else

    count=1;
    
    fid3=fopen(filename,'rt');
    
    second=0;
    fgetl(fid3); %read header line
    
    while(~feof(fid3))
        hold=fgetl(fid3);
        if(isequal(hold,''))
            break;
        end
        date=sscanf(hold,'%s',1);
        set1 = find(date=='/',1,'first');
        if(set1)
            set2 = find(date=='/',1,'last');
            month=str2double(date(1:set1-1));
            if(~isnan(month))
                day=str2double(date(set1+1:set2-1));
                year=str2double(date(set2+1:length(date)));
                time=sscanf(hold,'%*s %s',1);
                loc=find(time==':');
                if(length(loc)==1)
                    hour=str2double(time(1:loc-1));
                    minute=str2double(time(loc+1:loc+2));
                elseif(length(loc)==2)
                    hour=str2double(time(1:loc(1)-1));
                    minute=str2double(time(loc(1)+1:loc(1)+2));
                    second=str2double(time(loc(2)+1:length(time)));
                end
                vector_time=[year month day hour minute second];
                temp=sscanf(hold,' %s%c ');


                temp=regexprep(temp,'\t','_');
                loc11=find(temp=='_',1,'last');
                temp(loc11:length(temp));
                temp1=str2double(temp(loc11+1:length(temp)));

                if(temp1<5.0)
                  field.value(count)=temp1;
%                  field.value(count)=sscanf(hold,'%*s %*s %*s %*s %*s %f',1);
                  temp=datestr(vector_time);
                  field.time(count)=datenum(temp);
                  count=count+1;
                end
            end
            clear set
        end
    end
    save(tempaaa,'field')    
    fclose(fid3);
end