function field=read_rivergages(filename)
%
%       This function reads a usgs formated text file with measured data
%
%       INPUT VARIABLES
%
%       filename = file containing the usgs data
%
%       OUTPUT VARIABLES
%
%       field = structure that contains the measurement times and values
%
%       field.time = measurement times
%
%       field.value = measurement values at the given times
%
%       Finished 3/12/2010
%
%tic

%% open the USGS formatted data file
loc=find(filename=='.',1,'first');
tempaaa=filename(1:loc-1);
if exist([tempaaa '.mat'],'file')
    disp('Using pre-read grid')
    load(tempaaa)    
else

    fid=fopen(filename,'r');
    
    for i=1:19
        fgetl(fid);
    end
    
    count=1;
    while(~feof(fid))
        
        temp=fgetl(fid);
        if(strcmp(temp(1:2),'US'))
            fprintf(1,'END OF FILE REACHED\n');
            return;
        end
        temp1=sscanf(temp,'%s',1);
        loc1=find(temp1=='/',1,'first');
        if(isempty(loc1))
            return;
        end
        loc2=find(temp1=='/',1,'last');
        month=str2double(temp1(1:loc1-1));
        day=str2double(temp1(loc1+1:loc2-1));
        year=str2double(temp1(loc2+1:length(temp1)));
        clear temp1
        temp1=sscanf(temp,'%*s %s',1);
        loc1=find(temp1==':',1,'first');
        hour=str2double(temp1(1:loc1-1));
        second=str2double(temp1(loc1+1:length(temp1)));
        clear temp1
        temp1=sscanf(temp,'%*s %*s %s',1);
        if(~strcmp(temp1,'M'))
            temp2=regexprep(temp1,',','');
            value=str2double(temp2);
            vector_time=[year month day hour 0.0 second];
            field.time(count)=datenum(vector_time);
            field.value(count)=value;
            count=count+1;
        else
            fprintf(1,'BAD DATA AT %s\n',datestr(field.time(count-1)));
        end
        
    end
    
    save(tempaaa,'field')
    
    fclose(fid);
end
