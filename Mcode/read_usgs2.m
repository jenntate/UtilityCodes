function field=read_usgs2(filename)
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
loc=find(filename=='.',1,'last');
tempaaa=filename(1:loc-1);
%tempaaa=regexprep(tempaaa,'.','_');
if exist([tempaaa '.mat'],'file')
    disp('Using pre-read grid')
    load([tempaaa '.mat'])  
else
    fid1=fopen(filename,'rt');
    
    %% read the header information for the USGS data file
    
    compare='a';
    while(~isequal(compare,'DATE'))
        hold=fgetl(fid1);
        compare=hold(1:4);
    end
    
    fgetl(fid1);
    
    %% read each line of data from the usgs file
    
    count=1;
    
    while(~feof(fid1))
        
        % read a line of data
        
        fid=fgets(fid1);
        
        % determine if the end of the file has been reach and if so exits
        
        if(feof(fid1))
            break;
        end
        
        % reads the time information
        date=sscanf(fid,'%s',1);
        year=str2double(date(7:10));
        month=str2double(date(1:2));
        day=str2double(date(4:5));
        time=sscanf(fid,'%*s %s',1);
        hour=str2double(time(1:2));
        minute=str2double(time(4:5));
        second=str2double(time(7:8));
        
        % reads the measured value
        
        field.value(count)=sscanf(fid,'%*s %*s %*s %f',1);
        
        % converts the string time to numeric
        
        vector_time=[year month day hour minute second];
        temp=datestr(vector_time);
        field.time(count)=datenum(temp);
        count=count+1;
    end
    
    %% closes the USGS formatted file
    save([tempaaa '.mat'],'field')    
    fclose(fid1);
end
%toc