function field=read_usgs(filename)
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
    
    compare='z';
    while(~isequal(compare,'a'));
        hold=fgetl(fid1);
        compare=hold(1:1);
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
        
        year=str2double(fid(16:19));
        month=str2double(fid(21:22));
        day=str2double(fid(24:25));
        hour=str2double(fid(27:28));
        minute=str2double(fid(30:31));
        second=0.0;
        
        % reads the measured value and converts to unsigned magnitude
        
        field.speed(count)=sscanf(fid,'%*s %*s %*s %*s %*s %f',1);
        field.speed(count)=abs(field.speed(count));
        
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