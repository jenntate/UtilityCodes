function field=read_noaa(filename)
%
%       This subroutine reads in the field data from a NOAA formatted text
%       file into the structure field.
%
%       INPUT VARIABLE
%
%       filename = name of the NOAA formatted text file with the gage data
%
%       OUTPUT VARIABLE
%
%       field = structure that contains the measurement times and the
%       predicted and measured values for the given gage
%
%       field.time = times for gage measurements
%
%       field.predicted = predicted values for the corresponding time
%
%       field.values = measured values for the corresponding time
%
%       Finished 3/12/2010
%

%% opens the NOAA formatted data file
loc=find(filename=='.',1,'first');
tempaaa=filename(1:loc-1);
if exist([tempaaa '.mat'],'file')
    disp('Using pre-read grid')
    load(tempaaa)    
else

    fid1=fopen(filename,'rt');
    
    %% reads the first line and the gage number
    
    fgetl(fid1);
    gage=fgetl(fid1);
    gage_number=sscanf(gage,'%*s %*s %i',1);
    
    %% reads all the data in the file
    
    count=1;
    second=0;
    
    while(~feof(fid1))
        
        % reads each line of the data file
        
        fid=fgets(fid1);
        
        % determines if the end of the file has been reached and if so exits
        
        if(feof(fid1))
            break;
        end
        
        % reads an integer value
        
        test1=sscanf(fid,'%i',1);
        
        % determines if the current line is a line of data
        
        if(test1 == gage_number)
            
            % reads the time information from the file
            
            time=sscanf(fid,'%*s %s',1);
            
            year=str2double(time(1:4));
            month=str2double(time(5:6));
            day=str2double(time(7:8));
            
            clear time;
            
            time=sscanf(fid,'%*s %*s %s',1);
            loc=find(time==':',1,'first');
            hour=str2double(time(1:loc-1));
            minute=str2double(time(loc+1:loc+2));
            
            % reads the predicted and measured values
            
            field.predicted(count)=sscanf(fid,'%*s %*s %*s %f',1);
            field.value(count)=sscanf(fid,'%*s %*s %*s %*s %f',1);
            
            % converts the date string to a time number
            
            vector_time=[year month day hour minute second];
            temp=datestr(vector_time);
            field.time(count)=datenum(temp);
            count=count+1;
            
        end
    end
    
    %% closes the NOAA formatted data file
    save(tempaaa,'field')
    fclose(fid1);
end
