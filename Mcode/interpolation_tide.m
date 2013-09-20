
function interp = interpolation_tide(filename,hourly_inc,format,multiplier)

switch lower(format)    
    case {'USGS','usgs'} 
        data = read_usgs(filename);
    case {'USGS2','usgs2'} 
        data = read_usgs2(filename);
    case {'NOAA','noaa'} 
        data = read_noaa(filename); 
    case {'BUSH_1','bush_1'}
        data = read_bush_canal_1(filename);
    case {'BUSH_2','bush_2'} 
        data = read_bush_canal_2(filename);
    case {'DATA','data'}
        data = read_data_date_time_value(filename);
    otherwise
        error('\n\n FORMAT %s NOT FOUND FOR FILE = %s \n\n', ...
            field_data_format,field_data_file)
end




count=1;
for i=1:length(data.time)-1
    if((data.time(i+1)-data.time(i)) > 2*(1/24))
        time(count)=data.time(i);
        value(count)=data.value(i);
        count=count+1; 
        number=((data.time(i+1)-data.time(i))*(24/0.25));
        for j=1:number-1 
           time(count)=data.time(i)+ ...
               (j/number)*(data.time(i+1)-data.time(i));
           rise=(data.value(i+1)-data.value(i));
           run=(data.time(i+1)-data.time(i));
           value(count)=(rise/run)*(time(count)-data.time(i))+data.value(i);
           count=count+1;
        end  
    else
        time(count)=data.time(i);
        value(count)=data.value(i);
        count=count+1;
    end
end
time(count)=data.time(length(data.value));
value(count)=data.value(length(data.value));
        
time1=min(data.time):0.01*(1/24):max(data.time);

value1=spline(time,value,time1);

interp.time=min(data.time):hourly_inc*(1.0/24.0):max(data.time);

interp.value=spline(time1,value1,interp.time);
interp.value=multiplier*interp.value;



