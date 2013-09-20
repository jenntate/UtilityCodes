
function interp = interpolation_q(filename,hourly_inc,windowsize,zero_hour_time,format,multiplier)

switch lower(format)    
    case {'USGS1','usgs1'} 
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

%plot(time,value,'--ro')
%hold on
%plot(data.time,data.value,'-gx')

%interp.time=time;
%interp.value=value;
%interp.count=count-1;

time1=min(data.time):0.01*(1/24):max(data.time);

%%windowsize=windowsize*24.0/0.01;

value=spline(time,value,time1);

%%value=spline(data.time,data.value,time);

filtered_value=value;

for i=1+windowsize:length(time1)-windowsize
    filtered_value(i)=sum(value(i-windowsize:i+windowsize))/(2*windowsize+1);
end

%filtered_value=filter(ones(1,windowsize)/windowsize,1,interp.value);

interp.time=min(data.time):hourly_inc*(1.0/24.0):max(data.time);

interp.filtered_value=spline(time1,filtered_value,interp.time);
interp.filtered_value=multiplier*interp.filtered_value;
data.value=multiplier*data.value;

% Create figure
figure1 = figure('Color',[1 1 1]);

% Create axes

axes1 = axes('Parent',figure1,'FontSize',18,'FontName','Times New Roman');
hold(axes1,'all');
box(axes1,'on');

set(axes1,'YLim',[min(data.value) max(data.value)]);
set(axes1,'XLim',[min(interp.time) max(interp.time)]);

axis([min(interp.time) max(interp.time) min(data.value) max(data.value)])
datetick('x',2,'keepticks')

set(axes1,'YLim',[min(data.value) max(data.value)]);
set(axes1,'XLim',[min(interp.time) max(interp.time)]);

plot(data.time,data.value,'Parent',axes1, ...
    'LineWidth',0.5,'DisplayName','Raw Data')

plot(interp.time,interp.filtered_value,'-r','Parent',axes1, ...
    'LineWidth',0.5,'DisplayName','Filtered Data')

xlabel({'Time'},'FontSize',20,'FontWeight','bold','FontName','Times New Roman');

ylabel({'Discharges, cfs'},'FontSize',20,'FontWeight','bold',...
    'FontName','Times New Roman');

% Create title
location=find(filename=='_');

string1=['Filtering of Discharges'];
title(string1,'FontSize',24,'FontWeight','bold',...
    'FontName','Times New Roman');

% Create legend
legend1 = legend(axes1,'show');
set(legend1,'FontSize',14);

saveas(figure1,[filename(1:location(2)-1) '_q'],'png');
saveas(figure1,[filename(1:location(2)-1) '_q'],'fig');     

fid=fopen([filename(1:location(2)-1) '.out'],'w');

out_time=(interp.time-(datenum(datestr(zero_hour_time))))*24;

fprintf(fid,'!!! Data from file = %s\n',filename(1:location(2)-1));
fprintf(fid,'!!! Data hour 0 = %s\n',zero_hour_time);
fprintf(fid,'!!! Data measurements start %s\n',datestr(interp.time(1)));
fprintf(fid,'XY1  1  %i %i %i %i %i\n', length(out_time), 2, 0, 0, 0);

fprintf(fid,'%10.3f %10.3f\n',[out_time(:), interp.filtered_value(:)]');

fclose(fid);

