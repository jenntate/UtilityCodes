function plot_data_tflx(basename,time,value,string_number, ...
    units,fid,start_time,stop_time)
%
%   Subroutine to plot the data for point location comparisons between
%   different model simulations.  This subroutine will also determine the
%   minimum, maximum, average, and median values for the different data
%   sets as well.
%
%   Input:
%
%       basename - basename for the model simulations (tflx files)
%
%       time - array containing the time values for each model simulation
%
%       value - array containing the model values for each model
%       simulation
%
%       string_number - the string number which is being plotted and analyzed
%
%       units - the units to use with the y-axis name
%
%       fid - output file identifier to print the min, max, mean, and
%       median values for the data
%
%       start_time - start time over which to analyze the data (does not
%       include any data prior to this time in the plots or analysis)
%
%       stop_time - stop time over which to analyze the data (does not
%       include any data after this time in the plots or analysis)
%
%
%% converts the start and stop time strings to date numbers

start_time=datenum(start_time);
stop_time=datenum(stop_time);

%% create the axis information for the plot including the axis font
%  type/size and bounds

%% preallocation of arrays
min_x1=zeros(length(basename),1);
max_x1=zeros(length(basename),1);
min_y1=zeros(length(basename),1);
max_y1=zeros(length(basename),1);
mean_y1=zeros(length(basename),1);
median_y1=zeros(length(basename),1);

%% determines the minimum and maximum x values

for i=1:length(basename)
   min_x1(i)=min(time(i,:)); %If the array is smaller than the allocation for time, this returns zero.
   max_x1(i)=max(time(i,:));
end
min_x=max(min_x1);

% checks if the start_time is after the initial model times
min_x=max(min_x,start_time);
max_x=min(max_x1);

% checks if the stop time is prior to the intial model times
max_x=min(max_x,stop_time);

%% identifies the desired data (based on start time, stop time, and data
%  range

loc2=zeros(length(basename));

for i=1:length(basename)
    
   % finds the desired data in the array    
   loc1=find(time(i,:)>=min_x,1,'first');
   loc2(i)=find(time(i,:)<=max_x,1,'last');
   loc3=find(time(i,:)==0,1,'first');
   if(loc2(i) > (loc3-1))
       loc2(i)=loc3-1;
   end
%   loc2(i)=min(loc2(i),loc3-1);
   
   % calculates the minimim, maximum, mean, and median values  
   min_y1(i)=min(value(loc1:loc2(i),i));
   max_y1(i)=max(value(loc1:loc2(i),i));
   mean_y1(i)=mean(value(loc1:loc2(i),i));
   median_y1(i)=median(value(loc1:loc2(i),i));
   
   % prints the values to the output file   
   fprintf(fid,'\n\nData for file %s from time %s to %s\n',basename{i}, ...
       datestr(min_x),datestr(max_x));
   fprintf(fid,'Minimum Flux Value for String %i is %f\n',string_number,...
       min_y1(i));
   fprintf(fid,'Maximum Flux Value for String %i is %f\n',string_number,...
       max_y1(i));   
   fprintf(fid,'Average Flux Value for String %i is %f\n',string_number,...
       mean_y1(i));  
   fprintf(fid,'Median Flux Value for String %i is %f\n',string_number,...
       median_y1(i));     
   
end

%% determines the minimum and maximum y values for the plot bounds

min_y=min(min_y1);
max_y=max(max_y1);             

%% Create the time series comparison of the model and field values
figure1 = figure('Color',[1 1 1]);

%% creates the axes information and creates the box and allows for multiple
 % plotting without overwriting

axes1 = axes('Parent',figure1,'FontSize',12,'FontName','Times New Roman');
box(axes1,'on');
hold(axes1,'all');

%% sets the x axis bounds and label with dates

axis([min_x max_x min_y max_y])
datetick('x',2,'keepticks')
axis([min_x max_x min_y max_y])    

%% plots each time series set of data on the same plot

for i=1:length(basename)
    name=regexprep(basename{i},'_',' ');
    plot(time(i,1:loc2(i)),value(1:loc2(i),i),'Parent',axes1,'LineWidth',0.5,'DisplayName',name);
end
    
%% Specify the x-axis label

xlabel({'Time'},'FontSize',16,'FontName','Times New Roman');

%% Specify the y-axis label

ylabel(['Discharges, ' units],'FontSize',16,...
    'FontName','Times New Roman');

%% Create the plot title

title(['Discharge Comparisons for String ',int2str(string_number)],'FontSize',16,...
    'FontName','Times New Roman');

%% Create the legend for the plot

legend(axes1,'show');

%% saves the png and fig files with the string number and tflx in the filename
set(gcf,'renderer','zbuffer');
opengl neverselect
set(gcf,'Backingstore','off','doublebuffer','on');
saveas(gcf,['String_Number_' num2str(string_number) '_tlfx'],'png');
saveas(gcf,['String_Number_' num2str(string_number) '_tflx'],'fig'); 
close(gcf);