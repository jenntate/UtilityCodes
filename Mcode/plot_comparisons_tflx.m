function plot_comparisons_tflx(basename,data,units,string_number,fid, ...
    start_time,stop_time,map)
%
% 
%     Inputs
%
%        basename - cell including the rootnames of the different runs to
%                   compare
%
%        data - structure array that includes the data for a given point
%               location
%
%        units - units for y axis
%
%        string_number - String number for comparison
%
%        fid - filename of the output file for min and max summary
%
%        map - index of string number being plotted
%
%     Outputs
%
%        time series plots saved to png and fig files

%% determines the maximum number of time steps from all simulations for
%  array preallocation

nt=length(data{1}.time);
for p=2:length(basename)
  if(nt < length(data{p}.time))
    nt = length(data{p}.time);
  end
end

%% preallocate the time array and set equal to the datasets' time values

time=zeros(length(basename),nt);
for i=1:length(basename)
   time(i,1:length(data{i}.time))=data{i}.time;
end

%% preallocates the value array used to carry into the plot_data subroutine

value=zeros(nt,length(basename));


%% reads the attribute variable to determine the attributes to investigate         
%  sets the array to carry to the plot_data subroutine

for i=1:length(basename)
    value(1:length(data{i}.time),i)=data{i,1}.fluxes(:,map);             
end 

%% calls the plot_data subroutine to plot the data and determine
%  max,min,mean, and median values
plot_data_tflx(basename,time,value,string_number, ...
   units,fid,start_time,stop_time); 
