function tflx_comparisons(basename,data,units,start_time,stop_time)
%% Opens the output.out file to list the max, min, mean, and median values

fid=fopen('output.out','w');

%% reads the time series data for each nodal location and plots the data 
%  by calling the plot_comparisons_tflx subroutine
fprintf(1,'\n Beginning string comparisons ... \n')

% Reads the string numbers to compare
for map=1:length(data{1,1}.mapping)
    string_number=data{1,1}.mapping(map); %changed data{i,1} 
    fprintf(1,'\n Plotting String Number %i ...\n',string_number)

% plots the time series data for the given string
plot_comparisons_tflx(basename,data,units,string_number,fid, ...
start_time,stop_time,map)
end

%% close the output.out file with the max, min, mean, and median values

fclose(fid);
