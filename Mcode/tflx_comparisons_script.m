clear
tic

%% Rootname for the model simulations (MUST HAVE A CORESPONDING '_tflx')
%% file for each Rootname
basename{1}='anchorage_2006';
basename{2}='anchorage_2002';

%% start time and stop time over which to consider data
% Model start time (time zero in .bc)
t0 = '01/01/2006 00:00:00';
% Start time to begin plots
start_time = '08/01/2006 00:00:00';
% Stop time to end plots
stop_time  = '08/31/2006 00:00:00';

%% specify the units
time_units=1; % seconds - 1
units='cms';


%========================================================================
%% don't change below this line
%========================================================================
data=cell(length(basename),1);
fprintf(1,'\n Reading tflx discharges ... \n')
for i=1:length(basename)
    data{i}=read_tflx_discharges([basename{i}],t0,time_units);
end

%% Call tflx comparisons
tflx_comparisons(basename,data,units,start_time,stop_time)

%%
fprintf(1,'\n Job Complete. \n\n')
toc
