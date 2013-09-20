function createfigure_Discharge(time, data, data_file, name)
%
%   Function that creates a time series comparison for the model discharge
%
%   INPUT VARIABLES
%
%   time = variable x axis data
%
%   discharge = variable y axis data - percentiles
%
%   data_file = string containing the plot name 
%
%   y_name = variable to use for the y-axis name
%
%
%   Finished 4/2/2013 JNT
%

%% Create the time series plot of AdH computed discharges

figure1 = figure('Color',[1 1 1]);

%% create the axis information for the plot including the axis font
%  type/size and bounds

axes1 = axes('Parent',figure1,'FontSize',12,'FontName','Times New Roman');
box(axes1,'on');
hold(axes1,'all');

time

min_y=min(min(data))
max_y=max(max(data))

min_x=min(time)
max_x=max(time)

axis([min_x max_x min_y max_y]);
 datetick('x',2,'keepticks');
 axis([min_x max_x min_y max_y]);

%% Create the plot
 plot(time,data,'Parent',axes1,'LineWidth',0.5, 'DisplayName',name);


%% Specify the x-axis label

xlabel('Time','FontSize',16,'FontName','Times New Roman');

%% Specify the y-axis label

ylabel('Discharge','FontSize',16,'FontName','Times New Roman');

%% Create the string for the plot title

plot_title=strcat('Model Discharge', '_',data_file);
plot_title=regexprep(plot_title,'_',' ');

%% Create the plot title

title(plot_title,'FontSize',16,...
    'FontName','Times New Roman');

%% Create the legend for the plot

 legend(axes1,'show','Location','Best');
