function createfigurePct(data_m, data_f, percentile, data_file, y_label)
%
%   Function that creates a time series comparison for the model and field
%
%   INPUT VARIABLES
%
%   x_data = variable x axis data
%
%   y_data = variable y axis data - percentiles
%
%   data_file = string containing the plot name 
%
%   y_name = variable to use for the y-axis name
%
%   plot_title = string for the plot title
%
%   Finished 3/20/2013 JNT
%

%% Create the time series comparison of the model and field values

figure1 = figure('Color',[1 1 1]);

%% create the axis information for the plot including the axis font
%  type/size and bounds

axes1 = axes('Parent',figure1,'FontSize',12,'FontName','Times New Roman');
box(axes1,'on');
hold(axes1,'all');

% min_x=min(data_m);
% max_x=max(data_m);

min_y=min(percentile);
max_y=max(percentile);

min_model=min(data_m)
min_field=min(data_f)
max_model=max(data_m)
max_field=max(data_f)


% start1=min_model;
% start2=min_field;
% 
% min_x1=min(data_m(start1:max_model))
% min_x2=min(data_f(start2:max_field))
% max_x1=max(data_m(start1:max_model))
% max_x2=max(data_f(start2:max_field))

min_x=min(min_model,min_field)
max_x=max(max_model,max_field)


axis([min_x max_x min_y max_y]);
%datetick('x',2,'keepticks');
% axis([min_x max_x min_y max_y]);

%% Create the percent exceedance plot

plot(data_m,percentile,'Parent',axes1,'LineWidth',0.5,'DisplayName','Model');

plot(data_f,percentile,'Parent',axes1,'LineWidth',0.5,'DisplayName','Field');

%% Specify the x-axis label

xlabel({y_label},'FontSize',16,'FontName','Times New Roman');

%% Specify the y-axis label

ylabel('Percent','FontSize',16,'FontName','Times New Roman');

%% Create the string for the plot title

plot_title=strcat('Percent Exceedance', '_',data_file);
plot_title=regexprep(plot_title,'_',' ');

%% Create the plot title

title(plot_title,'FontSize',16,...
    'FontName','Times New Roman');

%% Create the legend for the plot

 legend(axes1,'show','Location','Best');
