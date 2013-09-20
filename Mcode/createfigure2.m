function createfigure2(model_time, model_value, field_time, field_value ...
    ,field_data_file,y_name,plot_title)
%
%   Function that creates a time series comparison for the model and field
%
%   INPUT VARIABLES
%
%   model_time = variable containing the model times
%
%   model_value = variable containing the model results
%
%   field_time = variable containing the field times
%
%   field_value = variable containing the field measurements
%
%   field_data_file = string containing the plot name 
%
%   y_name = variable to use for the y-axis name
%
%   plot_title = string for the plot title
%
%   Finished 3/12/2010
%

%% Create the time series comparison of the model and field values

figure1 = figure('Color',[1 1 1]);

%% create the axis information for the plot including the axis font
%  type/size and bounds

axes1 = axes('Parent',figure1,'FontSize',12,'FontName','Times New Roman');
box(axes1,'on');
hold(axes1,'all');

min_model_time=find(model_time >= field_time(1),1,'first');
min_field_time=find(field_time >= model_time(1),1,'first');
max_model_time=find(model_time <= max(field_time),1,'last');
max_field_time=find(field_time <= max(model_time),1,'last');


start1=min_model_time;
start2=min_field_time;

min_x1=min(model_time(start1:max_model_time));
min_x2=min(field_time(start2:max_field_time));
max_x1=max(model_time(start1:max_model_time));
max_x2=max(field_time(start2:max_field_time));

min_y1=min(model_value(start1:max_model_time));
min_y2=min(field_value(start2:max_field_time));
max_y1=max(model_value(start1:max_model_time));
max_y2=max(field_value(start2:max_field_time));

min_x=max(min_x1,min_x2);
max_x=min(max_x1,max_x2);
min_y=min(min_y1,min_y2);
max_y=max(max_y1,max_y2);
axis([min_x max_x min_y max_y]);
datetick('x',6,'keepticks');
axis([min_x max_x min_y max_y]);

%% Create the model time series plot

plot(model_time,model_value,'Parent',axes1,'LineWidth',0.5,'DisplayName','2008 Model Data');

%% Create the field time series plot

plot(field_time,field_value,'r','Parent',axes1,'LineWidth',0.5,'DisplayName','2013 Field Data');

%% Specify the x-axis label

xlabel({'Time'},'FontSize',16,'FontName','Times New Roman');

%% Specify the y-axis label

ylabel({y_name},'FontSize',16,...
    'FontName','Times New Roman');

%% Create the string for the plot title

plot_title=strcat(plot_title, '_',field_data_file);
plot_title=regexprep(plot_title,'_',' ');

%% Create the plot title

title(plot_title,'FontSize',16,...
    'FontName','Times New Roman');

%% Create the legend for the plot

legend(axes1,'show','Location','Best');
