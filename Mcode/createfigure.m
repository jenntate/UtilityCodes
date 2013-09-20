function createfigure(field_data, model_data, field_data_file,y_label,...
    plot_title)
%
%   Function to create a scatter plot of the field versus the model
%   comparisons.  The plot is the model data (y-axis) verus the field data
%   (x-axis).  
%
%   INPUT VARIABLES
%
%   field_data = measured field data values.
%
%   model_data = model results corresponding to the same time as the field
%   data measurements.
%
%   field_data_file = This is the plot title.
%
%   y_label = This is the desired label for the y-axis.
%
%   plot_title = This is the initial part of the plot title to be followed
%   by the field_data_file string.
%
%   Finished 3/12/2010
%

%% Creates the figure

figure1 = figure('Color',[1 1 1]);

%% determine the minimum and maximum values to use for the plot bounds

bot=min(min(field_data),min(model_data));
top=max(max(field_data),max(model_data));

%% create and specify the axis properties

axes1 = axes('Parent',figure1,'FontSize',12,'FontName', ...
    'Times New Roman');
set(axes1,'XLim',[bot top]);
set(axes1,'YLim',[bot top]);
box(axes1,'on');
hold(axes1,'all');

%% Create the scatter plot

scatter(field_data,model_data);

%% create the string to use for the x-axis label

temp=strcat('Field Data_',y_label);
temp=regexprep(temp,'_',' ');

%% Set the x-axis label and font type/size

xlabel({temp},'FontSize',16,'FontName','Times New Roman');

%% create the string to use for the y-axis label

temp=strcat('Model Data_',y_label);
temp=regexprep(temp,'_',' ');

%% Set the y-axis label and font type/size

ylabel({temp},'FontSize',16,'FontName','Times New Roman');

%% create the string to use for the plot title

plot_title=strcat(plot_title, '_',field_data_file);
plot_title=regexprep(plot_title,'_',' ');

%% Set the plot title and font type/size

title(plot_title,'FontSize',16,...
    'FontName','Times New Roman');

%% Create a line to represent the ideal match of field verus model

line([bot-abs(top) top+abs(top)], [bot-abs(top) top+abs(top)], ...
    'LineWidth',2.0,'Color',[0 0 0]);

