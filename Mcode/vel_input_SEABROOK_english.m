tic

clear

% ADH root filename

basename{1}='Plan3-FINAL-Piers-2008';
shift='NO';  %YES = field data shift to match model mean
harm='NO';
start_time=datenum('01/01/2000 00:00:00');
stop_time=datenum('01/01/2020 00:00:00');
field_data_time_shift=43824.0; %time shift in hours converts 2013 data to 2008

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%   WATER SURFACE ELEVATION INPUT INFORMATION    %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Title to use for plots to be followed by the data filename

plot_title{1}='Velocity Magnitude Comparison Plots for ';

% Y-axis Label

y_label{1}='Velocity Magnitude, ft/s';

% output filename for the analysis 

analysis_output_file{1}='gage_analysis_file_vel_SEABROOK.txt'; 

%%%      data files containing the raw field data             %%%
%%%    first number is the x coordinate for each gage         %%%
%%%   second number is the y coordinate for each gage         %%%
%%%    third entry is the read format for the data file       %%%
%%%    fourth is the model unit conversion factor             %%%
%%%    fifth is the field unit conversion factor              %%%

vel_field_data_file   = {                                             ...
    'E:\Seabrook-2012\FieldData\Seabrook_West.txt'                , ...
    3691960.3,  558831.425  ,  'NOAA', 1, 1    ; ...
    'E:\Seabrook-2012\FieldData\Seabrook.txt'                , ...
    3691982.8,  558838.55  ,  'NOAA', 1, 1    ; ...
    'E:\Seabrook-2012\FieldData\Seabrook_East.txt'                , ...
    3692005.3,  558845.675  ,  'NOAA', 1, 1    ; ...
    };    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%        PERFORMS THE WSE ANALYSIS FIRST AND THEN PERFORMS       %%%%%
%%%%%%                 THE DISCHARGE ANALYSIS                         %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if(~isempty(vel_field_data_file))
    fprintf(1,'\nAnalyzing %i Velocity Magnitude Datasets\n\n',...
        length(vel_field_data_file(:,1)))    
    vel_analysis_mag(vel_field_data_file,plot_title{1},y_label{1}, ...
        analysis_output_file{1},basename,shift,start_time,stop_time, ...
        field_data_time_shift,harm);
    fprintf(1,'\nFinished Velocity Magnitude Analysis\n\n')
else
    fprintf(1,'\nNo Velocity Datasets to Analyze\n\n')
end






toc

