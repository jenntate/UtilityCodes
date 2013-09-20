tic

clear

% ADH root filename

basename{1}='ROAMS-June28';
shift='NO';  %YES = field data shift to match model mean
harm='NO';
start_time=datenum('01/01/2012 00:00:00');
stop_time=datenum('12/31/2012 00:00:00');
field_data_time_shift=-5.0; %time shift in hours converts local to GMT
   %subtracting from the field time (therefore adding 6 in this case) 
%%%field_data_time_shift=-30;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%   WATER SURFACE ELEVATION INPUT INFORMATION    %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Title to use for plots to be followed by the data filename

plot_title{1}='Water Surface Elevation Comparison Plots for ';

% Y-axis Label

y_label{1}='Water Surface Elevation, m, NAVD88(2004.65)';

% output filename for the analysis 

analysis_output_file{1}='gage_analysis_file_wse_Lejeune.txt'; 

%%%      data files containing the raw field data             %%%
%%%    first number is the x coordinate for each gage         %%%
%%%   second number is the y coordinate for each gage         %%%
%%%    third entry is the read format for the data file       %%%
%%%    fourth is the model unit conversion factor             %%%
%%%    fifth is the field unit conversion factor              %%%

wse_field_data_file   = {                                             ...
    'Mile_Hammock_Bay_2012.mat'                , ...
    286336.0          ,     3825744.0          ,  'NOAA', 1 , 1    ; ...
    'Wallace_Creek_2012.mat'                , ...
    283420.0          ,     3839310.0          ,  'NOAA', 1 , 1    ; ...
   };


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%        PERFORMS THE WSE ANALYSIS FIRST AND THEN PERFORMS       %%%%%
%%%%%%                 THE DISCHARGE ANALYSIS                         %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if(~isempty(wse_field_data_file))
    fprintf(1,'\nAnalyzing %i Water Surface Elevation Datasets\n\n',...
        length(wse_field_data_file(:,1))) 
    wse_analysis_JNT(wse_field_data_file,plot_title{1},y_label{1}, ...
        analysis_output_file{1},basename,shift,start_time,stop_time, ...
        field_data_time_shift,harm);
    fprintf(1,'\nFinished Water Surface Elevation Analysis\n\n')
else
    fprintf(1,'\nNo Water Surface Elevation Datasets to Analyze\n\n')
end



toc

