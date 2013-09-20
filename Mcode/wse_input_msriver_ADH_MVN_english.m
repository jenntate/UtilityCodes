tic

clear

% ADH root filename

basename{1}='July8-diversions-ALL';
shift='NO';  %YES = field data shift to match model mean
harm='NO';
start_time=datenum('01/01/2008 00:00:00');
stop_time=datenum('01/01/2011 00:00:00');
field_data_time_shift=-6.0; %time shift in hours converts local to GMT
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

y_label{1}='Water Surface Elevation, ft, NAVD88(2004.65)';

% output filename for the analysis 

analysis_output_file{1}='gage_analysis_file_wse_MS-Hydro.txt'; 

%%%      data files containing the raw field data             %%%
%%%    first number is the x coordinate for each gage         %%%
%%%   second number is the y coordinate for each gage         %%%
%%%    third entry is the read format for the data file       %%%
%%%    fourth is the model unit conversion factor             %%%
%%%    fifth is the field unit conversion factor              %%%

wse_field_data_file   = {                                             ...
    '../RiverStage/RedRiverLanding.mat'                , ...
    627810.000          ,     3426089.000          ,  'NOAA', 3.281, 1    ; ...
    '../RiverStage/BatonRouge.mat'                , ...
    672897.000          ,     3367408.000          ,  'NOAA', 3.281, 1    ; ...
    '../RiverStage/Donaldsonville.mat'                , ...
    694000.000          ,     3332953.000          ,  'NOAA', 3.281, 1    ; ...
    '../RiverStage/Reserve.mat'                , ...
    734431.000          ,     3326933.000          ,  'NOAA', 3.281, 1    ; ...
    '../RiverStage/BCNorth.mat'                , ...
    744332.000          ,     3321474.000          ,  'NOAA', 3.281, 1    ; ...
    '../RiverStage/BonnetCarre.mat'                , ...
    748428.000          ,     3320961.000          ,  'NOAA', 3.281, 1    ; ...
    '../RiverStage/NewOrleans.mat'                , ...
    776183.000          ,     3315002.000          ,  'NOAA', 3.281, 1    ; ...
%     '../RiverStage/HarveyLock.mat'                , ...
%     781437.000          ,     3312721.000          ,  'NOAA', 3.281, 1    ; ...
%     '../RiverStage/IHNCLock.mat'                , ...
%     786620.000          ,     3317513.000          ,  'NOAA', 3.281, 1    ; ...
%     '../RiverStage/AlgiersLock.mat'                , ...
%     792625.000          ,     3314249.000          ,  'NOAA', 3.281, 1    ; ...
    '../RiverStage/Alliance.mat'                , ...
    793553.000          ,     3287735.000          ,  'NOAA', 3.281, 1    ; ...
    '../RiverStage/WestPointe.mat'                , ...
    810497.000          ,     3275709.000          ,  'NOAA', 3.281, 1    ; ...
    '../RiverStage/Empire.mat'                , ...
    830665.500          ,     3256219.500          ,  'NOAA', 3.281, 1    ; ...
    '../RiverStage/Venice.mat'                , ...
    854721.000          ,     3244355.000          ,  'NOAA', 3.281, 1    ; ...
%     '../RiverStage/Venice-NOAA-2008-2010.mat'                , ...
%     854721.000          ,     3244355.000          ,  'NOAA', 3.281, 1    ; ...
    '../RiverStage/WestBay.mat'                , ...
    858280.820          ,     3237905.482          ,  'NOAA', 3.281, 1    ; ...
    '../RiverStage/HeadofPasses.mat'                , ...
    865429.000          ,     3228808.000          ,  'NOAA', 3.281, 1    ; ...
%     '../RiverStage/SWP_EastJetty.mat'                , ...
%     849231.000          ,     3204794.000          ,  'NOAA', 3.281, 1    ; ...
    '../RiverStage/SWPass-NOAA-2008-2010.mat'                , ...
    849231.000          ,     3204794.000          ,  'NOAA', 3.281, 1    ; ...
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
