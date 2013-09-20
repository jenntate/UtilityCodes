tic

clear

% ADH root filename

basename{1}='MTOG_ADH_EXISTING_CONDITIONS_ESL';
shift='NO';  %YES = field data shift to match model mean
harm='NO';
start_time=datenum('01/01/2004 00:00:00');
stop_time=datenum('01/01/2005 00:00:00');
field_data_time_shift=-6.0; %time shift in hours converts local to GMT


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

analysis_output_file{1}='gage_analysis_file_wse_MVN.txt'; 

%%%      data files containing the raw field data             %%%
%%%    first number is the x coordinate for each gage         %%%
%%%   second number is the y coordinate for each gage         %%%
%%%    third entry is the read format for the data file       %%%
%%%    fourth is the model unit conversion factor             %%%
%%%    fifth is the field unit conversion factor              %%%

wse_field_data_file   = {                                             ...
    'E:\MS_Hydro\RiverStage\RedRiverLanding.prn'                , ...
    627557.792          ,     3426026.206          ,  'MTOG', .3048, 1    ; ...
    'E:\MS_Hydro\RiverStage\BatonRouge.prn'                , ...
    672200.892          ,     3367707.018          ,  'MTOG', .3048, 1    ; ...
    'E:\MS_Hydro\RiverStage\Reserve.prn'                , ...
    734398.121          ,     3327306.809          ,  'MTOG', .3048, 1    ; ...
    'E:\MS_Hydro\RiverStage\Empire.prn'                , ...
    830355.606          ,     3255944.763          ,  'MTOG', .3048, 1    ; ...
    'E:\MS_Hydro\RiverStage\Venice.prn'                , ...
    854417.726          ,     3244067.254          ,  'MTOG', .3048, 1    ; ...
    %'MTOG_2004-2005\MVN\Grand_Bayou_Canal_MVN.prn'                  , ...
    %751752.547        ,     3269919.891      ,  'MTOG', 1, 0.3048   ; ... 
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
    wse_analysis(wse_field_data_file,plot_title{1},y_label{1}, ...
        analysis_output_file{1},basename,shift,start_time,stop_time, ...
        field_data_time_shift,harm);
    fprintf(1,'\nFinished Water Surface Elevation Analysis\n\n')
else
    fprintf(1,'\nNo Water Surface Elevation Datasets to Analyze\n\n')
end



toc

