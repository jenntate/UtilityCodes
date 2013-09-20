tic

clear

% ADH root filename

basename='Bush_Canal_1';

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

analysis_output_file{1}='gage_analysis_file_wse.txt'; 

%%%      data files containing the raw field data             %%%
%%%    first number is the x coordinate for each gage         %%%
%%%   second number is the y coordinate for each gage         %%%
%%%    third number is the read format for the data file      %%%

wse_field_data_file   = {                               ...
    'data/Caillou_Bay_USGS.txt'                       , ...
    3428374.0371398,     210608.63755536, 'USGS'      ; ...   
    'data/Caillou_Lake_USGS.txt'                      , ...
    3412266.4665589,     272769.24998097, 'USGS'      ; ...    
    'data/Bayou_Grand_Caillou_USGS.txt'               , ...
    3477814.0      ,     321658.0       , 'USGS'      ; ...    
    'data/GIWW_at_Houma_USGS.txt'                     , ...
    3478903.2750316,     399942.04495628, 'USGS'      ; ...    
    'data/GIWW_at_Larose_USGS.txt'                    , ...
    3582625.0      ,     391116.0       , 'USGS'      ; ...    
    'data/Company_Canal_near_Lockport_USGS.txt'       , ...
    3527243.1048071,     410881.25127938, 'USGS'      ; ...    
    'data/Bayou_Petit_Caillou_USGS.txt'               , ...
    3508781.0      ,     323294.0       , 'USGS'      ; ...   
    'data/Bayou_Terrebonne_USGS.txt'                  , ...
    3518135.0      ,     323828.0       , 'USGS'      ; ...    
    'data/Houma_Navigational_Canal_USGS.txt'          , ...
    3473044.3627297,     322433.21654367, 'USGS'      ; ... 
    'data/Bayou_Terrebonne_WSE_CHL.prn'               , ...
    3518214.0	   ,     324448.0       , 'BUSH_1'    ; ...      
    'data/Bayou_Grand_Caillou_WSE_CHL.prn'            , ...
    3472126.951572 ,     305091.83650439, 'BUSH_1'    ; ...   
    'data/GIWW_Bayou_Lafourche_WSE_CHL.prn'           , ...
    3586186.0805712,	 397357.97240739, 'BUSH_1'    ; ...     
    'data/Falgout_Canal_WSE_CHL.prn'                  , ...
    3454360.0	   ,     333230.0       , 'BUSH_1'    ; ...    
    'data/GIWW_Houma_WSE_CHL.prn'                     , ...
    3467998.3261517,	 386752.1280483 , 'BUSH_1'    ; ...    
    'data/Grand_Bayou_WSE_CHL.prn'                    , ...
    3577053.0	   ,     378029.0       , 'BUSH_1'    ; ...     
    'data/Humble_Canal_WSE_CHL.prn'                   , ...
    3526095.8471264,	 341317.54372905, 'BUSH_1'    ; ... 
    'data/Placid_Canal_WSE_CHL.prn'                   , ...
    3504031.0	   ,     306598.0       , 'BUSH_2'    ; ...       
    'data/Bayou_Petit_Caillou_WSE_CHL.prn'            , ...
    3499190.0715083,	 290522.3829524 , 'BUSH_2'    ; ...      
    'data/Bayou_Dularge_WSE_CHL.prn'                  , ...
    3454819.0	   ,     330584.0       , 'BUSH_2'    ; ...    
    'data/Lapeyrouse_Canal_WSE_CHL.prn'               , ...
    3500026.9855289,	 294797.05638406, 'BUSH_2'    ; ...  
    };

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%     DISCHARGE INPUT INFORMATION     %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Title to use for plots to be followed by the data filename

plot_title{2}='Discharge Comparison Plots for ';

% Y-axis Label

y_label{2}='Discharges, cfs';

% output filename for the analysis 

analysis_output_file{2}='gage_analysis_file_dis.txt'; 

% If reading the ADH outputted flux file, need to set initial time

t0='07/01/2009 00:00:00';

%%%         data files for the raw field discharge data                 %%%
%%%               first number is the flux string number                %%%
%%% second number is to flip the sign of the model discharged if needed %%% 
%%%                  third number is the read format                    %%%

dis_field_data_file   = {                                      ...
    'discharge/Bayou_Grand_Caillou_at_Dulac_USGS.txt'        , ...
     25, -1,  'USGS'                                         ; ...    
    'discharge/GIWW_West_of_Bayou_Lafourche_USGS.txt'        , ...
     21,  1,  'USGS'                                         ; ...    
    'discharge/GIWW_at_Houma_USGS.txt'                       , ...
     26,  1,  'USGS'                                         ; ...    
    'discharge/Houma_Navigational_Canal_at_Dulac_USGS.txt'   , ...
     24, -1,  'USGS'                                         ; ...     
    'discharge/Bayou_Grand_Caillou_Q_CHL.prn'                , ...
     23, -1,  'BUSH_1'                                       ; ...      
    'discharge/Bayou_Terrebonne_Q_CHL.prn'                   , ...
     15, -1,  'BUSH_1'                                       ; ...    
    'discharge/Falgout_Canal_Q_CHL.prn'                      , ...
     27,  1,  'BUSH_1'                                       ; ...    
    'discharge/GIWW_Houma_Q_CHL.prn'                         , ...
     20,  1,  'BUSH_1'                                       ; ...    
    'discharge/Grand_Bayou_Q_CHL.prn'                        , ...
     13,  1,  'BUSH_1'                                       ; ...    
    'discharge/Humble_Canal_Q_CHL.prn'                       , ...
     14,  1,  'BUSH_1'                                       ; ...    
    'discharge/Lapeyrouse_Canal_Q_CHL.prn'                   , ...
     17,  1,  'BUSH_2'                                       ; ...    
    'discharge/Placid_Canal_Q_CHL.prn'                       , ...
     16,  1,  'BUSH_2'                                       ; ...  
    'discharge/Bayou_Petit_Caillou_Q_CHL.prn'                , ...
     18,  1,  'BUSH_2'                                       ; ... 
    'discharge/Bayou_Dularge_Q_CHL.prn'                      , ...
     19,  1,  'BUSH_2'                                       ; ...     
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
        analysis_output_file{1},basename);
    fprintf(1,'\nFinished Water Surface Elevation Analysis\n\n')
else
    fprintf(1,'\nNo Water Surface Elevation Datasets to Analyze\n\n')
end

if(~isempty(dis_field_data_file))
    fprintf(1,'\nAnalyzing %i Discharge Datasets\n\n',...
        length(dis_field_data_file))    
    discharge_analysis(dis_field_data_file,plot_title{2},y_label{2}, ...
        analysis_output_file{2},basename, t0);
    fprintf(1,'\nFinished Disharge Analysis\n\n')
else
    fprintf(1,'\nNo Discharge Datasets to Analyze\n\n')
end

toc
