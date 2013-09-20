tic

clear

basename='DP_TOM';
y_label='Water Surface Elevation, ft, NAVD88';
analysis_output_file='gage_analysis_file.txt';
plot_title='Water Surface Elevation Comparison Plots for ';

% Field_data_format should be 1 for a USGS formatted file type
% Field_data_format should b2 2 for a NOAA formatted file type
% Field_data_format should be 3 for the following format
% yyyy   mm  dd  hh  mm  ss  value

% Gage Number 1 Gage 20

i=1;
gage_locations(((2*i)-1):(2*i))=[3605348.005 512086.6979];
field_data_format(i) = 1;
field_data_file{i}='data\Gage_20.prn';

% Gage Number 2 Gage 21
i=i+1;
gage_locations(((2*i)-1):(2*i))=[3617062.262 507555.3013];
field_data_format(i) = 6;
field_data_file{i}='data\Gage_21.prn';

% Gage Number 3 Gage 22
i=i+1;
gage_locations(((2*i)-1):(2*i))=[3617317.135 505226.4862];
field_data_format(i) = 7;
field_data_file{i}='data\Gage_22.prn';

% Gage Number 4 Gage 23
i=i+1;
gage_locations(((2*i)-1):(2*i))=[3621257.994 497449.6915];
field_data_format(i) = 6;
field_data_file{i}='data\Gage_23.prn';

% Gage Number 5 Gage 27
i=i+1;
gage_locations(((2*i)-1):(2*i))=[3620851.155 500384.0965];
field_data_format(i) = 6;
field_data_file{i}='data\Gage_27.prn';

% Gage Number 6 Gage 28
i=i+1;
gage_locations(((2*i)-1):(2*i))=[3618432.239 500927.4018];
field_data_format(i) = 7;
field_data_file{i}='data\Gage_28.prn';

% Gage Number 7 Gage 29
i=i+1;
gage_locations(((2*i)-1):(2*i))=[3611958.537 495708.3079];
field_data_format(i) = 1;
field_data_file{i}='data\Gage_29.prn';

% Gage Number 8 Gage 127
i=i+1;
gage_locations(((2*i)-1):(2*i))=[3620927.467 500445.3016];
field_data_format(i) = 1;
field_data_file{i}='data\Gage_127.prn';

% Gage Number 9 Gage 128
i=i+1;
gage_locations(((2*i)-1):(2*i))=[3618330.597 500928.8723];
field_data_format(i) = 1;
field_data_file{i}='data\Gage_128';

% Gage Number 10 Gage 129 
i=i+1;
gage_locations(((2*i)-1):(2*i))=[3612150.848 495626.8201];
field_data_format(i) = 1;
field_data_file{i}='data\Gage_129.prn';

% Gage Number 11 Highway 90
i=i+1;
gage_locations(((2*i)-1):(2*i))=[3602621 516724.7];
field_data_format(i) = 1;
field_data_file{i}='data\Highway_90.prn';

% Read the grid file

grid=read_adh_grid(basename);
dt=DelaunayTri(grid.x(:),grid.y(:));
number_stations=i;
out=fopen(analysis_output_file,'wb','n');

% Performs all the Analysis

for i=1:number_stations;
    
  match=perform_model_versus_field_analysis ...
      (gage_locations(((2*i)-1):(2*i)), field_data_format(i), ...
      field_data_file{i},basename,i,y_label,plot_title,dt);

  if(length(match.time) > 1)  
    covariance=cov(match.field_value(:),match.model_value(:));
    correlation_coef=corrcoef(match.field_value,match.model_value);
    standard_deviation_field=std(match.field_value);
    standard_deviation_model=std(match.model_value);
    variance_field=var(match.field_value);
    variance_model=var(match.model_value);
    mean_field=mean(match.field_value);
    mean_model=mean(match.model_value);
    median_field=median(match.field_value);
    median_model=median(match.model_value);
    max_field=max(match.field_value);
    max_model=max(match.model_value);
    min_field=min(match.field_value);
    min_model=min(match.model_value);    
    RMSE=RMSE_calculation(match.field_value,match.model_value);
    Nash_error=Nash_Sutcliffe_calculation(match.field_value, ...
        match.model_value);
  
    fprintf(out,'%s\n\n',field_data_file{i});
    fprintf(out,'Root Mean Square Error          = %5.2f\n', ...
        RMSE);
    fprintf(out,'Nash-Sutcliffe Coefficient      = %5.2f\n', ...
        Nash_error);
    fprintf(out,'Covariance                      = %5.2f\n', ...
        covariance(2));
    fprintf(out,'Correlation Coefficient         = %5.2f\n', ...
        correlation_coef(2));
    fprintf(out,'Standard Deviation (Field Data) = %5.2f\n', ...
        standard_deviation_field);
    fprintf(out,'Standard Deviation (Model Data) = %5.2f\n', ...
        standard_deviation_model);
    fprintf(out,'Variance (Field Data)           = %5.2f\n', ...
        variance_field);
    fprintf(out,'Variance (Model Data)           = %5.2f\n', ...
        variance_model);
    fprintf(out,'Mean (Field Data)               = %5.2f\n', ...
        mean_field);
    fprintf(out,'Mean (Model Data)               = %5.2f\n', ...
        mean_model);
    fprintf(out,'Median (Field Data)             = %5.2f\n', ...
        median_field);
    fprintf(out,'Median (Model Data)             = %5.2f\n', ...
        median_model);
    fprintf(out,'Max (Field Data)                = %5.2f\n', ...
        max_field);
    fprintf(out,'Max (Model Data)                = %5.2f\n', ...
        max_model);
    fprintf(out,'Min (Field Data)                = %5.2f\n', ...
        min_field);
    fprintf(out,'Min (Model Data)                = %5.2f\n', ...
        min_model);
    fprintf(out,'\n\n'); 
  end
  
end

fclose(out);

clear all

toc

