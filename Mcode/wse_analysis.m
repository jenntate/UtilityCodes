function wse_analysis(wse_field_data_file,plot_title,y_label, ...
    analysis_output_file,basename,shift,start_time,stop_time, ...
    field_data_time_shift,harm)

out=fopen(analysis_output_file,'w','n'); 
grid=read_adh_grid([basename{1} '.3dm']);
dt=DelaunayTri(grid.x(:),grid.y(:));
nearest=zeros(length(wse_field_data_file(:,1)),1);
temp=cell(length(basename),1);

for i=1:length(wse_field_data_file(:,1))
  
  % print statement to notify the user of the the gage number being analyzed

%  fprintf(1,'\nFinding nearest node to comparison location %i\n',i)

  % determines the nearest node to the gage location

  k=nearestNeighbor(dt, ...
      [wse_field_data_file{i,2} wse_field_data_file{i,3}]);

  % reads the time series solution values for the da file for the closest 
  %  node and saves to the model structure
  nearest(i)=k;
  fprintf(1,'Reading time series data for node %i for solution file %s\n', ...
      k, basename{1});

  model=load_da_time_series([basename{1} '.da'],k);
%  model.wse=grid.z+model.dep;

  if(length(basename) > 1)
     for h=2:length(basename)
        fprintf(1,'Reading time series data for node %i for solution file %s\n', ...
            k,basename{h});
        temp{h}=load_da_time_series([basename{h} '.da'],k);
        loc=find(temp{h}.time>max(model.time),1,'first');
        model.time(length(model.time)+1:length(model.time)+1+length(temp{h}.time)-loc) = ...
            temp{h}.time(loc:length(temp{h}.time));
        model.wse(length(model.wse)+1:length(model.wse)+1+length(temp{h}.wse)-loc) = ...
            temp{h}.wse(loc:length(temp{h}.wse));  
        clear temp
     end
  end  
%  tempaaa=wse_field_data_file{i,4}
%  tempbbb=wse_field_data_file{i,1}
  field=read_data(wse_field_data_file{i,4},wse_field_data_file{i,1});
    %% determines the variable part of the plot titles using the
  %  field_data_file string
  
  start=find(wse_field_data_file{i,1}=='\',1,'last')+1;
  start1=find(wse_field_data_file{i,1}=='/',1,'last')+1;
  if(isempty(start) && isempty(start1))
      start=1;
  elseif(isempty(start))
      start=start1;
  end
  stop=find(wse_field_data_file{i,1}=='.',1,'last')-1;
  name=wse_field_data_file{i,1}(start:stop);  
  clear start stop  
  
  % apply conversion to the model and field data
  
  model.wse=model.wse*wse_field_data_file{i,5};
  multi=wse_field_data_file{i,6};
  field.value=field.value*multi;
  field.time=field.time-field_data_time_shift/24.0; 
  
  start_model=find(model.time >= start_time,1,'first');

  stop_model=find(model.time <= stop_time,1,'last');

  start_field=find(field.time >= start_time,1,'first');
  
  stop_field=find(field.time <= stop_time,1,'last');
  
  % calls subroutine to perform wse analysis
      
  data(i)=perform_model_versus_field_analysis(field.time(start_field:stop_field),  ...
        field.value(start_field:stop_field), name, ...
        y_label,plot_title, model.time(start_model:stop_model), ...
        model.wse(start_model:stop_model), 1,shift,harm);    
    
  clear model field

end

for i=1:length(wse_field_data_file(:,1))  
  if(~isempty(data(i).RMSE))
    fprintf(out,'%s\n\n',wse_field_data_file{i,1});
    fprintf(out,'Reading time series data for node %i\n', nearest(i));
    fprintf(out,'Root Mean Square Error          = %5.2f\n', ...
        data(i).RMSE);
    fprintf(out,'Normalized Root Mean Square Error = %5.2f\n', ...
        data(i).RMSE/data(i).standard_deviation_field);
    fprintf(out,'Nash-Sutcliffe Coefficient      = %5.2f\n', ...
        data(i).Nash_error);
    fprintf(out,'Willmott Coefficient            = %5.2f\n', ...
        data(i).Willmott);      
    fprintf(out,'Covariance                      = %5.2f\n', ...
        data(i).covariance);
    fprintf(out,'Correlation Coefficient         = %5.2f\n', ...
        data(i).correlation_coef);
    fprintf(out,'Standard Deviation (Field Data) = %5.2f\n', ...
        data(i).standard_deviation_field);
    fprintf(out,'Standard Deviation (Model Data) = %5.2f\n', ...
        data(i).standard_deviation_model);
    fprintf(out,'Variance (Field Data)           = %5.2f\n', ...
        data(i).variance_field);
    fprintf(out,'Variance (Model Data)           = %5.2f\n', ...
        data(i).variance_model);
    fprintf(out,'Mean (Field Data)               = %5.2f\n', ...
        data(i).mean_field);
    fprintf(out,'Mean (Model Data)               = %5.2f\n', ...
        data(i).mean_model);
    fprintf(out,'Median (Field Data)             = %5.2f\n', ...
        data(i).median_field);
    fprintf(out,'Median (Model Data)             = %5.2f\n', ...
        data(i).median_model);
    fprintf(out,'Max (Field Data)                = %5.2f\n', ...
        data(i).max_field);
    fprintf(out,'Max (Model Data)                = %5.2f\n', ...
        data(i).max_model);
    fprintf(out,'Min (Field Data)                = %5.2f\n', ...
        data(i).min_field);
    fprintf(out,'Min (Model Data)                = %5.2f\n', ...
        data(i).min_model);
    if(strcmp(harm,'YES')==1 || strcmp(harm,'Yes')==1 || strcmp(harm,'yes')==1)
      num=data.harm_number;
      for w=1:num
        fprintf(out,'%5s Harmonic Amplitude (Field)   = %5.2f\n', ...
          data(i).harm_name{w}, data(i).harm_field(w));
        fprintf(out,'%5s Harmonic Amplitude (Model)   = %5.2f\n', ...
          data(i).harm_name{w}, data(i).harm_model(w));        
      end    
    end
    fprintf(out,'The field was shifted down by %5.2f (negative value means upward shift)\n',...
        data(i).diff);  
    fprintf(out,'\n\n'); 
  end
end

fclose(out);

clear all
