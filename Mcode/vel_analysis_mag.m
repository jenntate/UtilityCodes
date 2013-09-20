function vel_analysis_mag(vel_field_data_file,plot_title,y_label, ...
    analysis_output_file,basename,shift,start_time,stop_time, ...
    field_data_time_shift,harm)

out=fopen(analysis_output_file,'w','n'); 
grid=read_adh_grid([basename{1} '.3dm']);
dt=DelaunayTri(grid.x(:),grid.y(:));
nearest=zeros(length(vel_field_data_file(:,1)),1);
temp=cell(length(basename),1);

for i=1:length(vel_field_data_file(:,1))
  
  % print statement to notify the user of the the gage number being analyzed

%  fprintf(1,'\nFinding nearest node to comparison location %i\n',i)

  % determines the nearest node to the gage location

  k=nearestNeighbor(dt, ...
      [vel_field_data_file{i,2} vel_field_data_file{i,3}]);

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
  if(model.time(2)==model.time(1))
      model1.time=model.time(2:length(model.time));
      model1.u=model.u(2:length(model.u));
      model1.v=model.v(2:length(model.v));
      clear model
      model=model1;
      clear model1
  end
  field=read_data(vel_field_data_file{i,4},vel_field_data_file{i,1});
%   field.valuex=field.depth_averaged_x;
  
    %% determines the variable part of the plot titles using the
  %  field_data_file string
  
  start=find(vel_field_data_file{i,1}=='\',1,'last')+1;
  start1=find(vel_field_data_file{i,1}=='/',1,'last')+1;
  if(isempty(start) && isempty(start1))
      start=1;
  elseif(isempty(start))
      start=start1;
  end
  stop=find(vel_field_data_file{i,1}=='.',1,'last')-1;
  name=vel_field_data_file{i,1}(start:stop);  
  clear start stop
  
  % apply conversion to the model and field data
  model.u=hypot(model.u,model.v);
  model.u=model.u*vel_field_data_file{i,5};
  multi=vel_field_data_file{i,6};
  field.speed=field.speed*multi;
  field.time=field.time-field_data_time_shift/24.0; 
  
  start_model=find(model.time >= start_time,1,'first');

  stop_model=find(model.time <= stop_time,1,'last');

  start_field=find(field.time >= start_time,1,'first');
  
  stop_field=find(field.time <= stop_time,1,'last');
  
 
  % calls subroutine to perform wse analysis
  
  data_x(i)=perform_model_versus_field_analysis(field.time(start_field:stop_field),  ...
        field.speed(start_field:stop_field), name, ...
        y_label,plot_title, ...
        model.time(start_model:stop_model), ...
        model.u(start_model:stop_model), 4,shift,harm);    
       
    
  % Perform Percent Analysis
  percent=0:10:100;
  m_percentage=prctile(model.u,percent);
  f_percentage=prctile(field.speed,percent);
  percent=100.0-percent;
  createfigurePct(m_percentage, f_percentage, percent, name, y_label);
  save_name=[name '_pct'];
    print('-r600','-cmyk','-dtiff',save_name);  
    saveas(gcf,[name '_pct'],'fig');
  close(gcf);
  
  
%   field.valuey=field.depth_averaged_y;
%     
%   field.valuey=field.valuey*multi;  
%   
%   model.v=model.v*vel_field_data_file{i,5};
% 
%   data_y(i)=perform_model_versus_field_analysis(field.time(start_field:stop_field),  ...
%         field.valuey(start_field:stop_field), [name '_(Y_Component)'], ...
%         ['Y Component of ' y_label],plot_title, model.time(start_model:stop_model), ...
%         model.v(start_model:stop_model), 4,shift,harm);     
    
  clear model field

end

for i=1:length(vel_field_data_file(:,1))  
  if(~isempty(data_x(i).RMSE))
    fprintf(out,'%s\n\n',vel_field_data_file{i,1});
    fprintf(out,'Reading time series data for node %i\n', nearest(i));
    fprintf(out,'VELOCITY MAGNITUDE INFORMATION\n');
    fprintf(out,'Root Mean Square Error          = %5.2f\n', ...
        data_x(i).RMSE);
    fprintf(out,'Normalized Root Mean Square Error = %5.2f\n', ...
        data_x(i).RMSE/data_x(i).standard_deviation_field);
    fprintf(out,'Nash-Sutcliffe Coefficient      = %5.2f\n', ...
        data_x(i).Nash_error);
    fprintf(out,'Willmott Coefficient            = %5.2f\n', ...
        data_x(i).Willmott);      
    fprintf(out,'Covariance                      = %5.2f\n', ...
        data_x(i).covariance);
    fprintf(out,'Correlation Coefficient         = %5.2f\n', ...
        data_x(i).correlation_coef);
    fprintf(out,'Standard Deviation (Field Data) = %5.2f\n', ...
        data_x(i).standard_deviation_field);
    fprintf(out,'Standard Deviation (Model Data) = %5.2f\n', ...
        data_x(i).standard_deviation_model);
    fprintf(out,'Variance (Field Data)           = %5.2f\n', ...
        data_x(i).variance_field);
    fprintf(out,'Variance (Model Data)           = %5.2f\n', ...
        data_x(i).variance_model);
    fprintf(out,'Mean (Field Data)               = %5.2f\n', ...
        data_x(i).mean_field);
    fprintf(out,'Mean (Model Data)               = %5.2f\n', ...
        data_x(i).mean_model);
    fprintf(out,'Median (Field Data)             = %5.2f\n', ...
        data_x(i).median_field);
    fprintf(out,'Median (Model Data)             = %5.2f\n', ...
        data_x(i).median_model);
    fprintf(out,'Max (Field Data)                = %5.2f\n', ...
        data_x(i).max_field);
    fprintf(out,'Max (Model Data)                = %5.2f\n', ...
        data_x(i).max_model);
    fprintf(out,'Min (Field Data)                = %5.2f\n', ...
        data_x(i).min_field);
    fprintf(out,'Min (Model Data)                = %5.2f\n', ...
        data_x(i).min_model);
    if(strcmp(harm,'YES')==1 || strcmp(harm,'Yes')==1 || strcmp(harm,'yes')==1)
      num=data_x.harm_number;
      for w=1:num
        fprintf(out,'%5s Harmonic Amplitude (Field)   = %5.2f\n', ...
          data_x(i).harm_name{w}, data_x(i).harm_field(w));
        fprintf(out,'%5s Harmonic Amplitude (Model)   = %5.2f\n', ...
          data_x(i).harm_name{w}, data_x(i).harm_model(w));        
      end    
    end
    fprintf(out,'The field was shifted down by %5.2f (negative value means upward shift)\n',...
        data_x(i).diff);  
    fprintf(out,'\n\n'); 
  end
%   if(~isempty(data_y(i).RMSE))
%     fprintf(out,'%s\n\n',vel_field_data_file{i,1});
%     fprintf(out,'Reading time series data for node %i\n', nearest(i));
%     fprintf(out,'Y COMPONENT OF VELOCITY INFORMATION\n');
%     fprintf(out,'Root Mean Square Error          = %5.2f\n', ...
%         data_y(i).RMSE);
%     fprintf(out,'Normalized Root Mean Square Error = %5.2f\n', ...
%         data_y(i).RMSE/data_y(i).standard_deviation_field);
%     fprintf(out,'Nash-Sutcliffe Coefficient      = %5.2f\n', ...
%         data_y(i).Nash_error);
%     fprintf(out,'Willmott Coefficient            = %5.2f\n', ...
%         data_y(i).Willmott);      
%     fprintf(out,'Covariance                      = %5.2f\n', ...
%         data_y(i).covariance);
%     fprintf(out,'Correlation Coefficient         = %5.2f\n', ...
%         data_y(i).correlation_coef);
%     fprintf(out,'Standard Deviation (Field Data) = %5.2f\n', ...
%         data_y(i).standard_deviation_field);
%     fprintf(out,'Standard Deviation (Model Data) = %5.2f\n', ...
%         data_y(i).standard_deviation_model);
%     fprintf(out,'Variance (Field Data)           = %5.2f\n', ...
%         data_y(i).variance_field);
%     fprintf(out,'Variance (Model Data)           = %5.2f\n', ...
%         data_y(i).variance_model);
%     fprintf(out,'Mean (Field Data)               = %5.2f\n', ...
%         data_y(i).mean_field);
%     fprintf(out,'Mean (Model Data)               = %5.2f\n', ...
%         data_y(i).mean_model);
%     fprintf(out,'Median (Field Data)             = %5.2f\n', ...
%         data_y(i).median_field);
%     fprintf(out,'Median (Model Data)             = %5.2f\n', ...
%         data_y(i).median_model);
%     fprintf(out,'Max (Field Data)                = %5.2f\n', ...
%         data_y(i).max_field);
%     fprintf(out,'Max (Model Data)                = %5.2f\n', ...
%         data_y(i).max_model);
%     fprintf(out,'Min (Field Data)                = %5.2f\n', ...
%         data_y(i).min_field);
%     fprintf(out,'Min (Model Data)                = %5.2f\n', ...
%         data_y(i).min_model);
%     if(strcmp(harm,'YES')==1 || strcmp(harm,'Yes')==1 || strcmp(harm,'yes')==1)
%       num=data_y.harm_number;
%       for w=1:num
%         fprintf(out,'%5s Harmonic Amplitude (Field)   = %5.2f\n', ...
%           data_y(i).harm_name{w}, data_y(i).harm_field(w));
%         fprintf(out,'%5s Harmonic Amplitude (Model)   = %5.2f\n', ...
%           data_y(i).harm_name{w}, data_y(i).harm_model(w));        
%       end    
%     end
%     fprintf(out,'The field was shifted down by %5.2f (negative value means upward shift)\n',...
%         data_y(i).diff);  
%     fprintf(out,'\n\n'); 
%   end  
end

fclose(out);

clear all
