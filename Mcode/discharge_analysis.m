function discharge_analysis(dis_field_data_file,plot_title,y_label, ...
    analysis_output_file,basename, t0, start_time, stop_time,...
    field_data_time_shift,shift,harm)



   for h=1:length(basename)
      fprintf(1,'Reading fluxes for solution file %s\n', ...
          basename{h});
      temp=read_tflx_discharges(basename{h},t0,1);
          
      if(h~=1)
        loc=find(temp.time>max(model.time),1,'first'); 
        model.time(length(model.time)+1:length(model.time)+1+length(temp.time)-loc) = ...
            temp.time(loc:length(temp.time));
        len=length(model.fluxes(:,1));
        for i=1:length(temp.fluxes(1,:))
          model.fluxes(len+1: len+1+length(temp.fluxes(:,1))-loc,i) ...
              = temp.fluxes(loc:length(temp.fluxes(:,1)),i);
        end
      else
        model.time(1:length(temp.time)) = ...
            temp.time(1:length(temp.time));
        for i=1:length(temp.fluxes(1,:))
          model.fluxes(1:length(temp.fluxes),i) = ...
              temp.fluxes(1:length(temp.fluxes),i);
        end
      end
      model.mapping=temp.mapping;
      clear temp
   end
  

for i=1:length(dis_field_data_file(:,1))
  
   % finds the location of the desired string
      
   spot= model.mapping == dis_field_data_file{i,2};
%   plot(model.time(:,spot), model.fluxes(:,spot))
%   fprintf(1,'\nString Number %i corresponds to flux string number %i\n', ...
%           dis_field_data_file{i,2},spot)
    
   % sets fluxes variable to pass into analysis subroutine
      
   fluxes=dis_field_data_file{i,3}*model.fluxes(:,spot);
   field=read_data(dis_field_data_file{i,5},dis_field_data_file{i,1});
   multi=dis_field_data_file{i,4};
   field.value=field.value*multi;
   field.time=field.time-field_data_time_shift/24.0; 
  
   start_model=find(model.time >= start_time,1,'first');

   stop_model=find(model.time <= stop_time,1,'last');

   start_field=find(field.time >= start_time,1,'first');
  
   stop_field=find(field.time <= stop_time,1,'last');
  
  start=find(dis_field_data_file{i,1}=='\',1,'last')+1;
  start1=find(dis_field_data_file{i,1}=='/',1,'last')+1;
  if(isempty(start) && isempty(start1))
      start=1;
  elseif(isempty(start))
      start=start1;
  end
  stop=find(dis_field_data_file{i,1}=='.',1,'last')-1;
  name=dis_field_data_file{i,1}(start:stop);  
  clear start stop 
   
   % calls subroutine to perform wse analysis
      
   data(i)=perform_model_versus_field_analysis(field.time(start_field:stop_field),  ...
        field.value(start_field:stop_field), name, ...
        y_label,plot_title, model.time(start_model:stop_model), ...
        fluxes(start_model:stop_model), 2,shift,harm);    
    
  %clear model field

end

clear match fluxes model
out=fopen(analysis_output_file,'w','n'); 

for i=1:length(dis_field_data_file(:,1))
    if(~isempty(data(i).RMSE))
      fprintf(out,'%s\n\n',dis_field_data_file{i,1});
      fprintf(out,'Root Mean Square Error            = %5.2f\n', ...
          data(i).RMSE);
      fprintf(out,'Normalized Root Mean Square Error = %5.2f\n', ...
          data(i).RMSE/data(i).standard_deviation_field);
      fprintf(out,'Nash-Sutcliffe Coefficient        = %5.2f\n', ...
          data(i).Nash_error);
      fprintf(out,'Willmott Coefficient              = %5.2f\n', ...
          data(i).Willmott);    
      fprintf(out,'Covariance                        = %5.2f\n', ...
          data(i).covariance);
      fprintf(out,'Correlation Coefficient           = %5.2f\n', ...
          data(i).correlation_coef);
      fprintf(out,'Standard Deviation (Field Data)   = %5.2f\n', ...
          data(i).standard_deviation_field);
      fprintf(out,'Standard Deviation (Model Data)   = %5.2f\n', ...
          data(i).standard_deviation_model);
      fprintf(out,'Variance (Field Data)             = %5.2f\n', ...
          data(i).variance_field);
      fprintf(out,'Variance (Model Data)             = %5.2f\n', ...
          data(i).variance_model);
      fprintf(out,'Mean (Field Data)                 = %5.2f\n', ...
          data(i).mean_field);
      fprintf(out,'Mean (Model Data)                 = %5.2f\n', ...
          data(i).mean_model);
      fprintf(out,'Median (Field Data)               = %5.2f\n', ...
          data(i).median_field);
      fprintf(out,'Median (Model Data)               = %5.2f\n', ...
          data(i).median_model);
      fprintf(out,'Max (Field Data)                  = %5.2f\n', ...
          data(i).max_field);
      fprintf(out,'Max (Model Data)                  = %5.2f\n', ...
          data(i).max_model);
      fprintf(out,'Min (Field Data)                  = %5.2f\n', ...
          data(i).min_field);
      fprintf(out,'Min (Model Data)                  = %5.2f\n', ...
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
      fprintf(out,'\n\n'); 
    end
end

fclose(out);



clear all
  
  
  
  
  