function data=perform_model_versus_field_analysis_JNT(field_time,...
    field_value,name,y_label,plot_title,time,value,analysis_type,...
    shift,harm)
%
%   Function to perform the model verus field analysis for each gage
%   location.
%
%   INPUT VARIABLES
%
%   field_data_format = specifies the format to expect for the field data
%   file.
%
%   field_data_file = the file that has the field data to be compared
%
%   y_label = string specifing how to set the y_labels for the comparison
%   plots
%
%   plot_title = string to speciFfy the plot title for the comparison plots
%
%   OUTPUT VARIABLES
%
%   match = structure that contains the times, model values, and field
%   measurements for intersecting times of model results and field
%   measurements.
%
%   Finished 3/12/2010
%
%   Revised  3/18/2010 to handle discharge comparisons
%
%   Revised  3/23/2010 for field data reads as case statements and to save
%            wse comparison plots with _wse and discharge comparison plots 
%            with _q
%
%

model.time=time;
model.value=value;
field.time=field_time;
field.value=field_value;

clear time value field_time field_value

%% determines the max and min field and model times

max_model_time=max(model.time);
min_model_time=min(model.time);
max_field_time=max(field.time);
min_field_time=min(field.time);

%% determines if the field and model times intersect
if(isempty(max_field_time))
  max_field_time=min_model_time-1;
end
if(isempty(min_field_time))
  min_field_time=min_model_time-1;
end

if((min_model_time <= max_field_time && min_model_time >= min_field_time)...
    || (max_model_time >= min_field_time && max_model_time <= max_field_time) ...
    || (min_model_time <= min_field_time && max_model_time >= max_field_time))
    
  %% determines the start and stop of field/model data overlap

  hold1=find(field.time >= min_model_time,1,'first');
  hold2=find(field.time <= max_model_time,1,'last');

  %% determines the time increment for the model and field
  if(analysis_type==1 || analysis_type==3 || analysis_type==4)
    model_dt=model.time(3)-model.time(2);
    field_dt=field.time(3)-field.time(2);

    increment=int32(model_dt/field_dt);
    if(increment < 1)
       increment = 1;
    end
  elseif(analysis_type==2)
    field_dt=field.time(3)-field.time(2);
    increment=int32((1.0/24.0)/field_dt);
    if(increment < 1)
        increment = 1;
    end
  end
  
  fprintf(1,'Increment set to %i\n',increment);
%fprintf(1,'two\n');
  %% specifies the field time to match model times to using the spline
  %  command
  
  match.time=field.time(hold1:increment:hold2);
  match.field_value=field.value(hold1:increment:hold2);

  %% clears variables that are un-needed for the remainder of the
  %  calculations
  
  clear max_model_time min_model_time hold1 hold2 model_dt field_dt increment

  %% removes the values representing dry nodes from the model data.
  
  if(analysis_type==1)
      
    dry_entries1=find(abs(model.value) > 99999);
    
    for i=1:length(dry_entries1)
        if(dry_entries1(i) ==1)
            model.value(1)=0.0;
        else
          model.value(dry_entries1(i)) = model.value(dry_entries1(i)-1);
        end
    end
  end
%  fprintf(1,'three\n');
   %% matches the model values to the field times using a linear
  %  interpolation 
  
  match.model_value=spline(model.time,model.value,match.time);
  
  %% determines the mean values for the field and model and applies that
  %  difference to the field.  
  
  if(analysis_type==1 && (strcmp(shift,'YES') || strcmp(shift,'Yes') ...
          || strcmp(shift,'yes')))
       
    mean_model=mean(match.model_value);
    mean_field=mean(match.field_value);
    match.diff=mean_model-mean_field;
    if(abs(match.diff)>5)
        match.diff=0.0;
    end
    match.field_value=match.field_value + match.diff;
    field.value=field.value + match.diff;

    fprintf(1,'The field was shifted down by %f (negative value means upward shift)\n',...
      match.diff);
  else
    fprintf(1,'Not Matching Data Means\n');
    match.diff=0;
  end
 % fprintf(1,'four\n');
  %% determines the variable part of the plot titles using the
  %  field_data_file string
  
%   start=find(field_data_file=='\',1,'last')+1;
%   start1=find(field_data_file=='/',1,'last')+1;
%   if(isempty(start) && isempty(start1))
%       start=1;
%   elseif(isempty(start))
%       start=start1;
%   end
%   stop=find(field_data_file=='.',1,'last')-1;
%   name=field_data_file(start:stop);  
%   clear start stop
  
  %% function to create the time series plot of model verus field comparisons 
  if(analysis_type==1 || analysis_type==4)
    createfigure2(model.time,model.value,field.time,field.value, ...
        name, y_label,plot_title);
  elseif(analysis_type==3)
    createfigure2_salt(model.time,model.value,field.time,field.value, ...
        name, y_label,plot_title);      
  else
    diff=zeros(length(field.value)-1,1);
    for h=2:length(field.value)
        diff(h-1)=field.time(h)-field.time(h-1);        
    end
%    median(diff)
    if(median(diff) > 3.0/24.0 || length(field.value) < 50)
      fprintf(1,'Daily Averaged Field Values\n');
      createfigure2_dis(model.time,model.value,field.time,field.value, ...
          name, y_label,plot_title);

    else
       createfigure2(model.time,model.value,field.time,field.value, ...
          name, y_label,plot_title); 

    end
  end
  %% saves the time series plot as a .fig and .png files and closes the
  %  matlab figure  
  
  set(gcf,'renderer','zbuffer');
  opengl neverselect
  set(gcf,'Backingstore','off','doublebuffer','on');
  
  if(analysis_type==1)
%    saveas(gcf,[name '_wse'],'tiffn');
    save_name=[name '_wse'];
    print('-r600','-cmyk','-dtiff',save_name);  
    saveas(gcf,[name '_wse'],'fig');     
  elseif(analysis_type==2)
%    saveas(gcf,[name '_q'],'tiffn');
    save_name=[name '_q'];
    print('-r600','-cmyk','-dtiff',save_name);  
    saveas(gcf,[name '_q'],'fig'); 
  elseif(analysis_type==3)
%    saveas(gcf,[name '_salt'],'tiffn');
    save_name=[name '_salt'];
    print('-r600','-cmyk','-dtiff',save_name);  
    saveas(gcf,[name '_salt'],'fig');      
  elseif(analysis_type==4)
    save_name=[name '_' y_label(1) '_vel'];
    print('-r600','-cmyk','-dtiff',save_name);  
    saveas(gcf,name,'fig');       
  end
  
  
  close(gcf);
%  fprintf(1,'five\n');
  %% creates the box plot of the model verus field comparisons
  if(analysis_type==3)
    createfigure_salt(match.field_value,match.model_value,name, ...
      y_label,plot_title);    
  else
    createfigure(match.field_value,match.model_value,name, ...
      y_label,plot_title);
  end
  
  %% saves the box plot as a .fig and .png files and closes the matlab
  %  figure
  
  set(gcf,'renderer','zbuffer');
  opengl neverselect
  set(gcf,'Backingstore','off','doublebuffer','on');
  
  if(analysis_type==1)
%    saveas(gcf,[name '_wse_box'],'tiffn');
    save_name=[name '_wse_box'];
    print('-r600','-cmyk','-dtiff',save_name);      
    saveas(gcf,[name '_wse_box'],'fig');     
  elseif(analysis_type==2)
    %saveas(gcf,[name '_q_box'],'tiffn');
    save_name=[name '_q_box'];
    print('-r600','-cmyk','-dtiff',save_name);      
    saveas(gcf,[name '_q_box'],'fig'); 
  elseif(analysis_type==3)
    %saveas(gcf,[name '_salt_box'],'tiffn');
    save_name=[name '_salt_box'];
    print('-r600','-cmyk','-dtiff',save_name);    
    saveas(gcf,[name '_salt_box'],'fig'); 
  elseif(analysis_type==4)
    save_name=[name '_' y_label(1) '_vel_box'];
    print('-r600','-cmyk','-dtiff',save_name);  
    saveas(gcf,name,'fig');     
  end

  close(gcf)
  
  %% performing harmonic analysis
  if(strcmp(harm,'YES')==1 || strcmp(harm,'Yes')==1 || strcmp(harm,'yes')==1)
    [~,field_FREQ,field_TIDECON]=t_tide(match.field_value);
    [harm_name,model_FREQ,model_TIDECON]=t_tide(match.model_value);

    plot_harmonic_comparison(model_TIDECON,field_TIDECON,name,harm_name)
  
    set(gcf,'renderer','zbuffer');
    opengl neverselect
    set(gcf,'Backingstore','off','doublebuffer','on');
  
    if(analysis_type==1)
      %saveas(gcf,[name '_wse_harm'],'tiffn');
      save_name=[name '_wse_harm'];
      print('-r600','-cmyk','-dtiff',save_name);       
      saveas(gcf,[name '_wse_harm'],'fig');     
    elseif(analysis_type==2)
      %saveas(gcf,[name '_q_harm'],'tiffn');
      save_name=[name '_q_harm'];
      print('-r600','-cmyk','-dtiff',save_name);       
      saveas(gcf,[name '_q_harm'],'fig'); 
    elseif(analysis_type==3)
      %saveas(gcf,[name '_salt_harm'],'tiffn');
      save_name=[name '_salt_harm'];
      print('-r600','-cmyk','-dtiff',save_name);      
      saveas(gcf,[name '_salt_harm'],'fig'); 
    end

    close(gcf)  

    plot_harmonic_comparison_bar(model_TIDECON,field_TIDECON,harm_name)
    
    set(gcf,'renderer','zbuffer');
    opengl neverselect
    set(gcf,'Backingstore','off','doublebuffer','on')

    if(analysis_type==1)
      %saveas(gcf,[name '_wse_harm_bar'],'tiffn');
      save_name=[name '_wse_harm_bar'];
      print('-r600','-cmyk','-dtiff',save_name);        
      saveas(gcf,[name '_wse_harm_bar'],'fig');     
    elseif(analysis_type==2)
      %saveas(gcf,[name '_q_harm_bar'],'tiffn');
      save_name=[name '_q_harm_bar'];
      print('-r600','-cmyk','-dtiff',save_name);        
      saveas(gcf,[name '_q_harm_bar'],'fig'); 
    elseif(analysis_type==3)
      %saveas(gcf,[name '_salt_harm_bar'],'tiffn');
      save_name=[name '_salt_harm_bar'];
      print('-r600','-cmyk','-dtiff',save_name);  
      saveas(gcf,[name '_salt_harm_bar'],'fig'); 
    end    
    
    close(gcf);
        
    plot_harmonic_comparison_freq(model_FREQ,field_FREQ,harm_name)
    
    set(gcf,'renderer','zbuffer');
    opengl neverselect
    set(gcf,'Backingstore','off','doublebuffer','on')
    
    if(analysis_type==1)
      %saveas(gcf,[name '_wse_harm_freq'],'tiffn');
      save_name=[name '_wse_harm_freq'];
      print('-r600','-cmyk','-dtiff',save_name);  
      saveas(gcf,[name '_wse_harm_freq'],'fig');     
    elseif(analysis_type==2)
      %saveas(gcf,[name '_q_harm_freq'],'tiffn');
      save_name=[name '_q_harm_freq'];
      print('-r600','-cmyk','-dtiff',save_name);  
      saveas(gcf,[name '_q_harm_freq'],'fig'); 
    elseif(analysis_type==3)
      saveas(gcf,[name '_salt_harm_freq'],'tiffn');
      save_name=[name '_salt_harm_freq'];
      print('-r600','-cmyk','-dtiff',save_name);  
      saveas(gcf,[name '_salt_harm_freq'],'fig'); 
    end
    
    close(gcf);
    
    plot_harmonic_comparison_phase(model_TIDECON,field_TIDECON,harm_name)
    
    set(gcf,'renderer','zbuffer');
    opengl neverselect
    set(gcf,'Backingstore','off','doublebuffer','on')

    if(analysis_type==1)
      %saveas(gcf,[name '_wse_harm_phase'],'tiffn');
      save_name=[name '_wse_harm_phase'];
      print('-r600','-cmyk','-dtiff',save_name);       
      saveas(gcf,[name '_wse_harm_phase'],'fig');     
    elseif(analysis_type==2)
      %saveas(gcf,[name '_q_harm_phase'],'tiffn');
      save_name=[name '_q_harm_phase'];
      print('-r600','-cmyk','-dtiff',save_name);       
      saveas(gcf,[name '_q_harm_phase'],'fig'); 
    elseif(analysis_type==3)
      %saveas(gcf,[name '_salt_harm_phase'],'tiffn');
      save_name=[name '_salt_harm_phase'];
      print('-r600','-cmyk','-dtiff',save_name);      
      saveas(gcf,[name '_salt_harm_phase'],'fig'); 
    end     
    
    close(gcf);
    
    %% Compute Power Spectra
    
    spec1=power_spectra(match.field_value);
    spec2=power_spectra(match.model_value);
    
    %% Create the time series comparison of the model and field values

    figure('Color',[1 1 1]);

    %% create the axis information for the plot including the axis font
    %  type/size and bounds

    semilogy(spec1.freq,spec1.p,'b','LineWidth',0.5,'DisplayName','Model');
    hold on
    semilogy(spec2.freq,spec2.p,'m','LineWidth',0.5,'DisplayName','Field');    
    ylabel('Power','FontSize',16,'FontName','Times New Roman');
    xlabel('Frequency, Cycles/Hour','FontSize',16,'FontName','Times New Roman');
    title('Power Spectrum','FontSize',16,...
          'FontName','Times New Roman');
    colormap bone;
    set(gcf,'renderer','zbuffer');
    opengl neverselect
    set(gcf,'Backingstore','off','doublebuffer','on')
    legend('show')

    if(analysis_type==1)
      %saveas(gcf,[name '_wse_harm_power'],'tiffn');
      save_name=[name '_wse_harm_power'];
      print('-r600','-cmyk','-dtiff',save_name);  
      saveas(gcf,[name '_wse_harm_power'],'fig');     
    elseif(analysis_type==2)
      %saveas(gcf,[name '_q_harm_power'],'tiffn');
      save_name=[name '_q_harm_power'];
      print('-r600','-cmyk','-dtiff',save_name);       
      saveas(gcf,[name '_q_harm_power'],'fig'); 
    elseif(analysis_type==3)
      %saveas(gcf,[name '_salt_harm_power'],'tiffn');
      save_name=[name '_salt_harm_power'];
      print('-r600','-cmyk','-dtiff',save_name);      
      saveas(gcf,[name '_salt_harm_power'],'fig'); 
    end   
    
    close(gcf);
    
    %% determining error metrics and other information
  
    data.harm_number=length(model_TIDECON(:,1));
    for i=1:length(model_TIDECON(:,1))
      data.harm_name{i}=harm_name(i,:);
      data.harm_field(i)=field_TIDECON(i,1);
      data.harm_model(i)=model_TIDECON(i,1);
    end
%    data.harm_field
%    data.harm_model
%    loc_M2=11;
%    loc_S2=12;
%    loc_K1=6;
%    loc_N2=10;
%    loc_O1=4;
%    loc_M4=19;

%    data.field_harmonic_M2=field_TIDECON(loc_M2,1);
%    data.model_harmonic_M2=model_TIDECON(loc_M2,1); 
%    data.field_harmonic_S2=field_TIDECON(loc_S2,1);
%    data.model_harmonic_S2=model_TIDECON(loc_S2,1);
%    data.field_harmonic_K1=field_TIDECON(loc_K1,1);
%    data.model_harmonic_K1=model_TIDECON(loc_K1,1);
%    data.field_harmonic_N2=field_TIDECON(loc_N2,1);
%    data.model_harmonic_N2=model_TIDECON(loc_N2,1);
%    data.field_harmonic_O1=field_TIDECON(loc_O1,1);
%    data.model_harmonic_O1=model_TIDECON(loc_O1,1);
%    data.field_harmonic_M4=field_TIDECON(loc_M4,1);
%    data.model_harmonic_M4=model_TIDECON(loc_M4,1);
  end
  
  data.covariance=calc_cov(match.field_value(:),match.model_value(:));
  data.correlation_coef=calc_coef(match.field_value(:),match.model_value(:));
  data.standard_deviation_field=std(match.field_value);
  data.standard_deviation_model=std(match.model_value);
  data.variance_field=var(match.field_value);
  data.variance_model=var(match.model_value);
  data.mean_field=mean(match.field_value);
  data.mean_model=mean(match.model_value);
  data.median_field=median(match.field_value);
  data.median_model=median(match.model_value);
  data.max_field=max(match.field_value);
  data.max_model=max(match.model_value);
  data.min_field=min(match.field_value);
  data.min_model=min(match.model_value);    
  data.RMSE=RMSE_calculation(match.field_value,match.model_value);
  data.Nash_error=Nash_Sutcliffe_calculation(match.field_value, ...
      match.model_value);
  data.Willmott=Willmott_calculation(match.field_value, ...
      match.model_value);
  data.Pearson=Pearson_calculation(match.field_value, ...
      match.model_value);
  data.diff=match.diff;
  
else
   %% print statement to notify that there is NO overlap of the field and
   %  model data
    
   fprintf(1,'The Model and Field had no overlapping data for %s\n',...
       name); 
  data.covariance=0.0;
  data.correlation_coef=0.0;
  data.standard_deviation_field=0.0;
  data.standard_deviation_model=0.0;
  data.variance_field=0.0;
  data.variance_model=0.0;
  data.mean_field=0.0;
  data.mean_model=0.0;
  data.median_field=0.0;
  data.median_model=0.0;
  data.max_field=0.0;
  data.max_model=0.0;
  data.min_field=0.0;
  data.min_model=0.0;    
  data.RMSE=0.0;
  data.Nash_error=0.0;
  data.Willmott=0.0;
  data.Pearson=0.0;
  data.diff=0.0;
   
end

%% clears the variables from memory

clear len name field_data_file gcf y_label plot_title field model diff dt

