function match=perform_model_versus_field_analysis(gage_locations, ...
    field_data_format,field_data_file,basename,gage_number,grid, ...
    y_label,plot_title)


fprintf(1,'Finding nearest node to comparison location %i\n',gage_number)

dt=DelaunayTri(grid.x(:),grid.y(:));

k=nearestNeighbor(dt,gage_locations);

fprintf(1,'Reading time series data for node %i\n', k)

model=load_da_time_series(basename,k);

fprintf(1,'Reading field data from file %s\n',field_data_file)

if(field_data_format == 1)
  field = read_usgs(field_data_file);
elseif(field_data_format == 2)
  field = read_noaa(field_data_file); 
elseif(field_data_format == 3)
   field = read_yyyy_mm_dd_hh_ss_value(field_data_file);
elseif(field_data_format == 4)
   field = read_date_time_value(field_data_file);
elseif(field_data_format == 5)
   field = read_date_time_no_seconds_value(field_data_file);
end

time=zeros(1000000,1);
max_model_time=max(model.time);
min_model_time=min(model.time);
count=1;

for i=1:length(field.time)
   if(field.time(i) <= max_model_time && field.time(i) >= min_model_time)
       time(count)=field.time(i);
       field_value(count)=field.value(i);
       count=count+1;
   end
end

match.time=time(1:count);
match.field_value=field_value(1:count);

match.model_value=spline(model.time,model.value,match.time);

%match=match_data(field.time,field.value,model.time,model.wse);

mean_model=mean(match.model_value);
mean_field=mean(match.field_value);
diff=mean_model-mean_field;
match.field_value=match.field_value + diff;
field.value=field.value + diff;

createfigure2(model.time,model.wse,field.time,field.value, ...
    field_data_file, y_label,plot_title);

set(gcf,'renderer','zbuffer');
opengl neverselect
set(gcf,'Backingstore','off','doublebuffer','on');

len=size(field_data_file);
name=field_data_file(1:(len(2)-4));

saveas(gcf,name,'png');
saveas(gcf,name,'fig');

close(gcf);

createfigure(match.field_value,match.model_value,field_data_file, ...
    y_label,plot_title);

set(gcf,'renderer','zbuffer');
opengl neverselect
set(gcf,'Backingstore','off','doublebuffer','on');


len=size(field_data_file);
name=field_data_file(1:(len(2)-4));
name=strcat(name,'_box');

saveas(gcf,name,'png');
saveas(gcf,name,'fig');

close(gcf)