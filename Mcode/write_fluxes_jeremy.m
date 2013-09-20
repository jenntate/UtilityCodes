function write_fluxes_jeremy(data,file,time_step)

fid=fopen(file,'w');



%write header
start_time=min(data.time);
stop_time=max(data.time);
model.time=datenum(start_time):time_step/24.0:datenum(stop_time);
fprintf(fid,'        Time                ');
for i=1:length(data.mapping)
   
    fprintf(fid,['Discharge String Number ' int2str(data.mapping(i))]);
    model.fluxes(:,i)=spline(data.time,data.fluxes(:,i),model.time);
    fprintf(fid,'     ');
    fprintf(fid,['WSE String Number ' int2str(data.mapping(i))]);
    model.wse(:,i)=spline(data.time,data.wse(:,i),model.time);    
    fprintf(fid,'     ');
    fprintf(fid,['WSE Min String Number ' int2str(data.mapping(i))]);
    model.wsemin(:,i)=spline(data.time,data.wsemin(:,i),model.time);      
    fprintf(fid,'     ');
    fprintf(fid,['WSE Max String Number ' int2str(data.mapping(i))]);
    model.wsemax(:,i)=spline(data.time,data.wsemax(:,i),model.time);     
    fprintf(fid,'     ');
end
fprintf(fid,'\n');

for i=1:length(model.time)
    
    fprintf(fid,'%s',datestr(model.time(i)));
    for j=1:length(data.mapping)
       
        fprintf(fid,'%27.4f %27.4f %27.4f %27.4f',model.fluxes(i,j),...
            model.wse(i,j),model.wsemin(i,j),model.wsemax(i,j));
        
    end
    fprintf(fid,'\n');
end





