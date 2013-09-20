function write_adh_formatted_fluxes(basename,data,t0)

fid=fopen([basename '_tflx'],'w');
dt=data.time(2)-data.time(1);

fprintf(fid,'Time %.10e Flux-Dt %.10e\n',(data.time(1)-datenum(t0))*86400,dt);
for j=1:length(data.discharges(:,1))
   fprintf(fid,'String %2i Flux %4.4e WSE %2.4e WSEMin %2.4e WSEMax %2.4e\n', ...
       j, data.discharges(j,1),0.0,0.0,0.0);
end

for i=2:length(data.time)
    dt=data.time(i)-data.time(i-1);
    fprintf(fid,'Time %.10e Flux-Dt %.10e\n',(data.time(i)-datenum(t0))*86400,dt);
    for j=1:length(data.discharges(:,i))
       fprintf(fid,'String %2i Flux %2.4e WSE %2.4e WSEMin %2.4e WSEMax %2.4e\n', ...
           j, data.discharges(j,i),0.0,0.0,0.0);
    end    
end

fclose(fid);