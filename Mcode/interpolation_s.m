function interp = interpolation_s(filename,hourly_inc)

data=read_bush_canal_1(filename);

interp.time=min(data.time):hourly_inc*(1/24):max(data.time);

interp.value=spline(data.time,data.value,interp.time);

hold on

plot(data.time,data.value)

plot(interp.time,interp.value,'r')

datetick('x',2,'keepticks')

fid=fopen([filename(1:length(filename)-3) 'out'],'w');

time=(interp.time-min(interp.time))*24;

fprintf(fid,'%10.3f %10.3f\n',[time(:), interp.value(:)]');

fclose(fid);

