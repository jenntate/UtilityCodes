%% Filtering for Gage dpia1h

filename='dpia1h_data_Original.txt';

fprintf(1,'Reading wind data for gage %s\n',filename);

data=read_noaa_bouy(filename);

count=1;
count1=1;

ws.time=data.time;
wd.time=data.time;
ws.value=data.ws;
wd.value=data.wd;

if(data.ws(1) < 90)
   ws.time(count)=data.time(1);
   ws.value(count)=data.ws(1);
   count=count+1;
end
if(data.wd(1) < 900)
   wd.time(count1)=data.time(1);
   wd.value(count1)=data.wd(1);
   count1=count1+1;
end

for i=2:length(data.time)
    if(data.time(i) > data.time(i-1))
      if(data.ws(i) < 90)
          ws.time(count)=data.time(i);
          ws.value(count)=data.ws(i);
          count=count+1;
      end
      if(data.wd(i) < 900)
          wd.time(count1)=data.time(i);
          wd.value(count1)=data.wd(i);
          count1=count1+1;
      end
    end
end

increment = 0.5;

fprintf(1,'Interpolating raw data to %5.2f increment\n',increment);

interp=interpolate_wind_data(ws.time(1:count-1),ws.value(1:count-1), ...
    wd.time(1:count1-1),wd.value(1:count1-1),increment);

% Length of the data set

N=length(interp.time);

% time step size of the data set

dt=increment;

% Cut Off frequency / Cut Off Period would be dt*1/fc

% Cut Off Period in hours

Cut_Off_Period=2.0;

fprintf(1,'Filtering out signals with periods less than %5.2f hours\n', ...
    Cut_Off_Period);

fc=1.0/(Cut_Off_Period);

% Nyquist Frequency

fn=1.0/(2.0*dt);

% filter length

if(length(interp.time) > 1000)
  nf=1000;
else
  nf=length(interp.time);
end

% mirror the ends of the data for a better fit

xm=mirrordataends(interp.x,N,nf);
ym=mirrordataends(interp.y,N,nf);

% filter the data using the lanczos low pass filter

fprintf(1,'Filtering Data\n');

[xmf,x_Rh,x_Fh]=filter_data(xm,dt,fc,nf,'lanczos','low');
[ymf,y_Rh,y_Fh]=filter_data(ym,dt,fc,nf,'lanczos','low');

xf=xmf(nf+1:N+nf);
yf=ymf(nf+1:N+nf);

filtered_x=xf;
filtered_y=yf;

fprintf(1,'Filtering Completed\n');
fprintf(1,'Plotting Raw Versus Filtered Data\n');

plots(interp.time,interp.x,filtered_x,interp.y,filtered_y)

set(gcf,'renderer','zbuffer');
opengl neverselect
set(gcf,'Backingstore','off','doublebuffer','on');

saveas(gcf,'raw_versus_filtered_dpia1h','fig'); 
close(gcf);

fprintf(1,'Outputting AdH formatted XY2 Series\n');

fid=fopen('output_dpia1h.txt','wb','n');

zero_hour_time='01/01/2008 00:00:00';

out_time=(interp.time-(datenum(datestr(zero_hour_time))))*24;

fprintf(fid,'!!! Data from Station = dpia1h\n');
fprintf(fid,'!!! Data hour 0 = %s\n',zero_hour_time);
fprintf(fid,'!!! Data measurements start %s\n',datestr(interp.time(1)));
fprintf(fid,'XY2  1  %i %i %i %i %i\n', length(out_time), 2, 0, 0, 0);

for i=1:length(interp.time)
  fprintf(fid,'%8.2f %8.4f %8.4f\n',out_time(i),filtered_x(i),filtered_y(i));
end

fclose(fid);

clear

%% Filtering for Gage fmoa1h

filename='fmoa1h_filled_with_dpia1h_data.txt';

fprintf(1,'Reading wind data for gage %s\n',filename);

data=read_noaa_bouy(filename);

count=1;
count1=1;

ws.time=data.time;
wd.time=data.time;
ws.value=data.ws;
wd.value=data.wd;

if(data.ws(1) < 90)
   ws.time(count)=data.time(1);
   ws.value(count)=data.ws(1);
   count=count+1;
end
if(data.wd(1) < 900)
   wd.time(count1)=data.time(1);
   wd.value(count1)=data.wd(1);
   count1=count1+1;
end

for i=2:length(data.time)
    if(data.time(i) > data.time(i-1))
      if(data.ws(i) < 90)
          ws.time(count)=data.time(i);
          ws.value(count)=data.ws(i);
          count=count+1;
      end
      if(data.wd(i) < 900)
          wd.time(count1)=data.time(i);
          wd.value(count1)=data.wd(i);
          count1=count1+1;
      end
    end
end

increment = 0.5;

fprintf(1,'Interpolating raw data to %5.2f increment\n',increment);

interp=interpolate_wind_data(ws.time(1:count-1),ws.value(1:count-1), ...
    wd.time(1:count1-1),wd.value(1:count1-1),increment);

% Length of the data set

N=length(interp.time);

% time step size of the data set

dt=increment;

% Cut Off frequency / Cut Off Period would be dt*1/fc

% Cut Off Period in hours

Cut_Off_Period=2.0;

fprintf(1,'Filtering out signals with periods less than %5.2f hours\n', ...
    Cut_Off_Period);

fc=1.0/(Cut_Off_Period);

% Nyquist Frequency

fn=1.0/(2.0*dt);

% filter length

if(length(interp.time) > 1000)
  nf=1000;
else
  nf=length(interp.time);
end

% mirror the ends of the data for a better fit

xm=mirrordataends(interp.x,N,nf);
ym=mirrordataends(interp.y,N,nf);

% filter the data using the lanczos low pass filter

fprintf(1,'Filtering Data\n');

[xmf,x_Rh,x_Fh]=filter_data(xm,dt,fc,nf,'lanczos','low');
[ymf,y_Rh,y_Fh]=filter_data(ym,dt,fc,nf,'lanczos','low');

xf=xmf(nf+1:N+nf);
yf=ymf(nf+1:N+nf);

filtered_x=xf;
filtered_y=yf;

fprintf(1,'Filtering Completed\n');
fprintf(1,'Plotting Raw Versus Filtered Data\n');

plots(interp.time,interp.x,filtered_x,interp.y,filtered_y)

set(gcf,'renderer','zbuffer');
opengl neverselect
set(gcf,'Backingstore','off','doublebuffer','on');

saveas(gcf,'raw_versus_filtered_fmoa1h','fig'); 
close(gcf);

fprintf(1,'Outputting AdH formatted XY2 Series\n');

fid=fopen('output_fmoa1h.txt','wb','n');

zero_hour_time='01/01/2008 00:00:00';

out_time=(interp.time-(datenum(datestr(zero_hour_time))))*24;

fprintf(fid,'!!! Data from Station = fmoa1h\n');
fprintf(fid,'!!! Data hour 0 = %s\n',zero_hour_time);
fprintf(fid,'!!! Data measurements start %s\n',datestr(interp.time(1)));
fprintf(fid,'XY2  1  %i %i %i %i %i\n', length(out_time), 2, 0, 0, 0);

for i=1:length(interp.time)
  fprintf(fid,'%8.2f %8.4f %8.4f\n',out_time(i),filtered_x(i),filtered_y(i));
end

fclose(fid);

clear

%% Filtering for Gage gdxm6h

filename='gdxm6h_filled_with_dpia1h_data.txt';

fprintf(1,'Reading wind data for gage %s\n',filename);

data=read_noaa_bouy(filename);

count=1;
count1=1;

ws.time=data.time;
wd.time=data.time;
ws.value=data.ws;
wd.value=data.wd;

if(data.ws(1) < 90)
   ws.time(count)=data.time(1);
   ws.value(count)=data.ws(1);
   count=count+1;
end
if(data.wd(1) < 900)
   wd.time(count1)=data.time(1);
   wd.value(count1)=data.wd(1);
   count1=count1+1;
end

for i=2:length(data.time)
    if(data.time(i) > data.time(i-1))
      if(data.ws(i) < 90)
          ws.time(count)=data.time(i);
          ws.value(count)=data.ws(i);
          count=count+1;
      end
      if(data.wd(i) < 900)
          wd.time(count1)=data.time(i);
          wd.value(count1)=data.wd(i);
          count1=count1+1;
      end
    end
end

increment = 0.5;

fprintf(1,'Interpolating raw data to %5.2f increment\n',increment);

interp=interpolate_wind_data(ws.time(1:count-1),ws.value(1:count-1), ...
    wd.time(1:count1-1),wd.value(1:count1-1),increment);

% Length of the data set

N=length(interp.time);

% time step size of the data set

dt=increment;

% Cut Off frequency / Cut Off Period would be dt*1/fc

% Cut Off Period in hours

Cut_Off_Period=2.0;

fprintf(1,'Filtering out signals with periods less than %5.2f hours\n', ...
    Cut_Off_Period);

fc=1.0/(Cut_Off_Period);

% Nyquist Frequency

fn=1.0/(2.0*dt);

% filter length

if(length(interp.time) > 1000)
  nf=1000;
else
  nf=length(interp.time);
end

% mirror the ends of the data for a better fit

xm=mirrordataends(interp.x,N,nf);
ym=mirrordataends(interp.y,N,nf);

% filter the data using the lanczos low pass filter

fprintf(1,'Filtering Data\n');

[xmf,x_Rh,x_Fh]=filter_data(xm,dt,fc,nf,'lanczos','low');
[ymf,y_Rh,y_Fh]=filter_data(ym,dt,fc,nf,'lanczos','low');

xf=xmf(nf+1:N+nf);
yf=ymf(nf+1:N+nf);

filtered_x=xf;
filtered_y=yf;

fprintf(1,'Filtering Completed\n');
fprintf(1,'Plotting Raw Versus Filtered Data\n');

plots(interp.time,interp.x,filtered_x,interp.y,filtered_y)

set(gcf,'renderer','zbuffer');
opengl neverselect
set(gcf,'Backingstore','off','doublebuffer','on');

saveas(gcf,'raw_versus_filtered_gdxm6h','fig'); 
close(gcf);

fprintf(1,'Outputting AdH formatted XY2 Series\n');

fid=fopen('output_gdxm6h.txt','wb','n');

zero_hour_time='01/01/2008 00:00:00';

out_time=(interp.time-(datenum(datestr(zero_hour_time))))*24;

fprintf(fid,'!!! Data from Station = gdxm6h\n');
fprintf(fid,'!!! Data hour 0 = %s\n',zero_hour_time);
fprintf(fid,'!!! Data measurements start %s\n',datestr(interp.time(1)));
fprintf(fid,'XY2  1  %i %i %i %i %i\n', length(out_time), 2, 0, 0, 0);

for i=1:length(interp.time)
  fprintf(fid,'%8.2f %8.4f %8.4f\n',out_time(i),filtered_x(i),filtered_y(i));
end

fclose(fid);

clear

%% Filtering for Gage mbla1h

filename='mbla1h_filled_with_dpia1h_data.txt';

fprintf(1,'Reading wind data for gage %s\n',filename);

data=read_noaa_bouy(filename);

count=1;
count1=1;

ws.time=data.time;
wd.time=data.time;
ws.value=data.ws;
wd.value=data.wd;

if(data.ws(1) < 90)
   ws.time(count)=data.time(1);
   ws.value(count)=data.ws(1);
   count=count+1;
end
if(data.wd(1) < 900)
   wd.time(count1)=data.time(1);
   wd.value(count1)=data.wd(1);
   count1=count1+1;
end

for i=2:length(data.time)
    if(data.time(i) > data.time(i-1))
      if(data.ws(i) < 90)
          ws.time(count)=data.time(i);
          ws.value(count)=data.ws(i);
          count=count+1;
      end
      if(data.wd(i) < 900)
          wd.time(count1)=data.time(i);
          wd.value(count1)=data.wd(i);
          count1=count1+1;
      end
    end
end

increment = 0.5;

fprintf(1,'Interpolating raw data to %5.2f increment\n',increment);

interp=interpolate_wind_data(ws.time(1:count-1),ws.value(1:count-1), ...
    wd.time(1:count1-1),wd.value(1:count1-1),increment);

% Length of the data set

N=length(interp.time);

% time step size of the data set

dt=increment;

% Cut Off frequency / Cut Off Period would be dt*1/fc

% Cut Off Period in hours

Cut_Off_Period=2.0;

fprintf(1,'Filtering out signals with periods less than %5.2f hours\n', ...
    Cut_Off_Period);

fc=1.0/(Cut_Off_Period);

% Nyquist Frequency

fn=1.0/(2.0*dt);

% filter length

if(length(interp.time) > 1000)
  nf=1000;
else
  nf=length(interp.time);
end

% mirror the ends of the data for a better fit

xm=mirrordataends(interp.x,N,nf);
ym=mirrordataends(interp.y,N,nf);

% filter the data using the lanczos low pass filter

fprintf(1,'Filtering Data\n');

[xmf,x_Rh,x_Fh]=filter_data(xm,dt,fc,nf,'lanczos','low');
[ymf,y_Rh,y_Fh]=filter_data(ym,dt,fc,nf,'lanczos','low');

xf=xmf(nf+1:N+nf);
yf=ymf(nf+1:N+nf);

filtered_x=xf;
filtered_y=yf;

fprintf(1,'Filtering Completed\n');
fprintf(1,'Plotting Raw Versus Filtered Data\n');

plots(interp.time,interp.x,filtered_x,interp.y,filtered_y)

set(gcf,'renderer','zbuffer');
opengl neverselect
set(gcf,'Backingstore','off','doublebuffer','on');

saveas(gcf,'raw_versus_filtered_mbla1h','fig'); 
close(gcf);

fprintf(1,'Outputting AdH formatted XY2 Series\n');

fid=fopen('output_mbla1h.txt','wb','n');

zero_hour_time='01/01/2008 00:00:00';

out_time=(interp.time-(datenum(datestr(zero_hour_time))))*24;

fprintf(fid,'!!! Data from Station = mbla1h\n');
fprintf(fid,'!!! Data hour 0 = %s\n',zero_hour_time);
fprintf(fid,'!!! Data measurements start %s\n',datestr(interp.time(1)));
fprintf(fid,'XY2  1  %i %i %i %i %i\n', length(out_time), 2, 0, 0, 0);

for i=1:length(interp.time)
  fprintf(fid,'%8.2f %8.4f %8.4f\n',out_time(i),filtered_x(i),filtered_y(i));
end

fclose(fid);

clear


%% Filtering for Gage mcga1h

filename='mcga1h_filled_with_mhpa1h_data.txt';

fprintf(1,'Reading wind data for gage %s\n',filename);

data=read_noaa_bouy(filename);

count=1;
count1=1;

ws.time=data.time;
wd.time=data.time;
ws.value=data.ws;
wd.value=data.wd;

if(data.ws(1) < 90)
   ws.time(count)=data.time(1);
   ws.value(count)=data.ws(1);
   count=count+1;
end
if(data.wd(1) < 900)
   wd.time(count1)=data.time(1);
   wd.value(count1)=data.wd(1);
   count1=count1+1;
end

for i=2:length(data.time)
    if(data.time(i) > data.time(i-1))
      if(data.ws(i) < 90)
          ws.time(count)=data.time(i);
          ws.value(count)=data.ws(i);
          count=count+1;
      end
      if(data.wd(i) < 900)
          wd.time(count1)=data.time(i);
          wd.value(count1)=data.wd(i);
          count1=count1+1;
      end
    end
end

increment = 0.5;

fprintf(1,'Interpolating raw data to %5.2f increment\n',increment);

interp=interpolate_wind_data(ws.time(1:count-1),ws.value(1:count-1), ...
    wd.time(1:count1-1),wd.value(1:count1-1),increment);

% Length of the data set

N=length(interp.time);

% time step size of the data set

dt=increment;

% Cut Off frequency / Cut Off Period would be dt*1/fc

% Cut Off Period in hours

Cut_Off_Period=2.0;

fprintf(1,'Filtering out signals with periods less than %5.2f hours\n', ...
    Cut_Off_Period);

fc=1.0/(Cut_Off_Period);

% Nyquist Frequency

fn=1.0/(2.0*dt);

% filter length

if(length(interp.time) > 1000)
  nf=1000;
else
  nf=length(interp.time);
end

% mirror the ends of the data for a better fit

xm=mirrordataends(interp.x,N,nf);
ym=mirrordataends(interp.y,N,nf);

% filter the data using the lanczos low pass filter

fprintf(1,'Filtering Data\n');

[xmf,x_Rh,x_Fh]=filter_data(xm,dt,fc,nf,'lanczos','low');
[ymf,y_Rh,y_Fh]=filter_data(ym,dt,fc,nf,'lanczos','low');

xf=xmf(nf+1:N+nf);
yf=ymf(nf+1:N+nf);

filtered_x=xf;
filtered_y=yf;

fprintf(1,'Filtering Completed\n');
fprintf(1,'Plotting Raw Versus Filtered Data\n');

plots(interp.time,interp.x,filtered_x,interp.y,filtered_y)

set(gcf,'renderer','zbuffer');
opengl neverselect
set(gcf,'Backingstore','off','doublebuffer','on');

saveas(gcf,'raw_versus_filtered_mcga1h','fig'); 
close(gcf);

fprintf(1,'Outputting AdH formatted XY2 Series\n');

fid=fopen('output_mcga1h.txt','wb','n');

zero_hour_time='01/01/2008 00:00:00';

out_time=(interp.time-(datenum(datestr(zero_hour_time))))*24;

fprintf(fid,'!!! Data from Station = mcga1h\n');
fprintf(fid,'!!! Data hour 0 = %s\n',zero_hour_time);
fprintf(fid,'!!! Data measurements start %s\n',datestr(interp.time(1)));
fprintf(fid,'XY2  1  %i %i %i %i %i\n', length(out_time), 2, 0, 0, 0);

for i=1:length(interp.time)
  fprintf(fid,'%8.2f %8.4f %8.4f\n',out_time(i),filtered_x(i),filtered_y(i));
end

fclose(fid);

clear

%% Filtering for Gage mhpa1h

filename='mhpa1h_filled_with_mcga1h_data.txt';

fprintf(1,'Reading wind data for gage %s\n',filename);

data=read_noaa_bouy(filename);

count=1;
count1=1;

ws.time=data.time;
wd.time=data.time;
ws.value=data.ws;
wd.value=data.wd;

if(data.ws(1) < 90)
   ws.time(count)=data.time(1);
   ws.value(count)=data.ws(1);
   count=count+1;
end
if(data.wd(1) < 900)
   wd.time(count1)=data.time(1);
   wd.value(count1)=data.wd(1);
   count1=count1+1;
end

for i=2:length(data.time)
    if(data.time(i) > data.time(i-1))
      if(data.ws(i) < 90)
          ws.time(count)=data.time(i);
          ws.value(count)=data.ws(i);
          count=count+1;
      end
      if(data.wd(i) < 900)
          wd.time(count1)=data.time(i);
          wd.value(count1)=data.wd(i);
          count1=count1+1;
      end
    end
end

increment = 0.5;

fprintf(1,'Interpolating raw data to %5.2f increment\n',increment);

interp=interpolate_wind_data(ws.time(1:count-1),ws.value(1:count-1), ...
    wd.time(1:count1-1),wd.value(1:count1-1),increment);

% Length of the data set

N=length(interp.time);

% time step size of the data set

dt=increment;

% Cut Off frequency / Cut Off Period would be dt*1/fc

% Cut Off Period in hours

Cut_Off_Period=2.0;

fprintf(1,'Filtering out signals with periods less than %5.2f hours\n', ...
    Cut_Off_Period);

fc=1.0/(Cut_Off_Period);

% Nyquist Frequency

fn=1.0/(2.0*dt);

% filter length

if(length(interp.time) > 1000)
  nf=1000;
else
  nf=length(interp.time);
end

% mirror the ends of the data for a better fit

xm=mirrordataends(interp.x,N,nf);
ym=mirrordataends(interp.y,N,nf);

% filter the data using the lanczos low pass filter

fprintf(1,'Filtering Data\n');

[xmf,x_Rh,x_Fh]=filter_data(xm,dt,fc,nf,'lanczos','low');
[ymf,y_Rh,y_Fh]=filter_data(ym,dt,fc,nf,'lanczos','low');

xf=xmf(nf+1:N+nf);
yf=ymf(nf+1:N+nf);

filtered_x=xf;
filtered_y=yf;

fprintf(1,'Filtering Completed\n');
fprintf(1,'Plotting Raw Versus Filtered Data\n');

plots(interp.time,interp.x,filtered_x,interp.y,filtered_y)

set(gcf,'renderer','zbuffer');
opengl neverselect
set(gcf,'Backingstore','off','doublebuffer','on');

saveas(gcf,'raw_versus_filtered_mhpa1h','fig'); 
close(gcf);

fprintf(1,'Outputting AdH formatted XY2 Series\n');

fid=fopen('output_mhpa1h.txt','wb','n');

zero_hour_time='01/01/2008 00:00:00';

out_time=(interp.time-(datenum(datestr(zero_hour_time))))*24;

fprintf(fid,'!!! Data from Station = mhpa1h\n');
fprintf(fid,'!!! Data hour 0 = %s\n',zero_hour_time);
fprintf(fid,'!!! Data measurements start %s\n',datestr(interp.time(1)));
fprintf(fid,'XY2  1  %i %i %i %i %i\n', length(out_time), 2, 0, 0, 0);

for i=1:length(interp.time)
  fprintf(fid,'%8.2f %8.4f %8.4f\n',out_time(i),filtered_x(i),filtered_y(i));
end

fclose(fid);

clear

%% Filtering for Gage wkxa1h

filename='wkxa1h_filled_with_dpia1h_data.txt';

fprintf(1,'Reading wind data for gage %s\n',filename);

data=read_noaa_bouy(filename);

count=1;
count1=1;

ws.time=data.time;
wd.time=data.time;
ws.value=data.ws;
wd.value=data.wd;

if(data.ws(1) < 90)
   ws.time(count)=data.time(1);
   ws.value(count)=data.ws(1);
   count=count+1;
end
if(data.wd(1) < 900)
   wd.time(count1)=data.time(1);
   wd.value(count1)=data.wd(1);
   count1=count1+1;
end

for i=2:length(data.time)
    if(data.time(i) > data.time(i-1))
      if(data.ws(i) < 90)
          ws.time(count)=data.time(i);
          ws.value(count)=data.ws(i);
          count=count+1;
      end
      if(data.wd(i) < 900)
          wd.time(count1)=data.time(i);
          wd.value(count1)=data.wd(i);
          count1=count1+1;
      end
    end
end

increment = 0.5;

fprintf(1,'Interpolating raw data to %5.2f increment\n',increment);

interp=interpolate_wind_data(ws.time(1:count-1),ws.value(1:count-1), ...
    wd.time(1:count1-1),wd.value(1:count1-1),increment);

% Length of the data set

N=length(interp.time);

% time step size of the data set

dt=increment;

% Cut Off frequency / Cut Off Period would be dt*1/fc

% Cut Off Period in hours

Cut_Off_Period=2.0;

fprintf(1,'Filtering out signals with periods less than %5.2f hours\n', ...
    Cut_Off_Period);

fc=1.0/(Cut_Off_Period);

% Nyquist Frequency

fn=1.0/(2.0*dt);

% filter length

if(length(interp.time) > 1000)
  nf=1000;
else
  nf=length(interp.time);
end

% mirror the ends of the data for a better fit

xm=mirrordataends(interp.x,N,nf);
ym=mirrordataends(interp.y,N,nf);

% filter the data using the lanczos low pass filter

fprintf(1,'Filtering Data\n');

[xmf,x_Rh,x_Fh]=filter_data(xm,dt,fc,nf,'lanczos','low');
[ymf,y_Rh,y_Fh]=filter_data(ym,dt,fc,nf,'lanczos','low');

xf=xmf(nf+1:N+nf);
yf=ymf(nf+1:N+nf);

filtered_x=xf;
filtered_y=yf;

fprintf(1,'Filtering Completed\n');
fprintf(1,'Plotting Raw Versus Filtered Data\n');

plots(interp.time,interp.x,filtered_x,interp.y,filtered_y)

set(gcf,'renderer','zbuffer');
opengl neverselect
set(gcf,'Backingstore','off','doublebuffer','on');

saveas(gcf,'raw_versus_filtered_wkxa1h','fig'); 
close(gcf);

fprintf(1,'Outputting AdH formatted XY2 Series\n');

fid=fopen('output_wkxa1h.txt','wb','n');

zero_hour_time='01/01/2008 00:00:00';

out_time=(interp.time-(datenum(datestr(zero_hour_time))))*24;

fprintf(fid,'!!! Data from Station = wkxa1h\n');
fprintf(fid,'!!! Data hour 0 = %s\n',zero_hour_time);
fprintf(fid,'!!! Data measurements start %s\n',datestr(interp.time(1)));
fprintf(fid,'XY2  1  %i %i %i %i %i\n', length(out_time), 2, 0, 0, 0);

for i=1:length(interp.time)
  fprintf(fid,'%8.2f %8.4f %8.4f\n',out_time(i),filtered_x(i),filtered_y(i));
end

fclose(fid);

clear
