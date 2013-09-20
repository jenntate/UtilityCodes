function model=load_adcda(file,it)
%
%  Function to read all nodal results for a given time step fo the .da
%
%  model = structure array containing fields
%        .time (time [sec] from start of simulation)
%        .u    (east component of velocity [m/s])
%        .v    (north component of velocity [m/s])
%        .eta  (water surface elevation [m])
% file = file name for the .da file
%
% it   = time step iteration at which to receive data
%
%   Finished 3/12/2010
%

%% opens the .da binary model solution files

fid=fopen(file,'rb','n');

%% reads the header information

nt=fread(fid,1,'integer*4');
np=fread(fid,1,'integer*4');
dt=fread(fid,1,'real*4');

%% checks for consistency

if it>nt || it<1,
    fprintf(1,'NT= %4.0f, NP= %6.0f, DT= %5.3f\n',[nt,np,dt]);
    warning('Requested timestep out of range'),
    model.nt=nt;
    model.np=np;
    model.dt=dt;
    return
end

%% position file for reading
%  hdr bytes + (preceeding timestamps)* 4 bytes/rec * (np*3+2) recs

offset=4*3 + (it-1)*4*(np*3+2);
status=fseek(fid,offset,-1);

%% check for errors

if status<0,
   msg=ferror(fid);
   error(msg);
end

%% read data from da file

model.time=fread(fid,1,'float64');
model.eta=fread(fid,np,'float32');
model.u=fread(fid,np,'float32');
model.v=fread(fid,np,'float32');
model.dt=dt;
model.nt=nt;
model.np=np;

%% check again for errors during read

if numel(model.v)<np
   error('Reached end of file during read')
end

%% close the binary .da file

fclose(fid);
