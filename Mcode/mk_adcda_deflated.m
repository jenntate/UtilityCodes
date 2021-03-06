function mk_adcda(basename,t0)
% MK_ADCDA Function to create ADCIRC direct access binary file
%          from ascii fort.63 and fort.64 files
% SYNTAX:  mk_adcda(basename,t0)
%    basename = string argument containing filename (without
%               extension for *.63 and *.64 file).  Note basename
%               must be the same for both files.
%         t0  = time of simulation start (datenum format or string)
%
%  MK_ADCDA creates a file in the local directory containing
%           water surface, u, v in a single file, compressing
%           filesize and speeding access of ADCIRC data.
%
% See also: LOAD_ADCDA, MK_STWDA, LOAD_STWDA
%
% Note: change made 27 July 2004 to write adc.time as
% float64 to accomodate Matlab datenum format.

error(nargchk(2,2,nargin))
tconv=1/3600/24;  %convert seconds to days
if ischar(t0),
   try
      t0=datenum(t0);
   catch ME
      fprintf(1,'The date string entered cannot be converted.\n')
      fprintf(1,'Try mm/dd/yyyy HH:MM:SS.\n\n')
      rethrow(ME)
   end
end
%check date and issue warning if necessary
if abs(now-t0)>100*365.25
   warning('MK_ADCDA:Date_Check','Date entered is more than 100 years from today. Is this correct?')
end
%open the 63 & 64 files
fid63=fopen([basename,'.63'],'rt');
fid64=fopen([basename,'.64'],'rt');

% read 63 header information
fgetl(fid63);
nt = fscanf(fid63,'%f ',1);
np = fscanf(fid63,'%f',1);
dt = fscanf(fid63,'%f',1);
fgetl(fid63);
fscanf(fid63,'%i',2);
%read 64 header
fgetl(fid64);
nt2 = fscanf(fid64,'%f ',1);
np2 = fscanf(fid64,'%f',1);
dt2 = fscanf(fid64,'%f',1);
fgetl(fid64);
fscanf(fid64,'%i',2);

%check for consistency
if nt2~=nt || np2~=np || dt2~=dt,
    error('File Inconsistency... check headers')
end

%for testing only
%nt=10;

%create da file and file header
fid=fopen([basename,'.da'],'wb','n');
fwrite(fid,[nt,np],'integer*4');
fwrite(fid,dt,'real*4');


%step through each file and get one timestep at a time
%read the water-surface info
fprintf(1,'Loading Timestep: %s','     ');
for k = 1:nt
    fprintf(1,'\b\b\b\b\b%5.0f',k);
    %read 63
    temp = fscanf(fid63,'%f %f %f',3);
    fgetl(fid63);
    %check for end of file
    if isempty(temp) 
        nt=k-1;
        fseek(fid,0,'bof');
        fwrite(fid,nt,'integer*4');
        break
    end
    t = temp(1);
%    disp(temp(1));
    eta(1:np)=-999999;
    node = textscan(fid63,'%f %f',temp(3));
    eta(node{1})=node{2};
    clear node temp3
    %read 64
    temp = fscanf(fid64,'%f %f %f',3);
    fgetl(fid64);
    node =textscan(fid64,'%f %f %f',temp(3));
    uv(1,1:np)=0.0;
    uv(2,1:np)=0.0;
    uv(1,node{1})=node{2};
    uv(2,node{1})=node{3};    
    %dump data to da file
    fwrite(fid,t*tconv+t0,'float64');       %time
    fwrite(fid,eta,'float32');     %all etas
    fwrite(fid,uv(1,:),'float32'); %all u
    fwrite(fid,uv(2,:),'float32'); %all v
    clear uv node eta
end %for k
fprintf(1,'\n');
fclose(fid63);
fclose(fid64);
fclose(fid);
