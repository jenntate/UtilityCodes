function adc=load_da_stepnew(file,it)
%LOAD_ADCDA. function to load ADCIRC direct access (binary) file.
%  DA files are produced by the function MK_ADCDA
%USAGE: adc=load_adcda(file,itime)
%  adc = structure array containing fields
%        .time (time [sec] from start of simulation)
%        .u    (east component of velocity [m/s])
%        .v    (north component of velocity [m/s])
%        .eta  (water surface elevation [m])
% file = filename (with path if not in current directory)
% itime= index of timestamps saved
%        note if number of nodes, timestamps, or DT is returned if 
%        itime is specified as 0.
%
%see also: MK_ADCDA, LOAD_STWDA, MK_STWDA

%open the file

fid=fopen(file,'rb','n');

%read header information

nt=fread(fid,1,'integer*4');
np=fread(fid,1,'integer*4');
dt=fread(fid,1,'real*4');
temp=zeros(19,1);
for i=1:19
    temp(i)=fread(fid,1,'integer*4');
end

dep_check=temp(1);
wse_check=temp(2);
vel_check=temp(3);
err_check=temp(4);
sal_check=temp(5);
vor_check=temp(6);
snd_check=temp(7);
slt_check=temp(8);
smr_check=temp(9);
dpl_check=temp(10);
bsh_check=temp(11);
blt_check=temp(12);
bld_check=temp(13);
bed_check=temp(14);
alt_check=temp(15);
alb_check=temp(16);
cbp_check=temp(17);
number_layers=temp(18);
number_sed=temp(19);

%check consistency

if it>nt || it<1,
    fprintf(1,'NT= %4.0f, NP= %6.0f, DT= %5.3f\n',[nt,np,dt]);
%    warning('Requested timestep out of range'),
    adc.nt=nt;
    adc.np=np;
    adc.dt=dt;
    adc.wse_check=wse_check;
    adc.dep_check=dep_check;
    adc.vel_check=vel_check;
    adc.err_check=err_check;
    adc.sal_check=sal_check;
    adc.vor_check=vor_check;
    adc.snd_check=snd_check;
    adc.slt_check=slt_check;
    adc.smr_check=smr_check;
    adc.dpl_check=dpl_check;
    adc.bsh_check=bsh_check;
    adc.blt_check=blt_check;
    adc.bld_check=bld_check;
    adc.bed_check=bed_check;
    adc.alt_check=alt_check;
    adc.alb_check=alb_check;
    adc.cbp_check=cbp_check;
    adc.number_layers=number_layers;
    adc.number_sed=number_sed;
    return
end

%position file
% hdr bytes + (preceeding timestamps)* 4 bytes/rec * (np*3+2) recs

values=sum(temp(1:length(temp)-2));
if(vel_check > 0)
    values=values+1;
end
if(bed_check > 0)
    values=values+1;
end
if(slt_check > 0)
    values=values+slt_check*2;
end
if(snd_check > 0)
    values=values+snd_check*2;
end
if(bld_check > 0)
    values=values+bld_check*(number_sed-1);
end
if(cbp_check > 0)
    values=values+cbp_check*3;
end

offset=(4)*(3+length(temp)) + (it-1)*4*(np*values+2);
status=fseek(fid,offset,-1);

%check for errors

if status<0,
   msg=ferror(fid);
   error(msg);
end

%read data

adc.time=fread(fid,1,'float64');
if(dep_check > 0)
  adc.dep=fread(fid,np,'float32');
end
if(wse_check > 0)
  adc.wse=fread(fid,np,'float32');
end
if(vel_check > 0)
  adc.u=fread(fid,np,'float32');
  adc.v=fread(fid,np,'float32');
end
if(err_check > 0)
  adc.err=fread(fid,np,'float32');
end
if(sal_check > 0)
  adc.sal=fread(fid,np,'float32');
end
if(vor_check > 0)
  adc.vor=fread(fid,np,'float32');
end
if(snd_check > 0)
  for i=1:snd_check
    adc.snd(i,:)=fread(fid,np,'float32');
    adc.snd_rouse(i,:)=fread(fid,np,'float32');
    adc.snd_bdma(i,:)=fread(fid,np,'float32');
  end
end
if(slt_check > 0)
  for i=1:slt_check
    adc.slt(i,:)=fread(fid,np,'float32');
    adc.slt_rouse(i,:)=fread(fid,np,'float32');
    adc.slt_bdma(i,:)=fread(fid,np,'float32');    
  end
end
if(smr_check > 0)
  for i=1:smr_check
    adc.smr(i,:)=fread(fid,np,'float32');
  end  
end
if(dpl_check > 0)
  adc.dpl=fread(fid,np,'float32');
end
if(bsh_check > 0)
  adc.bsh=fread(fid,np,'float32');
end
if(blt_check > 0)
  for i=1:blt_check
    adc.blt(i,:)=fread(fid,np,'float32');
  end
end
if(bld_check > 0)
  for i=1:bld_check
    for j=1:number_sed
      adc.bld(i,j,:)=fread(fid,np,'float32');
    end
  end
end
if(bed_check > 0)
  adc.bed_u=fread(fid,np,'float32');
  adc.bed_v=fread(fid,np,'float32');
end
if(alt_check > 0)
  adc.alt=fread(fid,np,'float32');
end
if(alb_check > 0)
  for i=1:alb_check
    adc.alb(i,:)=fread(fid,np,'float32');
  end 
end
if(cbp_check > 0)
  for i=1:cbp_check
    adc.cbp_den(i,:)=fread(fid,np,'float32');
    adc.cbp_ces(i,:)=fread(fid,np,'float32');
    adc.cbp_erc(i,:)=fread(fid,np,'float32');
    adc.cbp_ere(i,:)=fread(fid,np,'float32');
  end
end


adc.dt=dt;
adc.nt=nt;
adc.np=np;

%check again for errors during read

%if numel(adc.v)<np
%   error('Reached end of file during read')
%end

fclose(fid);