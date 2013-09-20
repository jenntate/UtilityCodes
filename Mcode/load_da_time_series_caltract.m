function adc=load_da_time_series_caltract(file,node)
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
fid=fopen([file],'rt','n');
%read header information
nt=fread(fid,1,'integer*4');
np=fread(fid,1,'integer*4');
dt=fread(fid,1,'real*4');

dep_check=fread(fid,1,'integer*4');
wse_check=fread(fid,1,'integer*4');
vel_check=fread(fid,1,'integer*4');
err_check=fread(fid,1,'integer*4');
sal_check=fread(fid,1,'integer*4');
vor_check=fread(fid,1,'integer*4');
snd_check=fread(fid,1,'integer*4');
slt_check=fread(fid,1,'integer*4');
smr_check=fread(fid,1,'integer*4');
dpl_check=fread(fid,1,'integer*4');
bsh_check=fread(fid,1,'integer*4');
blt_check=fread(fid,1,'integer*4');
bld_check=fread(fid,1,'integer*4');
bed_check=fread(fid,1,'integer*4');
alt_check=fread(fid,1,'integer*4');
alb_check=fread(fid,1,'integer*4');
cbp_check=fread(fid,1,'integer*4');
number_layers=fread(fid,1,'integer*4');
number_sed=fread(fid,1,'integer*4');

temp=dep_check+wse_check+vel_check+err_check+sal_check+vor_check+snd_check+ ...
    slt_check+smr_check+dpl_check+bsh_check+blt_check+bld_check+bed_check+...
    alt_check+alb_check;

values=temp;

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

a=3+19;
%b=np;
d=np*values+2;
%e = a+c*d;
%offset = 4.0 * e;

% position file
% hdr bytes + (preceeding timestamps)* 4 bytes/rec * (np*3+2) recs
% offset=4*3 + (it-1)*4*(np*3+2);

for i=1:nt
    check=0;
    offset=4*((a)+(i-1)*(d));
    status=fseek(fid,offset,-1);
    if status<0,
      msg=ferror(fid);
      error(msg);
    end
    adc.time(i)=fread(fid,1,'float64');
    offset=offset+2*4;
    if(dep_check > 0)
      check=1;
      offset=offset+(node-1)*4;
      status=fseek(fid,offset,-1);
      if status<0,
        msg=ferror(fid);
        error(msg);
      end
      adc.dep(i)=fread(fid,1,'float32');
    end
    if(wse_check > 0)
      if(check==0)
        check=1;
        offset=offset+(node-1)*4;
      else
        offset=offset+(np)*4;
      end
      status=fseek(fid,offset,-1);
      if status<0,
        msg=ferror(fid);
        error(msg);
      end
      adc.wse(i)=fread(fid,1,'float32');
    end
    if(vel_check > 0)
      if(check==0)
        check=1;
        offset=offset+(node-1)*4;
      else
        offset=offset+(np)*4;
      end
      status=fseek(fid,offset,-1);
      if status<0,
        msg=ferror(fid);
        error(msg);
      end    
      adc.u(i)=fread(fid,1,'float32');
      offset=offset+np*4;
      status=fseek(fid,offset,-1);
      if status<0,
        msg=ferror(fid);
        error(msg);
      end    
      adc.v(i)=fread(fid,1,'float32');
    end
    if(err_check > 0)
      if(check==0)
        check=1;
        offset=offset+(node-1)*4;
      else
        offset=offset+(np)*4;
      end
      status=fseek(fid,offset,-1);
      if status<0,
        msg=ferror(fid);
        error(msg);
      end
      adc.err(i)=fread(fid,1,'float32');
    end    
    if(sal_check > 0)
      if(check==0)
        check=1;
        offset=offset+(node-1)*4;
      else
        offset=offset+(np)*4;
      end
      status=fseek(fid,offset,-1);
      if status<0,
        msg=ferror(fid);
        error(msg);
      end
      adc.sal(i)=fread(fid,1,'float32');
    end    
    if(vor_check > 0)
      if(check==0)
        check=1;
        offset=offset+(node-1)*4;
      else
        offset=offset+(np)*4;
      end
      status=fseek(fid,offset,-1);
      if status<0,
        msg=ferror(fid);
        error(msg);
      end
      adc.vor(i)=fread(fid,1,'float32');
    end    
    if(snd_check > 0)
      for j=1:snd_check
        if(check==0)
          check=1;
          offset=offset+(node-1)*4;
        else
          offset=offset+(np)*4;
        end
        status=fseek(fid,offset,-1);
        if status<0,
          msg=ferror(fid);
          error(msg);
        end
        adc.snd(j,i)=fread(fid,1,'float32');
        offset=offset+(np)*4;
        status=fseek(fid,offset,-1);
        if status<0,
          msg=ferror(fid);
          error(msg);
        end
        adc.snd_rouse(j,i)=fread(fid,1,'float32');
        offset=offset+(np)*4;
        status=fseek(fid,offset,-1);
        if status<0,
          msg=ferror(fid);
          error(msg);
        end
        adc.snd_bdma(j,i)=fread(fid,1,'float32');        
      end
    end      
    if(slt_check > 0)
      for j=1:slt_check
        if(check==0)
          check=1;
          offset=offset+(node-1)*4;
        else
          offset=offset+(np)*4;
        end
        status=fseek(fid,offset,-1);
        if status<0,
          msg=ferror(fid);
          error(msg);
        end
        adc.slt(j,i)=fread(fid,1,'float32');
        offset=offset+(np)*4;
        status=fseek(fid,offset,-1);
        if status<0,
          msg=ferror(fid);
          error(msg);
        end
        adc.slt_rouse(j,i)=fread(fid,1,'float32');
        offset=offset+(np)*4;
        status=fseek(fid,offset,-1);
        if status<0,
          msg=ferror(fid);
          error(msg);
        end
        adc.slt_bdma(j,i)=fread(fid,1,'float32');          
      end
    end     
    if(smr_check > 0)
      for h=1:smr_check
        if(check==0)
          check=1;
          offset=offset+(node-1)*4;
        else
          offset=offset+np*4;
        end
        status=fseek(fid,offset,-1);
        if status<0,
          msg=ferror(fid);
          error(msg);
        end
        adc.smr(h,i)=fread(fid,1,'float32');
      end
    end      
    if(dpl_check > 0)
      if(check==0)
        check=1;
        offset=offset+(node-1)*4;
      else
        offset=offset+np*4;
      end
      status=fseek(fid,offset,-1);
      if status<0,
        msg=ferror(fid);
        error(msg);
      end
      adc.dpl(i)=fread(fid,1,'float32');
    end      
    if(bsh_check > 0)
      if(check==0)
        check=1;
        offset=offset+(node-1)*4;
      else
        offset=offset+np*4;
      end
      status=fseek(fid,offset,-1);
      if status<0,
        msg=ferror(fid);
        error(msg);
      end
      adc.bsh(i)=fread(fid,1,'float32');
    end      
    if(blt_check > 0)
      for h=1:blt_check
        if(check==0)
          check=1;
          offset=offset+(node-1)*4;
        else
          offset=offset*np*4;
        end
        status=fseek(fid,offset,-1);
        if status<0,
          msg=ferror(fid);
          error(msg);
        end
        adc.blt(h,i)=fread(fid,1,'float32');
      end
    end  
    if(bld_check > 0)
      for j=1:bld_check
          for h=1:number_sed
            if(check==0)
              check=1;
              offset=offset+(node-1)*4;
            else
              offset=offset+np*4;
            end
            status=fseek(fid,offset,-1);
            if status<0,
              msg=ferror(fid);
              error(msg);
            end
            adc.bld(j,h,i)=fread(fid,1,'float32');
          end
      end
    end     
    if(bed_check > 0)
      if(check==0)
        check=1;
        offset=offset+(node-1)*4;
      else
        offset=offset+np*4;
      end
      status=fseek(fid,offset,-1);
      if status<0,
        msg=ferror(fid);
        error(msg);
      end    
      adc.bed_u(i)=fread(fid,1,'float32');
      offset=offset+np*4;
      status=fseek(fid,offset,-1);
      if status<0,
        msg=ferror(fid);
        error(msg);
      end    
      adc.bed_v(i)=fread(fid,1,'float32');
    end    
    if(alt_check > 0)
      if(check==0)
        check=1;
        offset=offset+(node-1)*4;
      else
        offset=offset+np*4;
      end
      status=fseek(fid,offset,-1);
      if status<0,
        msg=ferror(fid);
        error(msg);
      end
      adc.alt(i)=fread(fid,1,'float32');
    end      
    if(alb_check > 0)
      for h=1:alb_check
        if(check==0)
          check=1;
          offset=offset+(node-1)*4;
        else
          offset=offset+np*4;
        end
        status=fseek(fid,offset,-1);
        if status<0,
          msg=ferror(fid);
          error(msg);
        end
        adc.alb(h,i)=fread(fid,1,'float32');
      end
    end      
    if(cbp_check > 0)
      for j=1:cbp_check
        if(check==0)
          check=1;
          offset=offset+(node-1)*4;
        else
          offset=offset+np*4;
        end
        status=fseek(fid,offset,-1);
        if status<0,
          msg=ferror(fid);
          error(msg);
        end
        adc.cbp_den(j,i)=fread(fid,1,'float32');
        offset=offset+(np)*4;
        status=fseek(fid,offset,-1);
        if status<0,
          msg=ferror(fid);
          error(msg);
        end
        adc.cbp_ces(j,i)=fread(fid,1,'float32');        
        offset=offset+(np)*4;
        status=fseek(fid,offset,-1);
        if status<0,
          msg=ferror(fid);
          error(msg);
        end
        adc.cbp_erc(j,i)=fread(fid,1,'float32');
        offset=offset+(np)*4;
        status=fseek(fid,offset,-1);
        if status<0,
          msg=ferror(fid);
          error(msg);
        end
        adc.cbp_ere(j,i)=fread(fid,1,'float32');
      end
    end       
end

adc.dt=dt;
adc.nt=nt;
adc.np=np;

fclose(fid);
