function mkda_adh_subgrid(basename)
%% 
% ========================================================================
% MKDA_ADH_SUBGRID      
%       Function to generate a da file for a subset grid created by
%       create_subgrid.m. The *_SUB.da file can then be used with
%       mkh5_adh.m
%       
%       Created 5/17/2012 kcp
%
% ========================================================================
%% LOAD DATA
clear
tic
load(strcat([basename '_SUB']))
display('ORIGINAL GRID: ')  
fid=fopen(strcat([basename '_SUB.da']),'w');
header=load_da_stepnew(strcat([basename '.da']),0);
nt=header.nt;
dt=header.dt;
np=subgrid.nodes;
fprintf(1,'SUBGRID: \n NT= %i, NP= %i, DT= %f \n',nt,np,dt)

%% WRITE HEADER INFORMATION TO SUBGRID DA
fwrite(fid,header.nt,'integer*4');
fwrite(fid,subgrid.nodes,'integer*4');
fwrite(fid,header.dt,'real*4');
fwrite(fid,header.dep_check,'integer*4');
fwrite(fid,header.wse_check,'integer*4');
fwrite(fid,header.vel_check,'integer*4');
fwrite(fid,header.err_check,'integer*4');
fwrite(fid,header.sal_check,'integer*4');
fwrite(fid,header.vor_check,'integer*4');
fwrite(fid,header.snd_check,'integer*4');
fwrite(fid,header.slt_check,'integer*4');
fwrite(fid,header.smr_check,'integer*4');
fwrite(fid,header.dpl_check,'integer*4');
fwrite(fid,header.bsh_check,'integer*4');
fwrite(fid,header.blt_check,'integer*4');
fwrite(fid,header.bld_check,'integer*4');
fwrite(fid,header.bed_check,'integer*4');
fwrite(fid,header.alt_check,'integer*4');
fwrite(fid,header.alb_check,'integer*4');
fwrite(fid,header.cbp_check,'integer*4');
fwrite(fid,header.number_layers,'integer*4');
fwrite(fid,header.number_sed,'integer*4');

%% WRITE PARAMETER VALUES TO SUBGRID DA

 for k=1:header.nt
    data=load_da_stepnew(strcat([basename '.da']),k);
    fwrite(fid,data.time,'float64');
    
    if(header.dep_check > 0)
        fwrite(fid,data.dep(subtogrid),'float32');
    end  
    
    if(header.wse_check > 0)
        fwrite(fid,data.wse(subtogrid),'float32');   
    end  
    
    if(header.vel_check > 0)      
        fwrite(fid,data.u(subtogrid),'float32');                %all u
        fwrite(fid,data.v(subtogrid),'float32');                %all v 
    end
    
    if(header.err_check > 0) 
        fprintf(1,'CODE MUST BE MODIFIED TO INCLUDE THIS PARAMETER')
%         fwrite(fid,err.error(:),'float32');
    end
    
    if(header.sal_check > 0) 
        fprintf(1,'CODE MUST BE MODIFIED TO INCLUDE THIS PARAMETER')
%         fwrite(fid,salt.salinity(:),'float32');
    end
    
    if(header.vor_check > 0)
        fprintf(1,'CODE MUST BE MODIFIED TO INCLUDE THIS PARAMETER')
%         fwrite(fid,vor.vorticity(:),'float32');
    end    
    
    if(header.snd_check > 0) 
        fprintf(1,'CODE MUST BE MODIFIED TO INCLUDE THIS PARAMETER')
%         for i=1:snd_check
%           fwrite(fid,snd.sand(i,:),'float32');
%           fwrite(fid,snd.rouse(i,:),'float32');
%           fwrite(fid,snd.bdma(i,:),'float32');
%         end
    end
    
    if(header.slt_check > 0)
        fprintf(1,'CODE MUST BE MODIFIED TO INCLUDE THIS PARAMETER')
%         for i=1:slt_check
%           fwrite(fid,slt.silt(i,:),'float32');
%           fwrite(fid,slt.rouse(i,:),'float32');
%           fwrite(fid,slt.bdma(i,:),'float32');
%         end
    end    
    
    if(header.smr_check > 0)
        fprintf(1,'CODE MUST BE MODIFIED TO INCLUDE THIS PARAMETER')
%        for i=1:number_sed
%          fwrite(fid,smr.value(i,:),'float32');
%        end
    end
    
    if(header.dpl_check > 0) 
        fprintf(1,'CODE MUST BE MODIFIED TO INCLUDE THIS PARAMETER')
%         fwrite(fid,dpl.displacement(:),'float32');
    end
    
    if(header.bsh_check > 0) 
        fprintf(1,'CODE MUST BE MODIFIED TO INCLUDE THIS PARAMETER')
%         fwrite(fid,bsh.shear(:),'float32');
    end
    
    if(header.blt_check > 0)  
        fprintf(1,'CODE MUST BE MODIFIED TO INCLUDE THIS PARAMETER')
%         for i=1:blt_check
%           fwrite(fid,blt.value(i,:),'float32');
%         end
    end
    
    if(header.bld_check > 0)  
        fprintf(1,'CODE MUST BE MODIFIED TO INCLUDE THIS PARAMETER')
%         for i=1:bld_check
%           for j=1:number_sed
%             fwrite(fid,bld.bed(i,j,:),'float32');
%           end
%         end
    end
    
    if(header.bed_check > 0)  
        fprintf(1,'CODE MUST BE MODIFIED TO INCLUDE THIS PARAMETER')
%         fwrite(fid,bed.value(1,:),'float32');
%         fwrite(fid,bed.value(1,:),'float32');
    end
    
    if(header.alt_check > 0)
        fprintf(1,'CODE MUST BE MODIFIED TO INCLUDE THIS PARAMETER')
%         fwrite(fid,alt.value(:),'float32');
    end
    
    if(header.alb_check > 0) 
        fprintf(1,'CODE MUST BE MODIFIED TO INCLUDE THIS PARAMETER')
%         for i=1:number_sed
%           fwrite(fid,alb.value(i,:),'float32');
%         end
    end
    
    if(header.cbp_check > 0)
        fprintf(1,'CODE MUST BE MODIFIED TO INCLUDE THIS PARAMETER')
%         for i=1:cbp_check
%           fwrite(fid,cbp.den(i,:),'float32');
%           fwrite(fid,cbp.ces(i,:),'float32');
%           fwrite(fid,cbp.erc(i,:),'float32');
%           fwrite(fid,cbp.ere(i,:),'float32');
%         end
    end  
 end

fclose(fid);

toc

