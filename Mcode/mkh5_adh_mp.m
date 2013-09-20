function mkh5_adh_mp(dafile,attributes)
%
%   Function to read in the binary .da file and create a binary .h5 file
%   which can be read into SMS 10.1.  This .h5 file is a compressed file
%   which can combine multiple ADH solution file types (ex. dep, ovl, con,
%   bsh, etc).  The .h5 file pulls into SMS much easier and quicker.
%
%   INPUT VARIABLE
%
%   dafile = root file name for the .da file.  This will also be the root
%   file name for the .h5 output file.
%
%   attributes = string containing the parameters to be included in the da
%   file seperated by spaces.  These are:
%
%   dep = depth solution
%   wse = water surface elevation solution
%   vel = velocity vector and magnitude solution
%   sal = salinity solution
%   vor = vorticity solution
%   snd = sand concentration solution
%   cla = Clay concentration solution
%   err = error solution
%   dpl = bed displacement solution
%   alt = active layer thickness
%   ald = active layer distribution
%   blt = bed layer thickness
%   bld = bed layer distribution
%   cbp = cohesive bed properties
%   bsh = bed shear stress solution
%   bed = bedload vector and magnitude solution
%   smr = sediment mass residual
%
%   Example Usage: mkh5_adh('Mobile','dep wse vel err')
%
%   Finished 04/28/2010
%

%% Access DA file.  Get dimension sizes

h5file=[dafile,'.h5'];
dafile=[dafile,'.da'];
model=load_adhda_step(dafile,0);
model_1=load_da_time_series(dafile,1);
nt=model.nt;
np=model.np;
dt=model.dt;
number_sed=model.number_sed;
dumval=single(1:nt);
%dumval2=(0:nt-1)*dt;
dumval2=(model_1.time-model_1.time(1))*24; 

dep_check = 0;
wse_check = 0;
vel_check = 0;
err_check = 0;
sal_check = 0;
vor_check = 0;
snd_check = 0;
slt_check = 0;
smr_check = 0;
dpl_check = 0;
bsh_check = 0;
blt_check = 0;
bld_check = 0;
bed_check = 0;
alt_check = 0;
alb_check = 0;
cbp_check = 0;

temp1=sscanf(attributes,'%s');

for i=1:length(temp1)/3
   test=temp1((1+(i-1)*3):3*i);
   switch lower(test)
       case {'dep'}
           if(model.dep_check > 0)
             dep_check = model.dep_check;
             fprintf(1,'H5 file will include the Depth Solution\n'); 
           elseif(model.dep_check == 0)
             fprintf(1,'\n DEPTH DATA NOT INCLUDE IN DA FILE\n\n');
             dep_check = 0;
           end
       case {'wse'}
           if(model.wse_check > 0)
             wse_check = model.wse_check;
             fprintf(1,'H5 file will include the Water Surface Elevation Solution\n'); 
           elseif(model.wse_check == 0)
             fprintf(1,'\n WATER SURFACE ELEVATION DATA NOT INCLUDED IN DA FILE\n\n');
             wse_check = 0;
           end
       case {'vel'}
           if(model.vel_check > 0)
             vel_check = model.vel_check;
             fprintf(1,'H5 file will include the Velocity Solution\n'); 
           elseif(model.vel_check == 0)
             fprintf(1,'\n VELOCITY DATA NOT INCLUDED IN DA FILE\n\n');
             vel_check = 0;
           end
       case {'err'}
           if(model.err_check > 0)
             err_check = model.err_check;
             fprintf(1,'H5 file will include the Error Solution\n'); 
           elseif(model.err_check == 0)
             fprintf(1,'\nERROR DATA NOT INCLUDED IN DA FILE\n\n');
             err_check = 0;
           end           
       case {'sal'}
           if(model.sal_check > 0)
             sal_check = model.sal_check;
             fprintf(1,'H5 file will include the Salinity Solution\n'); 
           elseif(model.sal_check == 0)
             fprintf(1,'\nSALINITY DATA NOT INCLUDED IN DA FILE\n\n');
             sal_check = 0;
           end           
       case {'vor'}
           if(model.vor_check > 0)
             vor_check = model.vor_check;
             fprintf(1,'H5 file will include the Vorticity Solution\n'); 
           elseif(model.vor_check == 0)
             fprintf(1,'\nVORTICITY DATA NOT INCLUDED IN DA FILE\n\n');
             vor_check = 0;
           end
       case {'snd'}
           if(model.snd_check > 0)
             fprintf(1,'H5 file will include the Solution for %i Sands\n', model.snd_check); 
             snd_check = model.snd_check;
           elseif(model.snd_check == 0)
             fprintf(1,'\nSAND CONCENTRATION DATA NOT INCLUDED IN DA FILE\n\n');
             snd_check = 0;
           end
       case {'cla'}
           if(model.slt_check > 0)
             fprintf(1,'H5 file will include the Solution for Clays %i\n', model.slt_check);
             slt_check = model.slt_check;
           elseif(model.slt_check == 0)
             fprintf(1,'\nClay CONCENTRATION DATA NOT INCLUDED IN DA FILE\n\n');
             slt_check = 0;
           end           
       case {'smr'}
           if(model.smr_check > 0)
             fprintf(1,'H5 file will include the %i Sediment Mass Residuals\n',model.smr_check);
             smr_check=model.smr_check;
           elseif(model.smr_check == 0)
             fprintf(1,'\nSEDIMENT MASS RESIDUAL DATA NOT INCLUDED IN DA FILE\n\n');
             smr_check = 0;
           end           
       case {'dpl'}
           if(model.dpl_check > 0)
             dpl_check = model.dpl_check;
             fprintf(1,'H5 file will include the Bed Displacement Solution\n');
           elseif(model.dpl_check == 0)
             fprintf(1,'BED DISPLACEMENT DATA NOT INCLUDED IN DA FILE\n\n');
             dpl_check = 0;
           end
       case {'bsh'}
           if(model.bsh_check > 0)
             bsh_check = model.bsh_check;
             fprintf(1,'H5 file will include the Bed Shear Solution\n');
           elseif(model.bsh_check == 0)
             fprintf(1,'BED SHEAR DATA NOT INCLUDE IN DA FILE\n\n');
             bsh_check = 0;
           end
       case {'blt'}
           if(model.blt_check > 0)
             blt_check = model.blt_check;
             fprintf(1,'H5 file will include the Bed Layer Thickness Solution\n');
           elseif(model.blt_check == 0)
             fprintf(1,'\nBED LAYER THICKNESS SOLUTION NOT INCLUDED IN DA FILE\n\n');
             blt_check = 0;
           end
       case {'bld'}
           if(model.bld_check > 0)
             bld_check = model.bld_check;
             fprintf(1,'H5 file will include the Bed Layer Distribution Solution\n');
           elseif(model.bld_check == 0)
             fprintf(1,'\nBED LAYER DISTRIBUTION SOLUTION NOT INCLUDE IN DA FILE\n\n');
             bld_check = 0;
           end
       case {'bed'}
           if(model.bed_check > 0)
             bed_check = model.bed_check;
             fprintf(1,'H5 file will include the Bedload Solution\n');
           elseif(model.bed_check == 0)
             fprintf(1,'\nBEDLOAD SOLUTION NOT INCLUDE IN DA FILE\n\n');
             bed_check = 0;
           end
       case {'alt'}
           if(model.alt_check > 0)
             alt_check = model.alt_check;
             fprintf(1,'H5 file will include the Active Layer Thickness Solution\n');
           elseif(model.alt_check == 0)
             fprintf(1,'\nACTIVE LAYER THICKNESS SOLUTION NOT INCLUDED IN DA FILE\n\n');
             alt_check = 0;
           end
       case {'ald'}
           if(model.alb_check > 0)
             alb_check = model.alb_check;
             fprintf(1,'H5 file will include the Active Layer Distribution Solution\n');
           elseif(model.alb_check == 0)
             fprintf(1,'\nACTIVE LAYER DISTRIBUTION SOLUTION NOT INCLUDED IN DA FILE\n\n');
             alb_check = 0;
           end
       case {'cbp'}
           if(model.cbp_check > 0)
             cbp_check = model.cbp_check;
             fprintf(1,'H5 file will include the Cohesive Bed Properties Solution\n');
           elseif(model.cbp_check == 0)
             fprintf(1,'COHESIVE BED PROPERTIES DATA NOT INCLUDED IN DA FILE\n\n');
             cbp_check = 0;
           end         
       otherwise
           fprintf(1,'\nINVALID ENTRY %s NOT RECOGNIZED\n',test);
   end        
end


%% Load snapshot from DA, place in H5

fprintf(1,'Converting: \n DAfile: %s\n H5file: %s\n',dafile,h5file);
fprintf(1,'Preparing H5 file...\n');

%% Create File Type and File Version Info

hdf5write(h5file,'/File Type','Xmdf')
hdf5write(h5file,'/File Version',single(1.0),'WriteMode','append');

%% Write data create data holding structures GUID

hdf5write(h5file,'/Datasets/Guid','a79f5407-6c08-4ffb-a548-52d525513d70',...
   'WriteMode','append');

  %% add dataset attributes

h5attput(h5file,'/Datasets','Grouptype','MULTI DATASETS');

%% modify sizes of storage space for large datasets

fid=H5F.open(h5file,'H5F_ACC_RDWR','H5P_DEFAULT');
  
%% Velocity Specification

if(vel_check > 0)
  hdf5write(h5file,'/Datasets/Velocity/Maxs',...
     dumval,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Velocity/Mins',...
     dumval,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Velocity/Times',...
     dumval2,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Velocity/Values',...
     single(zeros(2,np,3)),'WriteMode','append');
   h5attput(h5file,'/Datasets/Velocity',...
     'Grouptype','DATASET VECTOR');
  h5attput(h5file,'/Datasets/Velocity',...
     'TimeUnits','Seconds');
  h5attput(h5file,'/Datasets/Velocity',...
     'DatasetUnits','m/s');
  h5attput(h5file,'/Datasets/Velocity',...
     'DatasetCompression',int32(-1));
  did=H5D.open(fid,'/Datasets/Velocity/Values');
  H5D.extend(did,[nt,np,2])

  hdf5write(h5file,'/Datasets/Velocity Mag/Maxs',...
     dumval,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Velocity Mag/Mins',...
     dumval,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Velocity Mag/Times',...
     dumval2,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Velocity Mag/Values',...
     single(zeros(np,2)),'WriteMode','append');
  h5attput(h5file,'/Datasets/Velocity Mag',...
     'Grouptype','DATASET SCALAR');
  h5attput(h5file,'/Datasets/Velocity Mag',...
     'TimeUnits','Seconds');
  h5attput(h5file,'/Datasets/Velocity Mag',...
     'DatasetUnits','m/s');
  h5attput(h5file,'/Datasets/Velocity Mag',...
     'DatasetCompression',int32(-1));
  did=H5D.open(fid,'/Datasets/Velocity Mag/Values');
  H5D.extend(did,[nt,np]) 
end

%% Water Surface Elevation Specification

if(wse_check > 0)
  hdf5write(h5file,'/Datasets/Water Surface Elevation/Maxs',...
     dumval,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Water Surface Elevation/Mins',...
     dumval,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Water Surface Elevation/Times',...
     dumval2,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Water Surface Elevation/Values',...
     single(zeros(np,2)),'WriteMode','append');
  hdf5write(h5file,'/Datasets/Water Surface Elevation/PROPERTIES/nullvalue',...
     single(-99999),'WriteMode','append');
  h5attput(h5file,'/Datasets/Water Surface Elevation',...
     'Grouptype','DATASET SCALAR');
  h5attput(h5file,'/Datasets/Water Surface Elevation',...
     'TimeUnits','Seconds');
  h5attput(h5file,'/Datasets/Water Surface Elevation',...
     'DatasetUnits','m');
  h5attput(h5file,'/Datasets/Water Surface Elevation',...
     'DatasetCompression',int32(-1));
  h5attput(h5file,'/Datasets/Water Surface Elevation/PROPERTIES',...
     'Grouptype','PROPERTIES');
  did=H5D.open(fid,'/Datasets/Water Surface Elevation/Values');
  H5D.extend(did,[nt,np]) 
end

%% Depth Specification

if(dep_check > 0)
  hdf5write(h5file,'/Datasets/Depth/Maxs',...
     dumval,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Depth/Mins',...
     dumval,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Depth/Times',...
     dumval2,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Depth/Values',...
     single(zeros(np,2)),'WriteMode','append');
  hdf5write(h5file,'/Datasets/Depth/PROPERTIES/nullvalue',...
     single(-99999),'WriteMode','append');
  h5attput(h5file,'/Datasets/Depth',...
     'Grouptype','DATASET SCALAR');
  h5attput(h5file,'/Datasets/Depth',...
     'TimeUnits','Seconds');
  h5attput(h5file,'/Datasets/Depth',...
     'DatasetUnits','m');
  h5attput(h5file,'/Datasets/Depth',...
     'DatasetCompression',int32(-1));
  h5attput(h5file,'/Datasets/Depth/PROPERTIES',...
     'Grouptype','PROPERTIES'); 
  did=H5D.open(fid,'/Datasets/Depth/Values');
  H5D.extend(did,[nt,np])
end

%% Error Specification

if(err_check > 0)
  hdf5write(h5file,'/Datasets/Error/Maxs',...
     dumval,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Error/Mins',...
     dumval,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Error/Times',...
     dumval2,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Error/Values',...
     single(zeros(np,2)),'WriteMode','append');
  hdf5write(h5file,'/Datasets/Error/PROPERTIES/nullvalue',...
     single(-99999),'WriteMode','append');
  h5attput(h5file,'/Datasets/Error',...
     'Grouptype','DATASET SCALAR');
  h5attput(h5file,'/Datasets/Error',...
     'TimeUnits','Seconds');
  h5attput(h5file,'/Datasets/Error',...
     'DatasetUnits','m');
  h5attput(h5file,'/Datasets/Error',...
     'DatasetCompression',int32(-1));
  h5attput(h5file,'/Datasets/Error/PROPERTIES',...
     'Grouptype','PROPERTIES'); 
  did=H5D.open(fid,'/Datasets/Error/Values');
  H5D.extend(did,[nt,np]) 
 
end

%% Salinity Specification

if(sal_check > 0)
  hdf5write(h5file,'/Datasets/Salinity/Maxs',...
     dumval,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Salinity/Mins',...
     dumval,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Salinity/Times',...
     dumval2,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Salinity/Values',...
     single(zeros(np,2)),'WriteMode','append');
  hdf5write(h5file,'/Datasets/Salinity/PROPERTIES/nullvalue',...
     single(-99999),'WriteMode','append');
  h5attput(h5file,'/Datasets/Salinity',...
     'Grouptype','DATASET SCALAR');
  h5attput(h5file,'/Datasets/Salinity',...
     'TimeUnits','Seconds');
  h5attput(h5file,'/Datasets/Salinity',...
     'DatasetUnits','m');
  h5attput(h5file,'/Datasets/Salinity',...
     'DatasetCompression',int32(-1));
  h5attput(h5file,'/Datasets/Salinity/PROPERTIES',...
     'Grouptype','PROPERTIES'); 
  did=H5D.open(fid,'/Datasets/Salinity/Values');
  H5D.extend(did,[nt,np]) 
end

%% Vorticity Specification

if(vor_check > 0)
  hdf5write(h5file,'/Datasets/Vorticity/Maxs',...
     dumval,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Vorticity/Mins',...
     dumval,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Vorticity/Times',...
     dumval2,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Vorticity/Values',...
     single(zeros(np,2)),'WriteMode','append');
  hdf5write(h5file,'/Datasets/Vorticity/PROPERTIES/nullvalue',...
     single(-99999),'WriteMode','append');
  h5attput(h5file,'/Datasets/Vorticity',...
     'Grouptype','DATASET SCALAR');
  h5attput(h5file,'/Datasets/Vorticity',...
     'TimeUnits','Seconds');
  h5attput(h5file,'/Datasets/Vorticity',...
     'DatasetUnits','m');
  h5attput(h5file,'/Datasets/Vorticity',...
     'DatasetCompression',int32(-1));
  h5attput(h5file,'/Datasets/Vorticity/PROPERTIES',...
     'Grouptype','PROPERTIES'); 
  did=H5D.open(fid,'/Datasets/Vorticity/Values');
  H5D.extend(did,[nt,np]) 
end

%% Sand Concentration Specification

if(snd_check > 0)
  for i=1:snd_check
    temp=['/Datasets/Sand ',int2str(i),' Concentration/Maxs'];      
    hdf5write(h5file,temp,...
       dumval,'WriteMode','append');
    temp=['/Datasets/Sand ',int2str(i),' Concentration/Mins'];
    hdf5write(h5file,temp,...
       dumval,'WriteMode','append');
    temp=['/Datasets/Sand ',int2str(i),' Concentration/Times'];
    hdf5write(h5file,temp,...
       dumval2,'WriteMode','append');
    temp=['/Datasets/Sand ',int2str(i),' Concentration/Values'];
    hdf5write(h5file,temp,...
       single(zeros(np,2)),'WriteMode','append');
    temp=['/Datasets/Sand ',int2str(i),' Concentration/PROPERTIES/nullvalue'];
    hdf5write(h5file,temp,...
       single(-99999),'WriteMode','append');
    temp=['/Datasets/Sand ',int2str(i),' Rouse Number/Maxs'];      
    hdf5write(h5file,temp,...
       dumval,'WriteMode','append');
    temp=['/Datasets/Sand ',int2str(i),' Rouse Number/Mins'];
    hdf5write(h5file,temp,...
       dumval,'WriteMode','append');
    temp=['/Datasets/Sand ',int2str(i),' Rouse Number/Times'];
    hdf5write(h5file,temp,...
       dumval2,'WriteMode','append');
    temp=['/Datasets/Sand ',int2str(i),' Rouse Number/Values'];
    hdf5write(h5file,temp,...
       single(zeros(np,2)),'WriteMode','append');
    temp=['/Datasets/Sand ',int2str(i),' Rouse Number/PROPERTIES/nullvalue'];
    hdf5write(h5file,temp,...
       single(-99999),'WriteMode','append');  
    temp=['/Datasets/Sand ',int2str(i),' Bedload Mass per Unit Area/Maxs'];      
    hdf5write(h5file,temp,...
       dumval,'WriteMode','append');
    temp=['/Datasets/Sand ',int2str(i),' Bedload Mass per Unit Area/Mins'];
    hdf5write(h5file,temp,...
       dumval,'WriteMode','append');
    temp=['/Datasets/Sand ',int2str(i),' Bedload Mass per Unit Area/Times'];
    hdf5write(h5file,temp,...
       dumval2,'WriteMode','append');
    temp=['/Datasets/Sand ',int2str(i),' Bedload Mass per Unit Area/Values'];
    hdf5write(h5file,temp,...
       single(zeros(np,2)),'WriteMode','append');
    temp=['/Datasets/Sand ',int2str(i),' Bedload Mass per Unit Area/PROPERTIES/nullvalue'];
    hdf5write(h5file,temp,...
     single(-99999),'WriteMode','append'); 
    temp=['/Datasets/Sand ',int2str(i), ' Concentration'];
    h5attput(h5file,temp,...
       'Grouptype','DATASET SCALAR');
    h5attput(h5file,temp,...
       'TimeUnits','Seconds');
    h5attput(h5file,temp,...
       'DatasetUnits','m');
    h5attput(h5file,temp,...
       'DatasetCompression',int32(-1));
    temp=['/Datasets/Sand ',int2str(i),' Concentration/PROPERTIES'];
    h5attput(h5file,temp,...
       'Grouptype','PROPERTIES');
    temp=['/Datasets/Sand ',int2str(i),' Rouse Number'];
    h5attput(h5file,temp,...
       'Grouptype','DATASET SCALAR');
    h5attput(h5file,temp,...
       'TimeUnits','Seconds');
    h5attput(h5file,temp,...
       'DatasetUnits','m');
    h5attput(h5file,temp,...
       'DatasetCompression',int32(-1));
    temp=['/Datasets/Sand ',int2str(i),' Rouse Number/PROPERTIES'];
    h5attput(h5file,temp,...
       'Grouptype','PROPERTIES');   
    temp=['/Datasets/Sand ',int2str(i),' Bedload Mass per Unit Area'];
    h5attput(h5file,temp,...
       'Grouptype','DATASET SCALAR');
    h5attput(h5file,temp,...
       'TimeUnits','Seconds');
    h5attput(h5file,temp,...
       'DatasetUnits','m');
    h5attput(h5file,temp,...
       'DatasetCompression',int32(-1));
    temp=['/Datasets/Sand ',int2str(i),' Bedload Mass per Unit Area/PROPERTIES'];
    h5attput(h5file,temp,...
       'Grouptype','PROPERTIES');
    temp=['/Datasets/Sand ',int2str(i),' Concentration/Values'];
    did=H5D.open(fid,temp);
    H5D.extend(did,[nt,np])
    temp=['/Datasets/Sand ',int2str(i),' Rouse Number/Values'];
    did=H5D.open(fid,temp);
    H5D.extend(did,[nt,np])
    temp=['/Datasets/Sand ',int2str(i),' Bedload Mass per Unit Area/Values'];
    did=H5D.open(fid,temp);
    H5D.extend(did,[nt,np])   
  end
end

%% Clay Concentration Specification

if(slt_check > 0)
  for i=1:slt_check
    temp=['/Datasets/Clay ',int2str(i),' Concentration/Maxs'];      
    hdf5write(h5file,temp,...
       dumval,'WriteMode','append');
    temp=['/Datasets/Clay ',int2str(i),' Concentration/Mins'];
    hdf5write(h5file,temp,...
       dumval,'WriteMode','append');
    temp=['/Datasets/Clay ',int2str(i),' Concentration/Times'];
    hdf5write(h5file,temp,...
       dumval2,'WriteMode','append');
    temp=['/Datasets/Clay ',int2str(i),' Concentration/Values'];
    hdf5write(h5file,temp,...
       single(zeros(np,2)),'WriteMode','append');
    temp=['/Datasets/Clay ',int2str(i),' Concentration/PROPERTIES/nullvalue'];
    hdf5write(h5file,temp,...
       single(-99999),'WriteMode','append');
    temp=['/Datasets/Clay ',int2str(i),' Rouse Number/Maxs'];      
    hdf5write(h5file,temp,...
       dumval,'WriteMode','append');
    temp=['/Datasets/Clay ',int2str(i),' Rouse Number/Mins'];
    hdf5write(h5file,temp,...
       dumval,'WriteMode','append');
    temp=['/Datasets/Clay ',int2str(i),' Rouse Number/Times'];
    hdf5write(h5file,temp,...
       dumval2,'WriteMode','append');
    temp=['/Datasets/Clay ',int2str(i),' Rouse Number/Values'];
    hdf5write(h5file,temp,...
       single(zeros(np,2)),'WriteMode','append');
    temp=['/Datasets/Clay ',int2str(i),' Rouse Number/PROPERTIES/nullvalue'];
    hdf5write(h5file,temp,...
       single(-99999),'WriteMode','append'); 
    temp=['/Datasets/Clay ',int2str(i),' Bedload Mass per Unit Area/Maxs'];      
    hdf5write(h5file,temp,...
       dumval,'WriteMode','append');
    temp=['/Datasets/Clay ',int2str(i),' Bedload Mass per Unit Area/Mins'];
    hdf5write(h5file,temp,...
       dumval,'WriteMode','append');
    temp=['/Datasets/Clay ',int2str(i),' Bedload Mass per Unit Area/Times'];
    hdf5write(h5file,temp,...
       dumval2,'WriteMode','append');
    temp=['/Datasets/Clay ',int2str(i),' Bedload Mass per Unit Area/Values'];
    hdf5write(h5file,temp,...
       single(zeros(np,2)),'WriteMode','append');
    temp=['/Datasets/Clay ',int2str(i),' Bedload Mass per Unit Area/PROPERTIES/nullvalue'];
    hdf5write(h5file,temp,...
       single(-99999),'WriteMode','append'); 
    temp=['/Datasets/Clay ',int2str(i),' Concentration'];
    h5attput(h5file,temp,...
       'Grouptype','DATASET SCALAR');
    h5attput(h5file,temp,...
       'TimeUnits','Seconds');
    h5attput(h5file,temp,...
       'DatasetUnits','m');
    h5attput(h5file,temp,...
       'DatasetCompression',int32(-1));
    temp=['/Datasets/Clay ',int2str(i),' Concentration/PROPERTIES'];
    h5attput(h5file,temp,...
       'Grouptype','PROPERTIES');   
    temp=['/Datasets/Clay ',int2str(i),' Rouse Number'];
    h5attput(h5file,temp,...
       'Grouptype','DATASET SCALAR');
    h5attput(h5file,temp,...
       'TimeUnits','Seconds');
    h5attput(h5file,temp,...
       'DatasetUnits','m');
    h5attput(h5file,temp,...
       'DatasetCompression',int32(-1));
    temp=['/Datasets/Clay ',int2str(i),' Rouse Number/PROPERTIES'];
    h5attput(h5file,temp,...
       'Grouptype','PROPERTIES');   
    temp=['/Datasets/Clay ',int2str(i),' Bedload Mass per Unit Area'];
    h5attput(h5file,temp,...
       'Grouptype','DATASET SCALAR');
    h5attput(h5file,temp,...
       'TimeUnits','Seconds');
    h5attput(h5file,temp,...
       'DatasetUnits','m');
    h5attput(h5file,temp,...
       'DatasetCompression',int32(-1));
    temp=['/Datasets/Clay ',int2str(i),' Bedload Mass per Unit Area/PROPERTIES'];
    h5attput(h5file,temp,...
       'Grouptype','PROPERTIES');   
    temp=['/Datasets/Clay ',int2str(i),' Concentration/Values'];
    did=H5D.open(fid,temp);
    H5D.extend(did,[nt,np])
    temp=['/Datasets/Clay ',int2str(i),' Rouse Number/Values'];
    did=H5D.open(fid,temp);
    H5D.extend(did,[nt,np])
    temp=['/Datasets/Clay ',int2str(i),' Bedload Mass per Unit Area/Values'];
    did=H5D.open(fid,temp);
    H5D.extend(did,[nt,np])      
  end
end

%% Sediment Mass Residual Specification

if(smr_check > 0)
  for i=1:smr_check
    temp=['/Datasets/Sediment Mass Residual Sediment ',int2str(i),'/Maxs'];
    hdf5write(h5file,temp,...
       dumval,'WriteMode','append');
    temp=['/Datasets/Sediment Mass Residual Sediment ',int2str(i),'/Mins'];   
    hdf5write(h5file,temp,...
       dumval,'WriteMode','append');
    temp=['/Datasets/Sediment Mass Residual Sediment ',int2str(i),'/Times'];  
    hdf5write(h5file,temp,...
       dumval2,'WriteMode','append');
    temp=['/Datasets/Sediment Mass Residual Sediment ',int2str(i),'/Values'];
    hdf5write(h5file,temp,...
       single(zeros(np,2)),'WriteMode','append');
    temp=['/Datasets/Sediment Mass Residual Sediment ',int2str(i),'/PROPERTIES/nullvalue'];
    hdf5write(h5file,temp,...
       single(-99999),'WriteMode','append');
    temp=['/Datasets/Sediment Mass Residual Sediment ',int2str(i)];
    h5attput(h5file,temp,...
       'Grouptype','DATASET SCALAR');
    h5attput(h5file,temp,...
       'TimeUnits','Seconds');
    h5attput(h5file,temp,...
       'DatasetUnits','m');
    h5attput(h5file,temp,...
       'DatasetCompression',int32(-1));
    temp=['/Datasets/Sediment Mass Residual Sediment ',int2str(i),'/PROPERTIES'];       
    h5attput(h5file,temp,...
       'Grouptype','PROPERTIES'); 
    temp=['/Datasets/Sediment Mass Residual Sediment ',int2str(i),'/Values'];
    did=H5D.open(fid,temp);
    H5D.extend(did,[nt,np])      
  end
end

%% Bed Displacement Specification

if(dpl_check > 0)
  hdf5write(h5file,'/Datasets/Bed Displacement/Maxs',...
     dumval,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Bed Displacement/Mins',...
     dumval,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Bed Displacement/Times',...
     dumval2,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Bed Displacement/Values',...
     single(zeros(np,2)),'WriteMode','append');
  hdf5write(h5file,'/Datasets/Bed Displacement/PROPERTIES/nullvalue',...
     single(-99999),'WriteMode','append');
  h5attput(h5file,'/Datasets/Bed Displacement',...
     'Grouptype','DATASET SCALAR');
  h5attput(h5file,'/Datasets/Bed Displacement',...
     'TimeUnits','Seconds');
  h5attput(h5file,'/Datasets/Bed Displacement',...
     'DatasetUnits','m');
  h5attput(h5file,'/Datasets/Bed Displacement',...
     'DatasetCompression',int32(-1));
  h5attput(h5file,'/Datasets/Bed Displacement/PROPERTIES',...
     'Grouptype','PROPERTIES'); 
  did=H5D.open(fid,'/Datasets/Bed Displacement/Values');
  H5D.extend(did,[nt,np])  
end

%% Bed Shear Specification

if(bsh_check > 0)
  hdf5write(h5file,'/Datasets/Bed Shear/Maxs',...
     dumval,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Bed Shear/Mins',...
     dumval,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Bed Shear/Times',...
     dumval2,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Bed Shear/Values',...
     single(zeros(np,2)),'WriteMode','append');
  hdf5write(h5file,'/Datasets/Bed Shear/PROPERTIES/nullvalue',...
     single(-99999),'WriteMode','append');
  h5attput(h5file,'/Datasets/Bed Shear',...
     'Grouptype','DATASET SCALAR');
  h5attput(h5file,'/Datasets/Bed Shear',...
     'TimeUnits','Seconds');
  h5attput(h5file,'/Datasets/Bed Shear',...
     'DatasetUnits','m');
  h5attput(h5file,'/Datasets/Bed Shear',...
     'DatasetCompression',int32(-1));
  h5attput(h5file,'/Datasets/Bed Shear/PROPERTIES',...
     'Grouptype','PROPERTIES'); 
  did=H5D.open(fid,'/Datasets/Bed Shear/Values');
  H5D.extend(did,[nt,np])  
end

%% Bed Layer Thickness Specification

if(blt_check > 0)
  for i=1:blt_check
    temp=['/Datasets/Bed Thickness Layer ',int2str(i),'/Maxs']; 
    hdf5write(h5file,temp,...
       dumval,'WriteMode','append');
    temp=['/Datasets/Bed Thickness Layer ',int2str(i),'/Mins'];
    hdf5write(h5file,temp,...
       dumval,'WriteMode','append');
    temp=['/Datasets/Bed Thickness Layer ',int2str(i),'/Times'];
    hdf5write(h5file,temp,...
       dumval2,'WriteMode','append');
    temp=['/Datasets/Bed Thickness Layer ',int2str(i),'/Values'];
    hdf5write(h5file,temp,...
       single(zeros(np,2)),'WriteMode','append');
    temp=['/Datasets/Bed Thickness Layer ',int2str(i),'/PROPERTIES/nullvalue'];
    hdf5write(h5file,temp,...
       single(-99999),'WriteMode','append');
    temp=['/Datasets/Bed Thickness Layer ',int2str(i)];
    h5attput(h5file,temp,...
       'Grouptype','DATASET SCALAR');
    h5attput(h5file,temp,...
       'TimeUnits','Seconds');
    h5attput(h5file,temp,...
       'DatasetUnits','m');
    h5attput(h5file,temp,...
       'DatasetCompression',int32(-1));
    temp=['/Datasets/Bed Thickness Layer ',int2str(i),'/PROPERTIES'];
    h5attput(h5file,temp,...
       'Grouptype','PROPERTIES');
    temp=['/Datasets/Bed Thickness Layer ',int2str(i),'/Values'];
    did=H5D.open(fid,temp);
    H5D.extend(did,[nt,np])      
  end
end

%% Bed Layer Distribution Specification

if(bld_check > 0)
  for i=1:bld_check
    for j=1:number_sed
      temp=['/Datasets/Bed Distribution Layer ',int2str(i), ...
          ' Sediment ',int2str(j),'/Maxs']; 
      hdf5write(h5file,temp,...
         dumval,'WriteMode','append');
      temp=['/Datasets/Bed Distribution Layer ',int2str(i), ...
          ' Sediment ',int2str(j),'/Mins']; 
      hdf5write(h5file,temp,...
         dumval,'WriteMode','append');
      temp=['/Datasets/Bed Distribution Layer ',int2str(i),...
          ' Sediment ',int2str(j),'/Times']; 
      hdf5write(h5file,temp,...
         dumval2,'WriteMode','append');
      temp=['/Datasets/Bed Distribution Layer ',int2str(i),...
          ' Sediment ',int2str(j),'/Values']; 
      hdf5write(h5file,temp,...
         single(zeros(np,2)),'WriteMode','append');
      temp=['/Datasets/Bed Distribution Layer ',int2str(i),...
          ' Sediment ',int2str(j),'/PROPERTIES/nullvalue']; 
      hdf5write(h5file,temp,...
         single(-99999),'WriteMode','append');
      temp=['/Datasets/Bed Distribution Layer ',int2str(i),...
          ' Sediment ',int2str(j)];
      h5attput(h5file,temp,...
         'Grouptype','DATASET SCALAR');
      h5attput(h5file,temp,...
         'TimeUnits','Seconds');
      h5attput(h5file,temp,...
         'DatasetUnits','m');
      h5attput(h5file,temp,...
         'DatasetCompression',int32(-1));
      temp=['/Datasets/Bed Distribution Layer ',int2str(i), ...
          ' Sediment ',int2str(j),'/PROPERTIES'];   
      h5attput(h5file,temp,...
         'Grouptype','PROPERTIES');  
      temp=['/Datasets/Bed Distribution Layer ',int2str(i),...
          ' Sediment ',int2str(j),'/Values'];      
      did=H5D.open(fid,temp);
      H5D.extend(did,[nt,np])
    end
  end
end

%% Bedload Specification

if(bed_check > 0)
  hdf5write(h5file,'/Datasets/Bedload/Maxs',...
     dumval,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Bedload/Mins',...
     dumval,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Bedload/Times',...
     dumval2,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Bedload/Values',...
     single(zeros(2,np,3)),'WriteMode','append');        
  hdf5write(h5file,'/Datasets/Bedload Mag/Maxs',...
     dumval,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Bedload Mag/Mins',...
     dumval,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Bedload Mag/Times',...
     dumval2,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Bedload Mag/Values',...
     single(zeros(np,2)),'WriteMode','append');
   h5attput(h5file,'/Datasets/Bedload',...
     'Grouptype','DATASET VECTOR');
  h5attput(h5file,'/Datasets/Bedload',...
     'TimeUnits','Seconds');
  h5attput(h5file,'/Datasets/Bedload',...
     'DatasetUnits','m/s');
  h5attput(h5file,'/Datasets/Bedload',...
     'DatasetCompression',int32(-1));
  h5attput(h5file,'/Datasets/Bedload Mag',...
     'Grouptype','DATASET SCALAR');
  h5attput(h5file,'/Datasets/Bedload Mag',...
     'TimeUnits','Seconds');
  h5attput(h5file,'/Datasets/Bedload Mag',...
     'DatasetUnits','m/s');
  h5attput(h5file,'/Datasets/Bedload Mag',...
     'DatasetCompression',int32(-1));
  did=H5D.open(fid,'/Datasets/Bedload/Values');
  H5D.extend(did,[nt,np,2])
  did=H5D.open(fid,'/Datasets/Bedload Mag/Values');
  H5D.extend(did,[nt,np])  
end

%% Active Layer Thickness

if(alt_check > 0)
  hdf5write(h5file,'/Datasets/Active Layer Thickness/Maxs',...
     dumval,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Active Layer Thickness/Mins',...
     dumval,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Active Layer Thickness/Times',...
     dumval2,'WriteMode','append');
  hdf5write(h5file,'/Datasets/Active Layer Thickness/Values',...
     single(zeros(np,2)),'WriteMode','append');
  hdf5write(h5file,'/Datasets/Active Layer Thickness/PROPERTIES/nullvalue',...
     single(-99999),'WriteMode','append');
  h5attput(h5file,'/Datasets/Active Layer Thickness',...
     'Grouptype','DATASET SCALAR');
  h5attput(h5file,'/Datasets/Active Layer Thickness',...
     'TimeUnits','Seconds');
  h5attput(h5file,'/Datasets/Active Layer Thickness',...
     'DatasetUnits','m');
  h5attput(h5file,'/Datasets/Active Layer Thickness',...
     'DatasetCompression',int32(-1));
  h5attput(h5file,'/Datasets/Active Layer Thickness/PROPERTIES',...
     'Grouptype','PROPERTIES');
  did=H5D.open(fid,'/Datasets/Active Layer Thickness/Values');
  H5D.extend(did,[nt,np])
end

%% Active Layer Distribution

if(alb_check > 0)
  for i=1:alb_check
    temp=['/Datasets/Active Layer Distribution Sediment ',int2str(i),'/Maxs'];
    hdf5write(h5file,temp,...
       dumval,'WriteMode','append');
    temp=['/Datasets/Active Layer Distribution Sediment ',int2str(i),'/Mins'];   
    hdf5write(h5file,temp,...
       dumval,'WriteMode','append');
    temp=['/Datasets/Active Layer Distribution Sediment ',int2str(i),'/Times'];  
    hdf5write(h5file,temp,...
       dumval2,'WriteMode','append');
    temp=['/Datasets/Active Layer Distribution Sediment ',int2str(i),'/Values'];
    hdf5write(h5file,temp,...
       single(zeros(np,2)),'WriteMode','append');
    temp=['/Datasets/Active Layer Distribution Sediment ',int2str(i),'/PROPERTIES/nullvalue'];
    hdf5write(h5file,temp,...
       single(-99999),'WriteMode','append');
    temp=['/Datasets/Active Layer Distribution Sediment ',int2str(i)];
    h5attput(h5file,temp,...
       'Grouptype','DATASET SCALAR');
    h5attput(h5file,temp,...
       'TimeUnits','Seconds');
    h5attput(h5file,temp,...
       'DatasetUnits','m');
    h5attput(h5file,temp,...
       'DatasetCompression',int32(-1));
    temp=['/Datasets/Active Layer Distribution Sediment ',int2str(i),'/PROPERTIES'];       
    h5attput(h5file,temp,...
       'Grouptype','PROPERTIES');  
    temp=['/Datasets/Active Layer Distribution Sediment ',int2str(i),'/Values'];
    did=H5D.open(fid,temp);
    H5D.extend(did,[nt,np])      
  end
end

%% Cohesive Bed Properties

if(cbp_check > 0)
  for i=1:cbp_check
      
    % Bulk Density
      
    temp=['/Datasets/Cohesive Bed Bulk Density Layer ',int2str(i),'/Maxs']; 
    hdf5write(h5file,temp,...
       dumval,'WriteMode','append');
    temp=['/Datasets/Cohesive Bed Bulk Density Layer ',int2str(i),'/Mins']; 
    hdf5write(h5file,temp,...
       dumval,'WriteMode','append');
    temp=['/Datasets/Cohesive Bed Bulk Density Layer ',int2str(i),'/Times']; 
    hdf5write(h5file,temp,...
       dumval2,'WriteMode','append');
    temp=['/Datasets/Cohesive Bed Bulk Density Layer ',int2str(i),'/Values']; 
    hdf5write(h5file,temp,...
       single(zeros(np,2)),'WriteMode','append');
    temp=['/Datasets/Cohesive Bed Bulk Density Layer ',int2str(i),'/PROPERTIES/nullvalue']; 
    hdf5write(h5file,temp,...
       single(-99999),'WriteMode','append');
    temp=['/Datasets/Cohesive Bed Bulk Density Layer ',int2str(i)];      
    h5attput(h5file,temp,...
       'Grouptype','DATASET SCALAR');
    h5attput(h5file,temp,...
       'TimeUnits','Seconds');
    h5attput(h5file,temp,...
       'DatasetUnits','m');
    h5attput(h5file,temp,...
       'DatasetCompression',int32(-1));
    temp=['/Datasets/Cohesive Bed Bulk Density Layer ',int2str(i),'/PROPERTIES'];      
    h5attput(h5file,temp,...
       'Grouptype','PROPERTIES');   
    temp=['/Datasets/Cohesive Bed Bulk Density Layer ',int2str(i),'/Values'];
    did=H5D.open(fid,temp);
    H5D.extend(did,[nt,np])      
    
    % Critical Erosion Shear
    
    temp=['/Datasets/Cohesive Bed Critical Erosion Shear Layer ',int2str(i),'/Maxs']; 
    hdf5write(h5file,temp,...
       dumval,'WriteMode','append');
    temp=['/Datasets/Cohesive Bed Critical Erosion Shear Layer ',int2str(i),'/Mins']; 
    hdf5write(h5file,temp,...
       dumval,'WriteMode','append');
    temp=['/Datasets/Cohesive Bed Critical Erosion Shear Layer ',int2str(i),'/Times']; 
    hdf5write(h5file,temp,...
       dumval2,'WriteMode','append');
    temp=['/Datasets/Cohesive Bed Critical Erosion Shear Layer ',int2str(i),'/Values']; 
    hdf5write(h5file,temp,...
       single(zeros(np,2)),'WriteMode','append');
    temp=['/Datasets/Cohesive Bed Critical Erosion Shear Layer ',int2str(i),'/PROPERTIES/nullvalue']; 
    hdf5write(h5file,temp,...
       single(-99999),'WriteMode','append');
    temp=['/Datasets/Cohesive Bed Critical Erosion Shear Layer ',int2str(i)];      
    h5attput(h5file,temp,...
       'Grouptype','DATASET SCALAR');
    h5attput(h5file,temp,...
       'TimeUnits','Seconds');
    h5attput(h5file,temp,...
       'DatasetUnits','m');
    h5attput(h5file,temp,...
       'DatasetCompression',int32(-1));
    temp=['/Datasets/Cohesive Bed Critical Erosion Shear Layer ',int2str(i),'/PROPERTIES'];      
    h5attput(h5file,temp,...
       'Grouptype','PROPERTIES');   
    temp=['/Datasets/Cohesive Bed Critical Erosion Shear Layer ',int2str(i),'/Values'];
    did=H5D.open(fid,temp);
    H5D.extend(did,[nt,np])       
    
    % Erosion Rate Constant
 
    temp=['/Datasets/Cohesive Bed Erosion Rate Constant Layer ',int2str(i),'/Maxs']; 
    hdf5write(h5file,temp,...
       dumval,'WriteMode','append');
    temp=['/Datasets/Cohesive Bed Erosion Rate Constant Layer ',int2str(i),'/Mins']; 
    hdf5write(h5file,temp,...
       dumval,'WriteMode','append');
    temp=['/Datasets/Cohesive Bed Erosion Rate Constant Layer ',int2str(i),'/Times']; 
    hdf5write(h5file,temp,...
       dumval2,'WriteMode','append');
    temp=['/Datasets/Cohesive Bed Erosion Rate Constant Layer ',int2str(i),'/Values']; 
    hdf5write(h5file,temp,...
       single(zeros(np,2)),'WriteMode','append');
    temp=['/Datasets/Cohesive Bed Erosion Rate Constant Layer ',int2str(i),'/PROPERTIES/nullvalue']; 
    hdf5write(h5file,temp,...
       single(-99999),'WriteMode','append');
    temp=['/Datasets/Cohesive Bed Erosion Rate Constant Layer ',int2str(i)];      
    h5attput(h5file,temp,...
       'Grouptype','DATASET SCALAR');
    h5attput(h5file,temp,...
       'TimeUnits','Seconds');
    h5attput(h5file,temp,...
       'DatasetUnits','m');
    h5attput(h5file,temp,...
       'DatasetCompression',int32(-1));
    temp=['/Datasets/Cohesive Bed Erosion Rate Constant Layer ',int2str(i),'/PROPERTIES'];      
    h5attput(h5file,temp,...
       'Grouptype','PROPERTIES');   
    temp=['/Datasets/Cohesive Bed Erosion Rate Constant Layer ',int2str(i),'/Values'];
    did=H5D.open(fid,temp);
    H5D.extend(did,[nt,np])     
    
    % Erosion Rate Exponent
   
    temp=['/Datasets/Cohesive Bed Erosion Rate Exponent ',int2str(i),'/Maxs']; 
    hdf5write(h5file,temp,...
       dumval,'WriteMode','append');
    temp=['/Datasets/Cohesive Bed Erosion Rate Exponent ',int2str(i),'/Mins']; 
    hdf5write(h5file,temp,...
       dumval,'WriteMode','append');
    temp=['/Datasets/Cohesive Bed Erosion Rate Exponent ',int2str(i),'/Times']; 
    hdf5write(h5file,temp,...
       dumval2,'WriteMode','append');
    temp=['/Datasets/Cohesive Bed Erosion Rate Exponent ',int2str(i),'/Values']; 
    hdf5write(h5file,temp,...
       single(zeros(np,2)),'WriteMode','append');
    temp=['/Datasets/Cohesive Bed Erosion Rate Exponent ',int2str(i),'/PROPERTIES/nullvalue']; 
    hdf5write(h5file,temp,...
       single(-99999),'WriteMode','append');
    temp=['/Datasets/Cohesive Bed Erosion Rate Exponent ',int2str(i)];      
    h5attput(h5file,temp,...
       'Grouptype','DATASET SCALAR');
    h5attput(h5file,temp,...
       'TimeUnits','Seconds');
    h5attput(h5file,temp,...
       'DatasetUnits','m');
    h5attput(h5file,temp,...
       'DatasetCompression',int32(-1));
    temp=['/Datasets/Cohesive Bed Erosion Rate Exponent ',int2str(i),'/PROPERTIES'];      
    h5attput(h5file,temp,...
       'Grouptype','PROPERTIES');   
    temp=['/Datasets/Cohesive Bed Erosion Rate Exponent ',int2str(i),'/Values'];
    did=H5D.open(fid,temp);
    H5D.extend(did,[nt,np])     
    
  end
end

H5F.close(fid);

%% populate file with data

fprintf(1,'Converting %g timesteps.\nTimestep: ',nt);
nbyte=fprintf(1,'%g (%4.1f%%)',0,0);

%% loops over each time step of data in the .da file

for k=1:nt;

   nbyte=fprintf(1,[repmat('\b',1,nbyte),'%g (%4.1f%%)'],k,(k-1)/nt*100)-nbyte;
   
   % load the model data for time step k
   
   model=load_adhda_step(dafile,k);
    
   % Velocity
   
   if(vel_check > 0)
     v3=single([model.u,model.v]');
   
     h5varput(h5file,'/Datasets/Velocity/Maxs',...
        k-1,1,max(v3(:)));
     h5varput(h5file,'/Datasets/Velocity/Mins',...
        k-1,1,single(0));
     h5varput(h5file,'/Datasets/Velocity/Values',...
        [0,0,k-1],[2,np,1],v3);      
   
     % Velocity mag
   
     v3mag=single(hypot(model.u,model.v)); 
     dry_loc=find(v3mag==0.0);
     h5varput(h5file,'/Datasets/Velocity Mag/Maxs',...
        k-1,1,max(v3mag(:)));
     h5varput(h5file,'/Datasets/Velocity Mag/Mins',...
        k-1,1,min(v3mag(:)));
     h5varput(h5file,'/Datasets/Velocity Mag/Values',...
        [0,k-1],[np,1],v3mag);
   end
   
   % Water Surface Elevation
   
   if(wse_check > 0)
     eta=single(model.eta);
     eta(dry_loc)=-1.0;
     h5varput(h5file,'/Datasets/Water Surface Elevation/Maxs',...
        k-1,1,max(eta));
     h5varput(h5file,'/Datasets/Water Surface Elevation/Mins',...
        k-1,1,min(eta));
     h5varput(h5file,'/Datasets/Water Surface Elevation/Values',...
        [0,k-1],[np,1],eta);  
   end

   if(dep_check > 0)
     eta=single(model.dep);
     eta(dry_loc)=-1.0;
     h5varput(h5file,'/Datasets/Depth/Maxs',...
        k-1,1,max(eta));
     h5varput(h5file,'/Datasets/Depth/Mins',...
        k-1,1,min(eta));
     h5varput(h5file,'/Datasets/Depth/Values',...
        [0,k-1],[np,1],eta);  
   end
   
   if(err_check > 0)
     eta=single(model.err);
     eta(dry_loc)=-1.0;
     h5varput(h5file,'/Datasets/Error/Maxs',...
        k-1,1,max(eta));
     h5varput(h5file,'/Datasets/Error/Mins',...
        k-1,1,min(eta));
     h5varput(h5file,'/Datasets/Error/Values',...
        [0,k-1],[np,1],eta);  
   end

   if(sal_check > 0)
     eta=single(model.sal);
     eta(eta<0.0)=0.0;
     eta(dry_loc)=-1.0;
     h5varput(h5file,'/Datasets/Salinity/Maxs',...
        k-1,1,max(eta));
     h5varput(h5file,'/Datasets/Salinity/Mins',...
        k-1,1,min(eta));
     h5varput(h5file,'/Datasets/Salinity/Values',...
        [0,k-1],[np,1],eta);  
   end
   
   if(vor_check > 0)
     eta=single(model.vor);
     eta(dry_loc)=-1.0;
     h5varput(h5file,'/Datasets/Vorticity/Maxs',...
        k-1,1,max(eta));
     h5varput(h5file,'/Datasets/Vorticity/Mins',...
        k-1,1,min(eta));
     h5varput(h5file,'/Datasets/Vorticity/Values',...
        [0,k-1],[np,1],eta);  
   end   
   
   if(snd_check > 0)
     for i=1:snd_check
       eta=single(model.snd(i,:));
       eta(dry_loc)=-1.0;
       temp=['/Datasets/Sand ',int2str(i),' Concentration/Maxs'];
       h5varput(h5file,temp,...
          k-1,1,max(eta));
       temp=['/Datasets/Sand ',int2str(i),' Concentration/Mins'];
       h5varput(h5file,temp,...
          k-1,1,min(eta));
       temp=['/Datasets/Sand ',int2str(i),' Concentration/Values'];
       h5varput(h5file,temp,...
          [0,k-1],[np,1],eta);
      
       eta=single(model.snd_rouse(i,:));
       eta(dry_loc)=-1.0;
       temp=['/Datasets/Sand ',int2str(i),' Rouse Number/Maxs'];
       h5varput(h5file,temp,...
          k-1,1,max(eta));
       temp=['/Datasets/Sand ',int2str(i),' Rouse Number/Mins'];
       h5varput(h5file,temp,...
          k-1,1,min(eta));
       temp=['/Datasets/Sand ',int2str(i),' Rouse Number/Values'];
       h5varput(h5file,temp,...
          [0,k-1],[np,1],eta);     
      
        eta=single(model.snd_bdma(i,:));
        eta(dry_loc)=-1.0;
       temp=['/Datasets/Sand ',int2str(i),' Bedload Mass per Unit Area/Maxs'];
       h5varput(h5file,temp,...
          k-1,1,max(eta));
       temp=['/Datasets/Sand ',int2str(i),' Bedload Mass per Unit Area/Mins'];
       h5varput(h5file,temp,...
          k-1,1,min(eta));
       temp=['/Datasets/Sand ',int2str(i),' Bedload Mass per Unit Area/Values'];
       h5varput(h5file,temp,...
          [0,k-1],[np,1],eta);      
      
     end
   end

   if(slt_check > 0)
     for i=1:slt_check
       eta=single(model.slt(i,:));
       eta(dry_loc)=-1.0;
       temp=['/Datasets/Clay ',int2str(i),' Concentration/Maxs'];
       h5varput(h5file,temp,...
          k-1,1,max(eta));
       temp=['/Datasets/Clay ',int2str(i),' Concentration/Mins'];
       h5varput(h5file,temp,...
          k-1,1,min(eta));
       temp=['/Datasets/Clay ',int2str(i),' Concentration/Values'];
       h5varput(h5file,temp,...
          [0,k-1],[np,1],eta);
      
       eta=single(model.slt_rouse(i,:));
       eta(dry_loc)=-1.0;
       temp=['/Datasets/Clay ',int2str(i),' Rouse Number/Maxs'];
       h5varput(h5file,temp,...
          k-1,1,max(eta));
       temp=['/Datasets/Clay ',int2str(i),' Rouse Number/Mins'];
       h5varput(h5file,temp,...
          k-1,1,min(eta));
       temp=['/Datasets/Clay ',int2str(i),' Rouse Number/Values'];
       h5varput(h5file,temp,...
          [0,k-1],[np,1],eta);   
      
       eta=single(model.slt_bdma(i,:));
       eta(dry_loc)=-1.0;
       temp=['/Datasets/Clay ',int2str(i),' Bedload Mass per Unit Area/Maxs'];
       h5varput(h5file,temp,...
          k-1,1,max(eta));
       temp=['/Datasets/Clay ',int2str(i),' Bedload Mass per Unit Area/Mins'];
       h5varput(h5file,temp,...
          k-1,1,min(eta));
       temp=['/Datasets/Clay ',int2str(i),' Bedload Mass per Unit Area/Values'];
       h5varput(h5file,temp,...
          [0,k-1],[np,1],eta);      
      
      
     end
   end
   
   if(smr_check > 0)
     for i=1:smr_check
       eta=single(model.smr(i,:));
       eta(dry_loc)=-1.0;
       temp=['/Datasets/Sediment Mass Residual Sediment ',int2str(i),'/Maxs'];
       h5varput(h5file,temp,...
          k-1,1,max(eta));
       temp=['/Datasets/Sediment Mass Residual Sediment ',int2str(i),'/Mins'];     
       h5varput(h5file,temp,...
          k-1,1,min(eta));
       temp=['/Datasets/Sediment Mass Residual Sediment ',int2str(i),'/Values'];
       h5varput(h5file,temp,...
          [0,k-1],[np,1],eta);  
     end
   end
   
   if(dpl_check > 0)
     eta=single(model.dpl);
     eta(dry_loc)=-1.0;
     h5varput(h5file,'/Datasets/Bed Displacement/Maxs',...
        k-1,1,max(eta));
     h5varput(h5file,'/Datasets/Bed Displacement/Mins',...
        k-1,1,min(eta));
     h5varput(h5file,'/Datasets/Bed Displacement/Values',...
        [0,k-1],[np,1],eta);  
   end   
   
   if(bsh_check > 0)
     eta=single(model.bsh);
     eta(dry_loc)=-1.0;
     h5varput(h5file,'/Datasets/Bed Shear/Maxs',...
        k-1,1,max(eta));
     h5varput(h5file,'/Datasets/Bed Shear/Mins',...
        k-1,1,min(eta));
     h5varput(h5file,'/Datasets/Bed Shear/Values',...
        [0,k-1],[np,1],eta);  
   end
   
   if(blt_check > 0)
     for i=1:blt_check
       eta=single(model.blt(i,:));
       eta(dry_loc)=-1.0;
       temp=['/Datasets/Bed Thickness Layer ',int2str(i),'/Maxs'];
       h5varput(h5file,temp,...
          k-1,1,max(eta));
       temp=['/Datasets/Bed Thickness Layer ',int2str(i),'/Mins'];      
       h5varput(h5file,temp,...
          k-1,1,min(eta));
       temp=['/Datasets/Bed Thickness Layer ',int2str(i),'/Values'];
       h5varput(h5file,temp,...
          [0,k-1],[np,1],eta);
     end
   end   
   
   if(bld_check > 0)
     for i=1:bld_check
       for j=1:number_sed
         eta=single(model.bld(i,j,:));
         eta(dry_loc)=-1.0;
         temp=['/Datasets/Bed Distribution Layer ',int2str(i),...
             ' Sediment ',int2str(j),'/Maxs'];    
         h5varput(h5file,temp,...
            k-1,1,max(eta));
         temp=['/Datasets/Bed Distribution Layer ',int2str(i),...
             ' Sediment ',int2str(j),'/Mins'];      
         h5varput(h5file,temp,...
            k-1,1,min(eta));
         temp=['/Datasets/Bed Distribution Layer ',int2str(i),...
             ' Sediment ',int2str(j),'/Values'];      
         h5varput(h5file,temp,...
            [0,k-1],[np,1],eta);
       end
     end
   end
   
   if(bed_check > 0)
     v3=single([model.bed_u,model.bed_v]');
     
     h5varput(h5file,'/Datasets/Bedload/Maxs',...
        k-1,1,max(v3(:)));
     h5varput(h5file,'/Datasets/Bedload/Mins',...
        k-1,1,single(0));
     h5varput(h5file,'/Datasets/Bedload/Values',...
        [0,0,k-1],[2,np,1],v3);      
   
     % Velocity mag
   
     v3mag=single(hypot(model.bed_u,model.bed_v)); 
   
     h5varput(h5file,'/Datasets/Bedload Mag/Maxs',...
        k-1,1,max(v3mag(:)));
     h5varput(h5file,'/Datasets/Bedload Mag/Mins',...
        k-1,1,min(v3mag(:)));
     h5varput(h5file,'/Datasets/Bedload Mag/Values',...
        [0,k-1],[np,1],v3mag);
   end
   
   if(alt_check > 0)
     eta=single(model.alt);
    
     h5varput(h5file,'/Datasets/Active Layer Thickness/Maxs',...
        k-1,1,max(eta));
     h5varput(h5file,'/Datasets/Active Layer Thickness/Mins',...
        k-1,1,min(eta));
     h5varput(h5file,'/Datasets/Active Layer Thickness/Values',...
        [0,k-1],[np,1],eta);  
   end
   
   if(alb_check > 0)
     
     for i=1:alb_check
       eta=single(model.alb(i,:));
       temp=['/Datasets/Active Layer Distribution Sediment ',int2str(i),'/Maxs'];
       h5varput(h5file,temp,...
          k-1,1,max(eta));
       temp=['/Datasets/Active Layer Distribution Sediment ',int2str(i),'/Mins'];     
       h5varput(h5file,temp,...
          k-1,1,min(eta));
       temp=['/Datasets/Active Layer Distribution Sediment ',int2str(i),'/Values'];
       h5varput(h5file,temp,...
          [0,k-1],[np,1],eta);  
     end       
   end
   
   if(cbp_check > 0)
     for i=1:cbp_check
       eta=single(model.cbp_den(i,:));
       temp=['/Datasets/Cohesive Bed Bulk Density Layer ',int2str(i),'/Maxs'];        
       h5varput(h5file,temp,...
          k-1,1,max(eta));
       temp=['/Datasets/Cohesive Bed Bulk Density Layer ',int2str(i),'/Mins'];          
       h5varput(h5file,temp,...
          k-1,1,min(eta));
       temp=['/Datasets/Cohesive Bed Bulk Density Layer ',int2str(i),'/Values'];          
       h5varput(h5file,temp,...
          [0,k-1],[np,1],eta);
      
       eta=single(model.cbp_ces(i,:));
       temp=['/Datasets/Cohesive Bed Critical Erosion Shear Layer ',int2str(i),'/Maxs'];        
       h5varput(h5file,temp,...
          k-1,1,max(eta));
       temp=['/Datasets/Cohesive Bed Critical Erosion Shear Layer ',int2str(i),'/Mins'];          
       h5varput(h5file,temp,...
          k-1,1,min(eta));
       temp=['/Datasets/Cohesive Bed Critical Erosion Shear Layer ',int2str(i),'/Values'];          
       h5varput(h5file,temp,...
          [0,k-1],[np,1],eta);
      
       eta=single(model.cbp_erc(i,:));
       temp=['/Datasets/Cohesive Bed Erosion Rate Constant Layer ',int2str(i),'/Maxs'];        
       h5varput(h5file,temp,...
          k-1,1,max(eta));
       temp=['/Datasets/Cohesive Bed Erosion Rate Constant Layer ',int2str(i),'/Mins'];          
       h5varput(h5file,temp,...
          k-1,1,min(eta));
       temp=['/Datasets/Cohesive Bed Erosion Rate Constant Layer ',int2str(i),'/Values'];          
       h5varput(h5file,temp,...
          [0,k-1],[np,1],eta);
      
       eta=single(model.cbp_ere(i,:));
       temp=['/Datasets/Cohesive Bed Erosion Rate Exponent ',int2str(i),'/Maxs'];        
       h5varput(h5file,temp,...
          k-1,1,max(eta));
       temp=['/Datasets/Cohesive Bed Erosion Rate Exponent ',int2str(i),'/Mins'];          
       h5varput(h5file,temp,...
          k-1,1,min(eta));
       temp=['/Datasets/Cohesive Bed Erosion Rate Exponent ',int2str(i),'/Values'];          
       h5varput(h5file,temp,...
          [0,k-1],[np,1],eta);
      
     end
   end   
   
end

fprintf(1,'\nJob Complete.\n');
fid=H5F.open(h5file,'H5F_ACC_RDWR','H5P_DEFAULT');

H5F.close(fid);
