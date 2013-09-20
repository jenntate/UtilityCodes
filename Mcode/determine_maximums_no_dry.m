function determine_maximums_no_dry(basename,attributes,start_time,stop_time)
%
%
%    Subroutine to detemine the maximum values for the all the attributes
%    variables along with the inundation times (total inundation time and 
%    the percentage of the total time inundated) if the dep attribute is
%    specified
%
%    Input variables:
%
%       basename = basename of the da file to analyze
%
%       attributes = string containing the parameters to determine maximums
%                    These are:
%
%                    dep = depth solution
%                    wse = water surface elevation solution
%                    vel = velocity vector and magnitude solution
%                    sal = salinity solution
%                    vor = vorticity solution
%                    snd = sand concentration solution
%                    slt = silt concentration solution
%                    err = error solution
%                    dpl = bed displacement solution
%                    alt = active layer thickness
%                    ald = active layer distribution
%                    blt = bed layer thickness
%                    bld = bed layer distribution
%                    cbp = cohesive bed properties
%                    bsh = bed shear stress solution
%                    bed = bedload vector and magnitude solution
%                    smr = sediment mass residual
%
%       start_time = string variable for start time over which to get
%                    maximums and do inundation times.
%                    (Example = '01/01/2008 00:00:00')
%
%       stop_time = string variable of the desired stop time over which to
%                   get maimums and do inundation times. 
%                    (Example = '01/01/2009 00:00:00')
%
%       Example Usage:
%       
%       determine_maxiums('Mobile','dep','01/01/2010 00:00:00', ...
%       '02/01/2010 00:00:00')
%
%       Output:
%
%       For each attribute specified there will be an ascii formatted adh
%       solution file created with the extension of *_max.dat.  If the dep
%       attribute is specified then the inundation time solution file will
%       be specified as *_percent_inundation.dat.  The first time step of
%       the inundation solution file will be the percent wetted value 
%       (between 0 and 100) and the second time step will be the actually 
%       wetted time in DAYS.
%
%

%% convert the start and stop times to serial date format
tic


%% reads the header information for all da files

check=load_da_stepnew([basename '.da'], 0);
grid=read_adh_grid([basename '.3dm']);

%basename=[basename '_' start_time '_' stop_time];

start_time=datenum(start_time);
stop_time=datenum(stop_time);

data=load_da_stepnew([basename '.da'],1);
two=load_da_stepnew([basename '.da'],data.nt);

if(start_time > two.time)
    fprintf(1,'ERROR START TIME IS AFTER THE FINAL DA TIME\n');
    STOP
end
if(stop_time < data.time)
    fprintf(1,'ERROR STOP TIME IS BEFORE THE INITIAL DA TIME\n');
    STOP
end

%last_time=data.time;
% if(data.time>=start_time)
%   first_time=data.time;
% else
%   first_time=start_time;
% end
count=0;
%last_time=first_time;

data_1=load_da_time_series([basename '.da'],1);

data_start=find(data_1.time>=start_time,1,'first');
data_stop=find(data_1.time<=stop_time,1,'last');
last_time=data_1.time(data_start);
c=data_1.time(data_start);
d=data_1.time(data_stop);
a=datestr(double(int64(c)),1);
b=datestr(double(int64(d)),1);
basename_1=basename;
basename=[basename '_' a '_' b];

%interval=int8((data_stop-data_start)/5.0);

%interval_1=data_start+interval;
%interval_2=interval_1+interval;
%interval_3=interval_2+interval;
%interval_4=interval_3+interval;

fprintf(1,'Desired data starts at time step %i and time %s\n',...
    data_start,datestr(data_1.time(data_start)));
fprintf(1,'Desired data stops at time step %i and time %s\n',...
    data_stop,datestr(data_1.time(data_stop)));

%% checks the input attributes to and that those attributes are in the da
% files

dep_check=0;
wse_check=0;
vel_check=0;
err_check=0;
sal_check=0;
vor_check=0;
snd_check=0;
slt_check=0;
smr_check=0;
dpl_check=0;
bsh_check=0;
blt_check=0;
bld_check=0;
bed_check=0;
alt_check=0;
alb_check=0;
cbp_check=0;

temp1=sscanf(attributes,'%s');
for i=1:length(temp1)/3
   test=temp1((1+(i-1)*3):3*i);
   switch lower(test)
       
       % checks the dep
       
       case {'dep'}
           dep_check=check.dep_check;
           if(check.dep_check < 1)
              fprintf(1,'\nAttribute %s does not exist in %s.da\n\n',...
                  'dep',basename);
              STOP;
           end
           fid_dep=fopen([basename '_dep_max.dat'],'w');
           fprintf(fid_dep,'DATASET\n');
           fprintf(fid_dep,'OBJTYPE "mesh2d"\n');
           fprintf(fid_dep,'BEGSCL\n');
           fprintf(fid_dep,'ND %i\n',grid.nodes);
           fprintf(fid_dep,'NC %i\n',grid.elements);
           fprintf(fid_dep,'NAME "DEPTH"\n');
           fprintf(fid_dep,'TIMEUNITS SECONDS\n');
           dep.min=zeros(check.np,1);
           dep.max=zeros(check.np,1);
           sum_dep=zeros(check.np,1);
           dep.min(:,:)=999999999.9;
           dep.max(:,:)=-999999999.9;
           
           fid_time=fopen([basename '_dep_inundated.dat'],'w');
           fprintf(fid_time,'DATASET\n');
           fprintf(fid_time,'OBJTYPE "mesh2d"\n');
           fprintf(fid_time,'BEGSCL\n');
           fprintf(fid_time,'ND %i\n',grid.nodes);
           fprintf(fid_time,'NC %i\n',grid.elements);
           fprintf(fid_time,'NAME "Inundation Time"\n');
           fprintf(fid_time,'TIMEUNITS SECONDS\n');
           wetted_time=zeros(check.np,1);    
           
       % checks the wse    
           
       case {'wse'}
           wse_check=check.wse_check;
           if(check.wse_check < 1)
               fprintf(1,'\nAttribute %s does not exist in %s.da\n\n',...
                   'wse',basename);
               STOP;
           end
%           name=[basename '_wse_max.dat']
           fid_wse=fopen([basename '_wse_max.dat'],'w');
           fprintf(fid_wse,'DATASET\n');
           fprintf(fid_wse,'OBJTYPE "mesh2d"\n');
           fprintf(fid_wse,'BEGSCL\n');
           fprintf(fid_wse,'ND %i\n',grid.nodes);
           fprintf(fid_wse,'NC %i\n',grid.elements);
           fprintf(fid_wse,'NAME "Water Surface Elevation"\n');
           fprintf(fid_wse,'TIMEUNITS SECONDS\n');
           wse.min=zeros(check.np,1);
           wse.max=zeros(check.np,1);
           sum_wse=zeros(check.np,1);
           wse.min(:,:)=99999999.9;
           wse.max(:,:)=-999999999.9;
           
       % checks the velocity    
           
       case {'vel'}
           vel_check=check.vel_check;
           if(check.vel_check < 1)
               fprintf(1,'\nAttribute %s does not exist in %s.da\n\n',...
                   'vel',basename);
               STOP;
           end
           fid_vel=fopen([basename '_vel_max.dat'],'w');
           fprintf(fid_vel,'DATASET\n');
           fprintf(fid_vel,'OBJTYPE "mesh2d"\n');
           fprintf(fid_vel,'BEGSCL\n');
           fprintf(fid_vel,'ND %i\n',grid.nodes);
           fprintf(fid_vel,'NC %i\n',grid.elements);
           fprintf(fid_vel,'NAME "Velocities"\n');
           fprintf(fid_vel,'TIMEUNITS SECONDS\n');
           vel.min=zeros(check.np,1);
           vel.max=zeros(check.np,1);
           sum_vel=zeros(check.np,1);
           %vel_mag=zeros(check.np,1);
           vel.min(:,:)=99999999.9;
           vel.max(:,:)=-999999999.9;
           
           fid_vel1=fopen([basename '_ovl_max.dat'],'w');
           fprintf(fid_vel1,'DATASET\n');
           fprintf(fid_vel1,'OBJTYPE "mesh2d"\n');
           fprintf(fid_vel1,'BEGVEC\n');
           fprintf(fid_vel1,'ND %i\n',grid.nodes);
           fprintf(fid_vel1,'NC %i\n',grid.elements);
           fprintf(fid_vel1,'NAME "Velocities"\n');
           fprintf(fid_vel1,'TIMEUNITS SECONDS\n');
           vel.xmin=zeros(check.np,1);
           vel.xmax=zeros(check.np,1);
           sum_xvel=zeros(check.np,1);
           vel.ymin=zeros(check.np,1);
           vel.ymax=zeros(check.np,1);
           sum_yvel=zeros(check.np,1);
           %velx=zeros(check.np,1);
           %vely=zeros(check.np,1);
           vel.xmin(:,:)=99999999.9;
           vel.xmax(:,:)=-999999999.9;           
           vel.ymin(:,:)=99999999.9;
           vel.ymax(:,:)=-999999999.9;           
           
       % checks the error    
           
       case {'err'}
           err_check=check.err_check;
           if(check.err_check < 1)
               fprintf(1,'\nAttribute %s does not exist in %s.da\n\n',...
                   'err',basename);
               STOP;
           end 
           fid_err=fopen([basename '_err_max.dat'],'w');
           fprintf(fid_err,'DATASET\n');
           fprintf(fid_err,'OBJTYPE "mesh2d"\n');
           fprintf(fid_err,'BEGSCL\n');
           fprintf(fid_err,'ND %i\n',grid.nodes);
           fprintf(fid_err,'NC %i\n',grid.elements);
           fprintf(fid_err,'NAME "Error"\n');
           fprintf(fid_err,'TIMEUNITS SECONDS\n');           
           err.min=zeros(check.np,1);
           err.max=zeros(check.np,1);
           sum_err=zeros(check.np,1);
           err.min(:,:)=9999999.9;
           err.max(:,:)=-99999999.9;  
           
       % checks the salinity    
           
       case {'sal'}
           sal_check=check.sal_check;
           if(check.sal_check < 1)
               fprintf(1,'\nAttribute %s does not exist in %s.da\n\n',...
                   'sal',basename);
               STOP;
           end
           fid_sal=fopen([basename '_sal_max.dat'],'w');
           fprintf(fid_sal,'DATASET\n');
           fprintf(fid_sal,'OBJTYPE "mesh2d"\n');
           fprintf(fid_sal,'BEGSCL\n');
           fprintf(fid_sal,'ND %i\n',grid.nodes);
           fprintf(fid_sal,'NC %i\n',grid.elements);
           fprintf(fid_sal,'NAME "Salinity"\n');
           fprintf(fid_sal,'TIMEUNITS SECONDS\n');           
           sal.min=zeros(check.np,1);
           sal.max=zeros(check.np,1);
           sum_sal=zeros(check.np,1);
           sal.min(:,:)=9999999.9;
           sal.max(:,:)=-99999999.9;
           
       % checks the vorticity    
           
       case {'vor'}
           vor_check=check.vor_check;
           if(check.vor_check < 1)
                 fprintf(1,'\nAttribute %s does not exist in %s.da\n\n',...
                     'vor',basename);
                 STOP;
           end   
           fid_vor=fopen([basename '_vor_max.dat'],'w');
           fprintf(fid_vor,'DATASET\n');
           fprintf(fid_vor,'OBJTYPE "mesh2d"\n');
           fprintf(fid_vor,'BEGSCL\n');
           fprintf(fid_vor,'ND %i\n',grid.nodes);
           fprintf(fid_vor,'NC %i\n',grid.elements);
           fprintf(fid_vor,'NAME "Vorticity"\n');
           fprintf(fid_vor,'TIMEUNITS SECONDS\n');             
           vor.min=zeros(check.np,1);
           vor.max=zeros(check.np,1);
           sum_vor=zeros(check.np,1);
           vor.min(:,:)=99999999.9;
           vor.max(:,:)=-99999999.9;
           
       % checks the sand    
           
       case {'snd'}
           snd_check=check.snd_check;
           if(check.snd_check < 1)
               fprintf(1,'\nAttribute %s does not exist in %s.da\n\n',...
                   'snd',basename);
               STOP;
           end
           fid_snd=zeros(check.snd_check,1);
           for y=1:snd_check
             if(y==1)
               fid_snd(y)=fopen([basename '_snd1_max.dat'],'w');
               fprintf(fid_snd(y),'DATASET\n');
               fprintf(fid_snd(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_snd(y),'BEGSCL\n');
               fprintf(fid_snd(y),'ND %i\n',grid.nodes);
               fprintf(fid_snd(y),'NC %i\n',grid.elements);
               fprintf(fid_snd(y),'NAME "Sand 1"\n');
               fprintf(fid_snd(y),'TIMEUNITS SECONDS\n');                
             end
             if(y==2)
               fid_snd(y)=fopen([basename '_snd2_max.dat'],'w');
               fprintf(fid_snd(y),'DATASET\n');
               fprintf(fid_snd(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_snd(y),'BEGSCL\n');
               fprintf(fid_snd(y),'ND %i\n',grid.nodes);
               fprintf(fid_snd(y),'NC %i\n',grid.elements);
               fprintf(fid_snd(y),'NAME "Sand 2"\n');
               fprintf(fid_snd(y),'TIMEUNITS SECONDS\n');                
             end
             if(y==3)
               fid_snd(y)=fopen([basename '_snd3_max.dat'],'w');
               fprintf(fid_snd(y),'DATASET\n');
               fprintf(fid_snd(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_snd(y),'BEGSCL\n');
               fprintf(fid_snd(y),'ND %i\n',grid.nodes);
               fprintf(fid_snd(y),'NC %i\n',grid.elements);
               fprintf(fid_snd(y),'NAME "Sand 3"\n');
               fprintf(fid_snd(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==4)
               fid_snd(y)=fopen([basename '_snd4_max.dat'],'w');
               fprintf(fid_snd(y),'DATASET\n');
               fprintf(fid_snd(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_snd(y),'BEGSCL\n');
               fprintf(fid_snd(y),'ND %i\n',grid.nodes);
               fprintf(fid_snd(y),'NC %i\n',grid.elements);
               fprintf(fid_snd(y),'NAME "Sand 4"\n');
               fprintf(fid_snd(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==5)
               fid_snd(y)=fopen([basename '_snd5_max.dat'],'w');
               fprintf(fid_snd(y),'DATASET\n');
               fprintf(fid_snd(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_snd(y),'BEGSCL\n');
               fprintf(fid_snd(y),'ND %i\n',grid.nodes);
               fprintf(fid_snd(y),'NC %i\n',grid.elements);
               fprintf(fid_snd(y),'NAME "Sand 5"\n');
               fprintf(fid_snd(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==6)
               fid_snd(y)=fopen([basename '_snd6_max.dat'],'w');
               fprintf(fid_snd(y),'DATASET\n');
               fprintf(fid_snd(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_snd(y),'BEGSCL\n');
               fprintf(fid_snd(y),'ND %i\n',grid.nodes);
               fprintf(fid_snd(y),'NC %i\n',grid.elements);
               fprintf(fid_snd(y),'NAME "Sand 6"\n');
               fprintf(fid_snd(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==7)
               fid_snd(y)=fopen([basename '_snd7_max.dat'],'w');
               fprintf(fid_snd(y),'DATASET\n');
               fprintf(fid_snd(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_snd(y),'BEGSCL\n');
               fprintf(fid_snd(y),'ND %i\n',grid.nodes);
               fprintf(fid_snd(y),'NC %i\n',grid.elements);
               fprintf(fid_snd(y),'NAME "Sand 7"\n');
               fprintf(fid_snd(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==8)
               fid_snd(y)=fopen([basename '_snd8_max.dat'],'w');
               fprintf(fid_snd(y),'DATASET\n');
               fprintf(fid_snd(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_snd(y),'BEGSCL\n');
               fprintf(fid_snd(y),'ND %i\n',grid.nodes);
               fprintf(fid_snd(y),'NC %i\n',grid.elements);
               fprintf(fid_snd(y),'NAME "Sand 8"\n');
               fprintf(fid_snd(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==9)
               fid_snd(y)=fopen([basename '_snd9_max.dat'],'w');
               fprintf(fid_snd(y),'DATASET\n');
               fprintf(fid_snd(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_snd(y),'BEGSCL\n');
               fprintf(fid_snd(y),'ND %i\n',grid.nodes);
               fprintf(fid_snd(y),'NC %i\n',grid.elements);
               fprintf(fid_snd(y),'NAME "Sand 9"\n');
               fprintf(fid_snd(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==10)
               fid_snd(y)=fopen([basename '_snd10_max.dat'],'w');
               fprintf(fid_snd(y),'DATASET\n');
               fprintf(fid_snd(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_snd(y),'BEGSCL\n');
               fprintf(fid_snd(y),'ND %i\n',grid.nodes);
               fprintf(fid_snd(y),'NC %i\n',grid.elements);
               fprintf(fid_snd(y),'NAME "Sand 10"\n');
               fprintf(fid_snd(y),'TIMEUNITS SECONDS\n'); 
             end              
           end
           
           snd.min=zeros(snd_check,check.np);
           snd.max=zeros(snd_check,check.np);
           sum_snd=zeros(snd_check,check.np);
           snd_rouse.min=zeros(snd_check,check.np);
           snd_rouse.max=zeros(snd_check,check.np);
           sum_snd_rouse=zeros(snd_check,check.np);
           snd_bdma.min=zeros(snd_check,check.np);
           snd_bdma.max=zeros(snd_check,check.np);
           sum_snd_bdma=zeros(snd_check,check.np);
           snd.min(:,:)=9999999.9;
           snd.max(:,:)=-99999999.9;
           snd_rouse.min(:,:)=9999999.9;
           snd_rouse.max(:,:)=-9999999.9;
           snd_bdma.min(:,:)=99999999.9;
           snd_bdma.max(:,:)=-99999999.9;
           
       % checks the silts    
           
       case {'slt'}
           slt_check=check.slt_check;
           if(check.slt_check < 1)
               fprintf(1,'\nAttribute %s does not exist in %s.da\n\n',...
                   'slt',basename);
               STOP;
           end
           fid_slt=zeros(check.slt_check,1);
           for y=1:slt_check
             if(y==1)
               fid_slt(y)=fopen([basename '_slt1_max.dat'],'w');
               fprintf(fid_slt(y),'DATASET\n');
               fprintf(fid_slt(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_slt(y),'BEGSCL\n');
               fprintf(fid_slt(y),'ND %i\n',grid.nodes);
               fprintf(fid_slt(y),'NC %i\n',grid.elements);
               fprintf(fid_slt(y),'NAME "Silt 1"\n');
               fprintf(fid_slt(y),'TIMEUNITS SECONDS\n');                
             end
             if(y==2)
               fid_slt(y)=fopen([basename '_slt2_max.dat'],'w');
               fprintf(fid_slt(y),'DATASET\n');
               fprintf(fid_slt(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_slt(y),'BEGSCL\n');
               fprintf(fid_slt(y),'ND %i\n',grid.nodes);
               fprintf(fid_slt(y),'NC %i\n',grid.elements);
               fprintf(fid_slt(y),'NAME "Silt 2"\n');
               fprintf(fid_slt(y),'TIMEUNITS SECONDS\n');                
             end
             if(y==3)
               fid_slt(y)=fopen([basename '_slt3_max.dat'],'w');
               fprintf(fid_slt(y),'DATASET\n');
               fprintf(fid_slt(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_slt(y),'BEGSCL\n');
               fprintf(fid_slt(y),'ND %i\n',grid.nodes);
               fprintf(fid_slt(y),'NC %i\n',grid.elements);
               fprintf(fid_slt(y),'NAME "Silt 3"\n');
               fprintf(fid_slt(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==4)
               fid_slt(y)=fopen([basename '_slt4_max.dat'],'w');
               fprintf(fid_slt(y),'DATASET\n');
               fprintf(fid_slt(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_slt(y),'BEGSCL\n');
               fprintf(fid_slt(y),'ND %i\n',grid.nodes);
               fprintf(fid_slt(y),'NC %i\n',grid.elements);
               fprintf(fid_slt(y),'NAME "Silt 4"\n');
               fprintf(fid_slt(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==5)
               fid_slt(y)=fopen([basename '_slt5_max.dat'],'w');
               fprintf(fid_slt(y),'DATASET\n');
               fprintf(fid_slt(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_slt(y),'BEGSCL\n');
               fprintf(fid_slt(y),'ND %i\n',grid.nodes);
               fprintf(fid_slt(y),'NC %i\n',grid.elements);
               fprintf(fid_slt(y),'NAME "Silt 5"\n');
               fprintf(fid_slt(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==6)
               fid_slt(y)=fopen([basename '_slt6_max.dat'],'w');
               fprintf(fid_slt(y),'DATASET\n');
               fprintf(fid_slt(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_slt(y),'BEGSCL\n');
               fprintf(fid_slt(y),'ND %i\n',grid.nodes);
               fprintf(fid_slt(y),'NC %i\n',grid.elements);
               fprintf(fid_slt(y),'NAME "Silt 6"\n');
               fprintf(fid_slt(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==7)
               fid_slt(y)=fopen([basename '_slt7_max.dat'],'w');
               fprintf(fid_slt(y),'DATASET\n');
               fprintf(fid_slt(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_slt(y),'BEGSCL\n');
               fprintf(fid_slt(y),'ND %i\n',grid.nodes);
               fprintf(fid_slt(y),'NC %i\n',grid.elements);
               fprintf(fid_slt(y),'NAME "Silt 7"\n');
               fprintf(fid_slt(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==8)
               fid_slt(y)=fopen([basename '_slt8_max.dat'],'w');
               fprintf(fid_slt(y),'DATASET\n');
               fprintf(fid_slt(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_slt(y),'BEGSCL\n');
               fprintf(fid_slt(y),'ND %i\n',grid.nodes);
               fprintf(fid_slt(y),'NC %i\n',grid.elements);
               fprintf(fid_slt(y),'NAME "Silt 8"\n');
               fprintf(fid_slt(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==9)
               fid_slt(y)=fopen([basename '_slt9_max.dat'],'w');
               fprintf(fid_slt(y),'DATASET\n');
               fprintf(fid_slt(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_slt(y),'BEGSCL\n');
               fprintf(fid_slt(y),'ND %i\n',grid.nodes);
               fprintf(fid_slt(y),'NC %i\n',grid.elements);
               fprintf(fid_slt(y),'NAME "Silt 9"\n');
               fprintf(fid_slt(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==10)
               fid_slt(y)=fopen([basename '_slt10_max.dat'],'w');
               fprintf(fid_slt(y),'DATASET\n');
               fprintf(fid_slt(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_slt(y),'BEGSCL\n');
               fprintf(fid_slt(y),'ND %i\n',grid.nodes);
               fprintf(fid_slt(y),'NC %i\n',grid.elements);
               fprintf(fid_slt(y),'NAME "Silt 10"\n');
               fprintf(fid_slt(y),'TIMEUNITS SECONDS\n'); 
             end              
           end
           
           slt.min=zeros(slt_check,check.np);
           slt.max=zeros(slt_check,check.np);
           sum_slt=zeros(slt_check,check.np);
           slt_rouse.min=zeros(slt_check,check.np);
           slt_rouse.max=zeros(slt_check,check.np);
           sum_slt_rouse=zeros(slt_check,check.np);
           slt_bdma.min=zeros(slt_check,check.np);
           slt_bdma.max=zeros(slt_check,check.np);
           sum_slt_bdma=zeros(slt_check,check.np);
           slt.min(:,:)=99999999.9;
           slt.max(:,:)=-99999999.9;
           slt_rouse.min(:,:)=99999999.9;
           slt_rouse.max(:,:)=-999999999.9;
           slt_bdma.min(:,:)=999999999.9;
           slt_bdma.max(:,:)=-99999999.9;
           
       % checks the bed load transport    
           
       case {'blt'}
           blt_check=check.blt_check;
           if(check.blt_check < 1)
               fprintf(1,'\nAttribute %s does not exist in %s.da\n\n',...
                   'blt',basename);
               STOP;
           end
           fid_blt=zeros(check.blt_check,1);
           for y=1:blt_check
             if(y==1)
               fid_blt(y)=fopen([basename '_blt1_max.dat'],'w');
               fprintf(fid_blt(y),'DATASET\n');
               fprintf(fid_blt(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_blt(y),'BEGSCL\n');
               fprintf(fid_blt(y),'ND %i\n',grid.nodes);
               fprintf(fid_blt(y),'NC %i\n',grid.elements);
               fprintf(fid_blt(y),'NAME "Bed Layer Thickness 1"\n');
               fprintf(fid_blt(y),'TIMEUNITS SECONDS\n');                
             end
             if(y==2)
               fid_blt(y)=fopen([basename '_blt2_max.dat'],'w');
               fprintf(fid_blt(y),'DATASET\n');
               fprintf(fid_blt(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_blt(y),'BEGSCL\n');
               fprintf(fid_blt(y),'ND %i\n',grid.nodes);
               fprintf(fid_blt(y),'NC %i\n',grid.elements);
               fprintf(fid_blt(y),'NAME "Bed Layer Thickness 2"\n');
               fprintf(fid_blt(y),'TIMEUNITS SECONDS\n');                
             end
             if(y==3)
               fid_blt(y)=fopen([basename '_blt3_max.dat'],'w');
               fprintf(fid_blt(y),'DATASET\n');
               fprintf(fid_blt(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_blt(y),'BEGSCL\n');
               fprintf(fid_blt(y),'ND %i\n',grid.nodes);
               fprintf(fid_blt(y),'NC %i\n',grid.elements);
               fprintf(fid_blt(y),'NAME "Bed Layer Thickness 3"\n');
               fprintf(fid_blt(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==4)
               fid_blt(y)=fopen([basename '_blt4_max.dat'],'w');
               fprintf(fid_blt(y),'DATASET\n');
               fprintf(fid_blt(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_blt(y),'BEGSCL\n');
               fprintf(fid_blt(y),'ND %i\n',grid.nodes);
               fprintf(fid_blt(y),'NC %i\n',grid.elements);
               fprintf(fid_blt(y),'NAME "Bed Layer Thickness 4"\n');
               fprintf(fid_blt(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==5)
               fid_blt(y)=fopen([basename '_blt5_max.dat'],'w');
               fprintf(fid_blt(y),'DATASET\n');
               fprintf(fid_blt(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_blt(y),'BEGSCL\n');
               fprintf(fid_blt(y),'ND %i\n',grid.nodes);
               fprintf(fid_blt(y),'NC %i\n',grid.elements);
               fprintf(fid_blt(y),'NAME "Bed Layer Thickness 5"\n');
               fprintf(fid_blt(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==6)
               fid_blt(y)=fopen([basename '_blt6_max.dat'],'w');
               fprintf(fid_blt(y),'DATASET\n');
               fprintf(fid_blt(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_blt(y),'BEGSCL\n');
               fprintf(fid_blt(y),'ND %i\n',grid.nodes);
               fprintf(fid_blt(y),'NC %i\n',grid.elements);
               fprintf(fid_blt(y),'NAME "Bed Layer Thickness 6"\n');
               fprintf(fid_blt(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==7)
               fid_blt(y)=fopen([basename '_blt7_max.dat'],'w');
               fprintf(fid_blt(y),'DATASET\n');
               fprintf(fid_blt(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_blt(y),'BEGSCL\n');
               fprintf(fid_blt(y),'ND %i\n',grid.nodes);
               fprintf(fid_blt(y),'NC %i\n',grid.elements);
               fprintf(fid_blt(y),'NAME "Bed Layer Thickness 7"\n');
               fprintf(fid_blt(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==8)
               fid_blt(y)=fopen([basename '_blt8_max.dat'],'w');
               fprintf(fid_blt(y),'DATASET\n');
               fprintf(fid_blt(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_blt(y),'BEGSCL\n');
               fprintf(fid_blt(y),'ND %i\n',grid.nodes);
               fprintf(fid_blt(y),'NC %i\n',grid.elements);
               fprintf(fid_blt(y),'NAME "Bed Layer Thickness 8"\n');
               fprintf(fid_blt(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==9)
               fid_blt(y)=fopen([basename '_blt9_max.dat'],'w');
               fprintf(fid_blt(y),'DATASET\n');
               fprintf(fid_blt(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_blt(y),'BEGSCL\n');
               fprintf(fid_blt(y),'ND %i\n',grid.nodes);
               fprintf(fid_blt(y),'NC %i\n',grid.elements);
               fprintf(fid_blt(y),'NAME "Bed Layer Thickness 9"\n');
               fprintf(fid_blt(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==10)
               fid_blt(y)=fopen([basename '_blt10_max.dat'],'w');
               fprintf(fid_blt(y),'DATASET\n');
               fprintf(fid_blt(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_blt(y),'BEGSCL\n');
               fprintf(fid_blt(y),'ND %i\n',grid.nodes);
               fprintf(fid_blt(y),'NC %i\n',grid.elements);
               fprintf(fid_blt(y),'NAME "Bed Layer Thickness 10"\n');
               fprintf(fid_blt(y),'TIMEUNITS SECONDS\n'); 
             end              
           end
           
           blt.min=zeros(check.number_layers,check.np);
           blt.max=zeros(check.number_layers,check.np);
           sum_blt=zeros(check.number_layers,check.np);
           blt.min(:,:)=99999999.9;
           blt.max(:,:)=-99999999.9;
           
       % checks the bed load displacement    
           
       case {'bld'}
           bld_check=check.bld_check;
           if(check.bld_check < 1)
               fprintf(1,'\nAttribute %s does not exist in %s.da\n\n',...
                   'bld',basename);
               STOP;
           end

           fid_bld=zeros(check.bld_check,1);
           for y=1:bld_check
             if(y==1)
               fid_bld(y)=fopen([basename '_bld1_max.dat'],'w');
               fprintf(fid_bld(y),'DATASET\n');
               fprintf(fid_bld(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_bld(y),'BEGSCL\n');
               fprintf(fid_bld(y),'ND %i\n',grid.nodes);
               fprintf(fid_bld(y),'NC %i\n',grid.elements);
               fprintf(fid_bld(y),'NAME "Bed Layer Distribution 1"\n');
               fprintf(fid_bld(y),'TIMEUNITS SECONDS\n');                
             end
             if(y==2)
               fid_bld(y)=fopen([basename '_bld2_max.dat'],'w');
               fprintf(fid_bld(y),'DATASET\n');
               fprintf(fid_bld(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_bld(y),'BEGSCL\n');
               fprintf(fid_bld(y),'ND %i\n',grid.nodes);
               fprintf(fid_bld(y),'NC %i\n',grid.elements);
               fprintf(fid_bld(y),'NAME "Bed Layer Distribution 2"\n');
               fprintf(fid_bld(y),'TIMEUNITS SECONDS\n');                
             end
             if(y==3)
               fid_bld(y)=fopen([basename '_bld3_max.dat'],'w');
               fprintf(fid_bld(y),'DATASET\n');
               fprintf(fid_bld(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_bld(y),'BEGSCL\n');
               fprintf(fid_bld(y),'ND %i\n',grid.nodes);
               fprintf(fid_bld(y),'NC %i\n',grid.elements);
               fprintf(fid_bld(y),'NAME "Bed Layer Distribution 3"\n');
               fprintf(fid_bld(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==4)
               fid_bld(y)=fopen([basename '_bld4_max.dat'],'w');
               fprintf(fid_bld(y),'DATASET\n');
               fprintf(fid_bld(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_bld(y),'BEGSCL\n');
               fprintf(fid_bld(y),'ND %i\n',grid.nodes);
               fprintf(fid_bld(y),'NC %i\n',grid.elements);
               fprintf(fid_bld(y),'NAME "Bed Layer Distribution 4"\n');
               fprintf(fid_bld(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==5)
               fid_bld(y)=fopen([basename '_bld5_max.dat'],'w');
               fprintf(fid_bld(y),'DATASET\n');
               fprintf(fid_bld(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_bld(y),'BEGSCL\n');
               fprintf(fid_bld(y),'ND %i\n',grid.nodes);
               fprintf(fid_bld(y),'NC %i\n',grid.elements);
               fprintf(fid_bld(y),'NAME "Bed Layer Distribution 5"\n');
               fprintf(fid_bld(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==6)
               fid_bld(y)=fopen([basename '_bld6_max.dat'],'w');
               fprintf(fid_bld(y),'DATASET\n');
               fprintf(fid_bld(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_bld(y),'BEGSCL\n');
               fprintf(fid_bld(y),'ND %i\n',grid.nodes);
               fprintf(fid_bld(y),'NC %i\n',grid.elements);
               fprintf(fid_bld(y),'NAME "Bed Layer Distribution 6"\n');
               fprintf(fid_bld(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==7)
               fid_bld(y)=fopen([basename '_bld7_max.dat'],'w');
               fprintf(fid_bld(y),'DATASET\n');
               fprintf(fid_bld(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_bld(y),'BEGSCL\n');
               fprintf(fid_bld(y),'ND %i\n',grid.nodes);
               fprintf(fid_bld(y),'NC %i\n',grid.elements);
               fprintf(fid_bld(y),'NAME "Bed Layer Distribution 7"\n');
               fprintf(fid_bld(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==8)
               fid_bld(y)=fopen([basename '_bld8_max.dat'],'w');
               fprintf(fid_bld(y),'DATASET\n');
               fprintf(fid_bld(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_bld(y),'BEGSCL\n');
               fprintf(fid_bld(y),'ND %i\n',grid.nodes);
               fprintf(fid_bld(y),'NC %i\n',grid.elements);
               fprintf(fid_bld(y),'NAME "Bed Layer Distribution 8"\n');
               fprintf(fid_bld(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==9)
               fid_bld(y)=fopen([basename '_bld9_max.dat'],'w');
               fprintf(fid_bld(y),'DATASET\n');
               fprintf(fid_bld(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_bld(y),'BEGSCL\n');
               fprintf(fid_bld(y),'ND %i\n',grid.nodes);
               fprintf(fid_bld(y),'NC %i\n',grid.elements);
               fprintf(fid_bld(y),'NAME "Bed Layer Distribution 9"\n');
               fprintf(fid_bld(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==10)
               fid_bld(y)=fopen([basename '_bld10_max.dat'],'w');
               fprintf(fid_bld(y),'DATASET\n');
               fprintf(fid_bld(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_bld(y),'BEGSCL\n');
               fprintf(fid_bld(y),'ND %i\n',grid.nodes);
               fprintf(fid_bld(y),'NC %i\n',grid.elements);
               fprintf(fid_bld(y),'NAME "Bed Layer Distribution 10"\n');
               fprintf(fid_bld(y),'TIMEUNITS SECONDS\n'); 
             end              
           end                      
           
           bld.min=zeros(check.number_layers,check.number_sed,check.np);
           bld.max=zeros(check.number_layers,check.number_sed,check.np);
           sum_bld=zeros(check.number_layers,check.number_sed,check.np);
           bld.min(:,:)=99999999.9;
           bld.max(:,:)=-999999999.9;
           
       % checks the smr    
           
       case {'smr'}
           smr_check=check.smr_check;
           if(check.smr_check < 1)
               fprintf(1,'\nAttribute %s does not exist in %s.da\n\n',...
                   'smr',basename);
               STOP;
           end
           fid_smr=fopen([basename '_smr_max.dat'],'w');
           fprintf(fid_smr,'DATASET\n');
           fprintf(fid_smr,'OBJTYPE "mesh2d"\n');
           fprintf(fid_smr,'BEGSCL\n');
           fprintf(fid_smr,'ND %i\n',grid.nodes);
           fprintf(fid_smr,'NC %i\n',grid.elements);
           fprintf(fid_smr,'NAME "Sediment Mass Residual"\n');
           fprintf(fid_smr,'TIMEUNITS SECONDS\n'); 
           
           smr.min=zeros(smr_check,check.np);
           smr.max=zeros(smr_check,check.np);
           sum_smr=zeros(smr_check,check.np);
           smr.min(:,:)=99999999.9;
           smr.max(:,:)=-999999999.9;    
           
       % checks the bed displacement    
           
       case {'dpl'}
           dpl_check=check.dpl_check;
           if(check.dpl_check < 1)
               fprintf(1,'\nAttribute %s does not exist in %s.da\n\n',...
                   'dpl',basename);
               STOP;
           end
           fid_dpl=fopen([basename '_dpl_max.dat'],'w');
           fprintf(fid_dpl,'DATASET\n');
           fprintf(fid_dpl,'OBJTYPE "mesh2d"\n');
           fprintf(fid_dpl,'BEGSCL\n');
           fprintf(fid_dpl,'ND %i\n',grid.nodes);
           fprintf(fid_dpl,'NC %i\n',grid.elements);
           fprintf(fid_dpl,'NAME "Displacement"\n');
           fprintf(fid_dpl,'TIMEUNITS SECONDS\n');
           
           dpl.min=zeros(check.np,1);
           dpl.max=zeros(check.np,1);
           sum_dpl=zeros(check.np,1);
           dpl.min(:,:)=99999999.9;
           dpl.max(:,:)=-99999999.9;
           
       % checks the bed shear    
           
       case {'bsh'}
           bsh_check=check.bsh_check;
           if(check.bsh_check < 1)
               fprintf(1,'\nAttribute %s does not exist in %s.da\n\n',...
                   'bsh',basename);
               STOP;
           end
           fid_bsh=fopen([basename '_bsh_max.dat'],'w');
           fprintf(fid_bsh,'DATASET\n');
           fprintf(fid_bsh,'OBJTYPE "mesh2d"\n');
           fprintf(fid_bsh,'BEGSCL\n');
           fprintf(fid_bsh,'ND %i\n',grid.nodes);
           fprintf(fid_bsh,'NC %i\n',grid.elements);
           fprintf(fid_bsh,'NAME "Bed Shear"\n');
           fprintf(fid_bsh,'TIMEUNITS SECONDS\n');             
           
           bsh.min=zeros(check.np,1);
           bsh.max=zeros(check.np,1);
           sum_bsh=zeros(check.np,1);
           bsh.min(:,:)=99999999.9;
           bsh.max(:,:)=-99999999.9;
           
       % checks the bed vector    
           
       case {'bed'}
           bed_check=check.bed_check;
           if(check.bed_check < 1)
               fprintf(1,'\nAttribute %s does not exist in %s.da\n\n',...
                   'bed',basename);
               STOP;
           end
           fid_bed=fopen([basename '_bed_max.dat'],'w');
           fprintf(fid_bed,'DATASET\n');
           fprintf(fid_bed,'OBJTYPE "mesh2d"\n');
           fprintf(fid_bed,'BEGSCL\n');
           fprintf(fid_bed,'ND %i\n',grid.nodes);
           fprintf(fid_bed,'NC %i\n',grid.elements);
           fprintf(fid_bed,'NAME "Bed Vector"\n');
           fprintf(fid_bed,'TIMEUNITS SECONDS\n');             
           
           bed.min=zeros(check.np,1);
           bed.max=zeros(check.np,1);
           sum_bed=zeros(check.np,1);           
           bed_mag=zeros(check.np,1);
           bed.min(:,:)=99999999.9;
           bed.max(:,:)=-99999999.9;
           
       % checks the cbp    
           
       case {'cbp'}
           cbp_check=check.cbp_check;
           if(check.cbp_check < 1)
               fprintf(1,'\nAttribute %s does not exist in %s.da\n\n',...
                   'cbp',basename);
               STOP;
           end

           fid_cbp=zeros(check.cbp_check,1);
           for y=1:cbp_check
             if(y==1)
               fid_cbp(y)=fopen([basename '_cbp1_max.dat'],'w');
               fprintf(fid_cbp(y),'DATASET\n');
               fprintf(fid_cbp(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_cbp(y),'BEGSCL\n');
               fprintf(fid_cbp(y),'ND %i\n',grid.nodes);
               fprintf(fid_cbp(y),'NC %i\n',grid.elements);
               fprintf(fid_cbp(y),'NAME "Cohesive Bed Properties 1"\n');
               fprintf(fid_cbp(y),'TIMEUNITS SECONDS\n');                
             end
             if(y==2)
               fid_cbp(y)=fopen([basename '_cbp2_max.dat'],'w');
               fprintf(fid_cbp(y),'DATASET\n');
               fprintf(fid_cbp(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_cbp(y),'BEGSCL\n');
               fprintf(fid_cbp(y),'ND %i\n',grid.nodes);
               fprintf(fid_cbp(y),'NC %i\n',grid.elements);
               fprintf(fid_cbp(y),'NAME "Cohesive Bed Properties 2"\n');
               fprintf(fid_cbp(y),'TIMEUNITS SECONDS\n');                
             end
             if(y==3)
               fid_cbp(y)=fopen([basename '_cbp3_max.dat'],'w');
               fprintf(fid_cbp(y),'DATASET\n');
               fprintf(fid_cbp(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_cbp(y),'BEGSCL\n');
               fprintf(fid_cbp(y),'ND %i\n',grid.nodes);
               fprintf(fid_cbp(y),'NC %i\n',grid.elements);
               fprintf(fid_cbp(y),'NAME "Cohesive Bed Properties 3"\n');
               fprintf(fid_cbp(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==4)
               fid_cbp(y)=fopen([basename '_cbp4_max.dat'],'w');
               fprintf(fid_cbp(y),'DATASET\n');
               fprintf(fid_cbp(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_cbp(y),'BEGSCL\n');
               fprintf(fid_cbp(y),'ND %i\n',grid.nodes);
               fprintf(fid_cbp(y),'NC %i\n',grid.elements);
               fprintf(fid_cbp(y),'NAME "Cohesive Bed Properties 4"\n');
               fprintf(fid_cbp(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==5)
               fid_cbp(y)=fopen([basename '_cbp5_max.dat'],'w');
               fprintf(fid_cbp(y),'DATASET\n');
               fprintf(fid_cbp(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_cbp(y),'BEGSCL\n');
               fprintf(fid_cbp(y),'ND %i\n',grid.nodes);
               fprintf(fid_cbp(y),'NC %i\n',grid.elements);
               fprintf(fid_cbp(y),'NAME "Cohesive Bed Properties 5"\n');
               fprintf(fid_cbp(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==6)
               fid_cbp(y)=fopen([basename '_cbp6_max.dat'],'w');
               fprintf(fid_cbp(y),'DATASET\n');
               fprintf(fid_cbp(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_cbp(y),'BEGSCL\n');
               fprintf(fid_cbp(y),'ND %i\n',grid.nodes);
               fprintf(fid_cbp(y),'NC %i\n',grid.elements);
               fprintf(fid_cbp(y),'NAME "Cohesive Bed Properties 6"\n');
               fprintf(fid_cbp(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==7)
               fid_cbp(y)=fopen([basename '_cbp7_max.dat'],'w');
               fprintf(fid_cbp(y),'DATASET\n');
               fprintf(fid_cbp(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_cbp(y),'BEGSCL\n');
               fprintf(fid_cbp(y),'ND %i\n',grid.nodes);
               fprintf(fid_cbp(y),'NC %i\n',grid.elements);
               fprintf(fid_cbp(y),'NAME "Cohesive Bed Properties 7"\n');
               fprintf(fid_cbp(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==8)
               fid_cbp(y)=fopen([basename '_cbp8_max.dat'],'w');
               fprintf(fid_cbp(y),'DATASET\n');
               fprintf(fid_cbp(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_cbp(y),'BEGSCL\n');
               fprintf(fid_cbp(y),'ND %i\n',grid.nodes);
               fprintf(fid_cbp(y),'NC %i\n',grid.elements);
               fprintf(fid_cbp(y),'NAME "Cohesive Bed Properties 8"\n');
               fprintf(fid_cbp(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==9)
               fid_cbp(y)=fopen([basename '_cbp9_max.dat'],'w');
               fprintf(fid_cbp(y),'DATASET\n');
               fprintf(fid_cbp(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_cbp(y),'BEGSCL\n');
               fprintf(fid_cbp(y),'ND %i\n',grid.nodes);
               fprintf(fid_cbp(y),'NC %i\n',grid.elements);
               fprintf(fid_cbp(y),'NAME "Cohesive Bed Properties 9"\n');
               fprintf(fid_cbp(y),'TIMEUNITS SECONDS\n'); 
             end
             if(y==10)
               fid_cbp(y)=fopen([basename '_cbp10_max.dat'],'w');
               fprintf(fid_cbp(y),'DATASET\n');
               fprintf(fid_cbp(y),'OBJTYPE "mesh2d"\n');
               fprintf(fid_cbp(y),'BEGSCL\n');
               fprintf(fid_cbp(y),'ND %i\n',grid.nodes);
               fprintf(fid_cbp(y),'NC %i\n',grid.elements);
               fprintf(fid_cbp(y),'NAME "Cohesive Bed Properties 10"\n');
               fprintf(fid_cbp(y),'TIMEUNITS SECONDS\n'); 
             end              
           end   
           
           cbp_den.min=zeros(cbp_check,check.np);
           cbp_den.max=zeros(cbp_check,check.np);
           sum_cbp_den=zeros(cbp_check,check.np);  
           cbp_ces.min=zeros(cbp_check,check.np);
           cbp_ces.max=zeros(cbp_check,check.np);
           sum_cbp_ces=zeros(cbp_check,check.np);  
           cbp_erc.min=zeros(cbp_check,check.np);
           cbp_erc.max=zeros(cbp_check,check.np);
           sum_cbp_erc=zeros(cbp_check,check.np);
           cbp_ere.min=zeros(cbp_check,check.np);
           cbp_ere.max=zeros(cbp_check,check.np);
           sum_cbp_ere=zeros(cbp_check,check.np);
           cbp_den.min(:,:)=99999999.9;
           cbp_den.max(:,:)=-99999999.9;
           cbp_ces.min(:,:)=999999999.9;
           cbp_ces.max(:,:)=-99999999.9;
           cbp_erc.min(:,:)=999999999.9;
           cbp_erc.max(:,:)=-99999999.9;          
           
       % checks the alt    
           
       case {'alt'}
           alt_check=check.alt_check;
           if(check.alt_check < 1)
               fprintf(1,'\nAttribute %s does not exist in %s.da\n\n',...
                   'alt',basename);
               STOP;
           end
           fid_alt=fopen([basename '_alt_max.dat'],'w');
           fprintf(fid_alt,'DATASET\n');
           fprintf(fid_alt,'OBJTYPE "mesh2d"\n');
           fprintf(fid_alt,'BEGSCL\n');
           fprintf(fid_alt,'ND %i\n',grid.nodes);
           fprintf(fid_alt,'NC %i\n',grid.elements);
           fprintf(fid_alt,'NAME "Active Layer Thickness"\n');
           fprintf(fid_alt,'TIMEUNITS SECONDS\n');             

           alt.min=zeros(check.np,1);
           alt.max=zeros(check.np,1);
           sum_alt=zeros(check.np,1);
           alt.min(:,:)=9999999.9;
           alt.max(:,:)=-99999999.9;      
           
       % checks the alb    
           
       case {'alb'}
           alb_check=check.alb_check;
           if(alb_check < 1)
               fprintf(1,'\nAttribute %s does not exist in %s.da\n\n',...
                   'alb',basename);
               STOP;
           end
           fid_alb=fopen([basename '_alb_max.dat'],'w');
           fprintf(fid_alb,'DATASET\n');
           fprintf(fid_alb,'OBJTYPE "mesh2d"\n');
           fprintf(fid_alb,'BEGSCL\n');
           fprintf(fid_alb,'ND %i\n',grid.nodes);
           fprintf(fid_alb,'NC %i\n',grid.elements);
           fprintf(fid_alb,'NAME "Active Layer Distribution"\n');
           fprintf(fid_alb,'TIMEUNITS SECONDS\n');             
           
           alb.min=zeros(check.np,1);
           alb.max=zeros(check.np,1);
           sum_alb=zeros(check.np,1);
           alb.min(:,:)=999999.9;
           alb.max(:,:)=-9999999.9;
           
       otherwise
           fprintf(1,'\nAttribute not recognized %s\n\n',test);
   end
end

%% loop over all the nodes in the da file

basename=basename_1;
dry=zeros(check.np,1);
total_time=0.0;
for i=data_start:data_stop
    %i
   data=load_da_stepnew([basename '.da'],i);
   
    if(mod(i-data_start,100)==0)
        fprintf(1,'Reading time step number %i and time %s\n',...
            i,datestr(data.time));
    end
%    elseif(i==interval_2)
%        fprintf(1,'Reading time step number %i and time %s\n',...
%            i,datestr(data.time));
%    elseif(i==interval_3)
%        fprintf(1,'Reading time step number %i and time %s\n',...
%            i,datestr(data.time));
%    elseif(i==interval_4)
%        fprintf(1,'Reading time step number %i and time %s\n',...
%            i,datestr(data.time));    
%    elseif(i==data_stop)
%        fprintf(1,'Reading last time step number %i and time %s\n',...
%            i,datestr(data.time));  
%    end
%     datestr(data.time)

     time_diff=data.time-last_time;
     total_time=total_time+time_diff;
     vel_mag=(data.u.*data.u+data.v.*data.v).^0.5;
     dry(vel_mag==0.0)=1;
     count=count+1;
     if(dep_check > 0)
       dep.min=min(dep.min,data.dep);
       dep.max=max(dep.max,data.dep);
       sum_dep=sum_dep+data.dep*time_diff;
       loc=find(vel_mag>0.0);
       for y=1:length(loc)
           wetted_time(loc(y))=wetted_time(loc(y))+time_diff;
       end
     end
     last_time=data.time; 
     if(wse_check > 0)
       wse_dep=data.wse-grid.z;
       wse.min=min(wse.min,wse_dep);
       wse.max=max(wse.max,wse_dep);
       sum_wse=sum_wse+wse_dep*time_diff;       
     end
     if(vel_check > 0)
       %for j=1:check.np          
          velx=data.u;
          vely=data.v;
       %end

       vel.min=min(vel.min,vel_mag);
       vel.max=max(vel.max,vel_mag);
       sum_vel=sum_vel+vel_mag*time_diff;       
       vel.xmin=min(vel.xmin,velx);
       vel.xmax=max(vel.xmax,velx);

       sum_xvel=sum_xvel+velx*time_diff;  
       vel.ymin=min(vel.ymin,vely);
       vel.ymax=max(vel.ymax,vely);
       sum_yvel=sum_yvel+vely*time_diff;
       
%       length(data.u)
%       length(velx)       
%       length(vel.xmax)
%       length(vel.xmin)
%       length(sum_xvel)      
%       stop
     end
     if(sal_check > 0)
       data.sal(data.sal<0.0)=0.0;
       sal.min=min(sal.min,data.sal);
       sal.max=max(sal.max,data.sal);
       %loc_sal=find(data.sal>0.0);
       sum_sal=sum_sal+data.sal*time_diff;        
     end
     if(vor_check > 0)
       vor.min=min(vor.min,data.vor);
       vor.max=max(vor.max,data.vor);
       sum_vor=sum_vor+data.vor*time_diff;        
     end
     if(snd_check > 0)
       for e=1:snd_check
          snd.min(e,:)=min(snd.min(e,:),data.snd(e,:));
          snd.max(e,:)=max(snd.max(e,:),data.snd(e,:));
          sum_snd(e,:)=sum_snd(e,:)+data.snd(e,:)*time_diff;
          snd_rouse.min(e,:)=min(snd_rouse.min(e,:),data.snd_rouse(e,:));
          snd_rouse.max(e,:)=max(snd_rouse.max(e,:),data.snd_rouse(e,:));
          sum_snd_rouse(e,:)=sum_snd_rouse(e,:)+data.snd_rouse(e,:)*time_diff;
          snd_bdma.min(e,:)=min(snd_bdma.min(e,:),data.snd_bdma(e,:));
          snd_bdma.max(e,:)=max(snd_bdma.max(e,:),data.snd_bdma(e,:));
          sum_snd_bdma(e,:)=sum_snd_bdma(e,:)+data.snd_bdma(e,:)*time_diff;       
       end
     end
     if(slt_check > 0)
       for e=1:slt_check
          slt.min(e,:)=min(slt.min(e,:),data.slt(e,:));
          slt.max(e,:)=max(slt.max(e,:),data.slt(e,:));
          sum_slt(e,:)=sum_slt(e,:)+data.slt(e,:)*time_diff;
          slt_rouse.min(e,:)=min(slt_rouse.min(e,:),data.slt_rouse(e,:));
          slt_rouse.max(e,:)=max(slt_rouse.max(e,:),data.slt_rouse(e,:));
          sum_slt_rouse(e,:)=sum_slt_rouse(e,:)+data.slt_rouse(e,:)*time_diff;
          slt_bdma.min(e,:)=min(slt_bdma.min(e,:),data.slt_bdma(e,:));
          slt_bdma.max(e,:)=max(slt_bdma.max(e,:),data.slt_bdma(e,:));
          sum_slt_bdma(e,:)=sum_slt_bdma(e,:)+data.slt_bdma(e,:)*time_diff;      
       end       
     end
     if(err_check > 0)
       err.min=min(err.min,data.err);
       err.max=max(err.max,data.err);
       sum_err=sum_err+data.err*time_diff;       
     end
     if(dpl_check > 0)
       dpl.min=min(dpl.min,data.dpl);
       dpl.max=max(dpl.max,data.dpl);
       sum_dpl=sum_dpl+data.dpl*time_diff;
     end
     if(alt_check > 0)
       alt.min=min(alt.min,data.alt);
       alt.max=max(alt.max,data.alt);
       sum_alt=sum_alt+data.alt*time_diff;
     end
     if(alb_check > 0)
       alb.min=min(alb.min,data.alb);
       alb.max=max(alb.max,data.alb);
       sum_alb=sum_alb+data.alb*time_diff;
     end
     if(blt_check > 0)
       for e=1:blt_check
          blt.min(e,:)=min(blt.min(e,:),data.blt(e,:));
          blt.max(e,:)=max(blt.max(e,:),data.blt(e,:));
          sum_blt(e,:)=sum_blt(e,:)+data.blt(e,:)*time_diff;
       end       
     end   
     if(bld_check > 0)
       for e=1:bld_check
          for w=1:data.number_layers
            bld.min(e,w,i)=min(bld.min(e,w,:),data.bld(e,w,:));
            bld.max(e,w,i)=max(bld.max(e,w,:),data.bld(e,w,:));
            sum_bld(e,w,:)=sum_bld(e,w,:)+data.bld(e,w,:)*time_diff;
          end
       end
     end
     if(bsh_check > 0)
       bsh.min=min(bsh.min,data.bsh);
       bsh.max=max(bsh.max,data.bsh);
       sum_bsh=sum_bsh+data.bsh*time_diff;
     end
     if(bed_check > 0)
       for j=1:check.np
          bed_mag(j)=(data.bed_u(j)*data.bed_u(j)+data.bed_v(j)*data.bed_v(j))^0.5; 
       end
       bed.min=min(bed.min,bed_mag);
       bed.max=max(bed.max,bed_mag);
       sum_bed=sum_bed+bed_mag*time_diff;
     end
     if(smr_check > 0)
       for e=1:smr_check
          smr.min(e,:)=min(smr.min(e,:),data.smr(e,:));
          smr.max(e,:)=max(smr.max(e,:),data.smr(e,:));
          sum_smr(e,:)=sum_smr(e,:)+data.smr(e,:)*time_diff;
       end         
     end
     if(cbp_check > 0)
       for e=1:cbp_check
          cbp_den.min(e,:)=min(cbp_den.min(e,:),data.cbp_den(e,:));
          cbp_den.max(e,:)=max(cbp_den.max(e,:),data.cbp_den(e,:));
          sum_cbp_den.avg(e,:)=sum_cbp_den(e,:)+data.cbp_den(e,:)*time_diff;
          cbp_ces.min(e,:)=min(cbp_ces.min(e,:),data.cbp_ces(e,:));
          cbp_ces.max(e,:)=max(cbp_ces.max(e,:),data.cbp_ces(e,:));
          sum_cbp_ces.avg(e,:)=sum_cbp_ces(e,:)+data.cbp_ces(e,:)*time_diff;
          cbp_erc.min(e,:)=min(cbp_erc.min(e,:),data.cbp_erc(e,:));
          cbp_erc.max(e,:)=max(cbp_erc.max(e,:),data.cbp_erc(e,:));
          sum_cbp_erc.avg(e,:)=sum_cbp_erc(e,:)+data.cbp_erc(e,:)*time_diff;          
       end        
     end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% writing solution files
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fprintf(1,'Analysis included %i time steps\n',count);
loc_dry=find(dry==1);
if(dep_check > 0)
    fprintf(fid_dep,'TS 0 0.00\n');
    dep.min(loc_dry)=-1;
    fprintf(fid_dep,'%e\n',dep.min);
    fprintf(fid_dep,'TS 0 50.00\n');
    temp_dep=sum_dep/total_time;
    temp_dep(loc_dry)=-1;
    fprintf(fid_dep,'%e\n',temp_dep);               
    fprintf(fid_dep,'TS 0 100.00\n');
    dep.max(loc_dry)=-1;
    fprintf(fid_dep,'%e\n',dep.max);            
    fprintf(fid_dep,'ENDDS\n');
    fclose(fid_dep);
    
    fprintf(fid_time,'TS 0 0.00\n');
    percent_wetted_time=100.0*wetted_time/(total_time);
    fprintf(fid_time,'%e\n',percent_wetted_time);
    fprintf(fid_time,'TS 0 100.00\n');
    fprintf(fid_time,'%e\n',wetted_time);              
    fprintf(fid_time,'ENDDS\n');  
    fclose(fid_time);

end

if(wse_check > 0)
    fprintf(fid_wse,'TS 0 0.00\n');
    temp_wse=wse.min+grid.z;
    temp_wse(loc_dry)=-100;
    fprintf(fid_wse,'%e\n',temp_wse);
    fprintf(fid_wse,'TS 0 50.00\n');
    temp_wse=(sum_wse/total_time)+grid.z;
    temp_wse(loc_dry)=-100;
    fprintf(fid_wse,'%e\n',temp_wse);               
    fprintf(fid_wse,'TS 0 100.00\n');
    temp_wse=wse.max+grid.z;
    temp_wse(loc_dry)=-100;
    fprintf(fid_wse,'%e\n',temp_wse);            
    fprintf(fid_wse,'ENDDS\n');
    fclose(fid_wse);
end

if(vel_check > 0)
    fprintf(fid_vel,'TS 0 0.00\n');
    vel.min(loc_dry)=-1;
    fprintf(fid_vel,'%e\n',vel.min);
    fprintf(fid_vel,'TS 0 50.00\n');
    temp_vel=sum_vel/total_time;
    temp_vel(loc_dry)=-1;
    fprintf(fid_vel,'%e\n',temp_vel);                
    fprintf(fid_vel,'TS 0 100.00\n');
    vel.max(loc_dry)=-1;
    fprintf(fid_vel,'%e\n',vel.max);            
    fprintf(fid_vel,'ENDDS\n');
    fclose(fid_vel);
    fprintf(fid_vel1,'TS 0 0.00\n');
%       length(data.u)       
%       length(vel.xmax)
%       length(vel.xmin)
%       length(vel.ymax)
%       length(vel.ymin)       
%       length(sum_xvel)     
%       length(sum_yvel)
    %for i=1:length(vel.xmin)
    vel.xmin(loc_dry)=0.0;
    vel.ymin(loc_dry)=0.0;
      fprintf(fid_vel1,'%e %e 0.0\n',vel.xmin,vel.ymin);
    %end
    fprintf(fid_vel1,'TS 0 50.00\n');
    %for i=1:length(sum_xvel)
    sum_xvel(loc_dry)=0.0;
    sum_yvel(loc_dry)=0.0;
      fprintf(fid_vel1,'%e %e 0.0\n',sum_xvel./total_time,sum_yvel./total_time);                
    %end
    fprintf(fid_vel1,'TS 0 100.00\n');
    %for i=1:length(vel.xmax)
    vel.xmax(loc_dry)=0.0;
    vel.ymax(loc_dry)=0.0;
      fprintf(fid_vel1,'%e %e 0.0\n',vel.xmax,vel.ymax);            
    %end
    fprintf(fid_vel1,'ENDDS\n');
    fclose(fid_vel1);    
end

if(sal_check > 0)
    fprintf(fid_sal,'TS 0 0.00\n');
    sal.min(loc_dry)=-1.0;
    fprintf(fid_sal,'%e\n',sal.min);
    fprintf(fid_sal,'TS 0 50.00\n');
    temp_sal=sum_sal/total_time;
    temp_sal(loc_dry)=-1;
    fprintf(fid_sal,'%e\n',temp_sal);               
    fprintf(fid_sal,'TS 0 100.00\n');
    sal.max(loc_dry)=-1.0;
    fprintf(fid_sal,'%e\n',sal.max);            
    fprintf(fid_sal,'ENDDS\n');
    fclose(fid_sal);
end

if(vor_check > 0)
    fprintf(fid_vor,'TS 0 0.00\n');
    fprintf(fid_vor,'%e\n',vor.min);
    fprintf(fid_vor,'TS 0 50.00\n');
    fprintf(fid_vor,'%e\n',sum_vor/total_time);                
    fprintf(fid_vor,'TS 0 100.00\n');
    fprintf(fid_vor,'%e\n',vor.max);            
    fprintf(fid_vor,'ENDDS\n');
    fclose(fid_vor);
end

if(snd_check > 0)
    for e=1:snd_check
      fprintf(fid_snd(e),'TS 0 0.00\n');
      for r=1:check.np
        fprintf(fid_snd(e),'%e %e %e\n',snd.min(e,r), ...
            snd_rouse.min(e,r),snd_bdma.min(e,r));
      end
      fprintf(fid_snd(e),'TS 0 50.00\n');
      for r=1:check.np
        fprintf(fid_snd(e),'%e %e %e\n',sum_snd(e,r)/total_time, ...
            sum_snd_rouse(e,r)/total_time,sum_snd_bdma(e,r)/total_time);
      end             
      fprintf(fid_snd(e),'TS 0 100.00\n');
      for r=1:check.np
        fprintf(fid_snd(e),'%e %e %e\n',snd.max(e,r), ...
            snd_rouse.max(e,r),snd_bdma.max(e,r));      
      end  
      fprintf(fid_snd(e),'ENDDS\n');
      fclose(fid_snd(e));
    end
end

if(slt_check > 0)
    for e=1:slt_check
      fprintf(fid_slt(e),'TS 0 0.00\n');
      for r=1:check.np
        fprintf(fid_slt(e),'%e %e %e\n',slt.min(e,r), ...
            slt_rouse.min(e,r),slt_bdma.min(e,r));
      end
      fprintf(fid_slt(e),'TS 0 50.00\n');
      for r=1:check.np
        fprintf(fid_slt(e),'%e %e %e\n',sum_slt(e,r)/total_time, ...
            sum_slt_rouse(e,r)/total_time,sum_slt_bdma(e,r)/total_time);
      end              
      fprintf(fid_slt(e),'TS 0 100.00\n');
      for r=1:check.np
        fprintf(fid_slt(e),'%e %e %e\n',slt.max(e,r), ...
            slt_rouse.max(e,r),slt_bdma.max(e,r));
      end  
      fprintf(fid_slt(e),'ENDDS\n');
      fclose(fid_slt(e));
    end      
end

if(err_check > 0)
    fprintf(fid_err,'TS 0 0.00\n');
    fprintf(fid_err,'%e\n',err.min);
    fprintf(fid_err,'TS 0 50.00\n');
    fprintf(fid_err,'%e\n',sum_err/total_time);               
    fprintf(fid_err,'TS 0 100.00\n');
    fprintf(fid_err,'%e\n',err.max);            
    fprintf(fid_err,'ENDDS\n');
    fclose(fid_err);
end

if(dpl_check > 0)
    fprintf(fid_dpl,'TS 0 0.00\n');
    fprintf(fid_dpl,'%e\n',dpl.min);
    fprintf(fid_dpl,'TS 0 50.00\n');
    fprintf(fid_dpl,'%e\n',sum_dpl/total_time);               
    fprintf(fid_dpl,'TS 0 100.00\n');
    fprintf(fid_dpl,'%e\n',dpl.max);            
    fprintf(fid_dpl,'ENDDS\n');
    fclose(fid_dpl);      
end

if(alt_check > 0)
    fprintf(fid_alt,'TS 0 0.00\n');
    fprintf(fid_alt,'%e\n',alt.min);
    fprintf(fid_alt,'TS 0 50.00\n');
    fprintf(fid_alt,'%e\n',sum_alt/total_time);                
    fprintf(fid_alt,'TS 0 100.00\n');
    fprintf(fid_alt,'%e\n',alt.max);            
    fprintf(fid_alt,'ENDDS\n');
    fclose(fid_alt);  
end

if(alb_check > 0)
    fprintf(fid_alb,'TS 0 0.00\n');
    for p=1:check.np
      for w=1:check.number_sed
        fprintf(fid_alb,'%e',alb.min(p,w));
      end
      fprintf(fid_alb,'\n');
    end
    fprintf(fid_alb,'TS 0 50.00\n');
    for p=1:check.np
      for w=1:check.number_sed
        fprintf(fid_alb,'%e',sum_alb(p,w)/total_time);
      end
      fprintf(fid_alb,'\n');
    end              
    fprintf(fid_alb,'TS 0 100.00\n');
    for p=1:check.np
      for w=1:check.number_sed
        fprintf(fid_alb,'%e',alb.max(p,w));
      end
      fprintf(fid_alb,'\n');
    end           
    fprintf(fid_alb,'ENDDS\n');
    fclose(fid_alb);  
end

if(blt_check > 0)
    for e=1:blt_check
      fprintf(fid_blt(e),'TS 0 0.00\n');
      for r=1:check.np
        fprintf(fid_blt(e),'%e\n',blt.min(e,:));
      end
      fprintf(fid_blt(e),'TS 0 50.00\n');
      for r=1:check.np
        fprintf(fid_blt(e),'%e\n',sum_blt(e,:)/total_time);
      end         
      fprintf(fid_blt(e),'TS 0 100.00\n');
      for r=1:check.np
        fprintf(fid_blt(e),'%e\n',blt.max(e,:));     
      end          
      fprintf(fid_blt(e),'ENDDS\n');
      fclose(fid_blt(e));
    end      
end

if(bld_check > 0)
    for e=1:bld_check
      fprintf(fid_bld(e),'TS 0 0.00\n');
      for r=1:check.np
        fprintf(fid_bld(e),'%e\n',bld.min(e,:));
      end
      fprintf(fid_bld(e),'TS 0 50.00\n');
      for r=1:check.np
        fprintf(fid_bld(e),'%e\n',sum_bld(e,:)/total_time);
      end          
      fprintf(fid_bld(e),'TS 0 100.00\n');
      for r=1:check.np
        fprintf(fid_bld(e),'%e\n',bld.max(e,:));   
      end  
      fprintf(fid_bld(e),'ENDDS\n');
      fclose(fid_bld(e));
    end   
end

if(bsh_check > 0)
    fprintf(fid_bsh,'TS 0 0.00\n');
    fprintf(fid_bsh,'%e\n',bsh.min);
    fprintf(fid_bsh,'TS 0 50.00\n');
    fprintf(fid_bsh,'%e\n',sum_bsh/total_time);                
    fprintf(fid_bsh,'TS 0 100.00\n');
    fprintf(fid_bsh,'%e\n',bsh.max);            
    fprintf(fid_bsh,'ENDDS\n');
    fclose(fid_bsh);        
end

if(bed_check > 0)
    fprintf(fid_bed,'TS 0 0.00\n');
    fprintf(fid_bed,'%e\n',bed.min);
    fprintf(fid_bed,'TS 0 50.00\n');
    fprintf(fid_bed,'%e\n',sum_bed/total_time);               
    fprintf(fid_bed,'TS 0 100.00\n');
    fprintf(fid_bed,'%e\n',bed.max);            
    fprintf(fid_bed,'ENDDS\n');
    fclose(fid_bed);         
end

if(smr_check > 0)
    fprintf(fid_smr,'TS 0 0.00\n');
    for p=1:check.np
      for w=1:check.number_sed
        fprintf(fid_smr,'%e',smr.min(p,w));
      end
      fprintf(fid_smr,'\n');
    end
    fprintf(fid_smr,'TS 0 50.00\n');
    for p=1:check.np
      for w=1:check.number_sed
        fprintf(fid_smr,'%e',sum_smr(p,w)/total_time);
      end
      fprintf(fid_smr,'\n');
    end              
    fprintf(fid_smr,'TS 0 100.00\n');
    for p=1:check.np
      for w=1:check.number_sed
        fprintf(fid_smr,'%e',smr.max(p,w));
      end
      fprintf(fid_smr,'\n');
    end           
    fprintf(fid_smr,'ENDDS\n');
    fclose(fid_smr);         
end

if(cbp_check > 0)
    fprintf(fid_bed,'TS 0 0.00\n');
    for d=1:cbp_check 
      for e=1:check.np
         fprintf(fid_cbp(d),'%e %e %e %e\n',cbp_den.min(d,e),cbp_ces.min(d,e), ...
             cbp_erc.min(d,e),cbp_ere.min(d,e));
      end
    end
    fprintf(fid_bed,'TS 0 50.00\n');
    for d=1:cbp_check 
      for e=1:check.np
         fprintf(fid_cbp(d),'%e %e %e %e\n',sum_cbp_den(d,e)/total_time, ...
             sum_cbp_ces(d,e)/total_time, ...
             sum_cbp_erc(d,e)/total_time,sum_cbp_ere(d,e)/total_time);
      end
    end              
    fprintf(fid_bed,'TS 0 100.00\n');
    for d=1:cbp_check 
      for e=1:check.np
         fprintf(fid_cbp(d),'%e %e %e %e\n',cbp_den.max(d,e),cbp_ces.max(d,e), ...
             cbp_erc.max(d,e),cbp_ere.max(d,e)); 
      end
      fprintf(fid_cbp(d),'ENDDS\n');
      fclose(fid_cbp(d));
    end         
end

fprintf(1,'Analysis Completed\n');

toc
