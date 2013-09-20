function check_da(basename)
%
%    Subroutine to check the da file information for:
%
%       number of time steps
%
%       average time step size
%
%       maximum time step size
%
%       start time for data
%
%       stop time for data
%
%       number of nodes in da file
%
%       parameters included in the da file
%
%       Input: basename - the full .da file name
%
%       Example Usage:
%
%       check_da('Mobile.da')
%
%       prints information about the Mobile.da file
%

data=load_da_stepnew(basename,0);
data1=load_da_time_series(basename,1);

largest_dt=0.0;
for i=2:data1.nt
    if(data1.time(i)-data1.time(i-1) > largest_dt)
        largest_dt=data1.time(i)-data1.time(i-1);
        one=data1.time(i-1);
        two=data1.time(i);
    end
end

average_dt=86400*(data1.time(data1.nt)-data1.time(1))/data.nt;

fprintf(1,'\nThe %s file starts at time %s\n',basename,...
    datestr(data1.time(1)));

fprintf(1,'\nThe %s file stops at time %s\n',basename,...
    datestr(data1.time(data.nt)));

fprintf(1,'\nThe average time step for file %s is %f seconds\n', basename, ...
    average_dt);

fprintf(1,'\nThe Largest Time Step for file %s was %f seconds from %s to %s\n',...
    basename,largest_dt*86400,datestr(one),datestr(two));

fprintf(1,'\nThe Number of Time Steps in the %s file is %i\n', ...
    basename,data.nt);

fprintf(1,'\nThe Number of Nodes in the %s file is %i\n',...
    basename,data.np);



if(data.wse_check > 0)
    fprintf(1,'\nThe %s file includes WATER SURFACE ELEVATION Data\n',...
        basename);
end

if(data.dep_check > 0)
    fprintf(1,'\nThe %s file includes DEPTH Data\n',...
        basename);
end

if(data.vel_check > 0)
    fprintf(1,'\nThe %s file includes VELOCITY Data\n',...
        basename);
end

if(data.err_check > 0)
    fprintf(1,'\nThe %s file includes ERROR Data\n',...
        basename);
end

if(data.sal_check > 0)
    fprintf(1,'\nThe %s file includes SALINITY Data\n',...
        basename);
end

if(data.vor_check > 0)
    fprintf(1,'\nThe %s file includes VORTICITY Data\n',...
        basename);
end

if(data.snd_check > 0)
    fprintf(1,'\nThe %s file includes SAND TRANSPORT Data\n',...
        basename);
end

if(data.slt_check > 0)
    fprintf(1,'\nThe %s file includes CLAY TRANSPORT Data\n',...
        basename);
end

if(data.smr_check > 0)
    fprintf(1,'\nThe %s file includes SEDIMENT MASS RESIDUAL Data\n',...
        basename);
end

if(data.dpl_check > 0)
    fprintf(1,'\nThe %s file includes BED DISPLACEMENT Data\n',...
        basename);
end

if(data.bsh_check > 0)
    fprintf(1,'\nThe %s file includes BED SHEAR Data\n',...
        basename);
end

if(data.blt_check > 0)
    fprintf(1,'\nThe %s file includes BED LAYER THICKNESS Data\n',...
        basename);
end

if(data.bld_check > 0)
    fprintf(1,'\nThe %s file includes BED LAYER DISTRIBUTION Data\n',...
        basename);
end

if(data.bed_check > 0)
    fprintf(1,'\nThe %s file includes BED VECTOR Data\n',...
        basename);
end

if(data.alt_check > 0)
    fprintf(1,'\nThe %s file includes ACTIVE LAYER THICKNESS Data\n',...
        basename);
end

if(data.alb_check > 0)
    fprintf(1,'\nThe %s file includes ACTIVE LAYER DISTRIBUTION Data\n',...
        basename);
end

if(data.cbp_check > 0)
    fprintf(1,'\nThe %s file includes COHESIVE BED PROPERTIES Data\n',...
        basename);
end

if(data.number_layers > 0)
    fprintf(1,'\nThe %s file includes %i Bed Layers\n',...
        basename,data.number_layers);
end

if(data.number_sed > 0)
    fprintf(1,'\nThe %s file includes %i Sediments\n',...
        basename,data.number_sed);
end














