
clear

tic

%% Rootname for the model simulations (MUST HAVE A .3dm corresponding .da
%% file for each Rootname

basename{1}='plan2_qrt1_2004';
basename{2}='plan3_qrt1_2004';

%% x and y location for Point 1 

x(1)=3536300.0;
y(1)=253900.0;

%% x and y location for Point 2

x(2)=3471370.0;
y(2)=303620.0;

%% start time and stop time over which to consider data

start_time = '01/01/2004 00:00:00';
stop_time  = '01/01/2005 00:00:00';

%% attribute to be analyzed
% options include
%                      dep = depth solution
%                      wse = water surface elevation solution
%                      vel = velocity vector and magnitude solution
%                      sal = salinity solution
%                      vor = vorticity solution
%                      snd = sand concentration solution
%                      slt = silt concentration solution
%                      err = error solution
%                      dpl = bed displacement solution
%                      alt = active layer thickness
%                      ald = active layer distribution
%                      blt = bed layer thickness
%                      bld = bed layer distribution
%                      cbp = cohesive bed properties
%                      bsh = bed shear stress solution
%                      bed = bedload vector and magnitude solution
%                      smr = sediment mass residual

attributes='wse sal';

%% units for the y axis associated with the attribute need a units
%% specification for each attribute

% associated with the first attribute listed

units{1}='meters';

% associated with the second attribute listed

units{2}='ppt';

%% don't change below this line

point_comparisons(basename,x,y,units,attributes,start_time,stop_time)

toc
