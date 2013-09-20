function fem_grid_struct=big_grd_to_opnml(fort14name)
%
%
%GRD_TO_OPNML2 Convert an ADCIRC grd file to an OPNML fem_grid_struct.
% Convert an ADCIRC grd file to an OPNML fem_grid_struct.
% ADCIRC grid information assumed in "fort.14" format.
% The boundary/island information at the tail of the fort.14
% file is ignored.
%
% Input:  fort14name - path/name of fort.14 file;  if not passed,
%                      assumes fort.14 in the currect working dir.
% Output: fem_grid_struct - OPNML grid structure
%
% Call:   fem_grid_struct=grd_to_opnml2(fort14name);
%         fem_grid_struct=grd_to_opnml2;
%
% Calls: opnml
%
% This is a modified version of the file GRD_TO_OPNML that ignores
% the comment lines in a fort.14 file.
%
% Modified by:  Chris Massey
% Date:         January 13, 2003.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


if nargin<1
   % assume fort.14 filename in the current wd.
   fort14name='fort.14';
end

% Open fort.14 file
[f14,message]=fopen(fort14name,'r');
if (f14<0)
   error(message)
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get grid info
gridname = fscanf(f14,'%s',1); % Reads in Grid_Name
fgets(f14); %moves the pointer to the next line (skipping any comments).

temp=fscanf(f14,'%d %d',2); % Reads in the Number of Elements and Number of Nodes
nn=temp(2);
ne=temp(1);
fgets(f14); % moves the pointer to the next line (skipping any comments).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get node locations
temp1 = fscanf(f14,'%d %f %f %f',[4 1])'; % Reads in First set of Nodes
x(1,1) = temp1(2);
y(1,1) = temp1(3);
z(1,1) = temp1(4);
fgets(f14); %moves the pointer to the next line (skipping any comments).

temp=fscanf(f14,'%d %f %f %f',[4 nn-1])'; % Reads in all other Nodes 
x(2:nn,1)=temp(:,2);
y(2:nn,1)=temp(:,3);
z(2:nn,1)=temp(:,4);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get elements
temp=fscanf(f14,'%d %d %d %d %d',[5 1])'; % Reads in the First Element
e(1,1:3)=temp(:,3:5);
fgets(f14); %moves the pointer to the next line (skipping any comments).

temp=fscanf(f14,'%d %d %d %d %d',[5 ne-1])'; % Reads in the Remaining Elements
e(2:ne,1:3)=temp(:,3:5);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fem_grid_struct.name=gridname;
fem_grid_struct.x=x;
fem_grid_struct.y=y;
fem_grid_struct.z=z;
fem_grid_struct.e=e;
%fem_grid_struct.bnd=detbndy(e);
%fem_grid_struct=el_areas(fem_grid_struct);
fclose(f14);
