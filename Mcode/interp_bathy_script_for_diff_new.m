% Sample script 
% Written By:  Chris Massey, USACE-ERDC-CHL, Dec. 3, 2009
%

% Tate this is a sample of how to interpolate bathymetry
% I haven't tested the script as I don't have Matlab on my laptop

rlon = mean(fem.x);
rlat = mean(fem.y);

% Set up the bounding box for the region of intersest
%bbox = [xmin xmax ymin ymax];
bbox = [-200 200 -200 200];  %You'll have to set this
%NOTE:  You don't have to use a box, you could instead use any closed polygon shape 
%       as far as find_region is concerned.

% Find the elements and nodes in the bounding box
[elems,nodes] = find_region(fem,bbox([1 2 2 1 1]),bbox([3 3 4 4 3]),[rlon,rlat]);

%Pull out the nodes/elements and create a new fem structure
fem_sub = extract_mesh(fem,nodes,elems); % This is a new fem struct with just the nodes/elements of interest

fem_sub1 = grd_ll2xy(fem_sub,rlat,rlon,0);  %Sub Mesh in x,y coordinates

% Load in the bathymetry data (you can change this as needed
data = load('bathy.xyz'); %This will load a three column set of data points (x,y,z) in each row (lon/lat assumed) (file name = bathy.xyz)
lonlat_x = data(:,1);
lonlat_y = data(:,2);
z = data(:,3);
clear data

%Convert bathy data to x/y
[x,y] = ll2xy(lonlat_y,lonlat_x,rlat,rlon,0); %Converts to x/y
clear lonlat_x lonlat_y


%Triangulate bathy data in x/y coordinates
tri = delaunay(x(:),y(:),{'Qt','Qbb','Qc','Qz'});

%Sample the bathymetry data unto the extracted mesh
fem_sub2 = sample_depth(fem_sub1,tri,[x(:),y(:),z(:)],2);

% Create a copy of the full mesh structure in lon/lat
fem2 = fem;

%Update the bathymetry for the nodes in the bounding box (from the extracted mesh)
fem.z(nodes) = fem_sub2.z;  


