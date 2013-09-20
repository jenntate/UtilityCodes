% Sample script 
% Written By:  Chris Massey, USACE-ERDC-CHL, Dec. 3, 2009
%

% Tate this is a sample of how to interpolate bathymetry
% I haven't tested the script as I don't have Matlab on my laptop
bbox = [-94.25 -93.25 29.5 30.2];
rlon = mean(fem.x);
rlat = mean(fem.y);

% Find the elements and nodes in the bounding box
[elems,nodes] = find_region(fem,bbox([1 2 2 1 1]), ...
    bbox([3 3 4 4 3]),[rlon,rlat]);

%Pull out the nodes/elements and create a new fem structure
fem_sub = extract_mesh(fem,nodes,elems); % This is a new fem struct with just the nodes/elements of interest

fem_sub1 = grd_ll2xy(fem_sub,rlat,rlon,0);  %Sub Mesh in x,y coordinates

rlon = mean(fem_new.x);
rlat = mean(fem_new.y);

% Find the elements and nodes in the bounding box
[elems_new,nodes_new] = find_region(fem_new, ...
    bbox([1 2 2 1 1]),bbox([3 3 4 4 3]),[rlon,rlat]);

%Pull out the nodes/elements and create a new fem structure
fem_new_sub = extract_mesh(fem_new,nodes_new,elems_new); % This is a new fem struct with just the nodes/elements of interest

fem_new_sub1 = grd_ll2xy(fem_new_sub,rlat,rlon,0);  %Sub Mesh in x,y coordinates

%Triangulate bathy data in x/y coordinates
tri = delaunay(fem_new_sub1.x(:),fem_new_sub1.y(:),{'Qt','Qbb','Qc','Qz'});

%Sample the bathymetry data unto the extracted mesh
fem_interped = sample_depth(fem,tri, ...
    [fem_new_sub1.x(:),fem_new_sub1.y(:),fem_new_sub1.z(:)],2);

%Update the bathymetry for the nodes in the bounding box (from the extracted mesh)
fem_new = fem_interped;


