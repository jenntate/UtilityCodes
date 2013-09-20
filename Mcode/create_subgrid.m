function create_subgrid(basename,sub_poly)

%% 
% ========================================================================
%   CREATE_SUBGRID - function to generate a subset .3dm grid from a larger
%       grid. Subset grid is created by cutting out a portion of the larger
%       grid which lies within a user-defined polygon (sub_poly). Polygon
%       can have unlimited nodes.
%
%       basename - rootname of original grid file
%       sub_poly - bounding polygon for the subgrid [x y]
%       
%       SYNTAX - 
%           basename=('CSR_MAY15'); 
%       
%           sub_poly=[
%           2316062.0 263128.0
%           2314951.0 261367.0
%           2314583.0 256153.0
%           2319385.0 251864.0
%           2321666.0 251912.0
%           2324092.0 255237.0
%           2321529.0 261068.0
%           2318149.0 263165.0
%           ];
%       
%       OUTPUT - function saves a subset grid (.3dm) and a .mat file which 
%       contains grid information and a referencing array that links the 
%       original grid node numbering to the subgrid numbering. 
% 
%       Created 5/17/2012 kcp
% 
% ========================================================================
%% READ ORIGINAL GRID
clear
tic
grid=read_adh_grid(strcat([basename '.3dm']));
%% CREATE NEW SUBGRID NODE DATA
logic=inpolygon(grid.x,grid.y,sub_poly(:,1),sub_poly(:,2));
count=1;
gridtosub=zeros(grid.nodes,1);
subtogrid=zeros(sum(logic,1),1);
display('GENERATING NODE INFORMATION')
for i=1:grid.nodes
    if (logic(i,1)==1)
        subgrid.x(count,1)=grid.x(i);
        subgrid.y(count,1)=grid.y(i);
        subgrid.z(count,1)=grid.z(i);
        subtogrid(count,1)=i;
        gridtosub(i,1)=count;
        count=count+1;
    end   
end
    subgrid.nodes=length(subtogrid);

%% CREATE NEW SUBGRID ELEMENT DATA
display('GENERATING ELEMENT INFORMATION')
count2=1;
for j=1:grid.elements
    if (logic(grid.ncon(j,1))==1) && (logic(grid.ncon(j,2))==1) && (logic(grid.ncon(j,3))==1)
        subgrid.ncon(count2,1)=gridtosub(grid.ncon(j,1),1);
        subgrid.ncon(count2,2)=gridtosub(grid.ncon(j,2),1);
        subgrid.ncon(count2,3)=gridtosub(grid.ncon(j,3),1);
        subgrid.ncon(count2,4)=gridtosub(grid.ncon(j,4),1);
        count2=count2+1;
    end
end
subgrid.elements=length(subgrid.ncon);
%% WRITE OUT NEW GRID
write_adh_grid(strcat([basename '_SUB']),subgrid);
%%
save(strcat([basename '_SUB']),'subgrid','subtogrid')
toc
