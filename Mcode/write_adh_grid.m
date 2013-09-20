function write_adh_grid(basename,grid)
%   
%   This subroutine writes out an ADH mesh file with the nodal and element
%   information contained in the structure grid.
%
%   basename = name of the output mesh.  The extension on this outputted
%   file will be .3dm
%
%   grid = the structure with all the mesh information.  
%   grid.x = x coordinates for all nodes
%   grid.y = y coordinates for all nodes
%   grid.z = elevation values for all nodes
%   grid.ncon = elemental connectivity for all elements
%
%   Finished 3/12/2010
%
%tic

%% opens the new ADH grid output file

fid14=fopen([basename,'.3dm'],'wb','n');

fprintf(1,'WRITING GRID FILE\n');

%% determines the number of nodes and elements in the mesh

max_elements=length(grid.ncon);
max_nodes=length(grid.x);

%% writes the first dummy line of the ADH mesh

grid.line{1} = 'MESH2D';

fprintf(fid14,'%s\n',grid.line{1});

%% writes the element information

for i=1:max_elements
    fprintf(fid14,'E3T %i %i %i %i %i\n',i, grid.ncon(i,1), ...
        grid.ncon(i,2), grid.ncon(i,3), grid.ncon(i,4));
end

%% writes the nodal values

for i=1:max_nodes
     fprintf(fid14,'ND %i %f %f %f\n',i, grid.x(i), ...
        grid.y(i), grid.z(i));
end

%% writes the information at the end of the mesh file
%grid.line
%length(grid.line)
if(length(grid.line)>1)
  for i=2:length(grid.line)-1
      fprintf(fid14,'%s\n',grid.line{i+1});
  end
end

%% closes the new ADH mesh file

fclose(fid14);

fprintf(1,'FINISHED WRITING GRID FILE\n');

%toc


