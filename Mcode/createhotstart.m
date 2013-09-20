function createhotstart(grid_name,initial_water_level)
%
%
%
%
%
%
%
%    subroutine to create a hotstart file from an initial water level
%
%    createhotstart('MTOG_TATE7.2dm',0.0)
%

hotstart_file_name=[grid_name(1:length(grid_name-3)) '.hot'];

fid=fopen(hotstart_file_name,'w');

grid=read_adh_grid([grid_name '.3dm']);

fprintf(fid,'DATASET\n');

fprintf(fid,'OBJTYPE "mesh2d"\n');

fprintf(fid,'BEGSCL\n');

fprintf(fid,'ND %i\n',length(grid.x));

fprintf(fid,'NC %i\n',length(grid.ncon(:,1)));

fprintf(fid,'NAME ioh\n');

fprintf(fid,'TIMEUNITS seconds\n');

fprintf(fid,'TS 0 0\n');

for i=1:length(grid.x)

   fprintf(fid,'%f\n',initial_water_level-grid.z(i));

end

fprintf(fid,'ENDDS\n');

fclose(fid);
