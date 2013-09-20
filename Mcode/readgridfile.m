function [grid]=readgridfile(basename)
% READS IN THE ADH GRID FILE 
% PUTS THE APPROPRIATE VARIABLES IN SET ARRAYS AS:
% NCON CONTAINS THE ELEMENT CONNECTIONS AND MATERIAL TYPES
% NCON(1,1) IS THE FIRST NODE IN ELEMENT 1
% NCON(1,2) IS THE SECOND NODE IN ELEMENT 1
% NCON(1,3) IS THE THIRD NODE IN ELEMENT 1
% NCON(1,4) IS THE MATERIAL TYPE FOR ELEMENT 1
% COOR(1,1) IS THE X COORDINATE FOR NODE 1
% COOR(1,2) IS THE Y COORDINATE FOR NODE 1
% COOR(1,3) IS THE ELEVATION FOR NODE 1
% np should be the greater than or equal to the number of nodes

%tic

%open the 63 & 64 files
fid14=fopen([basename,'.3dm'],'rt');

%reading body of dep file

ce=1;
cn=1;
ct=1;
%grid.ncon=zeros(np,4);
%grid.coor=zeros(np,3);

fprintf(1,'READING GRID FILE\n')

while ~feof(fid14)
  test=fscanf(fid14,'%s',1);
  if(isequal(test,'E3T'))
     element=fscanf(fid14,'%i',1);
     grid.ncon(element,:)=fscanf(fid14,'%i %i %i %i',4);
     if(element~=ce)
         fprintf(1,'ERROR IN ELEMENT NUMBERING AT %i',ce)
         break
     end
     ce=ce+1;
  elseif(isequal(test,'ND'))
     node=fscanf(fid14,'%i',1); 
     grid.coor(node,:)=fscanf(fid14,'%f %f %f',3);
     if(node~=cn)
         fprintf(1,'ERROR IN NODE NUMBERING AT %i', cn)
         break
     end    
     cn=cn+1;
  else
      grid.line{ct}=strcat(char(test),char(fgetl(fid14)));
      ct=ct+1;
  end
end

fclose(fid14);

clear ct cn ce test element node fid14;

fprintf(1,'FINISHED READING GRID FILE\n')

%toc


