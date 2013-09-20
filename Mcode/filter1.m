function filter1(basename)
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
%tic

%open the 63 & 64 files
fid14=fopen([basename,'.xyz'],'rt');
fid=fopen([basename,'.out'],'wb','n');

fprintf(1,'READING FILE\n')
%test=fscanf(fid14,'%f %f %f',3);
%fprintf(1,'test %f %f %f',test(1),test(2),test(3))
%if(test(3)> -99)
%  fprintf(fid,'%f %f %f \n',test(1), test(2), test(3));
%end
while ~feof(fid14)
  test=fscanf(fid14,'%f %f %f',3);
%  fprintf(1,'test %f %f %f',test(1),test(2),test(3))
  if(test(3)> -99)
    fprintf(fid,'%f %f %f \n',test(1), test(2), test(3));
  end
  
end

fprintf(1,'FINISHED READING FILE\n')
%fprintf(1,'CE = %i\n',ce)
%fprintf(1,'CN = %i\n',cn)
%fprintf(fid,'%s\n',line1);

%for m=1:ce-1
%   fprintf(fid,'E3T %i %i %i %i %i \n',m,ncon(m,1),ncon(m,2), ...
%       ncon(m,3),ncon(m,4)); 
%end
%for m=1:cn-1
%   fprintf(fid,'ND %i %11.8e %11.8e %11.8e \n',m,coor(m,1),coor(m,2), ...
%       coor(m,3)); 
%end

%for m=1:ct-1
%   fprintf(fid,'%s\n',temp1{m}); 
%end

%toc
fclose(fid14);
fclose(fid);

