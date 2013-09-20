function [dep]=readdepfile(basename,nt)
% READS ADH DEPTH SOLUTION FILE 
% CREATES THE DEPTH AND TIME_DEP ARRAYS WITH
% DEPTH(1,1) BEING THE DEPTH AT TIME_DEP(1) FOR NODE 1
% DEPTH(1,2) BEING THE DEPTH AT TIME_DEP(1) FOR NODE 2

tic

if(nargin < 2)
    nt=500;
end

%open the 63 & 64 files

fid63=fopen([basename,'_dep.dat'],'rt');

fprintf(1,'READING DEPTH SOLUTION FILE\n')

compare='a';

while (~(isequal(compare,'NC')))
   compare=fscanf(fid63,'%2s',1);
   if(isequal(compare,'ND'))
       np=fscanf(fid63,'%i',1);
   elseif(isequal(compare,'NC'))
       ne=fscanf(fid63,'%i',1);
   else
       fscanf(fid63,'%s',1);
   end
end

dep.nodes=np;
dep.elements=ne;
dep.depth=zeros(nt,np);
%dep.time=zeros(nt,1);

while ~(isequal(compare,'TS'))
   compare=fscanf(fid63,'%2s',1);
   if(isequal(compare,'TS'))
       dep.time(1)=fscanf(fid63,'%*f %f',1);
       break;
   else
       fscanf(fid63,'%s',1);
   end
end

%reading body of dep file

dep.depth(1,:)=fscanf(fid63,'%f',[1 np]);
if(feof(fid63))
    fprintf(1,'END OF DEPTH SOLUTION FILE REACHED\n')      
end

count=2;

while ~feof(fid63)
  test=fscanf(fid63,'%s',1);
  if(isequal(test,'ENDDS'))
      fprintf(1,'END OF DEPTH SOLUTION FILE REACHED\n')
      break
  end
  dep.time(count)=fscanf(fid63,'%*f %f',1);
  temp=fscanf(fid63,'%f',[1 np]);
  if(feof(fid63))
      fprintf(1,'END OF DEPTH SOLUTION FILE REACHED\n')      
      break
  end
  dep.depth(count,:)=temp;
  count=count+1;
end

fclose(fid63);

clear temp  count  test compare  np  ne fid63;

toc



