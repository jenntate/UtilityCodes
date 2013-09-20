function [ovl]=readovlfile(basename,nt)
% READS THE ADH VELOCITY SOLUTION FILE 
% SAVES THE DATA INTO THE TIME_VEL AND VELOCITY ARRAYS
% VELOCITY(1,Y,Z) IS THE X VELOCITY FOR TIME STEP Y FOR NODE Z
% VELOCITY(2,Y,Z) IS THE X VELOCITY FOR TIME STEP Y FOR NODE Z
% For memory effeciency make nt just slightly larger than the number of
% time steps that the model achieved

%tic

if(nargin < 2)
    nt=500;
end

%open the 63 & 64 files
fid64=fopen([basename,'_ovl.dat'],'rt');

fprintf(1,'READING VELOCITY SOLUTION FILE\n')

compare='a';

while (~(isequal(compare,'NC')))
   compare=fscanf(fid64,'%2s',1);
   if(isequal(compare,'ND'))
       np=fscanf(fid64,'%i',1);
   elseif(isequal(compare,'NC'))
       ne=fscanf(fid64,'%i',1);
   else
       fscanf(fid64,'%s',1);
   end
end

ovl.velocity=zeros(nt,np,2);
%ovl.time=zeros(nt,1);

while ~(isequal(compare,'TS'))
   compare=fscanf(fid64,'%2s',1);
   if(isequal(compare,'TS'))
       ovl.time(1)=fscanf(fid64,'%*f %f',1);
       break;
   else
       fscanf(fid64,'%s',1);
   end
end

ovl.nodes=np;
ovl.elements=ne;

%reading body of dep file

temp=textscan(fid64,'%f %f %*f',[1 np]);
if(feof(fid64))
    fprintf(1,'END OF VELOCITY SOLUTION FILE REACHED\n')      
end
ovl.velocity(1,:,1)=temp{:,1};
ovl.velocity(1,:,2)=temp{:,2};

count=2;

while ~feof(fid64)
  test=fscanf(fid64,'%s',1);
  if(isequal(test,'ENDDS'))
      fprintf(1,'END OF VELOCITY SOLUTION FILE REACHED\n')
      break
  end
  ovl.time(count)=fscanf(fid64,'%*f %f',1);
  temp=textscan(fid64,'%f %f %*f',[1 np]);
  if(feof(fid64))
      fprintf(1,'END OF VELOCITY SOLUTION FILE REACHED\n')      
      break
  end
  ovl.velocity(count,:,1)=temp{:,1};
  ovl.velocity(count,:,2)=temp{:,2};
  count=count+1;
end

fclose(fid64);

clear temp  count  test compare  np  ne fid64;

%toc


