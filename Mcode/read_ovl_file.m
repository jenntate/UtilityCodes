function ovl=read_ovl_file(basename,nt)
%
%   Function to read the full velocity solution file into the ovl structure
%
%   INPUT VARIABLES
%
%   basename = root adh filename for the _ovl.dat file
%
%   nt = number of time steps for the solution file.  This input is
%   optional and is only needed for efficient memory usage.  No entry
%   defaults to nt = 500.
%
%   OUTPUT VARIABLES
%
%   ovl.time(n) = time for time step n of the solution
%
%   ovl.velocity(n,m,p) = velocity vector information for the solution file
%   where n is the time step, m is the node number, and p is 1 for the x
%   velocity and 2 for the y velocity
%
%   Finished 3/12/2010
%

%tic

%% sets nt if it isn't inputted.  needed for memory allocation

if(nargin < 2)
    nt=500;
end

%% opens velocity solution file

fid64=fopen([basename,'_ovl.dat'],'rt');

fprintf(1,'READING VELOCITY SOLUTION FILE\n')

%% reads the header information for the velocity solution file

compare='a';

while (~(isequal(compare,'TS')))
   compare=fscanf(fid64,'%2s',1);
   if(isequal(compare,'ND'))
       np=fscanf(fid64,'%i',1);
   elseif(isequal(compare,'NC'))
       ne=fscanf(fid64,'%i',1);
   if(isequal(compare,'TS'))
       ovl.time(1)=fscanf(fid64,'%*f %f',1);
       break;
   else
       fscanf(fid64,'%s',1);
   end
end

%% preallocates the velocity vector and sets the number of nodes and
%  elements

ovl.velocity=zeros(nt,np,2);

ovl.nodes=np;
ovl.elements=ne;

%% reads the velocity solution file

count=1;

while ~feof(fid64)
    
  % reads the velocity vector information for each node for this time step
    
  temp=textscan(fid64,'%f %f %*f',[1 np]);
  
  % checks for the end of the file
  
  if(feof(fid64))
      fprintf(1,'END OF VELOCITY SOLUTION FILE REACHED\n')      
      break
  end
  
  % sets the .velocity array 
  
  ovl.velocity(count,:,1)=temp{:,1};
  ovl.velocity(count,:,2)=temp{:,2};
  
  % increment to the next time step
  
  count=count+1;    
  
  % reads the next time step time
  
  test=fscanf(fid64,'%s',1);
  
  % checks for the end of the solution file
  
  if(isequal(test,'ENDDS'))
      fprintf(1,'END OF VELOCITY SOLUTION FILE REACHED\n')
      break
  end
  
  % reads the time for the next time step
  
  ovl.time(count)=fscanf(fid64,'%*f %f',1);

end

%% close the velocity solution file

fclose(fid64);

%% clear the variables

clear temp  count  test compare  np  ne fid64;

%toc


