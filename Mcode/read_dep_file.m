function dep=read_dep_file(basename,nt)
%
%   Function to read an ADH _dep.dat solution file and save the values in
%   the dep structure
%
%   INPUT VARIABLES
%
%   basename = ADH root name for the _dep.dat solution file.
%
%   nt = number of time steps for this solution file.  This is an optional
%   input variable that makes memory specification more efficient.  Set to
%   500 by default.
%
%   OUTPUT VARIABLES
%
%   dep = structure containing the dep solution information
%
%   dep.time(n) = depth solution file for time step n.
%
%   dep.depth(n,m) = depth solution for time step n and node m
%
%   Finished 3/12/2010
%

%tic

%% checks if nt is inputted, if not then defaulted to 500 for memory
%  purposes

if(nargin < 2)
    nt=500;
end

%% opens the _dep.dat solution file

fid63=fopen([basename,'_dep.dat'],'rt');

fprintf(1,'READING DEPTH SOLUTION FILE\n')

%% reads the header information for the for the _dep.dat file

compare='a';

while (~(isequal(compare,'TS')))
   compare=fscanf(fid63,'%2s',1);
   if(isequal(compare,'ND'))
       np=fscanf(fid63,'%i',1);
   elseif(isequal(compare,'NC'))
       ne=fscanf(fid63,'%i',1);
   if(isequal(compare,'TS'))
       dep.time(1)=fscanf(fid63,'%*f %f',1);
       break;       
   else
       fscanf(fid63,'%s',1);
   end
end

dep.nodes=np;
dep.elements=ne;
dep.depth=zeros(nt,np);

%% reads the body of the _dep.dat solutino file

count=1;

while ~feof(fid63)
    
  % reads the nodal solution file values for this time step
    
  temp=fscanf(fid63,'%f',[1 np]);
  
  % checks to see if this is the end of the solution file
  
  if(feof(fid63))
      fprintf(1,'END OF DEPTH SOLUTION FILE REACHED\n')      
      break
  end
  
  % sets the time value for the next time step
  
  dep.depth(count,:)=temp;    
  test=fscanf(fid63,'%s',1);
  
  % determines if the end of the solution file has been reached
  
  if(isequal(test,'ENDDS'))
      fprintf(1,'END OF DEPTH SOLUTION FILE REACHED\n')
      break
  end
  
  % reads the time value for the next time step
  
  dep.time(count)=fscanf(fid63,'%*f %f',1);

  count=count+1;
end

%% closes the _dep.dat solution file

fclose(fid63);

%% clears the variables

clear temp  count  test compare  np  ne fid63;

%toc

