function output=determine_dredge_volume(basename,grid)
%
%
%   Subroutine to determine the time series wetted area values for an
%   inputted simulation.
%
%   Input:
%
%       basename - basename of the .da file and the .3dm file to analyze
%
%   Output:
%
%       output - structure that includes output.time which is the time
%       values for the solution file and output.area which is the wetted
%       area values for the solution time.  
%
%   Example Usage:
%
%       data = determine_wetted_area('Mobile');
%
%       This will return a structure that includes data.time and data.area
%       which is the times and wetted area values.  This example is
%       analyzing a Mobile.3dm and Mobile.da file to determine the values.
%

tic

%% read the adh mesh

%grid=read_adh_grid([basename '.3dm']);

%% determines the area of each element

num_elements=max(grid.ncon(:,4));
area=zeros(length(grid.x),1);
total_area=zeros(num_elements,1);


for i=1:length(grid.ncon(:,1)) 
  x=grid.x(grid.ncon(i,1:3));
  y=grid.y(grid.ncon(i,1:3));
  area(i)=element_area(x,y);
  total_area(grid.ncon(i,4))=total_area(grid.ncon(i,4))+area(i);
end

output.total_area=total_area;

%% call to get the number of time steps in the .da file

check=load_da_stepnew([basename '.da'],0);

%% loops over every time step to get the wetted area for each

dredge_volume_out=zeros(check.nt,num_elements);
time=zeros(check.nt,1);

for i=check.nt-5:check.nt
    
   dredge_volume=zeros(num_elements,1); 
   % initalize wetted_area to 0.0 for each time step 
   if(mod(i,100)==0)
     fprintf(1,'Step %i of %i\n',i,check.nt);
   end
%   wetted_area(=0.0;
   
   % read the data for each time step and save to the data structure
   
   data=load_da_stepnew([basename '.da'],i);

   % loop over every element in the mesh to sum the wetted area of each
   
   for j=1:length(grid.ncon(:,1))

      % sets z array to the dep of each node in the element
      
      z=data.dpl(grid.ncon(j,1:3));
         
      dredge_volume(grid.ncon(j,4)) = dredge_volume(grid.ncon(j,4)) + ...
           (z(1)+z(2)+z(3))*area(j)/3.0;
      
   end
   
   % sets the total wetted area for this time step to the output structure
   
   dredge_volume_out(i,1:num_elements)=dredge_volume(1:num_elements);

   clear dredge_volume
   
   % sets the time for this time step to the output structure
   
   time(i)=data.time;
   
end

output.dredge_volume=dredge_volume_out;
output.time=time;

% clears all the variables

clear data i j basename dist1 dist2 dist3 check grid
clear x y x_a y_a z a area wetted_area check a

toc
