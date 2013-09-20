function output=determine_average_values(basename,start_time,stop_time)
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

grid=read_adh_grid([basename '.3dm']);

%% determines the area of each element

area=zeros(length(grid.x),1);

for i=1:length(grid.ncon(:,1)) 
  x=grid.x(grid.ncon(i,1:3));
  y=grid.y(grid.ncon(i,1:3));
  area(i)=element_area(x,y);
end

num_elements=max(grid.ncon(:,4));
salinity=zeros(num_elements,1);
num_elem=zeros(num_elements,1);

%% call to get the number of time steps in the .da file

check=load_da_stepnew([basename '.da'],0);

%% loops over every time step to get the wetted area for each

for i=1:check.nt
    
   % initalize wetted_area to 0.0 for each time step 
    
   i
   
   % read the data for each time step and save to the data structure
   
   data=load_da_stepnew([basename '.da'],i);
   
   % loop over every element in the mesh to sum the wetted area of each
   
   for j=1:length(grid.ncon(:,1))
       
      salinity(grid.ncon(j,4))=salinity(grid.ncon(j,4)) + ...
          mean(data.sal(grid.ncon(j,1)):data.sal(grid.ncon(j,2)): ...
          data.sal(grid.ncon(j,3)));
      num_elem(grid.ncon(j,4))=num_elem(grid.ncon(j,4))+1;
     
   end
   
%   for j=1:length(num_elem)
      
    salinity=salinity/num_elem;
       
%   end
   
   % sets the total wetted area for this time step to the output structure
   
   output.salinity_results(i,1:num_elements)=salinity(1:num_elements);
%   output.dep_results(i,1:num_elements)=salinity(1:num_elements);   
   output.time(i)=data.time;
   
end

% clears all the variables

clear data i j basename dist1 dist2 dist3 check grid
clear x y x_a y_a z a area wetted_area check a

toc
