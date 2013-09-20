function output=determine_wetted_area(basename,grid_filename)
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

grid=read_adh_grid(grid_filename);

%% determines the area of each element

area=zeros(length(grid.x),1);

for i=1:length(grid.ncon(:,1)) 
  x=grid.x(grid.ncon(i,1:3));
  y=grid.y(grid.ncon(i,1:3));
  area(i)=element_area(x,y);
end

num_elements=max(grid.ncon(:,4));
wetted_area=zeros(num_elements,1);
total_area=zeros(num_elements,1);
water_volume=zeros(num_elements,1);

%% call to get the number of time steps in the .da file

check=load_da_stepnew([basename '.da'],0);

%% loops over every time step to get the wetted area for each

for i=1:check.nt
    
   % initalize wetted_area to 0.0 for each time step 
    
   wetted_area(:)=0.0;
   water_volume(:)=0.0;
   
   % read the data for each time step and save to the data structure
   
   data=load_da_stepnew([basename '.da'],i);
   
   % loop over every element in the mesh to sum the wetted area of each
   
   for j=1:length(grid.ncon(:,1))
       
      total_area(grid.ncon(j,4))=total_area(grid.ncon(j,4))+area(j); 
      
      % sets the x and y arrays for the nodal coordinates of each element
       
      x=grid.x(grid.ncon(j,1:3));
      y=grid.y(grid.ncon(j,1:3));

      % sets z array to the dep of each node in the element
      
      z=data.dep(grid.ncon(j,1:3));
      
      % if statement to add the entire area of an element fully wetted
      
      if(z(1) > 0.0 && z(2) > 0.0 && z(3) > 0.0)
          
         wetted_area(grid.ncon(j,4)) = wetted_area(grid.ncon(j,4)) + area(j);
         
         water_volume(grid.ncon(j,4)) = water_volume(grid.ncon(j,4)) + ...
             (z(1)+z(2)+z(3))*area(j)/3.0;
         
      % if statement for completely dry elements, adds no area   
         
      elseif(z(1) <= 0.0 && z(2) <= 0.0 && z(3) <= 0.0)
          
      % else statement for partially wetted elements    
          
      else
          
        % if statement for nodes 1 and 2 as either both wet or both dry  
          
        if((z(1) > 0.0 && z(2) > 0.0) || (z(1) <= 0.0 && z(2) <= 0.0))

           % determines to new locations for node 1 and node 2 
            
           x_a(1) = x(3) - (x(3)-x(1)) * z(3) / (z(1)-z(3));
           y_a(1) = y(3) - (y(3)-y(1)) * z(3) / (z(1)-z(3));
           x_a(2) = x(3) - (x(3)-x(2)) * z(3) / (z(2)-z(3));
           y_a(2) = y(3) - (y(3)-y(2)) * z(3) / (z(2)-z(3));
           x_a(3) = x(3);
           y_a(3) = y(3);
           
           % call to element area subroutine to determine the area of the
           % subtriangle
           
           a = element_area(x_a,y_a);
           
           % if statement to determine if node 3 is wet or dry
           
           if(z(3) <= 0.0)
               
             % adds in the area of the element minus the subtriangle  
               
             wetted_area(grid.ncon(j,4)) = wetted_area(grid.ncon(j,4)) + area(j) - a;
             
             water_volume(grid.ncon(j,4)) = water_volume(grid.ncon(j,4)) + ...
                  (z(1)+z(2)+0.0)*(area(j)-a)/3.0;
             
           else
               
             % adds in the area of the subtraingle  
               
             wetted_area(grid.ncon(j,4)) = wetted_area(grid.ncon(j,4)) + a;
             
             water_volume(grid.ncon(j,4)) = water_volume(grid.ncon(j,4)) + ...
                  (0.0+0.0+z(3))*(a)/3.0;            
             
           end
         
        % if statement for nodes 1 and 2 as either both wet or both dry             
           
        elseif((z(1) > 0.0 && z(3) > 0.0) || (z(1) <= 0.0 && z(3) <= 0.0))
            
           % determines to new locations for node 1 and node 3  
            
           x_a(1) = x(2) - (x(2)-x(1)) * z(2) / (z(1)-z(2));
           y_a(1) = y(2) - (y(2)-y(1)) * z(2) / (z(1)-z(2));
           x_a(2) = x(2); 
           y_a(2) = y(2); 
           x_a(3) = x(2) - (x(2)-x(3)) * z(2) / (z(3)-z(2));
           y_a(3) = y(2) - (y(2)-y(3)) * z(2) / (z(3)-z(2));
           
           % call to element area subroutine to determine the area of the
           % subtriangle           
           
           a = element_area(x_a,y_a);

           % if statement to determine if node 2 is wet or dry           
           
           if(z(2) <= 0.0)
               
             % adds in the area of the element minus the subtriangle                 
               
             wetted_area(grid.ncon(j,4)) = wetted_area(grid.ncon(j,4)) + area(j) - a;
             
             water_volume(grid.ncon(j,4)) = water_volume(grid.ncon(j,4)) + ...
                  (z(1)+0.0+z(3))*(area(j)-a)/3.0;             
             
           else
               
             % adds in the area of the subtraingle                 
               
             wetted_area(grid.ncon(j,4)) = wetted_area(grid.ncon(j,4)) + a;
             
             water_volume(grid.ncon(j,4)) = water_volume(grid.ncon(j,4)) + ...
                  (0.0+z(2)+0.0)*(a)/3.0;                
             
           end
         
        % if statement for nodes 2 and 3 as either both wet or both dry              
           
        elseif((z(2) > 0.0 && z(3) > 0.0) || (z(2) <= 0.0 && z(3) <= 0.0))
            
           % determines to new locations for node 2 and node 3             
            
           x_a(1) = x(1);
           y_a(1) = y(1); 
           x_a(2) = x(1) - (x(1)-x(2)) * z(1) / (z(2)-z(1)); 
           y_a(2) = y(1) - (y(1)-y(2)) * z(1) / (z(2)-z(1)); 
           x_a(3) = x(1) - (x(1)-x(3)) * z(1) / (z(3)-z(1));
           y_a(3) = y(1) - (y(1)-y(3)) * z(1) / (z(3)-z(1)); 
           
           % call to element area subroutine to determine the area of the
           % subtriangle            
           
           a = element_area(x_a,y_a);
           
           % if statement to determine if node 1 is wet or dry   
           
           if(z(1) <= 0.0)
               
             % adds in the area of the element minus the subtriangle    
               
             wetted_area(grid.ncon(j,4)) = wetted_area(grid.ncon(j,4)) + area(j) - a;
             
             water_volume(grid.ncon(j,4)) = water_volume(grid.ncon(j,4)) + ...
                  (0.0+z(2)+z(3))*(area(j)-a)/3.0;       
              
           else
               
             % adds in the area of the subtraingle   
               
             wetted_area(grid.ncon(j,4)) = wetted_area(grid.ncon(j,4)) + a;
            
             water_volume(grid.ncon(j,4)) = water_volume(grid.ncon(j,4)) + ...
                  (z(1)+0.0+0.0)*(a)/3.0;              
             
           end  
           
        end
        
      end
      
   end
   
   % sets the total wetted area for this time step to the output structure
   
   output.wetted_area(i,1:num_elements)=wetted_area(1:num_elements);
   
   output.total_wetted_area(i)=sum(output.wetted_area(i,1:num_elements));
   
   output.water_volume(i,1:num_elements)=water_volume(1:num_elements);

   % sets the time for this time step to the output structure
   
   output.time(i)=data.time;
   
end

output.total_area=sum(area);

% clears all the variables

clear data i j basename dist1 dist2 dist3 check grid
clear x y x_a y_a z a area wetted_area check a

toc
