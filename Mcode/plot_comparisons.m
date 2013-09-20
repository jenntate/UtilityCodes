function plot_comparisons(basename,data,attribute,units,point_number,fid, ...
    start_time,stop_time)
%
% 
%     Inputs
%
%        basename - cell including the rootnames of the different runs to
%                   compare
%
%        data - structure array that includes the data for a given point
%               location
%
%        attribute - attribute to be plotted from the data structure.  
%                    These are:
%
%                    dep = depth solution
%                    wse = water surface elevation solution
%                    vel = velocity vector and magnitude solution
%                    sal = salinity solution
%                    vor = vorticity solution
%                    snd = sand concentration solution
%                    slt = silt concentration solution
%                    err = error solution
%                    dpl = bed displacement solution
%                    alt = active layer thickness
%                    ald = active layer distribution
%                    blt = bed layer thickness
%                    bld = bed layer distribution
%                    cbp = cohesive bed properties
%                    bsh = bed shear stress solution
%                    bed = bedload vector and magnitude solution
%                    smr = sediment mass residual
%
%        units - units for y axis
%
%     Outputs
%
%        time series plots saved to png and fig files
%
%
%
%

%% preallocate the check cell array

check=cell(length(basename),1);

%% read the available data for each simulation

for i=1:length(basename)
   check{i}=load_da_stepnew([basename{i} '.da'],0); 
end

%% determines the maximum number of time steps from all simulations for
%  array preallocation

nt=data{1}.nt;
for p=2:length(basename)
  if(nt < data{p}.nt)
    nt = data{p}.nt;
  end
end

%% preallocate the time array and set equal to the datasets' time values

len=zeros(length(basename),1);
time=zeros(length(basename),nt);

for i=1:length(basename)
   len(i)=length(data{i}.time);
   time(i,1:len(i))=data{i}.time(1:len(i));
end

%% preallocates the value array used to carry into the plot_data subroutine

value=zeros(length(basename),nt);

%% reads the attribute variable to determine the attributes to investigate

temp1=sscanf(attribute,'%s');

for i=1:length(temp1)/3
    
   % determines the attribute for this loop
    
   test=temp1((1+(i-1)*3):3*i);
   
   % case statement to get to the right attribute
   
   switch lower(test)
       
       % checks the dep
       
       case {'dep'}
                  
           for l=1:length(basename)
               
             % checks to ensure all .da files have the depth variable
             % included
               
             if(check{l}.dep_check < 1)
                fprintf(1,'\nAttribute %s does not exist in %s.da\n\n',...
                    test,basename{l});
                STOP;
             end
             
             % sets the array to carry to the plot_data subroutine
             
             value(l,1:len(l))=data{l}.dep;           
           
           end 
           
           % calls the plot_data subroutine to plot the data and determine
           % max,min,mean, and median values
           
           plot_data(basename,time,value,'Depth',test,point_number, ...
               units{i},fid,start_time,stop_time,len);         
           
       % checks the wse    
           
       case {'wse'}
           
           for l=1:length(basename)
               
             % checks to ensure all the .da files have the water surface
             % elevation variable included
               
             if(check{l}.wse_check < 1)
                fprintf(1,'\nAttribute %s does not exist in %s.da\n\n',...
                    test,basename{l});
                STOP;
             end
             
             % sets the array to carry to the plot_data subroutine
             
             value(l,1:len(l))=data{l}.wse;           
             
           end
           
           % calls the plot_data subroutine to plot the data and determine
           % max,min,mean, and median values
           
           plot_data(basename,time,value, ...
                 'Water Surface Elevation',test,point_number,units{i},...
                 fid,start_time,stop_time,len);          
           
       % checks the velocity    
           
       case {'vel'}
           
           % preallocates arrays for the u and v velocities
           
           value_u=zeros(length(basename),nt);  
           value_v=zeros(length(basename),nt);  
           
           % checks to ensure all the .da files have the velocity variable
           % included
           
           for l=1:length(basename)
             if(check{l}.vel_check < 1)
                fprintf(1,'\nAttribute %s does not exist in %s.da\n\n',...
                    test,basename{l});
                STOP;
             end
             
             % sets the array to carry to the plot_data subroutine
             
             value_u(l,1:len(l))=data{l}.u;
             value_v(l,1:len(l))=data{l}.v;
             
           end
           
           % preallocates the array for the velocity magnitudes
           
           vel_mag=zeros(length(basename),nt);
           
           % determines the velocity magnitudes
           
           for p=1:length(basename)
%             loc1=find(data{p}.time>=min_x,1,'first');
%             loc2=find(data{p}.time<=max_x,1,'last');
             for q=1:length(data{p}.time)
                vel_mag(p,q)=((data{p}.u(q)*data{p}.u(q) + ...
                    data{p}.v(q)*data{p}.v(q))^0.5);
             end                                   
           end 

           % calls the plot_data subroutine to plot the data and determine
           % the max,min,mean, and median values for the velocity magnitude
           
           plot_data(basename,time,vel_mag,'Velocity',test,...
                 point_number,units{i},fid,start_time,stop_time,len);
             
           % calls the plot data subroutine to plot the data and determine 
           % the max,min,mean, and median values for the x component of
           % velocity
             
           plot_data(basename,data{p}.time,data{p}.u, ...
                 'X Component of Velocity','x-vel', ...
                 point_number,units{i},fid,start_time,stop_time,len);
             
           % calls the plot data subroutine to plot the data and determine 
           % the max,min,mean, and median values for the y component of
           % velocity
             
           plot_data(basename,data{p}.time,data{p}.v, ...
                 'Y Component of Velocity','y-vel', ...
                 point_number,units{i},fid,start_time,stop_time,len);           
           
       % checks the error    
           
       case {'err'}
           
           for l=1:length(basename)
               
             % checks ensure all .da files have the Error attribute  
               
             if(check{l}.err_check < 1)
                fprintf(1,'\nAttribute %s does not exist in %s.da\n\n',...
                    test,basename{l});
                STOP;
             end
             
             % sets the array to be carried into the plot_data subroutine
             
             value(l,1:len(l))=data{l}.err;             
             
           end
           
           % calls the plot_data subroutine to plot the data and determine
           % the min,max,mean, and median values
           
           plot_data(basename,time,value,'Error',test,point_number, ...
               units{i},fid,start_time,stop_time,len);           
           
       % checks the salinity    
           
       case {'sal'}
           
           for l=1:length(basename)
               
             % checks to ensure all the .da files have the salinity parameter  
               
             if(check{l}.sal_check < 1)
                fprintf(1,'\nAttribute %s does not exist in %s.da\n\n',...
                    test,basename{l});
                STOP;
             end
             
             % sets the value array to be carried into the plot_data
             % subroutine
             
             value(l,1:len(l))=data{l}.sal;
             
           end
           
           % calls the plot_data subroutine to plot the data and determine
           % the min,max,mean, and median values
           
           plot_data(basename,time,value,'Salinity',test,point_number, ...
               units{i},fid,start_time,stop_time,len);         
           
       % checks the vorticity    
           
       case {'vor'}
           
           for l=1:length(basename)
           
             % checks to ensure all .da files have the Vorticity attribute
               
             if(check{l}.vor_check < 1)
                fprintf(1,'\nAttribute %s does not exist in %s.da\n\n',...
                    test,basename{l});
                STOP;
             end
             
             % sets the value array to carry into the plot_data subroutine
             
             value(l,1:len(l))=data{l}.vor;           
             
           end
           
           % calls the plot_data subroutine to plot the data and determine
           % the min,max,mean, and median values
           
           plot_data(basename,time,value,'Vorticity',test,point_number, ...
               units{i},fid,start_time,stop_time,len);           
           
       % checks the sand    
           
       case {'snd'}
           
           % preallocates the arrays for the rouse number and bed 
           
           value_rouse=zeros(length(basename),nt); 
           value_bdma=zeros(length(basename),nt); 
           
           for l=1:length(basename)
               
             % checks to ensure all .da files have the sand parameter
             % included
               
             if(check{l}.snd_check < 1)
                fprintf(1,'\nAttribute %s does not exist in %s.da\n\n',...
                    test,basename{l});
                STOP;
             end
             
             % sets the arrays to be carried into the plot_data subroutine
             
             value(l,1:len(l))=data{l}.snd;
             value_rouse(l,1:len(l))=data{l}.snd_rouse;             
             value_bdma(l,1:len(l))=data{l}.snd_bdma;             
             
           end
           
           % calls the plot_data subroutine to plot the data and determine
           % the min,max,mean, and median values for Sand Concentration
           
           plot_data(basename,time,value,'Sand Concentration',test, ...
               point_number,units{i},fid,start_time,stop_time,len);
           
           % calls the plot_data subroutine to plot the data and determine
           % the min,max,mean, and median values for the Sand Rouse Number
           
           plot_data(basename,time,value_rouse, ...
               'Rouse Number','snd_rouse',point_number,units{i}, ...
               fid,start_time,stop_time,len);  
           
           % calls the plot_data subroutine to plot the data and determine
           % the min,max,mean, and median values for the Sand Bedload Mass
           % per Unit Area
           
           plot_data(basename,time,value_bdma, ...
               'Bedload Mass per Unit Area','snd_bdma',point_number, ...
               units{i},fid,start_time,stop_time,len); 
           
       % checks the silts    
           
       case {'slt'}
           
           for l=1:length(basename)
               
             % checks to ensure all .da files have the Silt parameter included  
               
             if(check{l}.slt_check < 1)
                fprintf(1,'\nAttribute %s does not exist in %s.da\n\n',...
                    test,basename{l});
                STOP;
             end
             
             % sets the arrays to be carries into the plot_data subroutine
             
             value(l,1:len(l))=data{l}.slt;            
             %value_rouse(l,1:len(l))=data{l}.slt_rouse;              
             %value_bdma(l,1:len(l))=data{l}.slt_bdma;             
             
           end
           
           % calls the plot_data subroutine to plot the data and determine
           % the min,max,mean, and median values for the Silt Concentration
           
           plot_data(basename,time,value,'Silt Concentration',test, ...
               point_number,units{i},fid,start_time,stop_time,len);
           
           % calls the plot_data subroutine to plot the data and determine
           % the min,max,mean, and median values for the Silt Rouse Number
           
          % plot_data(basename,time,value_rouse, ...
          %     'Rouse Number','slt_rouse',point_number,units{i}, ...
          %     fid,start_time,stop_time,len);  
           
           % calls the plot_data subroutine to plot the data and determine
           % the min,max,mean, and median values for the Silt Bedload Mass
           % per Unit Area
           
          % plot_data(basename,time,value_bdma, ...
          %     'Bedload Mass per Unit Area','slt_bdma',point_number, ...
          %     units{i},fid,start_time,stop_time,len); 
           
       % checks the bed load thickness   
         
       case {'blt'}
           
           for l=1:length(basename)
               
             % checks each of the .da files to make sure the bed layer 
             % thickness parameter is included  
               
             if(check{l}.blt_check < 1)
                fprintf(1,'\nAttribute %s does not exist in %s.da\n\n',...
                    test,basename{l});
                STOP;
             end 
             
             % sets the value array to carry into the plot_data subroutine
             
             value(l,1:len(l))=data{l}.vor;
             
           end
           
           % sets the number of layers variable
           
           number_layers=check{1}.number_layers;
           
           for l=2:length(basename)
               
              % checks to make sure the number of layers is consistant
              % between simulations
               
              if(number_layers ~= check{l}.number_layers)
                  fprintf(1,'\nNumber of Layers are not consistant in %s.da\n\n', ...
                      basename{l});
              end
           end
           
           % loop to plot the layer data
           
           for n=1:check{l}.number_layers
               
               % sets the value arry to carry into the plot_data subroutine
               
               for l=1:length(basename) 
                   
                 value(l,1:len(l))=data{l}.blt(n,:);
               
               end
               
               % calls the plot_data subroutine to plot the bed layer
               % thickness values for each layer and the determine the
               % min,max,mean, and median values
               
               plot_data(basename,time,value, ...
                   ['Bed Layer Thickness Layer ' numstr(n)],...
                   ['blt_number_' numstr(n)],point_number,units{i}, ...
                   fid,start_time,stop_time,len);
               
           end
            
           
       % checks the bed load displacement    
         
       case {'bld'}
           
           for l=1:length(basename)
               
             % checks to make sure the number of layers is consistant 
             % between simulations  
               
             if(check{l}.bld_check < 1)
                fprintf(1,'\nAttribute %s does not exist in %s.da\n\n',...
                    test,basename{l});
                STOP;
             end
           end 
           
           % compares the number of sediments between all the .da files
           
           number_sed=check{1}.number_sed;
           for l=2:length(basename)
              if(number_sed ~= check{l}.number_sed)
                  fprintf(1,'\nNumber of Sediments are not consistant in %s.da\n\n', ...
                      basename{l});
              end
           end
           
           % sets the value array to carry into the plot_data array
           
           for e=1:check{l}.bld_check
               
             for n=1:check{l}.number_sed
                 
                 for l=1:length(basename)
                     
                   value(l,1:len(l))=data{l}.bld(e,n,:);
                   
                 end
                 
                 % calls the plot_data array to plot the data and
                 % determines the min,max,mean, and median of the data set
                 
                 plot_data(basename,time,value, ...
                     ['Bed Layer ' numstr(e) ' Distribution, Sediment ' numstr(n)],...
                     ['bld_' numstr(e) '_number_' numstr(n)],point_number, ...
                     units{i},fid,start_time,stop_time,len);
                 
             end
             
           end
           
       % checks the smr    
        
       case {'smr'}
           
           % checks each .da file to ensure each has the smr parameter
           
           for l=1:length(basename)
               
             if(check{l}.smr_check < 1)
                 
                fprintf(1,'\nAttribute %s does not exist in %s.da\n\n',...
                    test,basename{l});
                STOP;
                
             end
             
           end
           
           % compares the number of sediments between all the .da files
           
           number_sed=check{1}.number_sed;
           
           for l=2:length(basename)
               
              if(number_sed ~= check{l}.number_sed)
                  fprintf(1,'\nNumber of Sediments are not consistant in %s.da\n\n', ...
                      basename{l});
                  
              end
              
           end
           
           % sets the value array to be carried into the plot_data
           % subroutine
           
           for n=1:check{l}.number_sed
               
               for l=1:length(basename) 
               
                 value(l,1:len(l))=data{l}.smr(n,:);                   
               
               end
               
               % calls the plot_data subroutine to plot the data and
               % determine the min,max,mean, and median of the data
               
               plot_data(basename,time,value, ...
                   ['Sediment Mass Residual ' numstr(n)],...
                   ['smr_number_' numstr(n)],point_number,units{i}, ...
                   fid,start_time,stop_time,len);
               
           end      
           
       % checks the bed displacement    
           
       case {'dpl'}
           
           % checks the .da files to determine if the dpl attribute is
           % included in all of them
           
           for l=1:length(basename)
               
             if(check{l}.dpl_check < 1)
                 
                fprintf(1,'\nAttribute %s does not exist in %s.da\n\n',...
                    test,basename{l});
                STOP;
                
             end
             
             % sets the value array to carry into the plot_data subroutine
             
             value(l,1:len(l))=data{l}.dpl;               
             
           end
           
           % calls the plot_data subroutine to plot the data and determine
           % the min,max,mean, and median of the data
           
           plot_data(basename,time,value,'Displacement',test, ...
               point_number,units{i},fid,start_time,stop_time,len);    
           
       % checks the bed shear    
           
       case {'bsh'}
           
           % checks each .da file to determine if the bsh parameter is
           % included
           
           for l=1:length(basename)
               
             if(check{l}.bsh_check < 1)
                 
                fprintf(1,'\nAttribute %s does not exist in %s.da\n\n',...
                    test,basename{l});
                STOP;
                
             end
             
             % sets the value array to carry into the plot_data subroutine
             
             value(l,1:len(l))=data{l}.bsh;
             
           end
           
           % calls the plot_data subroutine to plot the data and determine
           % the min,max,mean, and median
           
           plot_data(basename,time,value,'Shear Stress',test, ...
               point_number,units{i},fid,start_time,stop_time,len);   
           
       % checks the bed vector    
           
       case {'bed'}
           
           % checks each .da file to determine if the bed parameter is
           % included
           
           for l=1:length(basename)
               
             if(check{l}.bed_check < 1)
                 
                fprintf(1,'\nAttribute %s does not exist in %s.da\n\n',...
                    test,basename{l});
                STOP;
                
             end
             
           end 
           
           % preallocates the bed_mag array to carry into the plot_data
           % subroutine
           
           bed_mag=zeros(length(basename),nt);
           
           % calculates the bed_mag array for each time
           
           for p=1:length(basename)
               
    %         loc1=find(data{p}.time>=min_x,1,'first');
    %         loc2=find(data{p}.time<=max_x,1,'last');
             
             for q=1:length(data{p}.bed_u)
                bed_mag(p,q)=((data{p}.bed_u(q)*data{p}.bed_u(q) + ...
                    data{p}.bed_v(q)*data{p}.bed_v(q))^0.5);
             end
             
           end
           
           % calls the plot_data array to plot the bed vector and calculate
           % the min,max,mean, and median values
           
           plot_data(basename,time,bed_mag,'Bed Vector',test,...
                 point_number,units{i},fid,start_time,stop_time,len);
             
           % calls the plot_data array to plot the x component and 
           % calculate the min,max,mean, and median values
             
           plot_data(basename,data{p}.time,data{p}.bed_u, ...
                 'Bed Vector X-Component','bed-x-component',point_number, ...
                 units{i},fid,start_time,stop_time,len);
             
           % calls the plot_data array to plot the y component and 
           % calculate the min,max,mean, adn median values
             
           plot_data(basename,data{p}.time,data{p}.bed_v, ...
                 'Bed Vector Y-Component','bed-y-component',point_number, ...
                 units{i},fid,start_time,stop_time,len);           
           
           
       % checks the cbp    
          
       case {'cbp'}
           
           % checks each of the da files for the cbp parameter
           
           for l=1:length(basename)
               
             if(check{l}.cbp_check < 1)
                 
                fprintf(1,'\nAttribute %s does not exist in %s.da\n\n',...
                    test,basename{l});
                STOP;
                
             end
             
           end
           
           % checks the number of sediments for each of the .da files
           
           number_sed=check{1}.cbp_check;
           
           for l=2:length(basename)
               
              if(number_sed ~= check{l}.cbp_check)
                  fprintf(1,'\nNumber of Sediments are not consistant in %s.da\n\n', ...
                      basename{l});
              end
              
           end
           
           % sets the value arrays for the den, ces, and ere data sets
           
           for n=1:check{l}.cbp_check
               
               % sets the value array for the cbp_den dataset
               
               for l=1:length(basename) 
                 value(l,1:len(l))=data{l}.cbp_den(n,:);      
               end
               
               % plots the density parameter and determines the
               % min,max,mean, and median values
               
               plot_data(basename,time,value, ...
                   ['Bed Density, Layer ' numstr(n)],...
                   ['cbp_den_number_' numstr(n)],point_number,units{i}, ...
                   fid,start_time,stop_time,len);
               
               % sets the value array for the cbp_ces dataset
               
               for l=1:length(basename) 
                 value(l,1:len(l))=data{l}.cbp_ces(n,:);                  
               end
               
               % plots the ces parameter and determines the min,max,mean,
               % and median values
               
               plot_data(basename,time,value, ...
                   ['Bed CES, Layer ' numstr(n)],...
                   ['cbp_ces_number_' numstr(n)],point_number,units{i}, ...
                   fid,start_time,stop_time,len);
               
               % sets the value array for the cbp_ere dataset
               
               for l=1:length(basename) 
                 value(l,1:len(l))=data{l}.cbp_ere(n,:);                    
               end
               
               % plots the ere parameter and determines the min,max,mean,
               % and median values
               
               plot_data(basename,time,value, ...
                   ['Bed ERE, Layer ' numstr(n)],...
                   ['cbp_ere_number_' numstr(n)],point_number,units{i}, ...
                   fid,start_time,stop_time,len);
               
           end           
           
       % checks the alt    
           
       case {'alt'}
           
           % checks each .da file to determine if the alt parameter is
           % included
           
           for l=1:length(basename)
               
             if(check{l}.alt_check < 1)
                 
                fprintf(1,'\nAttribute %s does not exist in %s.da\n\n',...
                    test,basename{l});
                STOP;
                
             end
             
             % sets the value array to include in the plot_data array
             
             value(l,1:len(l))=data{l}.alt;               
             
           end
           
           % calls the plot_data subroutine to plot the data and determine
           % the min,max,mean, and median values
           
           plot_data(basename,time,value,'Active Layer Thickness',test, ...
               point_number,units{i},fid,start_time,stop_time,len);       
           
       % checks the alb    
          
       case {'alb'}

           % checks each .da file for the alb parameter
           
           for l=1:length(basename)
               
             if(check{l}.alb_check < 1)
                 
                fprintf(1,'\nAttribute %s does not exist in %s.da\n\n',...
                    test,basename{l});
                STOP;
                
             end
             
           end
           
           % checks to make sure the number of layers is consistant between
           % datasets
           
           number_layers=check{1}.number_layers;
           
           for l=2:length(basename)
               
              if(number_layers ~= check{l}.number_layers)
                  
                  fprintf(1,'\nNumber of Layers are not consistant in %s.da\n\n', ...
                      basename{l});
              end
              
           end
           
           % sets the value array and plots the data
           
           for n=1:check{l}.number_layers
               
               % sets the value array to carry into the plot_data
               % subroutine
               
               for l=1:length(basename) 
                 value(l,1:len(l))=data{l}.alb(n,:);                  
               end
               
               % calls the plot_data subroutine and determines the
               % min,max,mean, and median values
               
               plot_data(basename,time,value, ...
                   ['Active Layer Distribution, Layer ' numstr(n)],...
                   ['ald_number_' numstr(n)],point_number,units{i}, ...
                   fid,start_time,stop_time,len);
               
           end
           
       % for an incorrect attribute entry    
           
       otherwise
           
           fprintf(1,'\nAttribute not recognized %s\n\n',test);

   end
   
end


