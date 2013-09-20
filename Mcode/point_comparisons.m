function point_comparisons(basename,x,y,units,attribute,start_time,stop_time)
%
%      Inputs
%
%         basename - basename for the model simulations
%
%         x - array of the x coordinates for the comparison locations
%
%         y - array of the y coordinates for the comparison locations
%
%         units - string including the units for the y axis
%
%         attributes - string of attributes to plot
%                      These are:
%
%                      dep = depth solution
%                      wse = water surface elevation solution
%                      vel = velocity vector and magnitude solution
%                      sal = salinity solution
%                      vor = vorticity solution
%                      snd = sand concentration solution
%                      slt = silt concentration solution
%                      err = error solution
%                      dpl = bed displacement solution
%                      alt = active layer thickness
%                      ald = active layer distribution
%                      blt = bed layer thickness
%                      bld = bed layer distribution
%                      cbp = cohesive bed properties
%                      bsh = bed shear stress solution
%                      bed = bedload vector and magnitude solution
%                      smr = sediment mass residual
%
%
%      Outputs
%
%        Time series plots at the given x,y locations of the attributes
%        requested.
%
%

%% Preallocation of arrays

node=zeros(length(basename),length(x));

%% Reads the grids and finds the closest node to each location 

for i=1:length(basename)
    
  % read the adh meshes
    
  grid=read_adh_grid([basename{i} '.3dm']);
  
  % triangulate the x,y dataset
  
  dt=DelaunayTri(grid.x(:),grid.y(:));
  
  % find the nearest node for each x,y location
  
  parfor j=1:length(x)
    node(i,j)=nearestNeighbor(dt,[x(j) y(j)]);      
  end
end

clear dt grid y

data=cell(length(basename),length(x));
temp1=cell(length(basename),1);

%% Opens the output.out file to list the max, min, mean, and median values

fid=fopen('output.out','w');

%% reads the time series data for each nodal location and plots the data by calling the plot_comparisons subroutine
limit=2;
if(length(x) < limit)
  fprintf(1,'There are less than %i points, using load_da_time_series routine\n',limit);
  for i=1:length(x)
     
    % reads the time series data for each node from each solution set  
    
    parfor j=1:length(basename)
      fprintf(1,'Reading Point %i from file %s.da\n',i,basename{j});
      data{j}=load_da_time_series([basename{j} '.da'],node(j,i));
    end
  
    % plots the time series data for the given location and attribute
  
    plot_comparisons(basename,data,attribute,units,i,fid,start_time,stop_time)
  
  end

else
  fprintf(1,'There are more than %i points, using the load_da_stepnew routine\n',limit-1);  
  for j=1:length(basename)
    fprintf(1,'Reading data from file %s.da\n',basename{j});
    value=load_da_stepnew([basename{j} '.da'],0);
    value1=load_da_stepnew([basename{j}, '.da'],1);
    if(value1.time < datenum(start_time))
        test=load_da_time_series([basename{j} '.da'],1);
        start_read=find(test.time>=datenum(start_time),1,'first');
        fprintf(1,'Starting read at time step %i and time %s\n',start_read,...
          datestr(test.time(start_read)));
    else
        start_read=1;

    end
    for s=1:length(x)
      data{j,s}.nt=value.nt;
    end

    for k=start_read:value.nt

       temp=load_da_stepnew([basename{j} '.da'],k);

       if(k==start_read)
         fprintf(1,'Starting read the the beginning of %s da file at time %s\n',...
            basename{j},datestr(temp.time)); 
       elseif(k==(value.nt-start_read)/5.0)
         fprintf(1,'Reading time step number %i and time %s\n',...
             k,datestr(temp.time));
       elseif(k==(value.nt-start_read)/4.0)
         fprintf(1,'Reading time step number %i and time %s\n',...
             k,datestr(temp.time));
       elseif(k==(value.nt-start_read)/3.0)
         fprintf(1,'Reading time step number %i and time %s\n',...
             k,datestr(temp.time));
       elseif(k==(value.nt-start_read)/2.0)
         fprintf(1,'Reading time step number %i and time %s\n',...
             k,datestr(temp.time));
       elseif(k==(value.nt-start_read)/1.0)
         fprintf(1,'Reading last time step number %i and time %s\n',...
             k,datestr(temp.time));         
       end
           
       if(temp.time >= datenum(start_time))
         for s=1:length(x)
           data{j,s}.time(k)=temp.time;
         end
         for b=1:length(x)
            if(value.sal_check>0)
              data{j,b}.sal(k)=temp.sal(node(j,b));
            end
            if(value.wse_check>0)
              data{j,b}.wse(k)=temp.wse(node(j,b));
            end
            if(value.dep_check>0)
              data{j,b}.dep(k)=temp.dep(node(j,b));
            end
            if(value.vel_check>0)
              data{j,b}.u(k)=temp.u(node(j,b));
              data{j,b}.v(k)=temp.v(node(j,b));
            end
            if(value.err_check>0)
              data{j,b}.err(k)=temp.err(node(j,b));
            end
            if(value.vor_check>0)
              data{j,b}.vor(k)=temp.vor(node(j,b));
            end
            if(value.bsh_check>0)
              data{j,b}.bsh(k)=temp.bsh(node(j,b));
            end
            if(value.dpl_check>0)
              data{j,b}.dpl(k)=temp.dpl(node(j,b));
            end
            if(value.bed_check>0)
              data{j,b}.bed_u(k)=temp.bed_u(node(j,b));
              data{j,b}.bed_v(k)=temp.bed_v(node(j,b));
            end
            if(value.snd_check>0)
              data{j,b}.snd(:,k)=temp.snd(:,node(j,b));
              data{j,b}.snd_rouse(:,k)=temp.snd_rouse(:,node(j,b));
              data{j,b}.snd_bdma(:,k)=temp.snd_bdma(:,node(j,b));
            end
            if(value.slt_check>0)
              data{j,b}.slt(:,k)=temp.slt(:,node(j,b));
              data{j,b}.slt_rouse(:,k)=temp.slt_rouse(:,node(j,b));
              data{j,b}.slt_bdma(:,k)=temp.slt_bdma(:,node(j,b));
            end
            if(value.smr_check>0)
              data{j,b}.smr(:,k)=temp.smr(:,node(j,b));
            end
            if(value.blt_check>0)
              data{j,b}.blt(:,k)=temp.blt(:,node(j,b));
            end
            if(value.alt_check>0)
              data{j,b}.alt(k)=temp.alt(node(j,b));
            end
            if(value.alb_check>0)
              data{j,b}.alb(:,k)=temp.alb(:,node(j,b));
            end
            if(value.cbp_check>0)
              data{j,b}.cbp_den(:,k)=temp.cbp_den(:,node(j,b));
              data{j,b}.cbp_ces(:,k)=temp.cbp_ces(:,node(j,b));
              data{j,b}.cbp_erc(:,k)=temp.cbp_erc(:,node(j,b));
              data{j,b}.cbp_ere(:,k)=temp.cbp_ere(:,node(j,b));
            end
            if(value.bld_check>0)
              for i=1:bld_check
                for u=1:value.number_sed
                  data{j,b}.bld(i,u,:)=temp.bld(i,u,node(j,b));
                end
              end              
            end

         end
         if(temp.time > datenum(stop_time))
             fprintf(1,'Finished reading %s file\n',basename{j});
             break;
         end         
       end
    end
  end
  
  clear temp
  
  % plots the time series data for the given location and attribute
  for i=1:length(x)
    for j=1:length(basename)        
      temp1{j}=data{j,i};
    end
    plot_comparisons(basename,temp1,attribute,units,i,fid,start_time,stop_time)    
  end
  
end

%% close the output.out file with the max, min, mean, and median values

fclose(fid);







