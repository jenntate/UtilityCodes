function interpolate_solution(new_rootname,old_rootname)
%
%
%
%   Subroutine to interpolate one da solution file to a different grid
%
%   Inputs:
%
%           new_rootname - this is the rootname of the grid (.3dm) to
%           interpolation the solution onto.
%
%           old_rootname - this is the rootname of the da and grid (.3dm)
%           that the solution is being interpolated from.
%
%   Outputs:
%
%           This code will create a new .da file (with the new_rootname)
%           that matches the new_rootname.3dm file
%
%   Example Usage:
%
%           interpolate_solution('New_Mobile_Grid','Old_Mobile_Solution')
%
%
%
%

tic

%% Reads the two grid files

old_grid=read_adh_grid([old_rootname '.3dm']);
new_grid=read_adh_grid([new_rootname '.3dm']);

%% Preallocates the arrays for the weighting/averaging

new_match_to_old=zeros(1,length(new_grid.x));
closest_nodes=zeros(length(new_grid.x),5);
distance=zeros(1,length(old_grid.x));
weights=zeros(length(new_grid.x),5);

%% Loops over the number of nodes in the new grid and gets the mapping

for i=1:length(new_grid.x)
    
     % matching node numbers between seperate meshes    
     
     found=0;
     x_list=find(old_grid.x==new_grid.x(i));
     if(~isempty(x_list))
       for j=1:length(x_list)
         if(old_grid.y(x_list(j))==new_grid.y(i))
            new_match_to_old(i)=x_list(j);
            found=1;
         end
       end
     end           
     
     % finds the closest 5 nodes and the weights associated with them
     
     if(found==0)
         for s=1:length(old_grid.x)
            distance(s)=((new_grid.x(i)-old_grid.x(s))^2 + ...
                (new_grid.y(i)-old_grid.y(s))^2)^0.5; 
         end
         min_dist=min(distance);
         closest_nodes(i,1)=find(distance==min_dist,1,'first');
         weights(i,1)=1.0/distance(closest_nodes(i,1));
         distance(closest_nodes(i,1))=9999999.99;
         min_dist=min(distance);
         closest_nodes(i,2)=find(distance==min_dist,1,'first');
         weights(i,2)=1.0/distance(closest_nodes(i,2));
         distance(closest_nodes(i,2))=9999999.99;
         min_dist=min(distance);
         closest_nodes(i,3)=find(distance==min_dist,1,'first');
         weights(i,3)=1.0/distance(closest_nodes(i,3));
         distance(closest_nodes(i,3))=9999999.99;
         min_dist=min(distance);
         closest_nodes(i,4)=find(distance==min_dist,1,'first');
         weights(i,4)=1.0/distance(closest_nodes(i,4));
         distance(closest_nodes(i,4))=9999999.99;
         min_dist=min(distance);
         closest_nodes(i,5)=find(distance==min_dist,1,'first');
         weights(i,5)=1.0/distance(closest_nodes(i,5));
         distance(closest_nodes(i,5))=9999999.99;
     end
end

%% clears the distance variable since it is now un-needed

clear distance

%% finds the parameters in the da file

check=load_da_stepnew([old_rootname '.da'], 0);

%% opens the new da file to be created

fid=fopen([new_rootname '.da'],'w');

%% write the da file header information (number of time steps, nodes, and
%  time step size)

fwrite(fid,check.nt,'integer*4');
fwrite(fid,check.np,'integer*4');
fwrite(fid,check.dt,'real*4');
fwrite(fid,check.dep_check,'integer*4');
fwrite(fid,check.wse_check,'integer*4');
fwrite(fid,check.vel_check,'integer*4');
fwrite(fid,check.err_check,'integer*4');
fwrite(fid,check.sal_check,'integer*4');
fwrite(fid,check.vor_check,'integer*4');
fwrite(fid,check.snd_check,'integer*4');
fwrite(fid,check.slt_check,'integer*4');
fwrite(fid,check.smr_check,'integer*4');
fwrite(fid,check.dpl_check,'integer*4');
fwrite(fid,check.bsh_check,'integer*4');
fwrite(fid,check.blt_check,'integer*4');
fwrite(fid,check.bld_check,'integer*4');
fwrite(fid,check.bed_check,'integer*4');
fwrite(fid,check.alt_check,'integer*4');
fwrite(fid,check.alb_check,'integer*4');
fwrite(fid,check.cbp_check,'integer*4');
fwrite(fid,check.number_layers,'integer*4');
fwrite(fid,check.number_sed,'integer*4');

%% Does the preallocating of arrays for the output variables

if(check.dep_check > 0 || check.wse_check > 0)
    wse=zeros(check.np,1);
end
if(check.vel_check > 0)
    vel_x=zeros(check.np,1);
    vel_y=zeros(check.np,1);
end
if(check.err_check > 0)
    err=zeros(check.np,1);
end
if(check.sal_check > 0)
    sal=zeros(check.np,1);
end
if(check.vor_check > 0)
    vor=zeros(check.np,1);
end
if(check.snd_check > 0)
    snd=zeros(check.np,1);
end
if(check.slt_check > 0)
    slt=zeros(check.np,1);
end
if(check.smr_check > 0)
    smr=zeros(check.np,1);
end
if(check.dpl_check > 0)
    dpl=zeros(check.np,1);
end
if(check.bsh_check > 0)
    bsh=zeros(check.np,1);
end
if(check.blt_check > 0)
    blt=zeros(check.np,1);
end
if(check.bld_check > 0)
    bld=zeros(check.np,1);
end
if(check.bed_check > 0)
    bed_x=zeros(check.np,1);
    bed_y=zeros(check.np,1);
end
if(check.alt_check > 0)
    alt=zeros(check.np,1);
end
if(check.alb_check > 0)
    alb=zeros(check.np,1);
end
if(check.cbp_check > 0)
    cbp_den=zeros(check.np,1);
    cbp_ces=zeros(check.np,1);
    cbp_erc=zeros(check.np,1);
    cbp_ere=zeros(check.np,1);
end

%% Loops over all time steps and determines the values for each nodes using the weights

for i=1:check.nt
    
    % reads the data for time step i
    
    data=load_da_stepnew([old_rootname '.da'], i);
    
    % writes the time associated with time step i
    
    fwrite(fid,data.time,'float64');
    
    % determines if the depth is included in the da file and if so
    % determines the appropriate values
    
    if(check.dep_check > 0)
       for y=1:length(new_grid.x)
          if(new_match_to_old(y) ~= 0)
             wse(y) = data.dep(new_match_to_old(y)) + ...
                old_grid.z(new_match_to_old(y)) - new_grid.z(y);            
          else
             wse1=old_grid.z(closest_nodes(y,1))+data.dep(closest_nodes(y,1));
             wse2=old_grid.z(closest_nodes(y,2))+data.dep(closest_nodes(y,2));
             wse3=old_grid.z(closest_nodes(y,3))+data.dep(closest_nodes(y,3));
             wse4=old_grid.z(closest_nodes(y,4))+data.dep(closest_nodes(y,4));
             wse5=old_grid.z(closest_nodes(y,5))+data.dep(closest_nodes(y,5));
             value1=(weights(y,1) * ...
                 wse1 +      ...
                 weights(y,2) *     ...
                 wse2 +      ...
                 weights(y,3) *     ...
                 wse3 +      ...
                 weights(y,4) *     ...
                 wse4 +      ...
                 weights(y,5) *     ...
                 wse5) /     ...
                 (weights(y,1) +    ...
                 weights(y,2) +     ...
                 weights(y,3) +     ...
                 weights(y,4) +     ...
                 weights(y,5));
             wse(y) = value1-new_grid.z(y);                  
          end
       end    
       fwrite(fid,wse(:),'float32');
    end
    
    % determines if the wse is included in the da file and if so determines
    % the appropriate values
    
    if(check.wse_check > 0)
       for y=1:length(new_grid.x)
          if(new_match_to_old(y) ~= 0)
             wse(y) = data.wse(new_match_to_old(y));
          else
             wse1=old_grid.z(closest_nodes(y,1))+data.wse(closest_nodes(y,1));
             wse2=old_grid.z(closest_nodes(y,2))+data.wse(closest_nodes(y,2));
             wse3=old_grid.z(closest_nodes(y,3))+data.wse(closest_nodes(y,3));
             wse4=old_grid.z(closest_nodes(y,4))+data.wse(closest_nodes(y,4));
             wse5=old_grid.z(closest_nodes(y,5))+data.wse(closest_nodes(y,5));
             wse(y)=(weights(y,1) * ...
                 wse1 +      ...
                 weights(y,2) *     ...
                 wse2 +      ...
                 weights(y,3) *     ...
                 wse3 +      ...
                 weights(y,4) *     ...
                 wse4 +      ...
                 weights(y,5) *     ...
                 wse5) /     ...
                 (weights(y,1) +    ...
                 weights(y,2) +     ...
                 weights(y,3) +     ...
                 weights(y,4) +     ...
                 weights(y,5));                  
          end
       end    
       fwrite(fid,wse(:),'float32');
    end    
    
    % determines if the velocity is included in the da file and if so
    % determines the appropriate values
    
    if(check.vel_check > 0)
       for y=1:length(new_grid.x)
          if(new_match_to_old(y) ~= 0)
              vel_x(y)=data.u(y);
              vel_y(y)=data.v(y);
          else
              vel_x(y)=(weights(y,1) * ...
                  data.u(closest_nodes(y,1)) +      ...
                  weights(y,2) *     ...
                  data.u(closest_nodes(y,2)) +      ...
                  weights(y,3) *     ...
                  data.u(closest_nodes(y,3)) +      ...
                  weights(y,4) *     ...
                  data.u(closest_nodes(y,4)) +      ...
                  weights(y,5) *     ...
                  data.u(closest_nodes(y,5))) /     ...
                  (weights(y,1) +    ...
                  weights(y,2) +     ...
                  weights(y,3) +     ...
                  weights(y,4) +     ...
                  weights(y,5));
              vel_y(y)=(weights(y,1) * ...
                  data.v(closest_nodes(y,2)) +      ...
                  weights(y,2) *     ...
                  data.v(closest_nodes(y,2)) +      ...
                  weights(y,3) *     ...
                  data.v(closest_nodes(y,2)) +      ...
                  weights(y,4) *     ...
                  data.v(closest_nodes(y,2)) +      ...
                  weights(y,5) *     ...
                  data.v(closest_nodes(y,2))) /     ...
                  (weights(y,1) +    ...
                  weights(y,2) +     ...
                  weights(y,3) +     ...
                  weights(y,4) +     ...
                  weights(y,5));
          end
       end
       fwrite(fid,vel_x(1,:),'float32');                %all u
       fwrite(fid,vel_y(2,:),'float32');                %all v
    end
    
    % determines if the error values are included in the da file and if so
    % determines the appropriate values
    
    if(check.err_check > 0)
       for y=1:length(new_grid.x)
          if(new_match_to_old(y) ~= 0)
             err(y) = data.err(new_match_to_old(y));
          else
             err(y)=(weights(y,1) * ...
                 data.err(closest_nodes(y,1)) +      ...
                 weights(y,2) *     ...
                 data.err(closest_nodes(y,2)) +      ...
                 weights(y,3) *     ...
                 data.err(closest_nodes(y,3)) +      ...
                 weights(y,4) *     ...
                 data.err(closest_nodes(y,4)) +      ...
                 weights(y,5) *     ...
                 data.err(closest_nodes(y,5))) /     ...
                 (weights(y,1) +    ...
                 weights(y,2) +     ...
                 weights(y,3) +     ...
                 weights(y,4) +     ...
                 weights(y,5));                  
          end
       end    
       fwrite(fid,err(:),'float32');
    end     
    
    % determines if the salinity values are included in the da file and if
    % so determines the appropriate values
    
    if(check.sal_check > 0)
       for y=1:length(new_grid.x)
          if(new_match_to_old(y) ~= 0)
             sal(y) = data.sal(new_match_to_old(y));            
          else
             sal(y)=(weights(y,1) * ...
                 data.sal(closest_nodes(y,1)) +      ...
                 weights(y,2) *     ...
                 data.sal(closest_nodes(y,2)) +      ...
                 weights(y,3) *     ...
                 data.sal(closest_nodes(y,3)) +      ...
                 weights(y,4) *     ...
                 data.sal(closest_nodes(y,4)) +      ...
                 weights(y,5) *     ...
                 data.sal(closest_nodes(y,5))) /     ...
                 (weights(y,1) +    ...
                 weights(y,2) +     ...
                 weights(y,3) +     ...
                 weights(y,4) +     ...
                 weights(y,5));                  
          end
       end    
       fwrite(fid,sal(:),'float32');
    end  
    
    % determines if the vorticity values are included in the da file and if
    % so determines the appropriate values
    
    if(check.vor_check > 0)
       for y=1:length(new_grid.x)
          if(new_match_to_old(y) ~= 0)
             vor(y) = data.vor(new_match_to_old(y));
          else
             vor(y)=(weights(y,1) * ...
                 data.vor(closest_nodes(y,1)) +      ...
                 weights(y,2) *     ...
                 data.vor(closest_nodes(y,2)) +      ...
                 weights(y,3) *     ...
                 data.vor(closest_nodes(y,3)) +      ...
                 weights(y,4) *     ...
                 data.vor(closest_nodes(y,4)) +      ...
                 weights(y,5) *     ...
                 data.vor(closest_nodes(y,5))) /     ...
                 (weights(y,1) +    ...
                 weights(y,2) +     ...
                 weights(y,3) +     ...
                 weights(y,4) +     ...
                 weights(y,5));                  
          end
       end    
       fwrite(fid,vor(:),'float32');
    end
    
    % determines if the sand concentrations are included in the da file and
    % if so determines the appropriate values
    
    if(check.snd_check > 0)
      for e=1:check.snd_check
         for y=1:length(new_grid.x)
            if(new_match_to_old(y) ~= 0)
               snd(y) = data.snd(e,new_match_to_old(y));
            else
               snd(y)=(weights(y,1) * ...
                   data.snd(e,closest_nodes(y,1)) +      ...
                   weights(y,2) *     ...
                   data.snd(e,closest_nodes(y,2)) +      ...
                   weights(y,3) *     ...
                   data.snd(e,closest_nodes(y,3)) +      ...
                   weights(y,4) *     ...
                   data.snd(e,closest_nodes(y,4)) +      ...
                   weights(y,5) *     ...
                   data.snd(e,closest_nodes(y,5))) /     ...
                   (weights(y,1) +    ...
                   weights(y,2) +     ...
                   weights(y,3) +     ...
                   weights(y,4) +     ...
                   weights(y,5));                  
            end
         end    
         fwrite(fid,snd(:),'float32');
      end
    end
    
    % determines if the silt concentrations are included in the da file and
    % if so determines the appropriate values
    
    if(check.slt_check > 0)
      for e=1:check.slt_check
         for y=1:length(new_grid.x)
            if(new_match_to_old(y) ~= 0)
               slt(y) = data.slt(e,new_match_to_old(y));
            else
               slt(y)=(weights(y,1) * ...
                   data.slt(e,closest_nodes(y,1)) +      ...
                   weights(y,2) *     ...
                   data.slt(e,closest_nodes(y,2)) +      ...
                   weights(y,3) *     ...
                   data.slt(e,closest_nodes(y,3)) +      ...
                   weights(y,4) *     ...
                   data.slt(e,closest_nodes(y,4)) +      ...
                   weights(y,5) *     ...
                   data.slt(e,closest_nodes(y,5))) /     ...
                   (weights(y,1) +    ...
                   weights(y,2) +     ...
                   weights(y,3) +     ...
                   weights(y,4) +     ...
                   weights(y,5));                  
            end
         end    
         fwrite(fid,slt(:),'float32');
      end
    end    
    
    % determines if the sediment mass residual is included in the da file
    % and if so determines the appropriate values
    
    if(check.smr_check > 0)
      for e=1:check.number_sed   
         for y=1:length(new_grid.x)
            if(new_match_to_old(y) ~= 0)
               smr(y) = data.smr(e,new_match_to_old(y));
            else
               smr(y)=(weights(y,1) * ...
                   data.smr(e,closest_nodes(y,1)) +      ...
                   weights(y,2) *     ...
                   data.smr(e,closest_nodes(y,2)) +      ...
                   weights(y,3) *     ...
                   data.smr(e,closest_nodes(y,3)) +      ...
                   weights(y,4) *     ...
                   data.smr(e,closest_nodes(y,4)) +      ...
                   weights(y,5) *     ...
                   data.smr(e,closest_nodes(y,5))) /     ...
                   (weights(y,1) +    ...
                   weights(y,2) +     ...
                   weights(y,3) +     ...
                   weights(y,4) +     ...
                   weights(y,5));                  
            end
         end    
         fwrite(fid,smr(:),'float32');
      end
    end    
    
    % determines if the displacement is include in the da file and if so
    % determines the appropriate values
    
    if(check.dpl_check > 0)
       for y=1:length(new_grid.x)
          if(new_match_to_old(y) ~= 0)
             dpl(y) = data.dpl(new_match_to_old(y));
          else
             dpl(y)=(weights(y,1) * ...
                 data.dpl(closest_nodes(y,1)) +      ...
                 weights(y,2) *     ...
                 data.dpl(closest_nodes(y,2)) +      ...
                 weights(y,3) *     ...
                 data.dpl(closest_nodes(y,3)) +      ...
                 weights(y,4) *     ...
                 data.dpl(closest_nodes(y,4)) +      ...
                 weights(y,5) *     ...
                 data.dpl(closest_nodes(y,5))) /     ...
                 (weights(y,1) +    ...
                 weights(y,2) +     ...
                 weights(y,3) +     ...
                 weights(y,4) +     ...
                 weights(y,5));                  
          end
       end    
       fwrite(fid,dpl(:),'float32');
    end     

    % determines if the bed shear is include in the da file and if so
    % determines the appropriate values
    
    if(check.bsh_check > 0)
       for y=1:length(new_grid.x)
          if(new_match_to_old(y) ~= 0)
             bsh(y) = data.bsh(new_match_to_old(y));
          else
             bsh(y)=(weights(y,1) * ...
                 data.bsh(closest_nodes(y,1)) +      ...
                 weights(y,2) *     ...
                 data.bsh(closest_nodes(y,2)) +      ...
                 weights(y,3) *     ...
                 data.bsh(closest_nodes(y,3)) +      ...
                 weights(y,4) *     ...
                 data.bsh(closest_nodes(y,4)) +      ...
                 weights(y,5) *     ...
                 data.bsh(closest_nodes(y,5))) /     ...
                 (weights(y,1) +    ...
                 weights(y,2) +     ...
                 weights(y,3) +     ...
                 weights(y,4) +     ...
                 weights(y,5));                  
          end
       end    
       fwrite(fid,bsh(:),'float32');
    end
    
    % determines if the bed layer thickness is in the da file and if so
    % determines the appropriate values
    
    if(check.blt_check > 0)
      for e=1:check.blt_check
         for y=1:length(new_grid.x)
            if(new_match_to_old(y) ~= 0)
               blt(y) = data.blt(e,new_match_to_old(y));
            else
               blt(y)=(weights(y,1) * ...
                   data.blt(e,closest_nodes(y,1)) +      ...
                   weights(y,2) *     ...
                   data.blt(e,closest_nodes(y,2)) +      ...
                   weights(y,3) *     ...
                   data.blt(e,closest_nodes(y,3)) +      ...
                   weights(y,4) *     ...
                   data.blt(e,closest_nodes(y,4)) +      ...
                   weights(y,5) *     ...
                   data.blt(e,closest_nodes(y,5))) /     ...
                   (weights(y,1) +    ...
                   weights(y,2) +     ...
                   weights(y,3) +     ...
                   weights(y,4) +     ...
                   weights(y,5));                  
            end
         end    
         fwrite(fid,blt(:),'float32');
      end
    end    
    
    % determines if the bed layer distribution is in the da file and if so
    % determines the appropriate values
    
    if(check.bld_check > 0)
      for h=1:check.bld_check
        for j=1:number_sed
            if(new_match_to_old(y) ~= 0)
               bld(y) = data.bld(h,j,new_match_to_old(y));
            else
               bld(y)=(weights(y,1) * ...
                   data.bld(h,j,closest_nodes(y,1)) +      ...
                   weights(y,2) *     ...
                   data.bld(h,j,closest_nodes(y,2)) +      ...
                   weights(y,3) *     ...
                   data.bld(h,j,closest_nodes(y,3)) +      ...
                   weights(y,4) *     ...
                   data.bld(h,j,closest_nodes(y,4)) +      ...
                   weights(y,5) *     ...
                   data.bld(h,j,closest_nodes(y,5))) /     ...
                   (weights(y,1) +    ...
                   weights(y,2) +     ...
                   weights(y,3) +     ...
                   weights(y,4) +     ...
                   weights(y,5));                  
            end
            fwrite(fid,bld(:),'float32');
        end
      end
    end

    % determines if the bed vector information is in the da file and if so
    % determines the appropriate values
    
    if(check.bed_check > 0)
       for y=1:length(new_grid.x)
          if(new_match_to_old(y) ~= 0)
              bed_x(y)=data.u(y);
              bed_y(y)=data.v(y);
          else
              bed_x(y)=(weights(y,1) * ...
                  data.bed_u(closest_nodes(y,1)) +      ...
                  weights(y,2) *     ...
                  data.bed_u(closest_nodes(y,2)) +      ...
                  weights(y,3) *     ...
                  data.bed_u(closest_nodes(y,3)) +      ...
                  weights(y,4) *     ...
                  data.bed_u(closest_nodes(y,4)) +      ...
                  weights(y,5) *     ...
                  data.bed_u(closest_nodes(y,5))) /     ...
                  (weights(y,1) +    ...
                  weights(y,2) +     ...
                  weights(y,3) +     ...
                  weights(y,4) +     ...
                  weights(y,5));
              bed_y(y)=(weights(y,1) * ...
                  data.bed_v(closest_nodes(y,2)) +      ...
                  weights(y,2) *     ...
                  data.bed_v(closest_nodes(y,2)) +      ...
                  weights(y,3) *     ...
                  data.bed_v(closest_nodes(y,2)) +      ...
                  weights(y,4) *     ...
                  data.bed_v(closest_nodes(y,2)) +      ...
                  weights(y,5) *     ...
                  data.bed_v(closest_nodes(y,2))) /     ...
                  (weights(y,1) +    ...
                  weights(y,2) +     ...
                  weights(y,3) +     ...
                  weights(y,4) +     ...
                  weights(y,5));
          end
       end
       fwrite(fid,bed_x(1,:),'float32');                %all u
       fwrite(fid,bed_y(2,:),'float32');                %all v
    end    
    
    % determines if the active layer thickness is included in the da file
    % and if so determines the appropriate values
    
    if(check.alt_check > 0)
       for y=1:length(new_grid.x)
          if(new_match_to_old(y) ~= 0)
             alt(y) = data.alt(new_match_to_old(y));
          else
             alt(y)=(weights(y,1) * ...
                 data.alt(closest_nodes(y,1)) +      ...
                 weights(y,2) *     ...
                 data.alt(closest_nodes(y,2)) +      ...
                 weights(y,3) *     ...
                 data.alt(closest_nodes(y,3)) +      ...
                 weights(y,4) *     ...
                 data.alt(closest_nodes(y,4)) +      ...
                 weights(y,5) *     ...
                 data.alt(closest_nodes(y,5))) /     ...
                 (weights(y,1) +    ...
                 weights(y,2) +     ...
                 weights(y,3) +     ...
                 weights(y,4) +     ...
                 weights(y,5));                  
          end
       end    
       fwrite(fid,alt(:),'float32');
    end     
    
    % determines if the active layer distribution is included in the da
    % file and if so determines the appropriate values
    
    if(check.alb_check > 0)
      for e=1:check.number_sed   
         for y=1:length(new_grid.x)
            if(new_match_to_old(y) ~= 0)
               alb(y) = data.ald(e,new_match_to_old(y));
            else
               alb(y)=(weights(y,1) * ...
                   data.alb(e,closest_nodes(y,1)) +      ...
                   weights(y,2) *     ...
                   data.alb(e,closest_nodes(y,2)) +      ...
                   weights(y,3) *     ...
                   data.alb(e,closest_nodes(y,3)) +      ...
                   weights(y,4) *     ...
                   data.alb(e,closest_nodes(y,4)) +      ...
                   weights(y,5) *     ...
                   data.alb(e,closest_nodes(y,5))) /     ...
                   (weights(y,1) +    ...
                   weights(y,2) +     ...
                   weights(y,3) +     ...
                   weights(y,4) +     ...
                   weights(y,5));                  
            end
         end    
         fwrite(fid,alb(:),'float32');
      end
    end      
    
    % determines if the cohesive bed properties are included in the da file
    % and if so determines the appropriate values
    
    if(check.cbp_check > 0)
      for e=1:check.cbp_check   
         for y=1:length(new_grid.x)
            if(new_match_to_old(y) ~= 0)
               cbp_den(y) = data.cbp_den(e,new_match_to_old(y));
               cbp_ces(y) = data.cbp_ces(e,new_match_to_old(y));
               cbp_erc(y) = data.cbp_erc(e,new_match_to_old(y));
               cbp_ere(y) = data.cbp_ere(e,new_match_to_old(y));
            else
               cbp_den(y)=(weights(y,1) * ...
                   data.cbp_den(e,closest_nodes(y,1)) +      ...
                   weights(y,2) *     ...
                   data.cbp_den(e,closest_nodes(y,2)) +      ...
                   weights(y,3) *     ...
                   data.cbp_den(e,closest_nodes(y,3)) +      ...
                   weights(y,4) *     ...
                   data.cbp_den(e,closest_nodes(y,4)) +      ...
                   weights(y,5) *     ...
                   data.cbp_den(e,closest_nodes(y,5))) /     ...
                   (weights(y,1) +    ...
                   weights(y,2) +     ...
                   weights(y,3) +     ...
                   weights(y,4) +     ...
                   weights(y,5));       
               cbp_ces(y)=(weights(y,1) * ...
                   data.cbp_ces(e,closest_nodes(y,1)) +      ...
                   weights(y,2) *     ...
                   data.cbp_ces(e,closest_nodes(y,2)) +      ...
                   weights(y,3) *     ...
                   data.cbp_ces(e,closest_nodes(y,3)) +      ...
                   weights(y,4) *     ...
                   data.cbp_ces(e,closest_nodes(y,4)) +      ...
                   weights(y,5) *     ...
                   data.cbp_ces(e,closest_nodes(y,5))) /     ...
                   (weights(y,1) +    ...
                   weights(y,2) +     ...
                   weights(y,3) +     ...
                   weights(y,4) +     ...
                   weights(y,5)); 
               cbp_erc(y)=(weights(y,1) * ...
                   data.cbp_erc(e,closest_nodes(y,1)) +      ...
                   weights(y,2) *     ...
                   data.cbp_erc(e,closest_nodes(y,2)) +      ...
                   weights(y,3) *     ...
                   data.cbp_erc(e,closest_nodes(y,3)) +      ...
                   weights(y,4) *     ...
                   data.cbp_erc(e,closest_nodes(y,4)) +      ...
                   weights(y,5) *     ...
                   data.cbp_erc(e,closest_nodes(y,5))) /     ...
                   (weights(y,1) +    ...
                   weights(y,2) +     ...
                   weights(y,3) +     ...
                   weights(y,4) +     ...
                   weights(y,5));
               cbp_ere(y)=(weights(y,1) * ...
                   data.cbp_ere(e,closest_nodes(y,1)) +      ...
                   weights(y,2) *     ...
                   data.cbp_ere(e,closest_nodes(y,2)) +      ...
                   weights(y,3) *     ...
                   data.cbp_ere(e,closest_nodes(y,3)) +      ...
                   weights(y,4) *     ...
                   data.cbp_ere(e,closest_nodes(y,4)) +      ...
                   weights(y,5) *     ...
                   data.cbp_ere(e,closest_nodes(y,5))) /     ...
                   (weights(y,1) +    ...
                   weights(y,2) +     ...
                   weights(y,3) +     ...
                   weights(y,4) +     ...
                   weights(y,5));               
            end
         end    
         fwrite(fid,cbp_den(:),'float32');
         fwrite(fid,cbp_ces(:),'float32');
         fwrite(fid,cbp_erc(:),'float32');
         fwrite(fid,cbp_ere(:),'float32');
      end
    end      
    
end

fprintf(1,'FINISHED WRITING da FILE\n');

%% close the da file and clear all variables from memory

fclose(fid);

clear all;

toc








