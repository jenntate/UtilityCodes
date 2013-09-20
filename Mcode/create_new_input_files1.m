function create_new_input_files1()
%
%
%
%   subroutine to take a old grid, bc file, and hotstart file and
%   interpolate those to a new-modified grid.
%
%   input parameters:
%
%   old_base_file_name - basename of the old grid, bc file, and hotstart
%   file
%
%   new_base_file_name - basename of the new grid.  The bc file and hostart
%   file created will have this same basename
%
%   example usage:
%
%   create_new_input_files('MTOG_Jan25','MTOG_Jan26')
%
%


tic

[fname, pname]=uigetfile('*.bc','Select the original boundary condition file');
if isstr(fname),a.bc=fullfile(pname,fname);end
[fname, pname]=uigetfile('*.3dm','Select the original mesh file');
if isstr(fname),a.dm=fullfile(pname,fname);end
[fname, pname]=uigetfile('*.hot','Select the original hotstart file');
if isstr(fname),a.hot=fullfile(pname,fname);end
[fname, pname]=uigetfile('*.3dm','Select the new mesh file');
if isstr(fname),b.dm=fullfile(pname,fname);end
b.bc=fullfile(pname,'.bc');
b.hot=fullfile(pname,'.hot');

fid_in_bc=fopen(a.bc,'r');
fid_in_hot=fopen(a.hot,'r');

if(fid_in_bc > 0)
    fprintf(1,'Input BC file exists\n');
else
    fprintf(1,'Input BC file does not exist\n');
    stop
end

if(fid_in_hot > 0)
    fprintf(1,'Input HOT file exists\n');
else
    fprintf(1,'Input HOT file does not exist\n');
    stop
end

fid_out_bc=fopen(b.bc,'w');
fid_out_hot=fopen(b.hot,'w');

old_grid=read_adh_grid(a.dm);
new_grid=read_adh_grid(b.dm);
if(length(new_grid.x) > length(old_grid.x))
    fprintf(1,'Number of nodes has increased from %i to %i\n',...
        length(old_grid.x),length(new_grid.x));
else
    fprintf(1,'Number of nodes has decreased from %i to %i\n',...
        length(old_grid.x),length(new_grid.x));
end
new_dt=DelaunayTri(new_grid.x(:),new_grid.y(:));
%old_dt=DelaunayTri(old_grid.x(:),old_grid.y(:));

%% Creating New BC File

while ~feof(fid_in_bc)
    hold=fgetl(fid_in_bc);
    card=sscanf(hold,'%s',1);
    if(isequal(card,'END'))
      break;
    end
    
    % NDS
    
    if(isequal(card,'NDS'))
      node=sscanf(hold,'%*s %i',1);
      string_number=sscanf(hold,'%*s %*i %i',1);
      k=nearestNeighbor(new_dt,[old_grid.x(node) old_grid.y(node)]);
      if(old_grid.x(node)==new_grid.x(k) && old_grid.y(node)==new_grid.y(k))
         if(node~=k)
           fprintf(1,'Old Node Number %i is New Node Number %i\n',node,k);
         end
         fprintf(fid_out_bc,'NDS %i %i\n',k,string_number);
      else
         dist=(((old_grid.x(node)-new_grid.x(k))^2+ ...
             (old_grid.x(node)-new_grid.x(k))^2))^0.5;
         fprintf(1,'Old Node Number %i is New Node Number %i\n',node,k);
         fprintf(1,'New Node is %f from old nodal location\n',dist);
         fprintf(fid_out_bc,'NDS %i %i\n',k,string_number);         
      end
      
      % EGS
      
    elseif(isequal(card,'EGS'))
      node1=sscanf(hold,'%*s %i',1);
      node2=sscanf(hold,'%*s %*i %i',1);
      string_number=sscanf(hold,'%*s %*i %*i %i',1);
      k=nearestNeighbor(new_dt,[old_grid.x(node1) old_grid.y(node1)]);
      j=nearestNeighbor(new_dt,[old_grid.x(node2) old_grid.y(node2)]);
      
      if(old_grid.x(node1)==new_grid.x(k) && old_grid.y(node1)==new_grid.y(k))
         if(node1~=k)
           fprintf(1,'Old Node Number %i is New Node Number %i\n',node1,k);
         end
      else
         dist=(((old_grid.x(node1)-new_grid.x(k))^2+ ...
             (old_grid.x(node1)-new_grid.x(k))^2))^0.5;
         fprintf(1,'Old Node Number %i is New Node Number %i\n',node1,k);
         fprintf(1,'New Node is %f from old nodal location\n',dist);     
      end
      if(old_grid.x(node2)==new_grid.x(j) && old_grid.y(node2)==new_grid.y(j))
         if(node2~=j)
           fprintf(1,'Old Node Number %i is New Node Number %i\n',node2,j);
         end
      else
         dist=(((old_grid.x(node2)-new_grid.x(j))^2+ ...
             (old_grid.x(node2)-new_grid.x(j))^2))^0.5;
         fprintf(1,'Old Node Number %i is New Node Number %i\n',node2,j);
         fprintf(1,'New Node is %f from old nodal location\n',dist);
      end
      
      fprintf(fid_out_bc,'EGS %i %i %i\n',k,j,string_number);
      
      % MDS
      
    elseif(isequal(card,'MDS'))        
      node1=sscanf(hold,'%*s %i',1);
      node2=sscanf(hold,'%*s %*i %i',1);
      string_number=sscanf(hold,'%*s %*i %*i %i',1);
      k=nearestNeighbor(new_dt,[old_grid.x(node1) old_grid.y(node1)]);
      j=nearestNeighbor(new_dt,[old_grid.x(node2) old_grid.y(node2)]);
      
      if(old_grid.x(node1)==new_grid.x(k) && old_grid.y(node1)==new_grid.y(k))
         if(node1~=k)
           fprintf(1,'Old Node Number %i is New Node Number %i\n',node1,k);
         end
      else
         dist=(((old_grid.x(node1)-new_grid.x(k))^2+ ...
             (old_grid.x(node1)-new_grid.x(k))^2))^0.5;
         fprintf(1,'Old Node Number %i is New Node Number %i\n',node1,k);
         fprintf(1,'New Node is %f from old nodal location\n',dist);     
      end
      if(old_grid.x(node2)==new_grid.x(j) && old_grid.y(node2)==new_grid.y(j))
         if(node2~=j)
           fprintf(1,'Old Node Number %i is New Node Number %i\n',node2,j);
         end
      else
         dist=(((old_grid.x(node2)-new_grid.x(j))^2+ ...
             (old_grid.x(node2)-new_grid.x(j))^2))^0.5;
         fprintf(1,'Old Node Number %i is New Node Number %i\n',node2,j);
         fprintf(1,'New Node is %f from old nodal location\n',dist);
      end
      fprintf(fid_out_bc,'MDS %i %i %i\n',k,j,string_number);             
    else
      fprintf(fid_out_bc,'%s\n',hold);        
    end   
end
fprintf(fid_out_bc,'END');

%% Creating New Hotstart File

new_match_to_old=zeros(length(new_grid.x),1);
closest_nodes=zeros(length(new_grid.x),5);
distance=zeros(length(old_grid.x),1);
weights=zeros(length(new_grid.x),5);

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
     
     if(found==0)
%         k=nearestNeighbor(old_dt,[new_grid.x(i) new_grid.y(i)]);
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
     
     % reading hotstart file and creating new file
    
    while ~feof(fid_in_hot)
       
       for m=1:20
       
         while(m>0)
           hold=fgetl(fid_in_hot);  
           if(feof(fid_in_hot))
               break;
           end
           card=sscanf(hold,'%s',1);
           if(isequal(card,'BEGVEC'))
              type='vector';
              fprintf(fid_out_hot,'%s\n',hold);
           elseif(isequal(card,'BEGSCL'))
              type='scalar';
              fprintf(fid_out_hot,'%s\n',hold);
           elseif(isequal(card,'ND'))
              fprintf(fid_out_hot,'%s %i\n',card,length(new_grid.x));
           elseif(isequal(card,'NC'))
              fprintf(fid_out_hot,'%s %i\n',card,length(new_grid.ncon(:,1)));
           elseif(isequal(card,'NAME'))
              parameter=sscanf(hold,'%*s %s',1);
              fprintf(fid_out_hot,'%s\n',hold);
           elseif(isequal(card,'TS'))
              fprintf(fid_out_hot,'%s\n',hold);
              break;
           else
              fprintf(fid_out_hot,'%s\n',hold);
           end
         end
         
         if(strcmp(type,'vector'))
             data=textscan(fid_in_hot,'%f %f %f',length(old_grid.x));
             for y=1:length(new_grid.x)
                if(new_match_to_old(y) ~= 0)
                  fprintf(fid_out_hot,'%15.8e %15.8e %15.8e\n',...
                      data{1}(new_match_to_old(y)), ...
                      data{2}(new_match_to_old(y)), ...
                      data{3}(new_match_to_old(y)));
                else
                  value1=(weights(y,1) * ...
                      data{1}(closest_nodes(y,1)) +      ...
                      weights(y,2) *     ...
                      data{1}(closest_nodes(y,2)) +      ...
                      weights(y,3) *     ...
                      data{1}(closest_nodes(y,3)) +      ...
                      weights(y,4) *     ...
                      data{1}(closest_nodes(y,4)) +      ...
                      weights(y,5) *     ...
                      data{1}(closest_nodes(y,5))) /     ...
                      (weights(y,1) +    ...
                      weights(y,2) +     ...
                      weights(y,3) +     ...
                      weights(y,4) +     ...
                      weights(y,5));
                  value2=(weights(y,1) * ...
                      data{2}(closest_nodes(y,2)) +      ...
                      weights(y,2) *     ...
                      data{2}(closest_nodes(y,2)) +      ...
                      weights(y,3) *     ...
                      data{2}(closest_nodes(y,2)) +      ...
                      weights(y,4) *     ...
                      data{2}(closest_nodes(y,2)) +      ...
                      weights(y,5) *     ...
                      data{2}(closest_nodes(y,2))) /     ...
                      (weights(y,1) +    ...
                      weights(y,2) +     ...
                      weights(y,3) +     ...
                      weights(y,4) +     ...
                      weights(y,5));
                  value3=(weights(y,1) * ...
                      data{3}(closest_nodes(y,3)) +      ...
                      weights(y,2) *     ...
                      data{3}(closest_nodes(y,3)) +      ...
                      weights(y,3) *     ...
                      data{3}(closest_nodes(y,3)) +      ...
                      weights(y,4) *     ...
                      data{3}(closest_nodes(y,3)) +      ...
                      weights(y,5) *     ...
                      data{3}(closest_nodes(y,3))) /     ...
                      (weights(y,1) +    ...
                      weights(y,2) +     ...
                      weights(y,3) +     ...
                      weights(y,4) +     ...
                      weights(y,5));                  
                  fprintf(fid_out_hot,'%15.8e %15.8e %15.8e\n',...
                      value1, ...
                      value2, ...
                      value3);  
                end
             end             
         elseif(strcmp(type,'scalar'))
             data=textscan(fid_in_hot,'%f',length(old_grid.x));
             if(isequal(parameter, 'ioh'))
%               wse=zeros(1,length(old_grid.z));
%               for w=1:length(old_grid.x)
%                 wse(w)=data{1}(w)+old_grid.z(w); 
%               end

               for y=1:length(new_grid.x)
                 if(new_match_to_old(y) ~= 0)
                      wse = data{1}(new_match_to_old(y)) + ...
                          old_grid.z(new_match_to_old(y)) - new_grid.z(y);
             %         wse(new_match_to_old(y)) = ...
             %             wse(new_match_to_old(y))-new_grid.z(y);
                    fprintf(fid_out_hot,'%15.8e\n',wse);
             %         wse(new_match_to_old(y)));
                 else
                    wse1=old_grid.z(closest_nodes(y,1))+data{1}(closest_nodes(y,1));
                    wse2=old_grid.z(closest_nodes(y,2))+data{1}(closest_nodes(y,2));
                    wse3=old_grid.z(closest_nodes(y,3))+data{1}(closest_nodes(y,3));
                    wse4=old_grid.z(closest_nodes(y,4))+data{1}(closest_nodes(y,4));
                    wse5=old_grid.z(closest_nodes(y,5))+data{1}(closest_nodes(y,5));
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
                    value1 = value1-new_grid.z(y);
                    fprintf(fid_out_hot,'%15.8e\n',...
                        value1);  
                 end
               end

             else
                 
               for y=1:length(new_grid.x)
                 if(new_match_to_old(y) ~= 0)
                    fprintf(fid_out_hot,'%15.8e\n',...
                      data{1}(new_match_to_old(y)));
                 else
                    value1=(weights(y,1) * ...
                        data{1}(closest_nodes(y,1)) +      ...
                        weights(y,2) *     ...
                        data{1}(closest_nodes(y,2)) +      ...
                        weights(y,3) *     ...
                        data{1}(closest_nodes(y,3)) +      ...
                        weights(y,4) *     ...
                        data{1}(closest_nodes(y,4)) +      ...
                        weights(y,5) *     ...
                        data{1}(closest_nodes(y,5))) /     ...
                        (weights(y,1) +    ...
                        weights(y,2) +     ...
                        weights(y,3) +     ...
                        weights(y,4) +     ...
                        weights(y,5));
                    fprintf(fid_out_hot,'%15.8e\n',...
                        value1);  
                 end
               
               end
             end
         end
         
         clear data type value1 value2 
         
         fgetl(fid_in_hot);
         hold=fgetl(fid_in_hot);
         card=sscanf(hold,'%s',1);         
         
         if(strcmp(card,'ENDDS'))
             fprintf(fid_out_hot,'%s\n',hold);        
         end
         
         if(feof(fid_in_hot))
             break;
         end
         
       end
    end
     


toc