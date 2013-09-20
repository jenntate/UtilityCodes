function data=calculate_fluxes(rootname,string1)
%
%
%    Subroutine to calculate fluxes from an AdH solution file and a
%    connecting string of node numbers.
%
%    Input Variables:
%
%    rootname - rootname for the adh grid and the .da file (Example would
%    be 'Mobile' for Mobile.3dm and Mobile.da
%
%    string1 - string of node numbers for the flux string (Example would be
%    string1 = [1 2 3 4 5]
%
%    Output Variables:
%
%    fluxes.time - time for flux measurements
%
%    fluxes.values - flux values for the flux string
%
%

tic

%% reading the adh grid
tic
if exist([rootname '.mat'],'file')
    disp('Using pre-loaded grid')
    temp=rootname;
    load(temp)
    clear temp      
else
    grid=read_adh_grid([rootname '.3dm']);
    temp=rootname;
    save(temp,'grid')
    clear temp
end
string=[];

for i=1:length(string1)
   
    string=[string string1{i}];
    
end

check=load_da_time_series([rootname '.da'],string(1));
save_data.time=check.time;
save_data.nt=check.nt;

for i=1:length(check.time)
   
    ts=load_da_stepnew([rootname '.da'],i);
    
    for j=1:length(string)
       
        save_data.dep(j,i)=ts.dep(string(j));
        save_data.u(j,i)=ts.u(string(j));
        save_data.v(j,i)=ts.v(string(j));
                
    end
    
end
toc
fprintf(1,'Solution File Data has been read\n');

%% loop over all strings to calculate fluxes
count1=1;

for zz=1:length(string1)

    fprintf(1,'Calculating Flux for String %i\n',zz);
    
    %% checking string for connectivity
    
    elements1=zeros(20,1);
    elements2=zeros(20,1);
    
    % loops over the length of the string to determine if every 1D element is
    % connected
    
    % loops over the three ncon terms to find all the elements connected to
    % node string1{zz}(1)
    
    count=1;
    
    for i=1:3
        tot=find(grid.ncon(:,i)==string1{zz}(1));
        for k=1:length(tot)
            elements1(count)=tot(k);
            count=count+1;
        end
    end
    
    clear tot
    
    for j=1:length(string1{zz})-1
        
        count=1;
        test=0;
        
        % loops over the three ncon terms to find all the elements connected to
        % node string1{zz}(j+1)
        
        for i=1:3
            tot=find(grid.ncon(:,i)==string1{zz}(j+1));
            for k=1:length(tot)
                elements2(count)=tot(k);
                count=count+1;
            end
        end
        
        % determines if node string1{zz}(j) and string1{zz}(j+1) are both in the same
        % element
        
        for k=1:length(elements1)
            loc=find(elements1(k)==elements2, 1);
            if(~isempty(loc))
                test=1;
            end
        end
        
        % outputs an error message if the nodes are not connected
        
        if(test==0)
            fprintf(1,'ERROR NODES %i AND %i ARE NOT CONNECTED\n', ...
                string1{zz}(j),string1{zz}(j+1));
            stop
        end
        
        % clears variables and sets elements1 for the next comparison
        
        clear tot elements1 loc test
        
        elements1=elements2;
        
        clear elements2
        
    end
    
    %% reading the values for the first node in the string
    
    %value1=load_da_time_series([rootname '.da'],string1{zz}(1));
    value1.time=save_data.time;
    value1.dep=save_data.dep(count1,:);
    value1.u=save_data.u(count1,:);
    value1.v=save_data.v(count1,:);
    value1.nt=save_data.nt;
    count1=count1+1;
    
    %% converting a wse to a dep variable
    
    %   for ee=1:length(value1.wse)
    %     value1.dep(ee)=value1.wse(ee)-grid.z(ee);
    %   end
    
    loc=find(value1.dep<=0.0);
    for i=1:length(loc)
        value1.dep(loc(i))=0.0;
        value1.u(loc(i))=0.0;
        value1.v(loc(i))=0.0;
    end
    clear loc
    
    %% preallocating variables
    
    total_flux=zeros(value1.nt,length(string1{zz})-1);
    vel_1=zeros(value1.nt,1);
    vel_2=zeros(value1.nt,1);
    dist=zeros(value1.nt,1);
    wse_1=zeros(value1.nt,1);
    wse_2=zeros(value1.nt,1);
    
    %% looping over all the 1D elements and summing the fluxes
    
    for i=1:length(string1{zz})-1
        
        % sets node1 and node2
        
        node1=string1{zz}(i);
        node2=string1{zz}(i+1);
        
        % reading the time series data for string node i+1
        
        %value2=load_da_time_series([rootname '.da'],node2);
        value2.time=save_data.time;
        value2.dep=save_data.dep(count1,:);
        value2.u=save_data.u(count1,:);
        value2.v=save_data.v(count1,:);
        count1=count1+1;
        
        %   for ee=1:length(value2.wse)
        %     value2.dep(ee)=value2.wse(ee)-grid.z(ee);
        %   end
        % converting the wse variable to a dep variable
        
        loc=find(value2.dep<=0.0);
        for j=1:length(loc)
            value2.dep(loc(j))=0.0;
            value2.u(loc(j))=0.0;
            value2.v(loc(j))=0.0;
        end
        clear loc
        
        % setting the two sets of coordinates for each 1D element
        
        x1=grid.x(node1);
        x2=grid.x(node2);
        y1=grid.y(node1);
        y2=grid.y(node2);
        
        % determining the length of the 1D element
        
        dist(1:length(value1.dep))=((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))^0.5;
        
        % calculating theta
        
        theta=atan(abs((y2-y1)/(x2-x1)));
        
        for m=1:length(value1.dep)
            
            % determines the velocity perpendicular to the 1D element at node i
            
            if(y2==y1 && x2 > x1)
                vel_1(m)=-value1.v(m);
                vel_2(m)=-value2.v(m);
            elseif(y2==y1 && x2 < x1)
                vel_1(m)=value1.v(m);
                vel_2(m)=value2.v(m);
            elseif(x2==x1 && y2>y1)
                vel_1(m)=value1.u(m);
                vel_2(m)=value2.u(m);
            elseif(x2==x1 && y2<y1)
                vel_1(m)=-value1.u(m);
                vel_2(m)=-value2.u(m);
            elseif(y2>y1 && x2>x1)
                vel_1(m)=-value1.v(m)*cos(theta)+value1.u(m)*sin(theta);
                vel_2(m)=-value2.v(m)*cos(theta)+value2.u(m)*sin(theta);
            elseif(y2<y1 && x2>x1)
                vel_1(m)=-value1.v(m)*cos(theta)-value1.u(m)*sin(theta);
                vel_2(m)=-value2.v(m)*cos(theta)-value2.u(m)*sin(theta);
            elseif(y2<y1 && x2<x1)
                vel_1(m)=value1.v(m)*cos(theta)-value1.u(m)*sin(theta);
                vel_2(m)=value2.v(m)*cos(theta)-value2.u(m)*sin(theta);
            elseif(y2>y1 && x2<x1)
                vel_1(m)=value1.v(m)*cos(theta)+value1.u(m)*sin(theta);
                vel_2(m)=value2.v(m)*cos(theta)+value2.u(m)*sin(theta);
            end
            
            % if statement for the node1 as dry and node2 as wet
            
            if(value1.dep(m) <= 0 && value2.dep(m) > 0)
                
                % determines the water surface elevation values for node2
                
                wse_2(m)=value2.dep(m)+grid.z(node2);
                
                % determines difference between the to nodal z values
                
                elev_diff=grid.z(node1)-grid.z(node2);
                
                % determines the ratio of the wetted area
                
                ratio=elev_diff/value2.dep(m);
                
                % ratio downs to distance between the two nodes to match the
                % partly dry 1D element
                
                dist(m)=ratio*dist(m);
                
            elseif(value1.dep(m) > 0 && value2.dep(m) <= 0)
                
                % determines the water surface elevation for wet node1
                
                wse_1(m)=value1.dep(m)+grid.z(node1);
                
                % determines the elevation difference between the two nodes
                
                elev_diff=grid.z(node2)-grid.z(node1);
                
                % determines the ratio of the wetted area
                
                ratio=elev_diff/value1.dep(m);
                
                % ratio downs the distance between the two nodes to match the
                % partly dry 1D element
                
                dist(m)=ratio*dist(m);
                
            end
            % averages the two velocities
            
            avg_vel=(vel_1(m)+vel_2(m))/2.0;
            
            % determines the average depth for nodes i and i+1
            
            avg_dep=(value1.dep(m)+value2.dep(m))/2.0;
            
            % sums the flux for the entire flux line
            
            total_flux(m,i) =total_flux(m,i)+avg_vel*avg_dep*dist(m);
            
        end
        
        % clears the variable value1 from memory
        
        clear value1
        
        % sets the variable value1 to value2
        
        value1=value2;
        
    end
    
    %% converting the time variable format to a string
    
    %time=datestr(value1.time);
    
    sum_fluxes=zeros(length(value1.time),1);
    
    for i=1:length(value1.time)
        sum_fluxes(i)=sum(total_flux(i,:));
    end
    
    %% printing the time and flux values
    
    %for i=1:length(value1.time)
    %  if(sum_fluxes(i) > 0.0)
    %    fprintf(1,'Flux for time %s is %f\n',time(i,:),sum_fluxes(i));
    %  end
    %end
    
    %% setting the outputted variables
    
    %fluxes.time=value1.time;
    %fluxes.values=sum_fluxes;
    
    data.discharges(zz,:)=sum_fluxes;

end

data.time=value1.time;

toc





