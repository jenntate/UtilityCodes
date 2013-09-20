function grid=read_adh_grid(basename,np,ne)
%
%   This subroutine reads in an ADH mesh and saves the nodal and elemental
%   information into a structure named grid.
%
%   INPUT INFORMATION
%
%   basename = the root name of the ADH mesh to be read in without the .3dm
%   extension.  This code assumes a .3dm extensions NOT a .2dm extension.
%
%   OPTIONAL INPUT
%
%   np = number of nodes in the mesh.  This would only be needed for
%   EXTRMELY large meshes (greater than 1.5 million nodes) for more
%   efficienty use of memory allocation.
%
%   ne = number of elements in the mesh. This would only be needed for
%   EXTRMELY large meshes (greater than 3 million elements) for more
%   efficienty use of memory allocation.
%
%   OUTPUT INFORMATION
%
%   grid = structure with all mesh information.
%
%   grid.x = x coordinates for all nodes in mesh
%   grid.y = y coordinates for all nodes in mesh
%   grid.z = elevations for all nodes in mesh
%   grid.ncon = array containing the nodes for each element in the mesh
%   grid.nodes = number of nodes in the mesh
%   grid.elements = number of elements in the mesh
%   grid.line = contains the information at the end of the mesh
%
%   Finished 3/12/2010
%
%tic

%% checks the subroutine input and sets np and ne if not used as input
loc=find(basename=='.',1,'first');
tempaaa=basename(1:loc-1);
if exist([tempaaa '.mat'],'file')
    s1=dir([tempaaa '.mat']);
    a=1;
    if(exist(basename,'file'))
        s2=dir(basename);
        if(datenum(s2.date)>datenum(s1.date))
            a=2;
        end
    end
end

if (exist([tempaaa '.mat'],'file') && a==1)
    disp('Using pre-read grid')
    load(tempaaa)    
else
    error(nargchk(1,3,nargin))
    
    if(nargin < 2)
        np=6000000;
    end
    
    if(nargin < 3)
        ne=10000000;
    end
    
    %% opens the ADH .3dm mesh file
    
    fid14=fopen(basename,'rt');
    
    %% preallocates the arrays for more efficient code
    
    ce=1;
    cn=1;
    ct=1;
    grid1.ncon=zeros(ne,4);
    grid1.x=zeros(np,1);
    grid1.y=zeros(np,1);
    grid1.z=zeros(np,1);
    
    fprintf(1,'READING GRID FILE\n');
    
    %% loops over all the lines in the mesh file
    
    while ~feof(fid14)
        test=fscanf(fid14,'%s',1);
        
        %  identifies an element card and saves the element information to the
        %  .ncon structure
        
        if(isequal(test,'E3T'))
            element=fscanf(fid14,'%i',1);
            grid1.ncon(element,1)=fscanf(fid14,'%i',1);
            grid1.ncon(element,2)=fscanf(fid14,'%i',1);
            grid1.ncon(element,3)=fscanf(fid14,'%i',1);
            grid1.ncon(element,4)=fscanf(fid14,'%i',1);
            if(element~=ce)
                fprintf(1,'ERROR IN ELEMENT NUMBERING AT %i',ce);
                break
            end
            ce=ce+1;
            
            %  identifies a nodal card and saves the nodal information to the .x,
            %  .y and .z structure
            
        elseif(isequal(test,'ND'))
            node=fscanf(fid14,'%i',1);
            grid1.x(node)=fscanf(fid14,'%f',1);
            grid1.y(node)=fscanf(fid14,'%f',1);
            grid1.z(node)=fscanf(fid14,'%f',1);
            if(node~=cn)
                fprintf(1,'ERROR IN NODE NUMBERING AT %i', cn);
                break
            end
            cn=cn+1;
            
            %  saves the information at the end of the mesh file to the .line
            %  structure
            
        else
            test1=fgetl(fid14);
            if(feof(fid14))
                grid.line{ct}=test;
                break;
            else
                grid.line{ct}=strcat(test,test1);
            end
            ct=ct+1;
        end
    end
    
    %% saves the information to the grid structure for output
    
    grid.ncon(1:ce-1,1:4)=grid1.ncon(1:ce-1,1:4);
    grid.x=grid1.x(1:cn-1);
    grid.y=grid1.y(1:cn-1);
    grid.z=grid1.z(1:cn-1);
    grid.nodes=cn-1;
    grid.elements=ce-1;
    
    %% closes the file and clears the variables
    
    fclose(fid14);
    
    save(tempaaa,'grid')
    clear temp
end

fprintf(1,'FINISHED READING GRID FILE\n');

%toc


