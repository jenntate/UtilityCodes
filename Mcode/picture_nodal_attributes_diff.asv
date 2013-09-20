% PICTURE_NODAL_ATTRIBUTES
%
% This routine creates contour images for an ADCIRC
% mesh's bathymetry and nodal attributes file.
%
% usage:  picture_nodal_attributes
%
% Inputs:
%    dp -- string containing the directory where the ADCIRC input files
%          are located and where the contour images will be stored.
%    f14_name -- string containing the name of the ADCIRC mesh file, 
%                (fort.14 file)
%    f13_name -- string containing the name of the ADCIRC nodal attributes
%                file, (fort.13 file).
%    bbox -- (optional) bounding box of the domain of interest for rendering,
%                format is bbox = [x_min, x_max, y_min, y_max];
%
% Outputs:
%    Two sets of image files, currently a .png and a Matlab figure *.fig
%     Note that for the Matlab figure file, the visibility is set to off,
%     so to load it back into Matlab and have it seen, do the following:
%     >>load filename.fig;
%     >>set(gcf,'visible','on');  
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Written By:  Chris Massey, USACE-ERDC-CHL, Vicksburg, MS 39180
% Date:  October 15, 2009
% Last Modified:
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This section contains some parameters that are to be changed by the user,
% see Inputs above.

%dp = 'D:\projects\reduce_band_code\see_fort.13';  % directory name
f14_name_new = 'tx2008_r09a.grd';
f13_name_new = 'tx2008_r09_c8.13';
f14_name = 'tx2008_r08.grd';                    % fort.14 name
f13_name = 'tx2008_r08_012.2.13';                    % fort.13 name
bbox = [-94.25 -93.25 29.5 30.2];  %If you comment out this line it will default to the entire domain.

bigmem = 0;  %If bigmem = 1 then you can get the full fem structure and
             %  be able to plot the boundary
             %If bigmem = 0 then you will not get the full fem structure
             %  and not be able to plot the boundary

exit_when_done = 0;  %if exit_when_done = 1, then exit matlab at then end of this script
                     %if exit_when_done = 0, then do not exit matlab at the end

%Picture Window Position and Size
rect = [520 405 754 685];


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%           No Changes Should be needed below here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

disp(['Your selection of bigmem = ',num2str(bigmem),' means :: ']);
if(bigmem == 1)
    disp('A full fem structure will be read-in and boundary plotting will be performed.');
else
    disp('A partial fem structure will be read-in and no boundary plotting performed.');
end

disp(' ');

if(exit_when_done == 1)
   disp('This script will terminate Matlab when completed');
end

disp(' ');


disp('Reading in the ADCIRC Mesh File.');
if(bigmem == 1)
    fem = grd_to_opnml2(f14_name);
    fem_new = grd_to_opnm12(f14_name_new);
else
    fem = big_grd_to_opnml(f14_name);
    fem_new = big_grd_to_opnml(f14_name_new);
end
fem.z=-fem.z;
fem_new.z=-fem_new.z;

disp('Reading in the ADCIRC Nodal Attributes File.');
%[nodalattr,es] = read_13(f13_name);
%[nodalattr_new,es_new] = read_13(f13_name_new);

% Set the Contour Bounding Box
%format is :: bbox = [x_min,x_max,y_min,y_max];
try
   size(bbox);
catch
    bbox = [min(fem.x),max(fem.x),min(fem.y),max(fem.y)];
end

disp('Bounding Box')
disp(['   x_minimum = ',num2str(bbox(1)),'      ','y_minimum = ',num2str(bbox(3))]);
disp(['   x_maximum = ',num2str(bbox(2)),'      ','y_maximum = ',num2str(bbox(4))]);
disp(' ');

interp_bathy_script_for_diff;

fem.z=fem.z-fem_new.z;

rlon = mean(fem.x);
rlat = mean(fem.y);

disp('Finding elements and nodes in boundbox');
[elems,nodes] = find_region(fem,bbox([1 2 2 1 1]),bbox([3 3 4 4 3]),[rlat,rlon]);

%rlon = mean(fem_new.x);
%rlat = mean(fem_new.y);

%[elems_new,nodes_new] = find_region(fem_new,bbox([1 2 2 1 1]),bbox([3 3 4 4 3]),[rlat,rlon]);
%clear nodes nodes_new;
%Not used in the script
%elems = 1:length(fem.e);  %All the Elements and Nodes

% Define a 3 x nnodes array of nodal positions
trix = fem.x(fem.e(elems,:))';
triy = fem.y(fem.e(elems,:))';

%trix_new = fem_new.x(fem.e(elems_new,:))';
%triy_new = fem_new.y(fem.e(elems_new,:))';

% Create the default figure window and set the axis
opengl neverselect

M = figure;
set(gcf,'renderer','zbuffer');
opengl neverselect
set(gcf,'Backingstore','off','doublebuffer','on');
set(M,'position',rect);
set(M,'visible','off');
hold on;
axis equal;
axis(bbox);

% Draw in the boundary
if(bigmem == 1)
    hbnd = plotbnd(fem);
    set(hbnd,'color','k');
end

% Set the x-axis and y-axis labels
xlabel('Longitude (deg)');
ylabel('Latitude (deg)');

disp(' ');
k = 0;

disp('Creating Bathymetry Contour');
caxis([-5 5]);
triz = fem.z(fem.e(elems,:))';
%triz_new = fem.z_new(fem.e(elems_new,:))';
hp = patch(trix,triy,triz,'EdgeColor','none');
ht = title('Bathymetry Contour');
tmp = colorbar;
ht2=title(tmp,'m');
% Save the images to file
saveas(gcf,'bathymetry_contour','fig');
%saveas(gcf,[dp,'bathymetry_contour'],'png');
print(gcf,'-dpng','bathymetry_contour');
%Delete the contour and titles
delete(hp);
delete(ht);
delete(ht2);
caxis('auto');

disp(' ')
%Loop over all the nodal attributes
%for i=1:nodalattr.nattr
%    % Create a string for image title and image file name
%    s_base = regexprep(nodalattr.name{i},'_',' ');  %Gets rid of _ for use with titles
%    fn_base = nodalattr.name{i};
%    
%    disp(['Now Processing :: ',s_base]);
%    %Loop over value per nodal attribute
%    vpern = nodalattr.vpern{i};
%    for j=1:nodalattr.vpern{i};
%        k = k + 1;
%        disp(['    ',num2str(j),' of ',num2str(vpern),' values per this attribute.']);
%        
%        % Create a data array filled with the default values
%        data = ones(size(fem.x)).*nodalattr.defaultval{i}(j);
%        %Reset values that are not default
%        if (nodalattr.numnotdef{i} ~= 0)
%            I = nodalattr.val{i}(1,:); %List of non-default values
%            data(I) = nodalattr.val{i}(j+1,:);
%        end
%        
%        % Define a 3 x nnodes array of nodal values from the data array
%        triz(1,:) = data(fem.e(elems,1));
%        triz(2,:) = data(fem.e(elems,2));
%        triz(3,:) = data(fem.e(elems,3));
%        
%        
%        %Create Color Contour of the Nodal Attribute
%        hp = patch(trix,triy,triz,'EdgeColor','none');
%        
%        %If more than one value per nodal attribute, append title and file
%        %name string with a counter.
%        if(nodalattr.vpern{i} > 1)
%            s = [s_base,' (value ',num2str(j),' of ',num2str(nodalattr.vpern{i}),' )'];
%            fn = [fn_base,'_v',num2str(j,'%03.0f'),'_of_',num2str(nodalattr.vpern{i},'%03.0f')];
%        else
%            s=s_base;
%            fn=fn_base;
%        end
%        %Create the title
%        ht = title(s);
%        
%        % Add a colorbar with an appropriate label for the units
%        tmp = colorbar;
%        ht2=title(tmp,nodalattr.units{i});
%        
%        % Save the images to file
%        saveas(gcf,fn,'fig');
% %       saveas(gcf,[dp,fn],'png');
%        print(gcf,'-dpng',fn);
%        
%        %Delete the contour and titles
%        delete(hp);
%        delete(ht);
%        delete(ht2);
%        
%        clear data %s fn
%        
%    end
%end
close(gcf);
% Clean up the workspace ! deleting everything except fem and nodalattr
clear I M bbox dp elems es f13_name f14_name fn hbnd bigmem s_base fn_base
clear hp ht ht2 i j k rect rlat rlon s tmp trix triy triz vpern

if(exit_when_done==1)
   quit
end


return
