function [elemnums,nodenums] = find_region(fem_struct,x,y,refl)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FIND_REGION --
%  This routine finds all the elements and nodes that
%  are located on are within a polygonal region
%  defined by the points (x,y)
%
%  Uses the routine inpolygon.m
%
% Variables:
% elemnums --  a vector containing all the element numbers in the region
% nodenums --  a vector containing all the unique node numbers of the 
%                elements in the region
% fem_struct -- the finite element grid structure
% x -- the x coordinates defining the polygonal region
% y --= the y coordinates defining the polygonal region
%
%     Note: The first point needs to be repeated at the end
%           of the list in order to close the polygon
%
% Optional:
% refl -- [rlon,rlat] the reference longitude and latitude
%         to be used for the CPP projection
%
%  Usage:
%
%    [elemnums,nodenums] = find_region(fem_struct,x,y)
%                   or
%    [elemnums,nodenums] = find_region(fem_struct,x,y,refl)
%
% FileName:    fidn_region.m
% Written By:  Chris Massey
% Date:        April 16, 2003
% Modified:    July 6, 2004 -- Gives nodes and converts to x/y from 
%                               long/lat if applicable
%              Oct. 16, 2009 : Chris Massey, USACE-ERDC-CHL, Vicksburg,MS
%                            -- Removed unnecessary calls to el_areas.m and
%                            belint.m.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Keywords:  find nodes, find elements
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Convert long/lat to x/y if needed
if(exist('refl','var'))
  [xpoly,ypoly]=ll2xy(y,x,refl(2),refl(1));
  x = xpoly;
  y = ypoly;
  [xgrd,ygrd]=ll2xy(fem_struct.y,fem_struct.x,refl(2),refl(1));
  fem_struct.x=xgrd; fem_struct.y=ygrd;
end


%  Initial list of nodes in or on the polygon
IN = inpolygon(fem_struct.x,fem_struct.y,x,y);
IN2 = find(IN > 0);

clear IN

J = ismember(fem_struct.e(:,1),IN2);
J1 = find(J == 1);
clear J
J = ismember(fem_struct.e(:,2),IN2);
J2 = find(J == 1);
clear J
J = ismember(fem_struct.e(:,3),IN2);
J3 = find(J == 1);
clear J
clear IN2

% The elements contained in or on the polygon
elemnums = unique([J1;J2;J3]);


clear J1 J2 J3

% The unique node numbers of all elements in or on the polygon
nodenums = unique(fem_struct.e(elemnums,:));


return
