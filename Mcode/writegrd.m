function writegrd(fem_struct,infile,gridname);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% writegrd writes an xmgredit style mesh information file 
% from a fem_struct.
% NOTE:  This file is similar to write_grd.m but differs in two
%        siginificant ways:
%        1.  write_grd performs a check to see if the fem_struct
%                is a valid finite element structure.
%        2.  write_grd appends the infile with a .grd automatically.
%        
% Usage:
%       writegrd(fem_struct,infile);
%              or
%       writegrd(fem_struct,infile,gridname);
%
% Variable Description:
%    fem_struct -- a finite element structure from opnml
%           Note: The integrity of the structure is not checked.
%    infile -- Optional:  Name of file where the data is to be written.
%    gridname -- Optional:  Name of the mesh to be written on the first
%                 line of the file, if not specified then gridname = infile
%
% FileName:  writegrd.m
% Written By:  Unknown
% Date:  Unknown
% Last Modified:
%      Aug. 18, 2006 by Chris Massey, NRL Code 7322, Stennis Space Center
%                Added the header information and description of the 
%                functions actions.
%      Aug. 18, 2006 -- Chris Massey, NRL Code 7322, Stennis Space Center
%               Made writegrd so that output was vectorized for speed
%               and used formatted output for the node and element lists.
%               Also used size instead of length for determining the
%               number of nodes and elements.  This is important in 
%               case a grid has fewer than three elements.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if nargin==1,
   infile=input('Enter filename to which data is to be written: ','s');
end
if nargin==2,
  gridname=infile;
end

nnod = size(fem_struct.x(:),1);
nele = size(fem_struct.e,1);

%Open the file for writing
fid = fopen(infile,'w');

% Write the name of the mesh
fprintf(fid,'%s\n',gridname);

% Write the Number of Elements and the Number of Nodes
fprintf(fid,'%10.0f %10.0f\n',nele,nnod);

% Write the Node List
fprintf(fid,'%10.0f %16.8f %16.8f %16.8f\n',...
        [[1:1:nnod]',fem_struct.x(:),fem_struct.y(:),fem_struct.z(:)]');

% Write the Element Nodal Connectivity List    
fprintf(fid,'%10.0f %3.0f %10.0f %10.0f %10.0f\n',...
        [[1:1:nele]',3*ones(nele,1),fem_struct.e(1:end,:)]');

%Close the file
fclose(fid);

return


%%%% This is the old way of writing the file out  %%%%%
%fid= fopen(infile,'w');
%fprintf(fid,'%s\n%i %i\n',gridname,nele,nnod);
%for j=1:nnod,
%  fprintf(fid,'%i %f %f %f\n',j,fem_struct.x(j),fem_struct.y(j),fem_struct.z(j));
%end
%for j=1:nele,
%  fprintf(fid,'%i %i %i %i %i\n',j,3,fem_struct.e(j,:));
%end
%fclose(fid);
