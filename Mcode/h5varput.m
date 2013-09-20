function h5varput ( varargin )
% H5VARPUT:  writes an HDF5 dataset
%
% H5VARPUT(HDFFILE,VARNAME,DATA) writes an entire dataset to the variable given 
% by VARNAME.
%
% H5VARPUT(HDF5FILE,VARNAME,START,COUNT,DATA) writes a contiguous portion of a 
% dataset.
%
% H5VARPUT(HDF5FILE,VARNAME,START,COUNT,STRIDE,DATA) writes a non-contiguous 
% portion of a dataset.
%

error(nargchk(3,6,nargin,'struct'));
error(nargoutchk(0,0,nargout,'struct'));

[h5file,varname,offset,count,stride,data] = parse_h5_varput_options ( varargin{:} );

flags = 'H5F_ACC_RDWR';
plist_id = 'H5P_DEFAULT';

file_id = H5F.open ( h5file, flags, plist_id );

dataset_id=H5D.open(file_id,varname);

datatype_id = H5D.get_type(dataset_id);

%
% Create the appropriate mem dataspace
if isempty(offset) && isempty(count) && isempty(stride)
	file_space_id = 'H5S_ALL';
else
	%
	% define the memory hyperslab
	file_space_id = H5D.get_space(dataset_id);
	H5S.select_hyperslab(file_space_id, 'H5S_SELECT_SET', ...
	                     fliplr(offset), fliplr(stride), fliplr(count), ...
						 ones(1,length(offset)));

end


%
% Create the appropriate output dataspace.
if isempty(offset) && isempty(count) && isempty(stride)
	mem_space_id = 'H5S_ALL';
else
	mem_space_id = H5S.create_simple(length(offset),fliplr(count),fliplr(count));
end


H5D.write(dataset_id, 'H5ML_DEFAULT', mem_space_id, file_space_id, plist_id, data);

H5T.close(datatype_id);
H5D.close(dataset_id);
H5F.close(file_id);




%===============================================================================
function [hfile,varname,start,count,stride,data] = parse_h5_varput_options ( varargin )
%
% Have to be able to check the following signatures.

% H5_VARGET(HFILE,VARIABLE,DATA) 
% H5_VARGET(HFILE,VARIABLE,START,COUNT,DATA)
% H5_VARGET(HFILE,VARIABLE,START,COUNT,STRIDE,DATA)

% First argument should be the filename.
if ~ischar(varargin{1})
	error ( 'MATLAB:H5VARPUT:badInput', 'File argument must be character')
end

hfile = varargin{1};

%
% 2nd argument should be the variable name.
if ~ischar(varargin{2})
	error ( 'MATLAB:H5VARPUT:badInput', 'Variable name argument must be character')
end

varname = varargin{2};


switch nargin
case 3

	start = [];
	stride = [];
	count = [];
	data = varargin{3};

case 4
	error ( 'MATLAB:H5VARPUT:badInput', 'Cannot have 4 input arguments.')

case 5
	%
	% Given the start, stride, and count.
	if ~isnumeric(varargin{3}) 
		error ( 'MATLAB:H5VARPUT:badInput', 'Start argument must be numeric')
	end
	start = varargin{3};

	if ~isnumeric(varargin{4}) 
		error ( 'MATLAB:H5VARPUT:badInput', 'Count argument must be numeric')
	end
	count = varargin{4};

	stride = [];
	data = varargin{5};

	
case 6

	%
	% Given the start, stride, and count.
	if ~isnumeric(varargin{3}) 
		error ( 'MATLAB:H5VARPUT:badInput', 'Start argument must be numeric')
	end
	start = varargin{3};

	if ~isnumeric(varargin{4}) 
		error ( 'MATLAB:H5VARPUT:badInput', 'Count argument must be numeric')
	end
	count = varargin{4};

	if ~isnumeric(varargin{5}) 
		error ( 'MATLAB:H5VARPUT:badInput', 'Stride argument must be numeric')
	end
	stride = varargin{5};
	data = varargin{6};

otherwise
	error ( 'MATLAB:H5VARPUT:badInput', 'Bad number of input arguments.')

end

return






