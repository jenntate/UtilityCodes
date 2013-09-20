function spec=power_spectra(xin,varargin)

dt=1;
inf.iname=[];
inf.irefname=[];

k=1;
while ~isempty(varargin),
  if ischar(varargin{1}),
    switch lower(varargin{1}(1:3)),
      case 'int',
        dt=varargin{2};
      case 'out',
         filen=varargin{2};
	 switch filen,
	   otherwise
	     [fid,~]=fopen(filen,'w');
	     if fid==-1, error(msg); end;
	  end;
       case 'pre',
         varargin(1)=[];
      case 'inf',
         inf.iname=varargin{2};
	 inf.irefname=varargin{3};
	 inf.amprat=varargin{4};
	 inf.ph=varargin{5};
	 varargin(1:3)=[];	 
      otherwise,
         error(['Can''t understand property:' varargin{1}]);
    end;
    varargin([1 2])=[]; 
  else  
    switch k,
      case 1,
        dt=varargin{1};
      otherwise
        error('Too many input parameters');
     end;
     varargin(1)=[];
  end;
  k=k+1;
end;

xin=xin(:); % makes xin a column vector
nobs=length(xin);

T=dt * nobs;

p=abs(fft(xin))/(nobs/2);
spec.p=p(1:(nobs/2)).^2;
spec.freq=(0:nobs/2-1)/T;