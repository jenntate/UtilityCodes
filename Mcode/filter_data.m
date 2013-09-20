function [xf,Rh,Fh] = filter_data(x,dt,fc,ns,win,ftype,fo)
% Applies a windowed sinc filter
%
% USAGE:
%   [xf,Rh,Fh] = winsinc(x,dt,fc,ns,win,ftype,fo)
%
% DESCRIPTION:
%   Digital Finite Impulse Response Window Filter or
%   simply Windowed Sinc Filter. 
%   This is a digital symetric non-recursive filter
%   which can be Lowpass, Highpass, or Bandpass
%   by using a windowed sinc function.
%   Non-Recursive filters are based on convolving 
%   the input series with a weight function.
%   Their advantage compared to recursive filters
%   is that they are always stable and matematically simpler.
%   Their disadvantage is that they may require a large
%   number of weights to achieve a desired response.
%   Some of these windows are available with fir1 like
%   hamming but others are not. 
%
%INPUT VARIABLES:
%   x = vector
%   dt = Sampling Interval
%   fc = Cutoff frequency
%   ns = Width of filter 
%       (>>ns produces a steeper cutoff
%       but is more computationally expensive)
%       2*ns + 1 = Order of Filter, (Number of Filter Coef.)
%   win = Window Type:
%       'welch','parzen','hanning','hamming'
%       'blackman','lanczos','kaiser','gauss'
%   
%   ftype = 'low', 'high' o 'band'
%   fo = Central Frequency (for bandpass only) 
%
%OUTPUT VARIABLES:
%   xf = filtered vector
%   Rh = Filter Response
%   Fh = Frequencies for Filter Response
%
%Alejandro Sanchez

N = length(x); 
fn = 1/(2*dt); %Nyquist frequency
df = 2*fn/(N-1); %delta frec
ftype = lower(ftype(1:3));
if strcmpi(ftype,'ban')
    fcc = fo;
    fo = fc;
    fc = fcc;
end
%Frecuencia de corte para pasa alta
if strcmpi(ftype,'hig')
    fc = fn - fc;   
end
n = -ns:ns;
w = 0*n; %initialize
c = 0*n;
an = abs(n);
ic = (an<ns & n~=0);
switch lower(win(1:3))
    case 'wel' %welch
        w(ic) = 1 - (n(ic)/ns).^2;
        w(~ic) = 0;
    case 'han' %hanning
        alpha2 = 1/2;
        w(ic) = alpha2 + (1 - alpha2)*cos(pi*n(ic)/ns);
        w(~ic) = 0;
    case 'ham' %hamming
        alpha2 = 0.54;
        w(ic) = alpha2 + (1 - alpha2)*cos(pi*n(ic)/ns);
        w(~ic) = 0;
    case 'bla' %blackman
        w(ic) = 0.42 + 0.5*cos(pi*n(ic)/ns) ...
            + 0.08*cos(2*pi*n(ic)/ns);
        w(~ic) = 0;
    case 'lan' %lanczos
        w(ic) = sin(pi*n(ic)/ns)./(pi*n(ic)/ns);
        w(~ic) = 0;
    case 'gau' %gauss
        sigma = ns/2;
        w(ic) = exp(-(n(ic)/sigma).^2);
        w(~ic) = 0;
    case 'box' %boxcar
        w = ones(size(n));
    otherwise
        error('Invalid Window')
        %w = ones(ns,1)/ns;
end %switch
%Basic Filter (sinc)
ho = fc/fn; %0<ho<1
iz = (n==0);
c(~iz) = sin(pi*n(~iz)*ho)./(pi*n(~iz));
%Obtain filter Coefficients
%Multiplication is time domain
%is a convolution in frequency domain
h = w.*c;
h(iz) = ho; 
%Make sure sum is equal to 1
h = h/sum(h);
%High pass, clever trick
if strcmpi(ftype,'hig')
    h(2:2:length(h)) = -1*h(2:2:length(h));
end
%Filter Frequencies
Fh = df:df:fn; 
Rh = 0*Fh; %initialize
switch ftype
case {'low','hig'}
    %Filter Response
    for k=1:length(Fh)
        Rh(k) = sum(h.*cos(2*pi*n*Fh(k)/(2*fn)));
    end
    try 
        xf = conv2(h,1,x,'same'); %note that conv2 is used to use 'same' option       
    catch %in case conv2 is not available
        xf = calcconv(h,x);
    end
case 'ban'
    %Filter Response
    for k=1:length(Fh);
        Rh(k) = sum(h.*cos(2*pi*n*(Fh(k)-fo)/(2*fn)));
    end
    %Corrimiento de los pesos del filtro
    th = n*dt;
    i = sqrt(-1); %Redundant but instructive
    hpri = h.*exp(2.0*i*pi*fo.*th);
    try 
        xfc = conv2(hpri,1,x,'same'); %note that conv2 is used to use 'same' option 
    catch %in case conv2 is not available
        xfc = calcconv(hpri,x);
    end
    xf = 2*real(xfc);         
end
xf = xf(:);
return