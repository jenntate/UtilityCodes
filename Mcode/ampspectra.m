function out = ampspectra(x,dt)
% Calculates the amplitude spectra
% using the windowed periodogram method
x = x(:);
isf = isfinite(x);
x = detrend(x(isf)); %also removes mean
N = sum(isf);
fn = 1/(2*dt); %Nyquist frequency
nfft = max(256,2^(nextpow2(N)));
f = (1:nfft/2)'/(nfft/2)*fn; %frequencies, Note: starts from 1
%Compute hamming window
w = 0.54 - 0.46*cos(2*pi*(0:N-1)'/(N-1));
x = w.*x; %apply window
R = w'*w;  %compute sum of squares of window
c = fft(x,nfft); %Compute Fourier Coefficients, Pad with zeros to length nfft
K = sqrt(5/2); %coefficient for hamming window
a = K*abs(c(2:nfft/2+1))/R; %amplitudes, note skip of mean
out.a = a;
out.f = f;
return