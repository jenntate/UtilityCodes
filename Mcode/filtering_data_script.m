

%%% set the shortest period of interest

shper=0.00001;
frqhi=1.0/shper;

%%% set the longest period of interest

lgper=99999999.0;
frqlo=1.0/lgper;

%%% read in noaa data

%data=read_noaa('Anchorage_NOAA_2006.txt');

data.time=0:0.1:100;
data.value=2*sin(data.time);

%%% find mean value

mean_tide=mean(data.value);

%%% shift data to mean level

fft_data=data.value-mean_tide;

NFFT = 2^nextpow2(length(data.time));

%finished_filtered_data=filter_data(fft_data,data,frqhi,frqlo);

TINC=data.time(2)-data.time(1);

Fs=TINC*24;
T=1.0/Fs;
L=length(data.time);

ffted_data=fft(fft_data,NFFT)/L;

f=Fs/2*linspace(0,1,NFFT/2+1);

for np=1:NFFT/2
    ii=np*2-1;
%    if(f(np) > frqhi || f(np) < frqlo)
%        fact = 0.0;
%    else
        fact = 1.0;
%    end
    fftdt(ii)   = ffted_data(ii) * fact;
    fftdt(ii+1) = ffted_data(ii+1) * fact;
end

inverse_filtered_data = ifft(fftdt,NFFT);

inverse_filtered_data=real(inverse_filtered_data);

output_filtered_data=zeros(length(data.time),1);
output_filtered_time=zeros(length(data.time),1);

for i=1:NFFT/2
    ii=i;
    output_filtered_data(i) = inverse_filtered_data(ii)*L;
end

%new_mean_tide=mean(output_filtered_data);

output_filtered_data=output_filtered_data+mean_tide;

output_filtered_time=data.time;

createfigure2(output_filtered_time,output_filtered_data,data.time,data.value,'WSE','WSE','WSE')

