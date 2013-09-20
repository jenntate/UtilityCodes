function Fourier_array=Fourier_anal(observed,computed)
% Fourier Norms used to analyze model performance for a certain observed
% and computed currents.
% INPUT VARIABLES
%
%   observed= time series for observed currents
%
%   computed= time series for computed currents
%   
%                       sum((observed-computed)^2)
%   Fourier Norm= sqrt(----------------------------)
%                          sum((observed-0)^2)
%
%   Finished 3/26/2010
%% Fourier Norm equation for time series of observed and computed currents

diff=observed-computed;
squared_diff=diff.*diff;
numerator=sum(squared_diff);

curr=observed-0;
squared_curr=curr.*curr;
denominator=sum(squared_curr);
Fourier_array=sqrt(numerator/denominator);
%% Clear all values but the Fourier Array Values

clear diff observed computed squared_diff numerator curr;
clear squared_curr denomonator;