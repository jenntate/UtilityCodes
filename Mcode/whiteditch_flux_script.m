function whiteditch_flux_script(basename)
% clear

% basename='whiteditch_feb14';
t0='01/01/2010 00:00:00';

%% Fluxstrings contain the X and Y coordinate of start and end node
%   Syntax: fluxstring{i}=[xcoord1, ycoord1, xcoord2, ycoord2];
%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Site 1
    fluxstring{1}=[216888.399, 3302516.48, 216987.2, 3302430.0];

%% Site 2
    fluxstring{2}=[221584.436, 3301972.95, 221703.355, 3302055.46];
 
%% Site 3
    fluxstring{3}=[229911.029, 3297604.67, 230163.531, 3297704.55];

%% Site 4
    fluxstring{4}=[234391.964, 3295137.7, 234324.354, 3294856.48];

%% Site 5
    fluxstring{5}=[247918.308, 3285754.96, 247907.205, 3285473.64];

%% Site 7

%% Site 8
    fluxstring{6}=[229354.376, 3293772.29, 229581.01, 3293833.98];

%% Site 9
    fluxstring{7}=[223154.42, 3287051.57, 223104.027, 3286859.35];

%% Site 10
    fluxstring{8}=[218593.708, 3289249.58, 218686.657, 3289129.13];

%% FSP West
    fluxstring{9}=[261479.0, 3251006.0, 261522.1, 3251019.0];

%% FSP East
    fluxstring{10}=[262138.8, 3251128.2, 262912.9, 3250992.4];

%% Dennesse Pass
%     fluxstring{11}=[221614.1, 3301988.0, 221675.6, 3302035.9];

%%
% determines the time series discharges
data=calculate_fluxes_kcp(basename,fluxstring);

%% writes the data out as an adh formatted _tflx file 
write_adh_formatted_fluxes(basename,data,t0)

clear all