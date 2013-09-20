rootname='MTOG_ADH50_MP';
t0='01/01/2004 00:00:00';


%% GIWW at Bayou Lafource
string1{1}=[318935 318934 319558 319559];
%% Bayou Grand Caillou USGS
string1{2}=[192187 194192 194191 194190 196168];   
%% Houma Navigation Canal USGS
string1{3}=[162065 162064 162063 164310 164309 166524];
%% GIWW at Houma USGS
string1{4}=[198421 254219 252309 250341 248342 246322 246321];
%% GIWW West of Minors Canal MVN
string1{5}=[198462 200373 202334 204322 206390 208488 210625 212877];
%% GIWW at Bay Wallace USGS
string1{6}=[82758 84906 87032 89170 91333 93546 95774 97993];
%% Bayou Penchant USGS
string1{7}=[49346 49347 51080 52828 54594];

% determines the time series discharges

data=calculate_fluxes(rootname,string1);

%% writes the data out as an adh formatted _tflx file 

write_adh_formatted_fluxes(rootname,data,t0)


