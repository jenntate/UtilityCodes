
rootname='Base_High_SLR';
t0='01/01/2004 00:00:00';

%% Calculate discharges for String 1 West GIWW
string1{1}=[213244 210969 210968 210967 208808 208807];
%% Calculate discharges for GIWW East of GIWW West Structure
string1{2}=[233363 230943 228474 226005 223540 221123 218701 216268 213887];
%% Calculate discharges for GIWW just west of Houma
string1{3}=[221551 219128 219127 221547 221546 221545 223950 226402 226400];
%% Calculate discharges for GIWW east of Houma
string1{4}=[62591 64384 66187 67996 67990 66173 64360 62556 60746 58948 57188 55401 53602];
%% Calculate discharges for South HNC
string1{5}=[69934 68079 66254 64442 62633 60830 59030];
%% Calculate discharges for Bayou Grand Caillou, West
string1{6}=[148644 148643 150912 153153 155398];
%% Calculate discharges for Bayou Grand Caillou, East
string1{7}=[162107 162108 164356 166571 168805 171012];
%% Calculate discharges for HNC North
string1{8}=[186223 188238 190226 190225 192217 194219 196200 198183 200152 ];
%% Calculate discharges Falgout Canal
string1{9}=[175416 177631 179818 179819 177638 175433 173186];
%% Calculate discharges for Bayou Dularge
string1{10}=[128158 125739 123364 121051 118794 118795 118797 116619 114528];
%% Calculate discharges for Bayou Petit Caillou, South
string1{11}=[64697 66490 68297 66485 64689 64688 64687 66479 68287];
%% Calculate discharges for Bayou Terrebonne, South
string1{12}=[220579 222989 225436 227907 230387 232826 235256 237642 239986];
%% Calculate discharges for Bush Canal
string1{13}=[252007 250057 248070 246052 243940 241694 239424 239425];
%% Calculate discharges for Bayou Terrebonne, North Humble Canal
string1{14}=[224697 227164 229646 232074 234495 236892 239239 241518];
%% Calculate discharges for Grand Bayou
string1{15}=[160506 160507 162766 164979 167184 169407 171612 173834];

% determines the time series discharges

data=calculate_fluxes(rootname,string1);

%% writes the data out as an adh formatted _tflx file 

write_adh_formatted_fluxes(rootname,data,t0)

