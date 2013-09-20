
rootname='caer_hydro_EVS_2';
t0='01/01/2010 00:00:00';

%% Calculate discharges for Bayou Penchant

% string number

string_number=1;

% string of node numbers for flux string

string1=[69146 68640 68641 68133 67596];

% determines the time series discharges

fluxes=calculate_fluxes(rootname,string1);

% sets the fluxes to the data structure

data.time=fluxes.time;
data.discharges(string_number,:)=fluxes.values;

% clears the fluxes array and the string1 array

clear fluxes string1

%% Calculate discharges for HNC 

% string number

string_number=2;

% string of node numbers for flux string

string1=[40763 40299 39832];

% determines the time series discharges

fluxes=calculate_fluxes(rootname,string1);

% sets the fluxes to the data structure

data.time=fluxes.time;
data.discharges(string_number,:)=fluxes.values;

% clears the fluxes array and string1 array

clear fluxes string1

%% Calculate discharges for Bayou Grand Caillou

% string number

string_number=3;

% string of node numbers for flux string

string1=[49924 49449 48988 48530];

% determines the time series discharges

fluxes=calculate_fluxes(rootname,string1);

% sets the fluxes to the data structure

data.time=fluxes.time;
data.discharges(string_number,:)=fluxes.values;

% clears the fluxes array and string1 array

clear fluxes string1

%% Calculate discharges for GIWW at Houma

% string number

string_number=4;

% string of node numbers for flux string

string1=[56147 55598 55063 54522 53989];

% determines the time series discharges

fluxes=calculate_fluxes(rootname,string1);

% sets the fluxes to the data structure

data.time=fluxes.time;
data.discharges(string_number,:)=fluxes.values;

% clears the fluxes array and string1 array

clear fluxes string1

%% Calculate discharges for GIWW West Minors Canal

% string number

string_number=5;

% string of node numbers for flux string

string1=[69844 69350 68842];

% determines the time series discharges

fluxes=calculate_fluxes(rootname,string1);

% sets the fluxes to the data structure

data.time=fluxes.time;
data.discharges(string_number,:)=fluxes.values;

% clears the fluxes array and string1 array

clear fluxes string1

%% Calculate discharges for GIWW Near Bay Wallace

% string number

string_number=6;

% string of node numbers for flux string

string1=[93523 94042 94576];

% determines the time series discharges

fluxes=calculate_fluxes(rootname,string1);

% sets the fluxes to the data structure

data.time=fluxes.time;
data.discharges(string_number,:)=fluxes.values;

% clears the fluxes array and string1 array

clear fluxes string1

%% Calculate discharges for GIWW West Bayou Lafourche

% string number

string_number=7;

% string of node numbers for flux string

string1=[117503 117811 118093];

% determines the time series discharges

fluxes=calculate_fluxes(rootname,string1);

% sets the fluxes to the data structure

data.time=fluxes.time;
data.discharges(string_number,:)=fluxes.values;

% clears the fluxes array and string1 array

clear fluxes string1

%% writes the data out as an adh formatted _tflx file 

write_adh_formatted_fluxes(rootname,data,t0)

