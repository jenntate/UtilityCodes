function [data,status] = get_noaa_met_data(stnid,bdate,edate,datumstr,...
                                     unitstr,shiftstr,timefreqstr,typestr)
% GET_NOAA_TIDAL_DATA is a function that reads NOAA Tides time series
% data for a given station from the website
% http://tidesandcurrents.noaa.gov/ and returns the time series data in a
% data structure.
% 
% Inputs:
%   stnid = NOAA Station Number, ex stnid = 8651370
%   bdate = Beginning Date YYYYMMDD format
%   edate = Ending Date YYYYMMDD format
%   datumstr = Vertical Datum {'MHHW','MHW','MTL','MSL','MLW',
%                              'MLLW','Station Datum','NAVD'}
%   unitstr = Vertical units of measurement for tides ('meters' or 'feet')
%   shiftstr = Time shift reference ('g' = 'GMT', 's' = 'LST',
%                                    'd' = 'LST/LDT' )
%   timefreqstr = Time between data values { 'Minutes', 'Hours' }
%   typestr = Type of data  {'H', 'P'} Historical/Verified = 'H' or
%                                      Preliminary/UnVerified = 'P' 
%
% Output
%   data is a structure with the following componets
%     data.name -- Station Location Name
%     data.ids  -- Station Number
%     data.datum -- Vertical Datum
%     data.type -- Verified or Preliminary
%     data.reftime -- Time Reference, GMT, LST
%     data.val_legend -- Time Series Legend
%     data.val_units -- Units for the Time Series
%     data.yyyymmdd  -- Year,Month,Day List
%     data.hr -- Hour List
%     data.min -- Minute List
%     data.val -- Time Series Data
%   status -- {0 or 1} indicates if station data was read = 1 or failed = 0
%
% Usage:
%  [data,status]=get_noaa_tidal_data(stnid,bdate,edate,datumstr,unitstr,...
%                                  shiftstr,timefreqstr,typestr);
%
% Example:  Station #8651370, Duck, NC for 08/24/2011 to 08/29/2011 GMT
%           time, referenced to mean tidal level in vertical units of feet
%           with a 6-minute time frequency for the verified historical
%           station data:
%
%  [data,status] = get_noaa_tidal_data(8651370,20110824,20110829,'MTL',...
%                                  'Feet','gmt','Minute','H');
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Written By:  Chris Massey, USACE-ERDC-CHL, Vicksburg, MS
% Date:  September 15, 2011
% Last Modified:  
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

noaa_url = 'http://tidesandcurrents.noaa.gov/';
base_url = [noaa_url,'data_menu.shtml?'];

datumlist={'MHHW','MHW','MTL','MSL','MLW',...
           'MLLW','Station Datum','NAVD'}; %datum list to search for
unitslist = {'Feet','Meters','ft','m'};
shiftlist = {'GMT','LST','LST/LDT','g','s','d'};
typelist = {'H','P'};  %H = Historical/Verified, P = Preliminary/UnVerified
freqlist = {'Hours','Minutes','HR','MIN'};
status = 1;
% Determine the Datum number to enter into the URL
tmp = strfind(upper(datumlist),upper(datumstr));
I = find(cellfun('isempty',tmp) == 0);
if isempty(I)
    disp('Bad Datum Type');
    status = 0;
    return
end
datum = I;

%Determine the Units number to enter into the URL
tmp = strfind(upper(unitslist),upper(unitstr));
I = find(cellfun('isempty',tmp) == 0);
if isempty(I)
    disp('Bad Units on Input');
    status = 0;
    return
end
unit = mod(I,2); 

%Determine the Time Shift
tmp = strfind(upper(shiftlist),upper(shiftstr));
I = find(cellfun('isempty',tmp) == 0);
if isempty(I)
    disp('Bad Time Shift on Input');
    status = 0;
    return
end
I1 = mod(I,3);
I1(I1==0) = 3;
I1 = I1 + 3;
shift = char(shiftlist(I1(1)));
shift2 = char(shiftlist(I1(1)-3));

% Determine the time frequency of data
tmp = strfind(upper(freqlist),upper(timefreqstr));
I = find(cellfun('isempty',tmp) == 0);
if isempty(I)
    disp('Bad Time Frequency Type on Input');
    status = 0;
    return
end
if (I(1) == 1 || I(1) == 3)
    sensorhist = 'h';
else
    sensorhist = 'h';
end


%Determine the Type of Data, Historical or Preliminary
tmp = strfind(upper(typelist),upper(typestr));
I = find(cellfun('isempty',tmp) == 0);
if isempty(I)
    disp('Bad Data Type on Input');
    status = 0;
    return
end
if (I == 1)
    type = 'Meteorological+Observations';
    type2 = 'Verified';
else
    type = 'Meteorological+Observations';
    type2 = 'Preliminary';
    sensorhist = 'h';  %Force this option if Preliminary Data
end

format='View+Data';         % View tidal data

% Create the url string
bdatestr = ['bdate=',num2str(bdate)];
edatestr = ['edate=',num2str(edate)];
%datumstr = ['datum=',num2str(datum)];
unitstr = ['unit=',num2str(unit)];
shiftstr = ['shift=',shift];
ids = num2str(stnid);
stnstr = ['stn=',ids];
typestr = ['type=',type];
formatstr = ['format=',format];
wlsensorhiststr = ['metinterval=',sensorhist];  %6 min or hourly

%turl = [base_url,bdatestr,'&',edatestr,'&',wlsensorhiststr,'&',...
%       datumstr,'&',unitstr,'&',shiftstr,'&',stnstr,'&',...
%       typestr,'&',formatstr];
turl = [base_url,bdatestr,'&',edatestr,'&',wlsensorhiststr,'&',...
       unitstr,'&',shiftstr,'&',stnstr,'&',...
       typestr,'&',formatstr];


%turl
nameurl = [base_url,stnstr,'&','type=Datums'];
%nameurl

% Start the data structure by setting the station id
data.name = ' ';
data.ids = ids;
data.datum = char(datumlist(datum));
data.type = type2;
data.reftime = shift2;
data.val_legend = [];
data.val_units = [];
data.yyyymmdd = [];
data.hr = [];
data.min = [];
data.val = [];


% Get the Name of this Station by loading the Datum webpage
[s,stats]=urlread(nameurl);
if (stats == 0) 
    disp('Error reading webpage ');
    disp(['  URL = ',turl]);
    status = 0;
    return
end
j1 = strfind(s,'Name:');
j2 = strfind(s,'Units:');
i1 = j2>j1;
j3 = j2(i1);
tmp = strtrim(s(j1+5:j3-1));
data.name = tmp;
clear s tmp j1 j2 i1 j3

%Load the Tidal website html data into a character array s
[s,stats]=urlread(turl);
if (stats == 0) 
    disp('Error reading webpage ');
    disp(['  URL = ',turl]);
    status = 0;
    return
end

% Get the Data Legend Names
j1 = strfind(s,'</h4><pre>');
j2 = strfind(s,'Time');
j3 = strfind(s,'DCP');
if (isempty(j1))
    disp('No Time Series Data Found');
    status = 0;
    return
end
i4 = j2 >= j1 & j2 <= j3;
j4 = j2(i4)+5;
data.val_legend = strtrim(s(j4:j3-2));

% Get the Data Units
j5 = strfind(s,'Units:');
j6 = strfind(s,'Data%');
i4 = find(j5 >= j3);
j7 = j5(i4(1))+6;
data.val_units = strtrim(s(j7:j6-2));

%Set the locations for extracting the time series data
kt = strfind(s,'<pre><h4>');
kt2 = strfind(s,'</pre>');
kt3 = strfind(s,ids);
I2 = find(kt3>kt & kt3<=kt2(1));
kt4 = kt3(I2);
kt4(end+1) = kt2(1);

% Extract the time series data
for i=1:length(I2)
    tmp = s(kt4(i):kt4(i+1)-1);
    tmp2 = sscanf(tmp,'%g %g %g:%g %g %g %g %g');
    nl = length(tmp2);
    data.yyyymmdd(i) = tmp2(2);
    data.hr(i) = tmp2(3);
    data.min(i) = tmp2(4);
    data.val(i,1:nl-4) = tmp2(5:nl);
end









 