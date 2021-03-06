function interp=interpolate_wind_data(WS_TIME,WS_MEASUREMENT,WD_TIME,WD_MEASUREMENT,increment)
%
%   Function to interpolate the wind speed and direction to a set increment
%   amount.
%
%   INPUT VARIABLES
%
%   WS_TIME = time series values for the wind speeds
%
%   WS_MEASUEMENT = time series values for the wind speeds
%
%   WD_TIME = time series values for the wind directions
%
%   WD_MEASUREMENT = time series values for the wind directions
%
%   increment = increment at which to interpolate the wind measurements
%
%   OUTPUT VARIABLES
%
%   interp = structure with the interpolated values
%
%   interp.time = interpolated times
%
%   interp.x = interpolated x component wind values
%
%   interp.y = interpolated y component wind values
%
%   Finished 3/12/2010
%

%% loop to remove any data that doesn't have both wind speed and direction
%  data

count=1;

for i=1:length(WS_TIME)
   for j=1:length(WD_TIME)
     if(WS_TIME(i)==WD_TIME(j))
        if(i==length(WS_TIME) || ...
                abs(WS_MEASUREMENT(i+1)-WS_MEASUREMENT(i)) < 10)
          match.time(count)=WS_TIME(i);
          match.ws_value(count)=WS_MEASUREMENT(i);
          match.wd_value(count)=WD_MEASUREMENT(j);
          count=count+1;
        end
        break;
     end
   end
end

for i=2:length(match.time)
   if(match.time(i-1)==match.time(i))
       fprintf(1,'Two data entries at time = %f\n',match.time(i))
   end
end

%% convert the USGS angle to ADH format

match.wd_value=270-match.wd_value;
hold=find(WD_MEASUREMENT < 0);

for i=1:length(hold)
    match.wd_value(i)=360+match.wd_value(i);
end

%% Seperate into x and y wind components

for i=1:length(match.wd_value)
  match.x(i)=match.ws_value(i)*(cos(match.wd_value(i)*pi/180));
  match.y(i)=match.ws_value(i)*(sin(match.wd_value(i)*pi/180));
end

%% set the time increment at which the interpolate

inc=increment/24;
start_time=match.time(1);
end_time=max(match.time);
interp.time=start_time:inc:end_time;

for i=1:length(match.time)
   loc=find(match.time==match.time(i));
   if(length(loc)>1)
      fprintf(1,'Two data entries at time = %s\n',datestr(match.time(i)))      
   end
   clear loc;
end

%% interpolate the data to the interp.time times

interp.x=spline(match.time,match.x,interp.time);
interp.y=spline(match.time,match.y,interp.time);

