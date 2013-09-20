function interp=interpolate_wind_data(WS_TIME,WS_MEASUREMENT,WD_TIME,WD_MEASUREMENT,increment)

ws.time=WS_TIME;
ws.value=WS_MEASUREMENT;
wd.time=WD_TIME;
wd.value=WD_MEASUREMENT;

count=1;

for i=1:length(ws.time)
   for j=1:length(wd.time)
     if(ws.time(i)==wd.time(j))
        match.time(count)=ws.time(i);
        match.ws_value(count)=ws.value(i);
        match.wd_value(count)=wd.value(j);
        count=count+1;
        break;
     end
   end
end

match.wd_value=270-match.wd_value;
hold=find(wd.value < 0);

for i=1:length(hold)
    match.wd_value(i)=360+match.wd_value(i);
end

%%% Seperate into wind components

for i=1:length(match.wd_value)
    match.x(i)=match.ws_value(i)*(cos(match.wd_value(i)*pi/180));
    match.y(i)=match.ws_value(i)*(sin(match.wd_value(i)*pi/180));
end

inc=increment/24;
count=1;

interp.time=match.time(1):inc:max(match.time);

%interp=match;

for count=1:length(interp.time)
  for i=1:length(match.time)
     if(interp.time(count) == match.time(i))
        interp.x(count)=match.x(i);
        interp.y(count)=match.y(i);
        break;
     elseif(i > 1 && match.time(i-1) < interp.time(count) ...
         && match.time(i) > interp.time(count))
     
         slope_x=(match.x(i)-match.x(i-1))/(match.time(i)-match.time(i-1));
         slope_y=(match.y(i)-match.y(i-1))/(match.time(i)-match.time(i-1));         
     
         interp.x(count)=match.x(i-1)+slope_x* ...
             (interp.time(count)-match.time(i-1));
         interp.y(count)=match.y(i-1)+slope_y* ...
             (interp.time(count)-match.time(i-1));         
         
         break;
     end        
  end
end


