function interp=interpolate_data(X1,X2,Y1,Y2,increment)

ws.time=X1;
ws.value=Y1;
wd.time=X2;
wd.value=Y2;

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

%%% Convert to ADH formated angle

match.wd_value=270-match.wd_value;
hold=find(wd.value < 0);

for i=1:length(hold)
    match.wd_value(i)=360+match.wd_value(i);
end

%%% Seperate into wind components

for i=1:length(match.wd_value)
    match.x(i)=match.ws_value(i)*(COS(match.wd_value(i)*pi/180));
    match.y(i)=match.ws_value(i)*(SIN(match.wd_value(i)*pi/180));
end

%%% Interpolate to the desired time increment

inc=increment/24;
count=1;

interp.time=match.time(1):inc:max(match.time);

for i=1:length(match.time)
     if(interp.time(count) == match.time(i))
        interp.x(count)=match.x(i);
        interp.y(count)=match.y(i);
        count=count+1;
     elseif(match.time(i) < interp.time(count) ...
         && match.time(i+1) > interp.time(count))
     
         slope_x=(match.x(i+1)-match.x(i))/(match.time(i+1)-match.time(i));
         slope_y=(match.y(i+1)-match.y(i))/(match.time(i+1)-match.time(i));         
     
         interp.x(count)=match.x(i)+slope_x* ...
             (interp.time(count)-match.time(i));
         interp.y(count)=match.y(i)+slope_y* ...
             (interp.time(count)-match.time(i));         
         
         count=count+1;
     end        
end


