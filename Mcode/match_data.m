function match=match_data(X1,Y1,X2,Y2)

field.time=X1;
field.value=Y1;
model.time=X2;
model.value=Y2;

count=1;

min_model_time=min(model.time);
max_model_time=max(model.time);

for i=1:length(field.time)
   if(field.time(i) >= min_model_time && field.time(i) <= max_model_time)
     for j=1:length(model.time)
       if(field.time(i)==model.time(j))
          match.time(count)=field.time(i);
          match.field_value(count)=field.value(i);
          match.model_value(count)=model.value(j);
          count=count+1;
          break;
       elseif(model.time(j) < field.time(i) && j+1 <= length(model.time) ...
               && model.time(j+1) > field.time(i))
           match.time(count)=field.time(i);
           match.model_value(count)=((model.value(j+1)-model.value(j))/ ...
               (model.time(j+1)-model.time(j)))* ...
               (field.time(i)-model.time(j))+model.value(j);
           match.field_value(count)=field.value(i);
           count=count+1;
           break;
       end
     end
   end
end



