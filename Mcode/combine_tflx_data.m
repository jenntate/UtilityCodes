
data=read_tflx_discharges('Mobile','01/01/2008 00:00:00',1);

data_hot=read_tflx_discharges('Mobile_hot','01/01/2008 00:00:00',1);

data_hot1=read_tflx_discharges('Mobile_hot1','01/01/2008 00:00:00',1);

%data_hot2=read_tflx_discharges('Mobile_Plan_hot2','01/01/2008 00:00:00',1);

max1=max(data.time);
time=data.time;
fluxes=data.fluxes;
mapping=data.mapping;

loc1=find(data_hot.time>max1);
len=length(time);
time(len+1:len+length(loc1))=data_hot.time(loc1);
fluxes(len+1:len+length(loc1),:)=data_hot.fluxes(loc1,:);
%for i=1:length(mapping)
%  fluxes(len+1:len+length(loc1),i)=data_hot.fluxes(loc1,i);
%end
  %time(length(time)+1:length(time)+1+length(loc1))=data_hot.fluxes(loc1);

max2=max(time);
len=length(time);
loc2=find(data_hot1.time>max2);

time(len+1:len+length(loc2))=data_hot1.time(loc2);
fluxes(len+1:len+length(loc2),:)=data_hot1.fluxes(loc2,:);

model.time=time;
model.fluxes=fluxes;
model.mapping=mapping;

save('Mobile_total_tflx','model');