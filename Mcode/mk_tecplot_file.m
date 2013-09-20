function mk_tecplot_file(ADH_root_name,dt)
%
%
%	Function to take ADH output adaptive mesh, depth, and
%	velocity files and combine into a single *tecplot.dat
%	file which can be imported into tecplot.  The code will
% 	determine which time steps have been outputted by the 
%	ADH code and output those steps in the tecplot file.
%
%	If the dt input variable is greater than 0 then the code
%	will attempt to grab time steps with as close to that dt
%	as possible skipping any time steps that are too close.
%
%

tic
test=ls;

hold=strfind(test,'.3dm-');
space=strfind(test,' ');

for i=1:length(hold)
   now=find(space>hold(i));
   time{i} = test(hold(i)+5:space(now(1)));
end

time_number = zeros(length(time),1);
sorted = cell(length(time));

for i=1:length(time)
   time_number(i)=str2num(time{i});
end

for i=1:length(time)
    hold=find(time_number==min(time_number));
    sorted{i}=deblank(time{hold});
    time_number(hold)=99999999.9;
end

if(dt <= 0)

  fid=fopen([ADH_root_name,'.dat'],'w');

  fprintf(fid,'%s ',sorted{1:length(time)});

  fclose(fid);

else

  filt_sorted{1}=sorted{1};
  prev=str2num(sorted{1});
  count=2;

  for i=1:length(time)
    sorted_number(i)=str2num(sorted{i});
  end

  for i=2:length(sorted)
    above=find(sorted_number>(prev+dt),1,'first');
    below=find(sorted_number<(prev+dt),1,'last');
    if(above)
      if((sorted_number(above))-(prev+dt) > (prev+dt)-(sorted_number(below)) && ...
        prev~=(sorted_number(below)))
        filt_sorted{count}=sorted{below};
        prev=(sorted_number(below));
        count=count+1;
      else
         filt_sorted{count}=sorted{above};
        prev=(sorted_number(above));
        count=count+1;
      end
    end
  end

  fid=fopen([ADH_root_name,'.dat'],'w');

  fprintf(fid,'%s ',filt_sorted{1:length(filt_sorted)});

  fclose(fid);

end

file=ADH_root_name(1:length(ADH_root_name));

fid=fopen('tmp','w');

fprintf(fid,'%s',file);

fclose(fid);

system(['/u/mcalpin/codes/SMS2TEC_GS1.exe <' 'tmp']);

toc
