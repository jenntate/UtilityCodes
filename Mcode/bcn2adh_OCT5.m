% bcn2adh.m
% Takes output from harmbc-noaa and formats it into strings and series for
% the AdH bc file.

clear
tic

% INPUT NODES ALONG TIDAL BOUNDARY
nodestring=[ 95232 94294 93418 92594 91797 91048 90329 89651 88994 ... 
    88367 87765 87191 86632 86090 85566 85061 84587 84140 83711 83308 ...
    82901 82503 ]
% INPUT EDGE STRINGS NUMBERS YOU WANT TO WRITE OUT
EGS=(109:131); 

% INPUT THE FILENAME OF THE TIDE DATA (THE OUTPUT FILE FROM HARMBC-NOAA)
filename='SWPass-Gulfport-Tide-ALL.txt';

% INPUT THE FIRST XY1 SERIES
seriescounter=1;

% OUTPUT FILE BC FILENAME
fileout='SWP-GP-strings.bc';

%%
l=length(nodestring);
fid=fopen(filename,'rt');
node=1;
wse=1;
count=1;
timecount=0;

for j=2:l
    strings(count,1)=nodestring(j-1);
    strings(count,2)=nodestring(j);
    count=count+1;
end
%%
count2=1;
while (~feof(fid))
temp=fgetl(fid);
    if (feof(fid))
        break;
    end
    card=sscanf(temp,'%s',1);
    if strcmp(card,'COMMENT')==1
        time=sscanf(temp,'%*s %*s %*s %*s %*s %*s %f',1);
        timecount=timecount+1;
    elseif strcmp(card,'BCN')==1
        node=sscanf(temp,'%*s %f %*s %*s %*s %*s %*s %*s %*s %*s %*f',1);
        wse=sscanf(temp,'%*s %*f %*s %*s %*s %*s %*s %*s %*f %*f %f',1);
        ind=find(nodestring==node,1,'first');
        if ind>0
        record.(genvarname(strcat('x',num2str(nodestring(ind)))))(timecount,1)=time;
        record.(genvarname(strcat('x',num2str(nodestring(ind)))))(timecount,2)=wse;    

        end
    end

    
    
end

fclose(fid);

%%
fid2=fopen(fileout,'w');
for k=1:l-1

    fprintf(fid2,'EGS %i %i %i \n',strings(k,1),strings(k,2),EGS(k));
end
fprintf(fid2,'\n \n');


%% AVERAGE WATER SURFACE ELEVATION BETWEEN NODES
% The first node of the edge string is used for reference
for i=2:l
    fprintf(fid2,'! NODE %i \n',nodestring(i-1));
    fprintf(fid2,'XY1 %i %i 2 2 \n',seriescounter(i-1),(timecount+1));
    fprintf(fid2,' 0.00 0.55 \n');
    for j=1:timecount 
        record.(genvarname(strcat('xb',num2str(nodestring(i-1)))))(j,1)=record.(genvarname(strcat('x',num2str(nodestring(i-1)))))(j,1);
        record.(genvarname(strcat('xb',num2str(nodestring(i-1)))))(j,2)=(record.(genvarname(strcat('x',num2str(nodestring(i-1)))))(j,2)+record.(genvarname(strcat('x',num2str(nodestring(i)))))(j,2))/2;    
        fprintf(fid2,' %8.2f %8.4f \n',record.(genvarname(strcat('xb',num2str(nodestring(i-1)))))(j,1),record.(genvarname(strcat('xb',num2str(nodestring(i-1)))))(j,2));
    end
    
    seriescounter(i)=seriescounter(i-1)+1;
    fprintf(fid2,'\n \n');
end





fprintf(fid2,'\n \n');
for k=1:l-1
    fprintf(fid2,'OTW %i %i \n',EGS(k),seriescounter(k));
end
fclose(fid2);

toc
























