% bcn2adh.m
% Takes output from harmbc-noaa and formats it into strings and series for
% the AdH bc file.
% Modified December 19, 2012 - KCP
% Modified March 22, 2013 - KCP
clear
tic

% INPUT NODES ALONG TIDAL BOUNDARY
nodestring=[1:22];
% nodestring=[22:51];


% nodestring=[150213 149275 148399 147575 146778 146029 ... 
%  145310 144632 143975 143346 142741 142164 141604 ...
%  141058 140522 140015 139541 139094 138661 138250 ... 
%  137843 137445];

% nodestring=[137445 137044 136656 136297 135954 135632 135326 135044 134778 134525 ... 
% 134284 134056 133839 133627 133425 133238 133059 132888 132729 132572 ...
% 132409 132255 132109 131982 131863 131761 131663 131564 131465 131466 ... 
% ];

% INPUT THE FILENAME OF THE TIDE DATA (THE OUTPUT FILE FROM HARMBC-NOAA)
% Accepts single or multiple input files, will concatenate in the order
% listed.
% filename={'Fourchon-SWPass-Tide-2008.out','Fourchon-SWPass-Tide-2009.out','Fourchon-SWPass-Tide-2010.out'};
% filename={'SWPass-Gulfport-Tide-2008.out','SWPass-Gulfport-Tide-2009.out','SWPass-Gulfport-Tide-2010.out'};
filename={'Fourchon-SWPass-Tide-2010.out'};
% filename={'SWPass-Gulfport-Tide-2010.out'};


% INPUT STARTING EDGE STRING NUMBER
EGS=40;
% EGS=61;

% INPUT THE FIRST XY1 SERIES
seriescounter=1;
% seriescounter=22;

% OUTPUT FILE BC FILENAME
fileout='Fourchon-SWP-strings_2010.bc';
% fileout='SWP-GPT-strings_2010.bc';

% ======================================================================
% NO CHANGES NECCESARY BELOW THIS POINT ================================
% ======================================================================
%%
% SETS UP INITIAL PARAMETERS AND PREALLOCATIONS
l=length(nodestring);
node=1;
wse=1;
count=1;
timecount=0;
timeoffset=0; 
strings=zeros(l-1,2);

for t=1:length(filename);    
    fid=fopen(filename{t},'rt');
    % GENERATES THE EGS STRINGS
    for j=2:l
        strings(count,1)=nodestring(j-1);
        strings(count,2)=nodestring(j);
        count=count+1;
    end
    %%
    % count2=1;
    fprintf(1,'Reading and reorganizing data for file %s ... \n',filename{t})
    while (~feof(fid))
    temp=fgetl(fid);
        if (feof(fid))
            break;
        end
        card=sscanf(temp,'%s',1);
        if strcmp(card,'COMMENT')==1
            time=sscanf(temp,'%*s %*s %*s %*s %*s %*s %f',1)+timeoffset; % 
            timecount=timecount+1;
        elseif strcmp(card,'BCN')==1
            node=sscanf(temp,'%*s %f %*s %*s %*s %*s %*s %*s %*s %*s %*f',1);
            wse=sscanf(temp,'%*s %*f %*s %*s %*s %*s %*s %*s %*f %*f %f',1);

            % REORGANIZES AND SORTS DATA BY NODES INSTEAD OF TIMESTEPS
            ind=find(nodestring==node,1,'first');
            if ind>0
            record.(genvarname(strcat('x',num2str(nodestring(ind)))))(timecount,1)=time;
            record.(genvarname(strcat('x',num2str(nodestring(ind)))))(timecount,2)=wse;    
            end
        end    
    end
    fclose(fid);
    timeoffset=time
end

%%
% OPEN OUTPUT FILE
fid2=fopen(fileout,'w');

% WRITE OUT EGS STRINGS
fprintf(1,'Writing EGS strings ... \n')
for k=1:l-1
    fprintf(fid2,'EGS %i %i %i \n',strings(k,1),strings(k,2),EGS+k-1);
end
fprintf(fid2,'\n \n');


%% AVERAGE WATER SURFACE ELEVATION BETWEEN NODES AND WRITE OUT SERIES
% The first node of the edge string is used for reference
for i=2:l
    fprintf(1,'Writing time series for node %i ... \n',nodestring(i-1))
    fprintf(fid2,'! NODE %i \n',nodestring(i-1));
    fprintf(fid2,'XY1 %i %i 2 2 \n',seriescounter(i-1),(timecount+1));
    fprintf(fid2,' 0.00 0.55 \n');
    for j=1:timecount 
%         record.(genvarname(strcat('xb',num2str(nodestring(i-1)))))(j,1)=record.(genvarname(strcat('x',num2str(nodestring(i-1)))))(j,1);
        record.(genvarname(strcat('xb',num2str(nodestring(i-1)))))(j,2)=(record.(genvarname(strcat('x',num2str(nodestring(i-1)))))(j,2)+record.(genvarname(strcat('x',num2str(nodestring(i)))))(j,2))/2;    
        fprintf(fid2,' %8.2f %8.4f \n',record.(genvarname(strcat('x',num2str(nodestring(i-1)))))(j,1),record.(genvarname(strcat('xb',num2str(nodestring(i-1)))))(j,2));
    end
    
    seriescounter(i)=seriescounter(i-1)+1;
    fprintf(fid2,'\n \n');
end

%%
% WRITE OUT OTW CARDS
fprintf(1,'Writing OTW cards ... \n')
fprintf(fid2,'\n \n');
for k=1:l-1
    fprintf(fid2,'NB OTW %i %i \n',EGS+k-1,seriescounter(k));
end
fclose(fid2);

fprintf(1,'Job Complete. \n\n')
toc
























