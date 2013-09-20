function determine_percentiles_hpc(da_file,da_out,inc)


tic


% da_file='Mobile_small.da';
% da_out='Mobile_small_pct.da';
% inc=1.0;

fid=fopen(da_out,'w');
check=load_da_stepnew(da_file,0);

pct=[0 1 2 5 10 20 30 40 50 60 70 80 90 95 97 98 99 100];

nodes=check.np;

%max_nodes=50000;
%nodes=min(max_nodes,check.np);

% if(check.np>100000)
%     loops=int64(check.np/max_nodes);
%     extra=mod(check.np,max_nodes);
%     if(extra/max_nodes>0.5)
%         loops=loops-1;
%     end
% end

fprintf(1,'There will be %i loops with the last loop containing %i nodes\n',loops+1,extra);

if(check.wse_check==1)
    wse=zeros(nodes,check.nt);
    wse_pct=zeros(check.np,length(pct));
end
if(check.dep_check==1)
    dep=zeros(nodes,check.nt);
    dep_pct=zeros(check.np,length(pct));
end
if(check.vel_check==1)
    vel_x=zeros(nodes,check.nt);
    vel_y=zeros(nodes,check.nt);
    vel_mag=zeros(nodes,check.nt);
    vel_pct=zeros(check.np,length(pct));
end
if(check.sal_check==1)
    sal=zeros(nodes,check.nt);
    sal_pct=zeros(check.np,length(pct));
end

fprintf(1,'The preallocations has been completed\n');

%time=zeros(check.nt,1);
data1=load_da_time_series(da_file,1);
time=data1.time;
time1=min(data1.time):inc/24.0:max(data1.time);
time1=time1';

%for j=1:loops+1
    
    %fprintf(1,'Starting loop = %i\n',j);
    
    %if(j==loops+1)
    %    nodes1=extra;
    %end
    %start=i+(j-1)*nodes;
    %stop=start+nodes-1;
    %stop=max(stop,check.np);

    start=1;
    stop=check.np;
    %if(j==loops+1)
    %    stop=extra;
    %else
    %    stop=nodes;
    %end
    for i=1:check.nt      
        data=load_da_stepnew(da_file,i);
        %time(i)=data.time;
        if(check.vel_check==1)
            vel_x(:,i)=data.u(start:stop);
            vel_y(:,i)=data.v(start:stop);
            vel_mag(:,i)=sqrt(data.u(start:stop).*data.u(start:stop)+...
                data.v(start:stop).*data.v(start:stop));
        end      
        if(check.wse_check==1)
            wse(:,i)=data.wse(start:stop);
        end
        if(check.dep_check==1)
            dep(:,i)=data.dep(start:stop);
            dep(vel_mag(:,i)==0.0,i)=0.0;
        end
        if(check.sal_check==1)
            sal(:,i)=data.sal(start:stop);
            sal(:,i)=max(sal(:,i),0.0);
        end 
    end
    
    for i=start:stop
        if(check.wse_check==1)
            wse1=spline(time,wse(i,:),time1);
            wse_pct(start:stop,:)=prctile(wse1,pct);
        end
        if(check.dep_check==1)
            dep1=spline(time,dep(i,:),time1);
            dep_pct(start:stop,:)=prctile(dep1,pct);
        end
        if(check.vel_check==1)
            vel_mag1=spline(time,vel_mag(i,:),time1);
            vel_pct(start:stop,:)=prctile(vel_mag1,pct);
        end
        if(check.sal_check==1)
            sal1=spline(time,sal(i,:),time1);
            sal_pct(start:stop,:)=prctile(sal1,pct);
        end
    end
%end

fprintf(1,'Completed percentile analysis\n');

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fprintf(1,'Writing new .da file\n');

fwrite(fid,length(pct),'integer*4');
fwrite(fid,check.np,'integer*4');
fwrite(fid,check.dt,'real*4');
fwrite(fid,check.dep_check,'integer*4');
fwrite(fid,check.wse_check,'integer*4');
fwrite(fid,check.vel_check,'integer*4');
fwrite(fid,check.err_check,'integer*4');
fwrite(fid,check.sal_check,'integer*4');
fwrite(fid,check.vor_check,'integer*4');
fwrite(fid,check.snd_check,'integer*4');
fwrite(fid,check.slt_check,'integer*4');
fwrite(fid,check.smr_check,'integer*4');
fwrite(fid,check.dpl_check,'integer*4');
fwrite(fid,check.bsh_check,'integer*4');
fwrite(fid,check.blt_check,'integer*4');
fwrite(fid,check.bld_check,'integer*4');
fwrite(fid,check.bed_check,'integer*4');
fwrite(fid,check.alt_check,'integer*4');
fwrite(fid,check.alb_check,'integer*4');
fwrite(fid,check.cbp_check,'integer*4');
fwrite(fid,check.number_layers,'integer*4');
fwrite(fid,check.number_sed,'integer*4');

for i=1:length(pct)
    fwrite(fid,pct(i),'float64');
    if(check.dep_check==1)      
        fwrite(fid,dep_pct(1:check.np,i),'float32');
    end
    if(check.wse_check==1)      
        fwrite(fid,wse_pct(1:check.np,i),'float32');
    end
    if(check.vel_check==1)      
        fwrite(fid,zeros(check.np,1),'float32');
        fwrite(fid,vel_pct(1:check.np,i),'float32');
    end
    if(check.sal_check==1)      
        fwrite(fid,sal_pct(1:check.np,i),'float32');
    end    
end
fclose(fid);

fprintf(1,'Completed writing .da file\n');

toc



