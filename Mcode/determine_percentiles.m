function determine_percentiles(da_file,da_out,inc,max_nodes)


tic


% da_file='Mobile_small.da';
% da_out='Mobile_small_pct.da';
% inc=1.0;
fid_dia=fopen('percent.out','w');
fid=fopen(da_out,'w');
check=load_da_stepnew(da_file,0);
%check.nt=10;
%check.wse_check=0;
%check.dep_check=0;
%check.sal_check=0;
pct=[0 1 2 5 10 20 30 40 50 60 70 80 90 95 97 98 99 100];

%nodes=check.np;

%max_nodes=20000;
nodes=min(max_nodes,check.np);

 if(check.np>2.0*max_nodes)
     loops=int64(check.np/max_nodes);
     extra=mod(check.np,max_nodes);
     if(extra/max_nodes>0.5)
         loops=loops-1;
     end
 else
     loops=0;
     extra=0.0;
 end

fprintf(fid_dia,'There will be %i loops with the last loop containing %i nodes\n',loops+1,extra);
%toc
%tic
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

fprintf(fid_dia,'The preallocations has been completed\n');
%toc
%tic
time=zeros(check.nt,1);
%data1=load_da_time_series(da_file,1);
%time=data1.time;
%time1=min(data1.time):inc/24.0:max(data1.time);
%time1=time1';
start=1;
fprintf(fid_dia,'Finished determining Time Series values\n');
%toc
for j=1:loops+1
    %tic
    fprintf(fid_dia,'Starting loop = %i\n',j);
    
    %if(j==loops+1)
    %    nodes1=extra;
    %end
    %start=i+(j-1)*nodes;
    %stop=start+nodes-1;
    %stop=max(stop,check.np);

    %start=1;
    stop=start+nodes-1;
    if(stop>check.np)
        stop=check.np;
        stop1=check.np-start+1;
    else
        stop1=nodes;
    end
    %if(j==loops+1)
    %    stop=extra;
    %else
    %    stop=nodes;
    %end
    for i=1:check.nt     
        if(mod(i,100)==0)
            fprintf(fid_dia,'Reading Time Step Number %i\n',i);
        end
        data=load_da_stepnew(da_file,i);
        time(i)=data.time;
        if(check.vel_check==1)
            vel_x(1:stop1,i)=data.u(start:stop);
            vel_y(1:stop1,i)=data.v(start:stop);
            vel_mag(1:stop1,i)=sqrt(data.u(start:stop).*data.u(start:stop)+...
                data.v(start:stop).*data.v(start:stop));
        end      
        if(check.wse_check==1)
            wse(1:stop1,i)=data.wse(start:stop);
        end
        if(check.dep_check==1)
            dep(1:stop1,i)=data.dep(start:stop);
            dep(vel_mag(1:stop1,i)==0.0,i)=0.0;
        end
        if(check.sal_check==1)
            sal(1:stop1,i)=data.sal(start:stop);
            sal(1:stop1,i)=max(sal(1:stop1,i),0.0);
        end 
    end
    time1=min(time):inc/24.0:max(time);
    fprintf(fid_dia,'Finished Reading Data for loop %i\n',j);
    for i=start:stop
        if(mod(i,10000)==0)
            fprintf(fid_dia,'Determining Percentile for node %i\n',i);
        end
        if(check.wse_check==1)
            wse1=spline(time,wse(i-start+1,:),time1);
            wse_pct(i,:)=prctile(wse1,pct);
        end
        if(check.dep_check==1)
            dep1=spline(time,dep(i-start+1,:),time1);
            dep_pct(i,:)=prctile(dep1,pct);
        end
        if(check.vel_check==1)
            vel_mag1=spline(time,vel_mag(i-start+1,:),time1);
            vel_pct(i,:)=prctile(vel_mag1,pct);
        end
        if(check.sal_check==1)
            sal1=spline(time,sal(i-start+1,:),time1);
            sal_pct(i,:)=prctile(sal1,pct);
        end
    end
    start=stop+1;
    %toc
end
%tic
fprintf(fid_dia,'Completed percentile analysis\n');

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fprintf(fid_dia,'Writing new .da file\n');

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

fprintf(fid_dia,'Completed writing .da file\n');

toc



