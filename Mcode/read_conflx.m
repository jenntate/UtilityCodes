function model=read_conflx(basename,t0,time_units)

tic

tempaaa=basename;
if exist([tempaaa '_conflx.mat'],'file')
    disp('Using pre-read tflx file')
    load([tempaaa '_conflx'])    
else
    fprintf(1,'Reading conflx file \n');
    
    fid=fopen([basename,'_conflx'],'rt');
    
    t0=datenum(t0);
    increment_amount=50;
    %time             = zeros(1,increment_amount);
    count=0;
    number=0;
    loc=1;
    string=zeros(100,1);
    
    s0=fgetl(fid);
    flds=regexp(s0,'\w*','match');
    % Read numeric data
    data=textscan(fid,'%f %f %f %f %f %f %f %f');
    % Convert data and headernames from cell arrays to structure
    s=cell2struct(data,flds,2);
    numstr=unique(s.STRING);
    numgs=unique(s.ITRNS);
    clear s0 flds data
    
%     wat_flux=zeros(200,numstr);
%     sus_flux=zeros(200,numstr);
%     bed_flux=zeros(200,numstr);
%     tot_flux=zeros(200,numstr);
    time=zeros(2000000,1);
    
    %% Parse out Adh Water and Sediment Conflux Data by Strings and Grain size
for k=1:length(numstr)
    in=s.STRING==numstr(k);
    for kk=1:length(numgs);
        in1=s.ITRNS==kk;
        time=s.TIME(in&in1);
        s_flux(:,kk,k)=abs(s.SUSCONFLX(in&in1));
        b_flux(:,kk,k)=abs(s.BEDLOADFLUX(in&in1));
        t_flux(:,kk,k)=abs(s.TOTALSEDFLUX(in&in1));
    end
end
wat_flux(:,:)=s.WATERFLUX(in&in1);
clear k kk in in1
    
for i=1:length(time)
    for j=1:length(numstr)
       sus_flux(i,j)=sum(s_flux(i,:,j));
       bed_flux(i,j)=sum(b_flux(i,:,j));
       tot_flux(i,j)=sum(t_flux(i,:,j));       
    end
end   
clear i j

    tconv=(1.0/86400.0)*time_units;
    model.time=time*tconv+t0;
    model.wat_flux=wat_flux(:,:);
    model.sus_flux=sus_flux(:,:);
    model.bed_flux=bed_flux(:,:);
    model.tot_flux=tot_flux(:,:);
    
    save([tempaaa '_conflx'],'model') 
%     Matrix=horzcat(model.time, model.tot_flux);
%     csvwrite([tempaaa '_conflx_ts'],Matrix)

    
    clear time
    

    
end
     fclose(fid);
toc