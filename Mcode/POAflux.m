%script to perform ADCP and ADCIRC flux estimates in Knik Arm
%

%% Parameters and Input files
%files
%adcp
pnadcp='G:\anchorage\Gary\new_EEV_constant_friction_0.5_0.25_2002\2002';
adcp_transect={'T4','raw0812.mat';...
   'T4','raw0817.mat',;...
   'T4','raw0823.mat'};
%adcirc Permit multiple ADCIRC simulations per plot
pnadcirc='';
adc_simname={'anchorage'};%...%1
   %'k5d';...%2
   %'m5h'};%3
grd={'anchorage.grd'};%...%1
%   'bathy_j5_w_k5c.grd';...%2
%   'M1.grd'};%3
da={'anchorage.da'};%...%1
 %  'bathy_j5_w_k5c_fort63bin_02_GOOD.da';...%2
 %  'm5h.da'};%3
if isequal(length(adc_simname),length(grd),length(da))
   nadc=length(grd);
else
   error('Check ADCIRC simulation specs.  Unequal cell array lengths: grd,da,adc_simname.');
end
% nadc=3; %debugging
fn14=cell(nadc,1);
fnda=cell(nadc,1);
adctime=cell(nadc,1);
fprintf(1,'Loading Grids and simulation times...\n');
for k=1:nadc
   fprintf(1,'Simulation %g of %g\n',k,nadc);
   fn14{k}=fullfile(pnadcirc,grd{k});
   fnda{k}=fullfile(pnadcirc,da{k});
   %load ADCIRC times
   a14(k)=load14(fn14{k});
   adc=load_adcda_pt(fnda{k},0);
   adctime{k}=adc.time;
end
%% transect definitions
load transect
% Find mean transect and load ADCP data
k1=3; %adcp file index
I=find(strcmp({transect.name},adcp_transect{k1,1})); %index into mean transects
load(fullfile(pnadcp,adcp_transect{k1,2}))
%Parameters
z0=5e-3; %hydraulic roughness for bbl
dvcrit=0.40; %Permissable variation in velocity between bins
dicrit=2.5;  %value to flag for bottom echo
btcorrcrit=200; %bottom-track correlation criterion
corrcrit=65;  %correlation criterion for velocities
%%Algorithm
%1. fill in missing ADCP depths from ADCIRC bathymetry
%2. Determine good ADCP data
%   a. bins
%   b. velocity data
%3. compute line flux from ADCP data
%   a. determine local positions
%   b. determine normal vectors
%   c. apply boundary-layer and surface layer (incl. transducer depth)
%   d. integrate flux over depth
%   c. integrate flux over r with dot product of (u,v)*nv
%4. compute flux from ADCIRC data along same transect line
%4a. or compute line flux from ADCIRC for idealized transect
%5. compare fluxes transect to transect (plot)
%%Main Loop
nJ=length(adcp); %number of ADCP transects for this file
vadcp=nan(nJ,1); %pre-allocate vol transport for ADCP
vadcirc=nan(nJ,nadc); %preallocate vol transport for ADCIRC
fprintf(1,'\nProcessing %g transects.\n',nJ);
nbyte=fprintf(1,'Transect: %g',0);
for J=1:length(adcp) %set adcp transect number
bspc=repmat('\b',1,nbyte);
nbyte=fprintf([bspc,'Transect: %g'],J)-nbyte;
nens=length(adcp(J).mtime);
nbin=adcp(J).config.n_cells;
if diff(minmax(adcp(J).mtime))<10/60/24 %transect less than 10 minutes long
   vadcp(J)=NaN;
   vadcirc(J,:)=nan;
   continue
end
%%1) Correct Bottom Track Data and fill gaps
%%Get ADCIRC data for transect (multiple ADCIRC solutions if specified)
for k=1:nadc
   %find ADCIRC times bracketing transect
   I1=find(adctime{k}>=min(adcp(J).mtime) & adctime{k}<=max(adcp(J).mtime));
   if isempty(I1) %skip this transect
      vadcp(J)=NaN;
      vadcirc(J,:)=nan;
      continue
   end 
   I1=unique([minmax(I1)+[-1,1],I1(:)']);
   %get ADCIRC depths at ensemble locations
   [zadc,tri,w]=griddatafast(a14(k).x,a14(k).y,a14(k).dep,...
      adcp(J).nav_longitude,adcp(J).nav_latitude,'Tri',a14(k).tri);
   %get ADCIRC wse at ensemble locations for times I1
   nt=length(I1);
   eta=zeros(nens,nt);
   uadc=zeros(nens,nt);
   vadc=zeros(nens,nt);
   for kk=1:nt
      adc=load_adcda(fnda{k},I1(kk));
      eta(:,kk)=gdfast(adc.eta,tri,w);
      uadc(:,kk)=gdfast(adc.u,tri,w);
      vadc(:,kk)=gdfast(adc.v,tri,w);
   end %k1: time loop
   [ATIME,ENS]=meshgrid(adctime{k}(I1),adcp(J).number);
   adcpeta=interp2(ATIME,ENS,eta,adcp(J).mtime,adcp(J).number);
   if k==1 %save only first adcirc solution for adcp bottom depth correction
      adcdep=zadc+adcpeta-mean(adcp(J).depth); %corrected for trans.depth
   end
   adcirc(k).depth=zadc+adcpeta;
   adcirc(k).u=interp2(ATIME,ENS,uadc,adcp(J).mtime,adcp(J).number);
   adcirc(k).v=interp2(ATIME,ENS,vadc,adcp(J).mtime,adcp(J).number);
end %k: loop to load ADCIRC solutions
%get adcp data
intens=squeeze(mean(adcp(J).intens,2));
corr=squeeze(mean(adcp(J).corr,2));
btrange=mean(adcp(J).bt_range,1);
btcorr=mean(adcp(J).bt_corr,1);
range=adcp(J).config.ranges;
bin=(1:nbin)';
%correct bottom track
inbad=btcorr<btcorrcrit & abs(adcdep-btrange)./adcdep > 0.2;
for k=1:nens %correct bottom track on each ensemble
   if inbad(k) %poor quality bt data
      di=diff([255;intens(:,k)]);
      Igr=find(di>dicrit&bin>4,1,'first');
      if ~isempty(Igr) %replace with intensity estimate
         btrange(k)=range(Igr);
      else %replace with ADCIRC depth
         btrange(k)=adcdep(k); %note: uses last loaded ADCIRC mesh
      end
   end
end   
%%2a) Determine good bins
[in1,in2]=deal(false(nbin,nens));
for k=1:nens
   %filter based on bottom track
   in1(:,k)=range<btrange(k);
   %filter based on echo-intensity gradient
   di=diff([255;intens(:,k)]);
   Igr=find(di>dicrit & range>0.5*btrange(k),1,'first');
   if isempty(Igr),
      in2(:,k)=true;
   else
      in2(1:Igr-1,k)=true;
   end
end
%filter based on correlations
in3=corr>corrcrit;
%apply all filters
in=all(cat(3,in1,in2,in3),3);
%%2b) Determine good velocities
%within set of good bins, look for outliers
u=adcp(J).east_vel;
v=adcp(J).north_vel;
u(~in)=nan;
v(~in)=nan;
[in4,in5]=deal(false(nbin,nens)); %keep for debugging
for k=1:nens
   du=[0;diff(u(:,k))];
   dv=[0;diff(v(:,k))];
   Iu=find(abs(du)>dvcrit);
   Iv=find(abs(dv)>dvcrit);
   %replace velocities
   for kk=1:length(Iu)
      if isempty(find(Iu==Iu(kk)-1, 1)), %if no preceeding bad flag
         u(Iu(kk),k)=nanmean(u(Iu(kk)+[-1,1],k));
         in4(Iu(kk),k)=true;
      end
   end
   for kk=1:length(Iv)
      if isempty(find(Iv==Iv(kk)-1, 1)), %if no preceeding bad flag
         v(Iv(kk),k)=nanmean(v(Iv(kk)+[-1,1],k));
         in5(Iv(kk),k)=true;
      end
   end
end
%%3) Compute line-intergral of ADCP flux
%determine local ensemble and arc positions
[r,af]=dist(adcp(J).nav_latitude,adcp(J).nav_longitude);
[dy,dx]=pol2cart(deg2rad(af),r);
x=[0,cumsum(dx)];
y=[0,cumsum(dy)];
R=[0,cumsum(r)]';
%determine normal vectors
vtm=transect(I).vec;
vatm=[x(end)-x(1),y(end)-y(1)]/abs(x(end)-x(1)+(y(end)-y(1))*i);
atdir=round(dot(vtm,vatm)); %adcp transect direction
dx=gradient(x);
dx([1,end])=dx([2,end-1]);
dy=gradient(y);
dy([1,end])=dy([2,end-1]);
v1=[dx(:),dy(:)]./(abs(dx(:)+dy(:)*i)*[1,1]); %ensemble direction vector
A=[0,-atdir;+atdir,0]; %transformation matrix for normal vector
nv=v1*A; %ensemble normal vector (up-estuary direction)
% vertical integration
tdep=mean(adcp(J).depth);
range2=tdep+range; %bin depth including transducer depth
btrange2=tdep+btrange; %bottrk range corrected for trans depth
uzint=zeros(nens,1);
vzint=zeros(nens,1);
for k=1:nens
   in=~isnan(u(:,k));
   if any(in),
      if btrange2(k)-max(range2(in))<0.01,
         rlog=[];
         ulog=[];
         vlog=[];
      else
         rlog=sort(logspace(log10(z0),log10(btrange2(k)-max(range2(in))),10)',1,'descend'); %distribute 10 points in BBL
         ulog=ulog_pvk(rlog,z0,rlog(1),u(find(in,1,'last'),k));
         vlog=ulog_pvk(rlog,z0,rlog(1),v(find(in,1,'last'),k));
      end
      ri=[0;range2(in);btrange2(k)-rlog(2:end)];
      ui=[u(1,k);u(in,k);ulog(2:end)];
      vi=[v(1,k);v(in,k);vlog(2:end)];
      uzint(k)=trapz(ri,ui);
      vzint(k)=trapz(ri,vi);
   else
      uzint(k)=0;
      vzint(k)=0;
   end
end
%line integration
vadcp(J)=trapz(R,dot([uzint,vzint],nv,2));
for k=1:nadc %line integral-ADCIRC
   vadcirc(J,k)=trapz(R,dot(adcirc(k).depth'*[1,1].*[adcirc(k).u',adcirc(k).v'],nv,2));
end
end %main loop on J
fprintf(1,'\n');
%%Plot results
figure(3)
clear t
for J=1:nJ  %compute median time for set of transects
   t(J)=median(adcp(J).mtime);
end
Ideep=find(btrange==max(btrange));
trideep=tsearch_bc(a14(1).x,a14(1).y,a14(1).tri,adcp(J).nav_longitude(Ideep),adcp(J).nav_latitude(Ideep));
nodedeep=a14(1).tri(trideep,1);
adc=load_adcda_pt(fnda{1},nodedeep);
tlim=minmax(t)+[-1,1]/24;
in=adc.time>=tlim(1) & adc.time<=tlim(2);
ax(1)=subplot(2,1,1);
plot(adc.time(in),adc.eta(in))
grid on
ylabel('\eta, m')
title(sprintf('TRANSECT: %s FILE: %s',adcp_transect{k1,:}))
ax(2)=subplot(2,1,2);
plot(t,vadcp,'bo-',t,vadcirc,'*-')
grid on
legend({strvcat('adcp',adc_simname{:})},'Location','ne','Interpreter','none')
xlabel(sprintf('transect time  %s',datestr(t(1),'(mm/dd/yyyy)')))
ylabel('volume transport rate (dV/dt), m^3 s^{ -1}')
set(ax,'XLim',tlim);
linkaxes(ax,'x')
datetick(ax(1),'x','keeplimits')
datetick(ax(2),'x','keeplimits')
%%save results to data struct
sdat(k1).t=t;
sdat(k1).eta=interp1(adc.time(in),adc.eta(in),t);
sdat(k1).vadcp=vadcp;
for k=1:length(adc_simname)
   sdat(k1).(regexprep(adc_simname{k},{' ','-'},{'_',''}))=vadcirc(:,k);
end
%% print to PDF file
% set(gcf,'PaperPositionMode','auto',...
%    'PaperOrientation','landscape')
print('-dpdf',sprintf('%s_%s_flux.pdf',...
   adcp_transect{k1,1},...
   regexp(adcp_transect{k1,2},'\d*','match','once')))
