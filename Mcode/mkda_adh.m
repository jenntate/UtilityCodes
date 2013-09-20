function mkda_adh(basename,t0,attributes)
%
% MKDA_ADH Function to create ADH direct access binary file
%          from ADH output files
% SYNTAX:  mk_adcda(basename,t0,attributes)
%    basename = string argument containing base filename ADH root filename
%               Note basename must be the same for both files.
%         t0  = time of simulation start (datenum format or string), IF t0
%               is not inputted then the solution file time will be used 
%               with no date format
%
%   attributes = string containing the parameters to be included in the da
%   file seperated by spaces.  These are:
%
%   dep = depth solution
%   wse = water surface elevation solution
%   vel = velocity vector and magnitude solution
%   sal = salinity solution
%   vor = vorticity solution
%   snd = sand concentration solution
%   cla = silt concentration solution
%   err = error solution
%   dpl = bed displacement solution
%   alt = active layer thickness
%   ald = active layer distribution
%   blt = bed layer thickness
%   bld = bed layer distribution
%   cbp = cohesive bed properties
%   bsh = bed shear stress solution
%   bed = bedload vector and magnitude solution
%   smr = sediment mass residual
%
%   EXAMPLE INPUT:
%   mkda_adh('Mobile','01/01/2008 00:00:00','dep wse vel sal')
%
%   Finished 04/28/2010
%

%% used to determine time for code execution

tic

%% checks the inputted model start time

if(nargin >= 2)
  if ischar(t0),
     try
        t0=datenum(t0);
     catch ME
        fprintf(1,'The date string entered cannot be converted.\n')
        fprintf(1,'Try mm/dd/yyyy HH:MM:SS.\n\n')
        rethrow(ME)
     end
  end
  
  % check date and issue warning if necessary
  
  if abs(now-t0)>100*365.25
   warning('MK_ADCDA:Date_Check','Date entered is more than 100 years from today. Is this correct?')
  end
  
else
   t0=0.0;
   tconv=1.0;
end

%% checks the number of function inputs

dep_check=0;
wse_check=0;
vel_check=0;
err_check=0;
sal_check=0;
vor_check=0;
snd_check=0;
slt_check=0;
smr_check=0;
dpl_check=0;
bsh_check=0;
blt_check=0;
bld_check=0;
bed_check=0;
alt_check=0;
alb_check=0;
cbp_check=0;
salt_number=0;
vor_number=0;
snd_number=zeros(10,1);
slt_number=zeros(10,1);
snds=1;
slts=1;
number_layers=0;


read_bc=0;
temp1=sscanf(attributes,'%s');

for i=1:length(temp1)/3
   test=temp1((1+(i-1)*3):3*i);
   switch lower(test)
       case {'sal'}
           read_bc=1;
           break
       case {'vor'}
           read_bc=1;
           break
       case {'snd'}
           read_bc=1;
           break
       case {'cla'}
           read_bc=1;
           break
       case {'blt'}
           read_bc=1;
           break
       case {'bld'}
           read_bc=1;
           break
       case {'cbp'}
           read_bc=1;
           break
       otherwise
           read_bc=0;
   end
end

if(read_bc==1)
  fid_bc=fopen([basename,'.bc'],'rt'); 
  while ~feof(fid_bc)
    hold=fgetl(fid_bc);
    card=sscanf(hold,'%s',1);
    if(isequal(card,'END'))
       break;
    end
    if(isequal(card,'CN'))
      trans=sscanf(hold,'%*s %s',1);
      if(isequal(trans,'SAL'))
         salt_number=sscanf(hold,'%*s %*s %i',1);
      elseif(isequal(trans,'VOR'))
         vor_number=sscanf(hold,'%*s %*s %i',1);
      elseif(isequal(trans,'SND'))
         snd_number(snds)=sscanf(hold,'%*s %*s %i',1);
         snds=snds+1;
       elseif(isequal(trans,'CLA'))
         slt_number(slts)=sscanf(hold,'%*s %*s %i',1);
         slts=slts+1;
      end
    elseif(isequal(card,'MP'))
      trans=sscanf(hold,'%*s %s',1);
      if(isequal(trans,'NBL'))
        number_layers = sscanf(hold,'%*s %*s %i',1);
      end
    end
  end
end

number_sed=(slts-1)+(snds-1); 
number_slt=slts-1;
number_snd=snds-1;
output_time=-1;
np=-1;
sed=0;
hy=0;

for i=1:length(temp1)/3
   test=temp1((1+(i-1)*3):3*i);
   switch lower(test)
       case {'dep'}
           hy=1;
           if(wse_check == 0)
             fid63=fopen([basename,'_dep.dat'],'rt');
             if(~fid63)
               fprintf(1,'\nERROR Depth File NOT FOUND\n\n');
               dep_check = 0;
               wse_check = -1;
             else
               dep=read_header(fid63);
               fprintf(1,'Read Header File for Depth\n')
               dep.depth=zeros(dep.np,1);
               test_dep=dep.test;
               if(output_time < 0)
                  output_time = dep.output_time; 
               else
                  if(~isequal(output_time,dep.output_time))
                     fprintf(1,'\nERROR TIME UNITS ARE NOT CONSISTANT BETWEEN FILES\n\n'); 
                     error('ERROR TIME UNITS ARE NOT CONSISTANT BETWEEN FILES');
                  end
               end
               if(np < 0)
                  np = dep.np; 
               elseif(~isequal(np,dep.np))
                  fprintf(1,'\nERROR NUMBER OF NODES ARE NOT CONSISTANT BETWEEN FILES\n\n'); 
                  error('ERROR NUMBER OF NODES ARE NOT CONSISTANT BETWEEN FILES');
               end              
               dep_check = 1;
             end
           else
               dep_check = 1;
           end
       case {'wse'}
           hy=1;
           if(dep_check == 0)
             fid63=fopen([basename,'_dep.dat'],'rt');
             if(~fid63)
               fprintf(1,'\nERROR Depth File NOT FOUND\n\n');
               dep_check = 0;
               wse_check = -1;
             else
               dep=read_header(fid63);
               fprintf(1,'Read Header File for Depth and Water Surface Elevation\n')
               dep.depth=zeros(dep.np,1);
               test_dep=dep.test;
               wse=zeros(dep.np,1);
               if(output_time < 0)
                  output_time = dep.output_time; 
               elseif(~isequal(output_time,dep.output_time))
                  fprintf(1,'\nERROR TIME UNITS ARE NOT CONSISTANT BETWEEN FILES\n\n'); 
                  error('ERROR TIME UNITS ARE NOT CONSISTANT BETWEEN FILES');
               end
               if(np < 0)
                  np = dep.np; 
               elseif(~isequal(np,dep.np))
                  fprintf(1,'\nERROR NUMBER OF NODES ARE NOT CONSISTANT BETWEEN FILES\n\n'); 
                  error('ERROR NUMBER OF NODES ARE NOT CONSISTANT BETWEEN FILES');
               end                
               wse_check = 1;
             end      
           else               
             if(wse_check == -1)
               fprintf(1,'\nERROR Depth File NOT FOUND\n\n');
               wse_check = 0;
             else
               fprintf(1,'Read Header File for Water Surface Elevation\n');
               wse=zeros(dep.np,1);
               wse_check = 1;
             end
           end
       case {'vel'}
           hy=1;
           fid64=fopen([basename,'_ovl.dat'],'rt');
           if(~fid64)
             fprintf(1,'\nERROR Velocity File NOT FOUND\n\n');
             vel_check = 0;
           else
             ovl=read_header(fid64);
             fprintf(1,'Read Header File for Velocity\n')
             ovl.velocity=zeros(3,ovl.np);
             test_ovl=ovl.test;
             if(output_time < 0)
                output_time = ovl.output_time; 
             elseif(~isequal(output_time,ovl.output_time))
                fprintf(1,'\nERROR TIME UNITS ARE NOT CONSISTANT BETWEEN FILES\n\n'); 
                error('ERROR TIME UNITS ARE NOT CONSISTANT BETWEEN FILES');
             end  
             if(np < 0)
                np = ovl.np; 
             elseif(~isequal(np,ovl.np))
                fprintf(1,'\nERROR NUMBER OF NODES ARE NOT CONSISTANT BETWEEN FILES\n\n'); 
                error('ERROR NUMBER OF NODES ARE NOT CONSISTANT BETWEEN FILES');
             end                
             vel_check = 1;
           end
       case {'err'}
           hy=1;
           fid_err=fopen([basename,'_err.dat'],'rt');
           if(~fid_err)
              fprintf(1,'\nERROR Error File NOT FOUND\n\n');
              err_check = 0;
           else
              err=read_header(fid_err);
              fprintf(1,'Read Header File for Error\n')
              err.error=zeros(err.np,1);  
              test_err=err.test;
              if(output_time < 0)
                 output_time = err.output_time; 
              elseif(~isequal(output_time,err.output_time))
                 fprintf(1,'\nERROR TIME UNITS ARE NOT CONSISTANT BETWEEN FILES\n\n'); 
                 error('ERROR TIME UNITS ARE NOT CONSISTANT BETWEEN FILES');
              end
              if(np < 0)
                 np = err.np; 
              else
                 if(~isequal(np,err.np))
                    fprintf(1,'\nERROR NUMBER OF NODES ARE NOT CONSISTANT BETWEEN FILES\n\n'); 
                    error('ERROR NUMBER OF NODES ARE NOT CONSISTANT BETWEEN FILES');
                 end
              end 
              err_check = 1;
           end
       case {'sal'}
           hy=1;
           if(salt_number == 0)
             fprintf(1,'\nERROR Salinity Calculations were NOT PERFORMED\n\n');
             sal_check = 0;
           else      
             fid_salt=fopen([basename,'_con',int2str(salt_number),'.dat'],'rt');
             if(~fid_salt)
               fprintf(1,'\nERROR Salinity Concentration File NOT FOUND\n\n');
               sal_check = 0;
             else
               salt=read_header(fid_salt);
               fprintf(1,'Read Header File for Salinity\n')
               salt.salinity=zeros(salt.np,1);  
               test_salt=salt.test;
               if(output_time < 0)
                  output_time = salt.output_time; 
               elseif(~isequal(output_time,salt.output_time))
                  fprintf(1,'\nERROR TIME UNITS ARE NOT CONSISTANT BETWEEN FILES\n\n'); 
                  error('ERROR TIME UNITS ARE NOT CONSISTANT BETWEEN FILES');
               end               
               if(np < 0)
                  np = salt.np; 
               elseif(~isequal(np,salt.np))
                     fprintf(1,'\nERROR NUMBER OF NODES ARE NOT CONSISTANT BETWEEN FILES\n\n'); 
                     error('ERROR NUMBER OF NODES ARE NOT CONSISTANT BETWEEN FILES');
               end
               sal_check = 1;
             end
           end
       case {'vor'}
           hy=1;
           if(vor_number == 0)
             fprintf(1,'\nERROR Vorticity Calculations were NOT PERFORMED\n\n');
             vor_check = 0;
           else
             fid_vor=fopen([basename,'_con',int2str(vor_number),'.dat'],'rt');
             if(~fid_vor)
               fprintf(1,'\nERROR Vorticity Concentration File NOT FOUND\n\n');
               vor_check = 0;
             else  
               vor=read_header(fid_vor);
               fprintf(1,'Read Header File for Vorticity\n')
               vor.vorticity=zeros(vor.np,1);  
               test_vor=vor.test;
               if(output_time < 0)
                  output_time = vor.output_time; 
               elseif(~isequal(output_time,vor.output_time))
                  fprintf(1,'\nERROR TIME UNITS ARE NOT CONSISTANT BETWEEN FILES\n\n'); 
                  error('ERROR TIME UNITS ARE NOT CONSISTANT BETWEEN FILES');
               end  
               if(np < 0)
                  np = vor.np; 
               elseif(~isequal(np,vor.np))
                     fprintf(1,'\nERROR NUMBER OF NODES ARE NOT CONSISTANT BETWEEN FILES\n\n'); 
                     error('ERROR NUMBER OF NODES ARE NOT CONSISTANT BETWEEN FILES');
               end               
               vor_check = 1;
             end
           end
       case {'snd'}   
             sed=1;
             if(number_snd == 0)
               fprintf(1,'\nERROR Sand Calculations were NOT PERFORMED\n\n');
               snd_check = 0;
             else      
               snd_check = number_snd;
               fid_snd=zeros(snd_check,1);
               test_snd=cell(snd_check,1);
               for q=1:snd_check
                 fid_snd(q)=fopen([basename,'_con',int2str(snd_number(q)),'.dat'],'rt');
                 if(~fid_snd(q))
                   fprintf(1,'\nERROR Sand %i Concentration File NOT FOUND\n\n',q);
                   snd_check = q - 1;
                   break;
                 else        
                   snd=read_header(fid_snd(q));
                   fprintf(1,'Read Header File for Sands %i\n',q)
                   snd.sands(q,:)=zeros(snd.np,1);
                   if(output_time < 0)
                     output_time = snd.output_time; 
                   elseif(~isequal(output_time,snd.output_time))
                     fprintf(1,'\nERROR TIME UNITS ARE NOT CONSISTANT BETWEEN FILES\n\n'); 
                     error('ERROR TIME UNITS ARE NOT CONSISTANT BETWEEN FILES');
                   end   
                   if(np < 0)
                     np = snd.np; 
                   elseif(~isequal(np,snd.np))
                     fprintf(1,'\nERROR NUMBER OF NODES ARE NOT CONSISTANT BETWEEN FILES\n\n'); 
                     error('ERROR NUMBER OF NODES ARE NOT CONSISTANT BETWEEN FILES');
                   end                  
                   test_snd{q}=snd.test;
                 end
               end
             end
       case {'cla'}
              sed=1;
              if(number_slt == 0)
                 fprintf(1,'\nERROR Clay Calculations were NOT PERFORMED\n\n');
                 slt_check = 0;
              else            
                 slt_check = number_slt;
                 fid_slt=zeros(slt_check,1);
                 test_slt=cell(slt_check,1);
                 for q=1:slt_check
                   fid_slt(q)=fopen([basename,'_con',int2str(slt_number(q)),'.dat'],'rt');
                   if(~fid_slt(q))
                     fprintf(1,'\nERROR Clay %i Concentration File NOT FOUND\n\n',q);
                     slt_check = q - 1;
                   else        
                     slt=read_header(fid_slt(q));
                     fprintf(1,'Read Header File for Clays %i\n',q)
                     slt.silts(q,:)=zeros(slt.np,1);
                     test_slt{q}=slt.test;
                     if(output_time < 0)
                       output_time = slt.output_time; 
                     elseif(~isequal(output_time,slt.output_time))
                       fprintf(1,'\nERROR TIME UNITS ARE NOT CONSISTANT BETWEEN FILES\n\n'); 
                       error('ERROR TIME UNITS ARE NOT CONSISTANT BETWEEN FILES');
                     end
                     if(np < 0)
                       np = slt.np; 
                     elseif(~isequal(np,slt.np))
                       fprintf(1,'\nERROR NUMBER OF NODES ARE NOT CONSISTANT BETWEEN FILES\n\n'); 
                       error('ERROR NUMBER OF NODES ARE NOT CONSISTANT BETWEEN FILES');
                     end                     
                   end
                 end
              end
       case {'smr'}
             sed=1;
             fid_smr=fopen([basename,'_smr.dat'],'rt');
             if(~fid_smr)
               fprintf(1,'\nERROR Sediment Mass Residual File NOT FOUND\n\n');
               smr_check = 0;
             else
               smr=read_header(fid_smr);
               fprintf(1,'Read Header File for Sediment Mass Residual\n')
               smr.value=zeros(number_sed,smr.np);
               test_smr=smr.test;
               smr_check = number_sed;
               if(output_time < 0)
                  output_time = smr.output_time; 
               elseif(~isequal(output_time,smr.output_time))
                  fprintf(1,'\nERROR TIME UNITS ARE NOT CONSISTANT BETWEEN FILES\n\n'); 
                  error('ERROR TIME UNITS ARE NOT CONSISTANT BETWEEN FILES');
               end
               if(np < 0)
                  np = smr.np; 
               elseif(~isequal(np,smr.np))
                     fprintf(1,'\nERROR NUMBER OF NODES ARE NOT CONSISTANT BETWEEN FILES\n\n'); 
                     error('ERROR NUMBER OF NODES ARE NOT CONSISTANT BETWEEN FILES');
               end               
             end
       case {'dpl'}
             sed=1;
             fid_dpl=fopen([basename,'_dpl.dat'],'rt');
             if(~fid_dpl)
               fprintf(1,'\nERROR Bed Displacement File NOT FOUND\n\n');
               dpl_check = 0;
             else
               dpl=read_header(fid_dpl);
               fprintf(1,'Read Header File for Bed Displacement\n')
               dpl.displacement=zeros(dpl.np,1);
               test_dpl=dpl.test;
               dpl_check = 1;
               if(output_time < 0)
                  output_time = dpl.output_time; 
               elseif(~isequal(output_time,dpl.output_time))
                     fprintf(1,'\nERROR TIME UNITS ARE NOT CONSISTANT BETWEEN FILES\n\n'); 
                     error('ERROR TIME UNITS ARE NOT CONSISTANT BETWEEN FILES');
               end
               if(np < 0)
                  np = dpl.np; 
               elseif(~isequal(np,dpl.np))
                     fprintf(1,'\nERROR NUMBER OF NODES ARE NOT CONSISTANT BETWEEN FILES\n\n'); 
                     error('ERROR NUMBER OF NODES ARE NOT CONSISTANT BETWEEN FILES');
               end               
             end
       case {'bsh'}
             sed=1;
             fid_bsh=fopen([basename,'_bsh.dat'],'rt');
             if(~fid_bsh)
               fprintf(1,'\nERROR Bed Shear File NOT FOUND\n\n');
               bsh_check = 0;
             else
               bsh=read_header(fid_bsh);
               fprintf(1,'Read Header File for Bed Shear\n')
               bsh.shear=zeros(bsh.np,1);
               test_bsh=bsh.test;           
               if(output_time < 0)
                  output_time = bsh.output_time; 
               elseif(~isequal(output_time,bsh.output_time))
                     fprintf(1,'\nERROR TIME UNITS ARE NOT CONSISTANT BETWEEN FILES\n\n'); 
                     error('ERROR TIME UNITS ARE NOT CONSISTANT BETWEEN FILES');
               end
               if(np < 0)
                  np = bsh.np; 
               elseif(~isequal(np,bsh.np))
                     fprintf(1,'\nERROR NUMBER OF NODES ARE NOT CONSISTANT BETWEEN FILES\n\n'); 
                     error('ERROR NUMBER OF NODES ARE NOT CONSISTANT BETWEEN FILES');
               end               
               bsh_check = 1;
             end
       case {'blt'}
             sed=1;
             blt_check = number_layers;
             fid_blt=zeros(blt_check,1);
             test_blt=cell(blt_check,1);
             for q=1:blt_check
               fid_blt(q)=fopen([basename,'_blt',int2str(q),'.dat'],'rt');
               if(~fid_blt(q))
                 fprintf(1,'\nERROR BED LAYER THICKNESS %i File NOT FOUND\n\n',q);
                 blt_check = q - 1;
                 break;
               else
                 blt=read_header(fid_blt(q));
                 fprintf(1,'Read Header File for Bed Layer Thickness %i\n',q)
                 blt.value(q,:)=zeros(blt.np,1);
                 test_blt{q}=blt.test;
                 if(output_time < 0)
                   output_time = blt.output_time; 
                 elseif(~isequal(output_time,blt.output_time))
                     fprintf(1,'\nERROR TIME UNITS ARE NOT CONSISTANT BETWEEN FILES\n\n'); 
                     error('ERROR TIME UNITS ARE NOT CONSISTANT BETWEEN FILES');
                 end
                 if(np < 0)
                   np = blt.np; 
                 elseif(~isequal(np,blt.np))
                    fprintf(1,'\nERROR NUMBER OF NODES ARE NOT CONSISTANT BETWEEN FILES\n\n'); 
                    error('ERROR NUMBER OF NODES ARE NOT CONSISTANT BETWEEN FILES');
                 end                 
               end     
             end
       case {'bld'}
           sed=1;
           bld_check = number_layers;
           fid_bld=zeros(bld_check,1);
           test_bld=cell(bld_check,1);
           for q=1:bld_check
             fid_bld(q)=fopen([basename,'_bld',int2str(q),'.dat'],'rt');
             if(~fid_bld(q))
               fprintf(1,'\nERROR BED LAYER DISTRIBUTION %i File NOT FOUND\n\n',q);
               bld_check = q - 1;
             else
               bld=read_header(fid_bld(q));
               fprintf(1,'Read Header File for Bed Layer Distribution %i\n',q)
               for j=1:number_sed
                 bld.bed(q,j,:)=zeros(bld.np,1);
               end
               test_bld{q}=bld.test;
               if(output_time < 0)
                  output_time = bld.output_time; 
               elseif(~isequal(output_time,bld.output_time))
                     fprintf(1,'\nERROR TIME UNITS ARE NOT CONSISTANT BETWEEN FILES\n\n'); 
                     error('ERROR TIME UNITS ARE NOT CONSISTANT BETWEEN FILES');
               end
               if(np < 0)
                  np = bld.np; 
               elseif(~isequal(np,bld.np))
                     fprintf(1,'\nERROR NUMBER OF NODES ARE NOT CONSISTANT BETWEEN FILES\n\n'); 
                     error('ERROR NUMBER OF NODES ARE NOT CONSISTANT BETWEEN FILES');
               end               
             end
           end
       case {'bed'}
           sed=1;
           fid_bed=fopen([basename,'_bedload.dat'],'rt');
           if(~fid_bed)
             fprintf(1,'\nERROR BEDLOAD File NOT FOUND\n\n');
             bed_check = 0;
           else
             bed=read_header(fid_bed);
             fprintf(1,'Read Header File for Bedload\n')
             bed.value=zeros(3,bed.np);
             test_bed=bed.test;
             if(output_time < 0)
                output_time = bed.output_time; 
             elseif(~isequal(output_time,bed.output_time))
                   fprintf(1,'\nERROR TIME UNITS ARE NOT CONSISTANT BETWEEN FILES\n\n'); 
                   error('ERROR TIME UNITS ARE NOT CONSISTANT BETWEEN FILES');
             end
             if(np < 0)
                np = bed.np; 
             elseif(~isequal(np,bed.np))
                   fprintf(1,'\nERROR NUMBER OF NODES ARE NOT CONSISTANT BETWEEN FILES\n\n'); 
                   error('ERROR NUMBER OF NODES ARE NOT CONSISTANT BETWEEN FILES');
             end             
             bed_check = 1;
           end
       case {'alt'}
           sed=1;
           fid_alt=fopen([basename,'_alt.dat'],'rt');
           if(~fid_alt)
             fprintf(1,'\nERROR ACTIVE LAYER THICKNESS File NOT FOUND\n\n');
             alt_check = 0;
           else
             alt=read_header(fid_alt);
             fprintf(1,'Read Header File for Active Layer Thickness\n')
             alt.value=zeros(alt.np,1);
             test_alt=alt.test;           
             if(output_time < 0)
                output_time = alt.output_time; 
             elseif(~isequal(output_time,alt.output_time))
                   fprintf(1,'\nERROR TIME UNITS ARE NOT CONSISTANT BETWEEN FILES\n\n'); 
                   error('ERROR TIME UNITS ARE NOT CONSISTANT BETWEEN FILES');
             end
             if(np < 0)
                np = alt.np; 
             elseif(~isequal(np,alt.np))
                fprintf(1,'\nERROR NUMBER OF NODES ARE NOT CONSISTANT BETWEEN FILES\n\n'); 
                error('ERROR NUMBER OF NODES ARE NOT CONSISTANT BETWEEN FILES');
             end             
             alt_check = 1;
           end
       case {'ald'}
           sed=1;
           fid_alb=fopen([basename,'_ald.dat'],'rt');
           if(~fid_alb)
             fprintf(1,'\nERROR ACTIVE LAYER DISTRIBUTION File NOT FOUND\n\n');
             alb_check = 0;
           else
             alb=read_header(fid_alb);
             fprintf(1,'Read Header File for Active Layer Distribution\n')
             alb.value=zeros(number_sed, alb.np);
             test_alb=alb.test;           
             alb_check = number_sed;
             if(output_time < 0)
                output_time = alb.output_time; 
             elseif(~isequal(output_time,alb.output_time))
                   fprintf(1,'\nERROR TIME UNITS ARE NOT CONSISTANT BETWEEN FILES\n\n'); 
                   error('ERROR TIME UNITS ARE NOT CONSISTANT BETWEEN FILES');
             end
             if(np < 0)
                np = alb.np; 
             elseif(~isequal(np,alb.np))
                fprintf(1,'\nERROR NUMBER OF NODES ARE NOT CONSISTANT BETWEEN FILES\n\n'); 
                error('ERROR NUMBER OF NODES ARE NOT CONSISTANT BETWEEN FILES');
             end             
           end
       case {'cbp'}
           sed=1;
           cbp_check = number_layers;
           fid_cbp=zeros(cbp_check,1);
           test_cbp=cell(cbp_check,1);
           for q=1:cbp_check
             fid_cbp(q)=fopen([basename,'_cbp',int2str(q),'.dat'],'rt');
             if(~fid_cbp(q))
               fprintf(1,'\nERROR COHESIVE BED PROPERTIES %i File NOT FOUND\n\n',q);
               cbp_check = q - 1;
               break;
             else
               cbp=read_header(fid_cbp(q));
               fprintf(1,'Read Header File for Cohesive Bed Properties %i\n',q)
               cbp.ces(q,:)=zeros(cbp.np,1);
               cbp.den(q,:)=zeros(cbp.np,1);
               cbp.erc(q,:)=zeros(cbp.np,1);
               cbp.ere(q,:)=zeros(cbp.np,1);
               test_cbp{q}=cbp.test;
               if(output_time < 0)
                  output_time = cbp.output_time; 
               elseif(~isequal(output_time,cbp.output_time))
                     fprintf(1,'\nERROR TIME UNITS ARE NOT CONSISTANT BETWEEN FILES\n\n'); 
                     error('ERROR TIME UNITS ARE NOT CONSISTANT BETWEEN FILES');
               end
               if(np < 0)
                  np = cbp.np; 
               elseif(~isequal(np,cbp.np))
                  fprintf(1,'\nERROR NUMBER OF NODES ARE NOT CONSISTANT BETWEEN FILES\n\n'); 
                  error('ERROR NUMBER OF NODES ARE NOT CONSISTANT BETWEEN FILES');
               end               
             end           
           end
       otherwise
           fprintf(1,'\nINVALID ENTRY %s NOT RECOGNIZED\n',test)
   end        
end


%% specifies the time conversion amount based on the solution file time
%  units
if(nargin >= 2)
  if(output_time == 0)
      fprintf(1,'Solutin files are in SECONDS\n')
      tconv=1/3600/24;  %convert seconds to days
  elseif(output_time == 1)
      fprintf(1,'Solutin files are in MINUTES\n')    
      tconv=1/60/24;   %convert minutes to days   
  elseif(output_time == 2)
      fprintf(1,'Solutin files are in HOURS\n')
      tconv=1/24;       %convert hours to days
  elseif(output_time == 3)
      fprintf(1,'Solutin files are in DAYS\n')
      tconv=1;          %convert days to days
  elseif(output_time == 4)
      fprintf(1,'Solutin files are in WEEKS\n')
      tconv=1*7;        %convert weeks to days
  end
end

%% read the ADH grid file

grid=read_adh_grid([basename '.3dm']);

%% open the outputted da file

fid=fopen([basename,'.da'],'wb','n');

fprintf(1,'WRITING da FILE\n')

count=1;
nt=0;
dt=0;

%% write the da file header information (number of time steps, nodes, and
%  time step size)

fwrite(fid,nt,'integer*4');
fwrite(fid,np,'integer*4');
fwrite(fid,dt,'real*4');
fwrite(fid,dep_check,'integer*4');
fwrite(fid,wse_check,'integer*4');
fwrite(fid,vel_check,'integer*4');
fwrite(fid,err_check,'integer*4');
fwrite(fid,sal_check,'integer*4');
fwrite(fid,vor_check,'integer*4');
fwrite(fid,snd_check,'integer*4');
fwrite(fid,slt_check,'integer*4');
fwrite(fid,smr_check,'integer*4');
fwrite(fid,dpl_check,'integer*4');
fwrite(fid,bsh_check,'integer*4');
fwrite(fid,blt_check,'integer*4');
fwrite(fid,bld_check,'integer*4');
fwrite(fid,bed_check,'integer*4');
fwrite(fid,alt_check,'integer*4');
fwrite(fid,alb_check,'integer*4');
fwrite(fid,cbp_check,'integer*4');
fwrite(fid,number_layers,'integer*4');
fwrite(fid,number_sed,'integer*4');

%% loop over the depth and velocity solution files outputting the values to
%  the binary da file

start=0;

while 1 > 0

  %  Reading the time for each depth solution time step
  if(dep_check > 0 || wse_check > 0)
   
    compare=sscanf(test_dep,'%s',1);
    if(isequal(compare,'ENDDS'))
      fprintf(1,'END OF DEPTH SOLUTION FILE REACHED\n')
      break
    end  
    dep.time(count)=sscanf(test_dep,'%*2s %*i %f',1);
    fprintf(1,'Time = %f\n',dep.time(count))
    dep.depth=fscanf(fid63,'%f',[1,dep.np]);
    if(feof(fid63))
      fprintf(1,'END OF DEPTH SOLUTION FILE REACHED\n')      
      break
    end  
    fgetl(fid63);
    test_dep=fgetl(fid63);
    %  converts the depth solution to a water surface elevation solution
    if(wse_check > 0)
      for n=1:dep.np
          wse(n)=dep.depth(n)+grid.z(n);
        if(dep.depth(n) <= 0)
            wse(n) = -999999.0;
        end
      end
    end
  end
  if(vel_check > 0)
    compare=sscanf(test_ovl,'%s',1);
    if(isequal(compare,'ENDDS'))
      fprintf(1,'END OF VELOCITY SOLUTION FILE REACHED\n')
      break
    end
    ovl.time(count)=sscanf(test_ovl,'%*s %*i %f',1);
    ovl.velocity=fscanf(fid64,'%f %f %f',[3,ovl.np]);
    if(feof(fid64))
      fprintf(1,'END OF VELOCITY SOLUTION FILE REACHED\n')      
      break
    end
    fgetl(fid64);
    test_ovl=fgetl(fid64);
  end
  if(err_check > 0)
    compare=sscanf(test_err,'%s',1);
    if(isequal(compare,'ENDDS'))
      fprintf(1,'END OF ERROR SOLUTION FILE REACHED\n')
      break
    end
    err.time(count)=sscanf(test_err,'%*2s %*i %f',1);
    err.error=fscanf(fid_err,'%f',[1,err.np]);
    if(feof(fid_err))
      fprintf(1,'END OF ERROR SOLUTION FILE REACHED\n')      
      break
    end  
    fgetl(fid_err);
    test_err=fgetl(fid_err);
  end
  if(sal_check > 0)
    compare=sscanf(test_salt,'%s',1);
    if(isequal(compare,'ENDDS'))
      fprintf(1,'END OF SALINITY SOLUTION FILE REACHED\n')
      break
    end
    salt.time(count)=sscanf(test_salt,'%*2s %*i %f',1);
    salt.salinity=fscanf(fid_salt,'%f',[1,salt.np]);
    if(feof(fid_salt))
      fprintf(1,'END OF SALINITY SOLUTION FILE REACHED\n')      
      break
    end  
    fgetl(fid_salt);
    test_salt=fgetl(fid_salt);
  end  
  if(vor_check > 0)
    compare=sscanf(test_vor,'%s',1);
    if(isequal(compare,'ENDDS'))
      fprintf(1,'END OF VORTICITY SOLUTION FILE REACHED\n')
      break
    end
    vor.time(count)=sscanf(test_vor,'%*2s %*i %f',1);
    vor.vorticity=fscanf(fid_vor,'%f',[1,vor.np]);
    if(feof(fid_vor))
      fprintf(1,'END OF VORTICITY SOLUTION FILE REACHED\n')      
      break
    end  
    fgetl(fid_vor);
    test_vor=fgetl(fid_vor);
  end

  if(snd_check > 0)
    for i=1:snd_check
      compare=sscanf(test_snd{i},'%s',1);
      if(isequal(compare,'ENDDS'))
        fprintf(1,'END OF SAND %i SOLUTION FILE REACHED\n',i)
        break
      end
      snd.time(i,count)=sscanf(test_snd{i},'%*2s %*i %f',1);
      hold=textscan(fid_snd(i),'%f %f %f',snd.np);
      snd.sand(i,:)=hold{1};
      snd.rouse(i,:)=hold{2};
      snd.bdma(i,:)=hold{3};
      if(feof(fid_snd(i)))
        fprintf(1,'END OF SAND SOLUTION FILE REACHED\n')      
        break
      end  
      fgetl(fid_snd(i));
      test_snd{i}=fgetl(fid_snd(i));
    end
  end  
  if(slt_check > 0)
    for i=1:slt_check
      compare=sscanf(test_slt{i},'%s',1);
      if(isequal(compare,'ENDDS'))
        fprintf(1,'END OF CLAY %i SOLUTION FILE REACHED\n',i)
        break
      end        
      slt.time(i,count)=sscanf(test_slt{i},'%*2s %*i %f',1);
      hold=fscanf(fid_slt(i),'%f %*f %*f',[1,slt.np]);
      slt.sand(i,1:snd.np)=hold(1,:);
      slt.rouse(i,1:snd.np)=hold(2,:);
      slt.bdma(i,1:snd.np)=hold(3,:);
      if(feof(fid_slt(i)))
        fprintf(1,'END OF CLAY SOLUTION FILE REACHED\n')      
        break
      end  
      fgetl(fid_slt(i));
      test_slt{i}=fgetl(fid_slt(i));
    end
  end    
  
  if(smr_check > 0)
    compare=sscanf(test_smr,'%s',1);
    if(isequal(compare,'ENDDS'))
      fprintf(1,'END OF SEDIMENT MASS RESIDUAL SOLUTION FILE REACHED\n')
      break
    end      
    smr.time(count)=sscanf(test_smr,'%*2s %*i %f',1); 
    holder=fscanf(fid_smr,'%f',[1,smr.np*number_sed]);
    total=1;
    for i=1:length(holder)
       if(mod(i,number_sed)==0)
          smr.value(number_sed,total)=holder(i);
          total=total+1;
       else
          smr.value(mod(i,number_sed),total)=holder(i);
       end
    end
    if(feof(fid_smr))
      fprintf(1,'END OF SEDIMENT MASS RESIDUAL SOLUTION FILE REACHED\n')      
      break
    end  
    fgetl(fid_smr);
    test_smr=fgetl(fid_smr);
  end  
  if(dpl_check > 0)
    compare=sscanf(test_dpl,'%s',1);
    if(isequal(compare,'ENDDS'))
      fprintf(1,'END OF DISPLACEMENT SOLUTION FILE REACHED\n')
      break
    end      
    dpl.time(count)=sscanf(test_dpl,'%*2s %*i %f',1);
    dpl.displacement=fscanf(fid_dpl,'%f',[1,dpl.np]);
    if(feof(fid_dpl))
      fprintf(1,'END OF DISPLACEMENT SOLUTION FILE REACHED\n')      
      break
    end  
    fgetl(fid_dpl);
    test_dpl=fgetl(fid_dpl);
  end   
  if(bsh_check > 0)
    compare=sscanf(test_bsh,'%s',1);
    if(isequal(compare,'ENDDS'))
      fprintf(1,'END OF BED SHEAR STRESS SOLUTION FILE REACHED\n')
      break
    end      
    bsh.time(count)=sscanf(test_bsh,'%*2s %*i %f',1);
    bsh.shear=fscanf(fid_bsh,'%f',[1,bsh.np]);
    if(feof(fid_bsh))
      fprintf(1,'END OF BED SHEAR SOLUTION FILE REACHED\n')      
      break
    end  
    fgetl(fid_bsh);
    test_bsh=fgetl(fid_bsh);
  end  
  if(blt_check > 0)
    for i=1:blt_check
      compare=sscanf(test_blt{i},'%s',1);
      if(isequal(compare,'ENDDS'))
        fprintf(1,'END OF BED LAYER THICKNESS %i SOLUTION FILE REACHED\n',i)
        break
      end 
      blt.time(i,count)=sscanf(test_blt{i},'%*2s %*i %f',1);
      blt.value(i,:)=fscanf(fid_blt(i),'%f',[1,blt.np]);
      if(feof(fid_blt(i)))
        fprintf(1,'END OF BED LAYER THICKNESS SOLUTION FILE REACHED\n')      
        break
      end  
      fgetl(fid_blt(i));
      test_blt{i}=fgetl(fid_blt(i));
    end
  end   
  if(bld_check > 0)
    for i=1:bld_check
      compare=sscanf(test_bld{i},'%s',1);
      if(isequal(compare,'ENDDS'))
        fprintf(1,'END OF BED LAYER DENSITY %i SOLUTION FILE REACHED\n',i)
        break
      end         
      bld.time(i,count)=sscanf(test_bld{i},'%*2s %*i %f',1);   
      holder=fscanf(fid_bld(i),'%f',[1,bld.np*number_sed]);
      total=1;
      for j=1:length(holder)
         if(mod(j,number_sed)==0)
            bld.bed(i,number_sed,total)=holder(j);
            total=total+1;
         else
            bld.bed(i,mod(j,number_sed),total)=holder(j);
         end
      end  
      if(feof(fid_bld(i)))
        fprintf(1,'END OF BED LAYER DISTRIBUTION SOLUTION FILE REACHED\n')      
        break
      end  
      fgetl(fid_bld(i));
      test_bld{i}=fgetl(fid_bld(i));
    end
  end 
  if(bed_check > 0)
    compare=sscanf(test_bed,'%s',1);
    if(isequal(compare,'ENDDS'))
      fprintf(1,'END OF BEDLOAD SOLUTION FILE REACHED\n')
      break
    end        
    bed.time(count)=sscanf(test_bed,'%*s %*i %f',1);
    bed.value=fscanf(fid_bed,'%f %f %f',[3,bed.np]);
    if(feof(fid_bed))
      fprintf(1,'END OF BED SOLUTION FILE REACHED\n')      
      break
    end  
    fgetl(fid_bed);
    test_bed=fgetl(fid_bed);
  end   
  if(alt_check > 0)
    compare=sscanf(test_alt,'%s',1);
    if(isequal(compare,'ENDDS'))
      fprintf(1,'END OF ACTIVE LAYER THICKNESS SOLUTION FILE REACHED\n')
      break
    end        
    alt.time(count)=sscanf(test_alt,'%*2s %*i %f',1);
    alt.value=fscanf(fid_alt,'%f',[1,alt.np]);
    if(feof(fid_alt))
      fprintf(1,'END OF ACTIVE LAYER THICKNESS SOLUTION FILE REACHED\n')      
      break
    end  
    fgetl(fid_alt);
    test_alt=fgetl(fid_alt);
  end    
  if(alb_check > 0)
    compare=sscanf(test_alb,'%s',1);
    if(isequal(compare,'ENDDS'))
      fprintf(1,'END OF ACTIVE LAYER DISTRIBUTION SOLUTION FILE REACHED\n')
      break
    end        
    alb.time(count)=sscanf(test_alb,'%*2s %*i %f',1);
    holder=fscanf(fid_alb,'%f',[1,alb.np*number_sed]);
    total=1;
    for i=1:length(holder)
       if(mod(i,number_sed)==0)
          alb.value(number_sed,total)=holder(i);
          total=total+1;
       else
          alb.value(mod(i,number_sed),total)=holder(i);
       end
    end      
    if(feof(fid_alb))
      fprintf(1,'END OF ALB SOLUTION FILE REACHED\n')      
      break
    end  
    fgetl(fid_alb);
    test_alb=fgetl(fid_alb);
  end  
  if(cbp_check > 0)
    for i=1:cbp_check
      compare=sscanf(test_cbp{i},'%s',1);
      if(isequal(compare,'ENDDS'))
        fprintf(1,'END OF COHESIVE BED PROPERTIES %i SOLUTION FILE REACHED\n',i)
        break
      end           
      cbp.time(i,count)=sscanf(test_cbp{i},'%*2s %*i %f',1);
      bob=fscanf(fid_cbp(i),'%f %f %f %f',[4,cbp.np]);
      cbp.den(i,:)=bob(1,:);
      cbp.ces(i,:)=bob(2,:);
      cbp.erc(i,:)=bob(3,:);
      cbp.ere(i,:)=bob(4,:);
      if(feof(fid_cbp(i)))
        fprintf(1,'END OF COHESIVE BED PROPERTIES SOLUTION FILE REACHED\n')      
        break
      end  
      fgetl(fid_cbp(i));
      test_cbp{i}=fgetl(fid_cbp(i));
    end
  end

  time_check=0;

    %  Writes the time and the water surface elevation and velocity
    %  solutions 
    
    if(dep_check > 0)
        time_check=1;
        fwrite(fid,dep.time(count)*tconv+t0,'float64');         %time
        fwrite(fid,dep.depth(:),'float32');
    end
    
    if(wse_check > 0)
        if(time_check == 0)
           fwrite(fid,dep.time(count)*tconv+t0,'float64');
           time_check=1;
        end
        fwrite(fid,wse(:),'float32');
    end
    
    if(vel_check > 0)
        if(time_check == 0)
           fwrite(fid,ovl.time(count)*tconv+t0,'float64');
           time_check=1;
        end        
        fwrite(fid,ovl.velocity(1,:),'float32');                %all u
        fwrite(fid,ovl.velocity(2,:),'float32');                %all v 
    end
    
    if(err_check > 0)
        if(time_check == 0)
           fwrite(fid,err.time(count)*tconv+t0,'float64');
           time_check=1;
        end         
        fwrite(fid,err.error(:),'float32');
    end
    
    if(sal_check > 0)
        if(time_check == 0)
           fwrite(fid,salt.time(count)*tconv+t0,'float64');
           time_check=1;
        end   
        fwrite(fid,salt.salinity(:),'float32');
    end
    
    if(vor_check > 0)
        if(time_check == 0)
           fwrite(fid,vor.time(count)*tconv+t0,'float64');
           time_check=1;
        end   
        fwrite(fid,vor.vorticity(:),'float32');
    end    
    
    if(snd_check > 0)
        if(time_check == 0)
           fwrite(fid,snd.time(count)*tconv+t0,'float64');
           time_check=1;
        end   
        for i=1:snd_check
          fwrite(fid,snd.sand(i,:),'float32');
          fwrite(fid,snd.rouse(i,:),'float32');
          fwrite(fid,snd.bdma(i,:),'float32');
        end
    end
    
    if(slt_check > 0)
        if(time_check == 0)
           fwrite(fid,slt.time(count)*tconv+t0,'float64');
           time_check=1;
        end   
        for i=1:slt_check
          fwrite(fid,slt.silt(i,:),'float32');
          fwrite(fid,slt.rouse(i,:),'float32');
          fwrite(fid,slt.bdma(i,:),'float32');
        end
    end    
    
    if(smr_check > 0)
       if(time_check == 0)
           fwrite(fid,smr.time(count)*tconv+t0,'float64');
           time_check=1;
        end    
       for i=1:number_sed
         fwrite(fid,smr.value(i,:),'float32');
       end
    end
    
    if(dpl_check > 0)
        if(time_check == 0)
           fwrite(fid,dpl.time(count)*tconv+t0,'float64');
           time_check=1;
        end   
        fwrite(fid,dpl.displacement(:),'float32');
    end
    
    if(bsh_check > 0)
        if(time_check == 0)
           fwrite(fid,bsh.time(count)*tconv+t0,'float64');
           time_check=1;
        end   
        fwrite(fid,bsh.shear(:),'float32');
    end
    
    if(blt_check > 0)
        if(time_check == 0)
           fwrite(fid,blt.time(count)*tconv+t0,'float64');
           time_check=1;
        end   
        for i=1:blt_check
          fwrite(fid,blt.value(i,:),'float32');
        end
    end
    
    if(bld_check > 0)
        if(time_check == 0)
           fwrite(fid,bld.time(count)*tconv+t0,'float64');
           time_check=1;
        end   
        for i=1:bld_check
          for j=1:number_sed
            fwrite(fid,bld.bed(i,j,:),'float32');
          end
        end
    end
    
    if(bed_check > 0)    
        if(time_check == 0)
           fwrite(fid,bed.time(count)*tconv+t0,'float64');
           time_check=1;
        end   
        fwrite(fid,bed.value(1,:),'float32');
        fwrite(fid,bed.value(1,:),'float32');
    end
    
    if(alt_check > 0)
        if(time_check == 0)
           fwrite(fid,alt.time(count)*tconv+t0,'float64');
           time_check=1;
        end   
        fwrite(fid,alt.value(:),'float32');
    end
    
    if(alb_check > 0)
        if(time_check == 0)
           fwrite(fid,alb.time(count)*tconv+t0,'float64');
           time_check=1;
        end   
        for i=1:number_sed
          fwrite(fid,alb.value(i,:),'float32');
        end
    end
    
    if(cbp_check > 0)
        if(time_check == 0)
           fwrite(fid,cbp.time(count)*tconv+t0,'float64');
        end   
        for i=1:cbp_check
          fwrite(fid,cbp.den(i,:),'float32');
          fwrite(fid,cbp.ces(i,:),'float32');
          fwrite(fid,cbp.erc(i,:),'float32');
          fwrite(fid,cbp.ere(i,:),'float32');
        end
    end     
    
    count=count+1;
  
end

%%  Re-determines and writes the header information for the da file.  This
%   is needed since initially the number of time steps is unknown

nt=count-1;
time=-1;
if((dep_check > 0 || wse_check > 0) && time < 0)
  time=dep.time;
end
if(vel_check > 0)
    len=min(length(time),length(ovl.time));
    if(time < 0)
        time=ovl.time;
    elseif(~isequal(time(1:len),ovl.time(1:len)))
        fprintf(1,'\nERROR: SOLUTION TIMES DO NOT MATCH\n\n')
        error('ERROR: SOLUTION TIMES DO NOT MATCH');
    end
end
if(err_check > 0)
    len=min(length(time),length(err.time));
    if(time < 0)
        time=err.time;
    elseif(~isequal(time(1:len),err.time(1:len)))
        fprintf(1,'\nERROR: SOLUTION TIMES DO NOT MATCH\n\n')
        error('ERROR: SOLUTION TIMES DO NOT MATCH');
    end
end
if(sal_check > 0)
    len=min(length(time),length(salt.time));
    if(time < 0)
        time=salt.time;
    elseif(~isequal(time(1:len),salt.time(1:len)))
        fprintf(1,'\nERROR: SOLUTION TIMES DO NOT MATCH\n\n')
        error('ERROR: SOLUTION TIMES DO NOT MATCH');
    end
end    
if(vor_check > 0)
    len=min(length(time),length(vor.time));    
    if(time < 0)
        time=vor.time;
    elseif(~isequal(time(1:len),vor.time(1:len)))
        fprintf(1,'\nERROR: SOLUTION TIMES DO NOT MATCH\n\n')
        error('ERROR: SOLUTION TIMES DO NOT MATCH');
    end
end
if(snd_check > 0)
    for i=1:snd_check
      len=min(length(time),length(snd.time(i,:))); 
      if(time < 0)
          time=snd.time(i,:);
      elseif(~isequal(time(1:len),snd.time(i,1:len)))
          fprintf(1,'\nERROR: SOLUTION TIMES DO NOT MATCH\n\n')
          error('ERROR: SOLUTION TIMES DO NOT MATCH');
      end
    end
end
if(slt_check > 0)
    for i=1:slt_check
      len=min(length(time),length(slt.time(i,:)));
      if(time < 0)
          time=slt.time(i,:);
      elseif(~isequal(time(1:len),slt.time(i,1:len)))
          fprintf(1,'\nERROR: SOLUTION TIMES DO NOT MATCH\n\n')
          error('ERROR: SOLUTION TIMES DO NOT MATCH');
      end
    end
end
if(dpl_check > 0)
    len=min(length(time),length(dpl.time));
    if(time < 0)
        time=dpl.time;
    elseif(~isequal(time(1:len),dpl.time(1:len)))
        fprintf(1,'\nERROR: SOLUTION TIMES DO NOT MATCH\n\n')
        error('ERROR: SOLUTION TIMES DO NOT MATCH');
    end
end
if(alt_check > 0)
    len=min(length(time),length(alt.time));
    if(time < 0)
        time=alt.time;
    elseif(~isequal(time(1:len),alt.time(1:len)))
        fprintf(1,'\nERROR: SOLUTION TIMES DO NOT MATCH\n\n')
        error('ERROR: SOLUTION TIMES DO NOT MATCH');
    end
end
if(alb_check > 0)
    len=min(length(time),length(alb.time));
    if(time < 0)
        time=alb.time;
    elseif(~isequal(time(1:len),alb.time(1:len)))
        fprintf(1,'\nERROR: SOLUTION TIMES DO NOT MATCH\n\n')
        error('ERROR: SOLUTION TIMES DO NOT MATCH');
    end
end
if(blt_check > 0)
    for i=1:blt_check
      len=min(length(time),length(blt.time(i,:)));
      if(time < 0)
          time=blt.time(i,:);
      elseif(~isequal(time(1:len),blt.time(i,1:len)))
          fprintf(1,'\nERROR: SOLUTION TIMES DO NOT MATCH\n\n')
          error('ERROR: SOLUTION TIMES DO NOT MATCH');
      end
    end
end
if(bld_check > 0)
    for i=1:bld_check
        len=min(length(time),length(bld.time(i,:)));
        if(time < 0)
          time=bld.time(i,:);
        elseif(~isequal(time(1:len),bld.time(i,1:len)))
          fprintf(1,'\nERROR: SOLUTION TIMES DO NOT MATCH\n\n')
          error('ERROR: SOLUTION TIMES DO NOT MATCH');
        end
    end
end
if(cbp_check > 0)
    for i=1:cbp_check
        len=min(length(time),length(cbp.time(i,:)));
        if(time < 0)
          time=cbp.time(i,:);
        elseif(~isequal(time(1:len),cbp.time(i,1:len)))
          fprintf(1,'\nERROR: SOLUTION TIMES DO NOT MATCH\n\n')
          error('ERROR: SOLUTION TIMES DO NOT MATCH');
        end
    end
end
if(bsh_check > 0)
    len=min(length(time),length(bsh.time));
    if(time < 0)
      time=bsh.time;
    elseif(~isequal(time(1:len),bsh.time(1:len)))
      fprintf(1,'\nERROR: SOLUTION TIMES DO NOT MATCH\n\n')
      error('ERROR: SOLUTION TIMES DO NOT MATCH');
    end
end
if(bed_check > 0)
    len=min(length(time),length(bed.time));
    if(time < 0)
      time=bed.time;
    elseif(~isequal(time(1:len),bed.time(1:len)))
      fprintf(1,'\nERROR: SOLUTION TIMES DO NOT MATCH\n\n')
      error('ERROR: SOLUTION TIMES DO NOT MATCH');
    end
end
if(smr_check > 0)
    len=min(length(time),length(smr.time));
    if(time < 0)
      time=smr.time;
    elseif(~isequal(time(1:len),smr.time(1:len)))
      fprintf(1,'\nERROR: SOLUTION TIMES DO NOT MATCH\n\n')
      error('ERROR: SOLUTION TIMES DO NOT MATCH');
    end
end

if(nt > 1)
  dt=time(nt)-time(nt-1);
else
  dt=1.0;
end

fseek(fid,0,'bof');
fwrite(fid,[nt,np],'integer*4');
fwrite(fid,dt,'real*4');

fprintf(1,'FINISHED WRITING da FILE\n')

%% close the da file and clear all variables from memory

fclose(fid);

clear all;

toc

