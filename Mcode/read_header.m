function info=read_header(fid63)

info.output_time=0;

while (~feof(fid63))
   info.test=fgetl(fid63);
   compare=sscanf(info.test,'%s',1);
   
   % determines the number of nodes for the depth solution file
   
   if(isequal(compare,'ND'));
       info.np=sscanf(info.test,'%*s %i',1);
       
   % determines the number of elements for the depth solution file
       
   elseif(isequal(compare,'NC'))
       info.ne=sscanf(info.test,'%*s %i',1);
       
   % identifies the end of the header information
       
   elseif(isequal(compare,'TS'))
       break;
       
   % identifies the output time units for the depth solution file
       
   elseif(isequal(compare,'TIMEUNITS'))
       timeunits=sscanf(info.test,'%*s %s',1);
       if(isequal(timeunits,'SECONDS'))
           info.output_time=0;
       elseif(isequal(timeunits,'MINUTES'))
           info.output_time=1;
       elseif(isequal(timeunits,'HOURS'))
           info.output_time=2;
       elseif(isequal(timeunits,'DAYS'))
           info.output_time=3;
       elseif(isequal(timeunits,'WEEKS'))
           info.output_time=4;
       end
   end
   
end