function write_adh_flux_format(basename,data)
%
%
%
%   subroutine to output fluxes in adh format
%
%   input parameters:
%
%   basename - basename of the output file (_tflx will be added to the 
%              end of this basename)
%
%   data - stucture containing the data times and values
%
%   example usage:
%
%   write_adh_flux_format('MTOG_RMA10_3300-4400',data)
%
%

fid=fopen([basename '_tflx'],'w');
dt=data.time(2)-data.time(1);

fprintf(fid,'Time %.10e Flux-Dt %.10e\n',data.time(1),dt);
for j=1:length(data.discharges(1,:))
   fprintf(fid,'String %2i Flux %4.4e WSE %2.4e WSEMin %2.4e WSEMax %2.4e\n', ...
       j, data.discharges(1,j),data.wse(1,j),data.wse(1,j),data.wse(1,j));
end

for i=2:length(data.time)
    dt=data.time(i)-data.time(i-1);
    fprintf(fid,'Time %.10e Flux-Dt %.10e\n',data.time(i),dt);
    for j=1:length(data.discharges(i,:))
       fprintf(fid,'String %2i Flux %2.4e WSE %2.4e WSEMin %2.4e WSEMax %2.4e\n', ...
           j, data.discharges(i,j),data.wse(i,j),data.wse(i,j),data.wse(i,j));
    end    
end

fclose(fid);

clear dt data i j fid



