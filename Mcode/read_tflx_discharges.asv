function model = read_tflx_discharges(basename,t0,time_units)
%tic


%loc=find(filename=='.',1,'first');
tempaaa=basename;
if exist([tempaaa '_tflx.mat'],'file')
    disp('Using pre-read tflx file')
    load([tempaaa '_tflx'])  
else
    fprintf(1,'Reading tflx file \n');
    
    fid=fopen([basename,'_tflx'],'rt');
    
    t0=datenum(t0);
    
    increment_amount=50;
    
    time             = zeros(1,increment_amount);
    mapping_strings  = zeros(1,100);
    string_flux      = zeros(100,increment_amount);
    string_wse       = zeros(100,increment_amount);
    string_wsemin    = zeros(100,increment_amount);
    string_wsemax    = zeros(100,increment_amount);
    
    count=0;
    number=0;
    
    while(~feof(fid))
        if(feof(fid))
            break;
        end        
        hold=fgetl(fid);
        if(feof(fid))
            break;
        end
        compare=sscanf(hold,'%s',1);
        
        if(isequal(compare,'Time'))
            
            number=0;
            count=count+1;
            temp=sscanf(hold,'%*s %f',1);
            if(mod(count,increment_amount)==0)
                time(count+1:count+increment_amount+1)=0;
%                           time(count)
            end
            if(count==1 || time(count-1)-temp~=0)
                time(count)=temp;
            else
                count=count-1;
            end
        else
            
            string_num=sscanf(hold,'%*s %i',1);
            
            number=number+1;
            
            if(count == 1)
                
                mapping_strings(number)=string_num;
                
            end
            
            string_flux(count,number)= ...
                sscanf(hold,'%*s %*i %*s %f',1);
            string_wse(count,number)= ...
                sscanf(hold,'%*s %*i %*s %*s %*s %f',1);
            string_wsemin(count,number)= ...
                sscanf(hold,'%*s %*i %*s %*s %*s %*s %*s %f',1);
            string_wsemax(count,number)= ...
                sscanf(hold,'%*s %*i %*s %*s %*s %*s %*s %*s %*s %f',1);          
            if(mod(count,increment_amount)==0)
                string_flux(count+1:count+increment_amount+1,:)=0;
                string_wse(count+1:count+increment_amount+1,:)=0;
                string_wsemin(count+1:count+increment_amount+1,:)=0;
                string_wsemax(count+1:count+increment_amount+1,:)=0;
            end
            
        end

    end

    tconv=(1.0/86400.0)*time_units;
    max=find(mapping_strings>0);
    model.time=time(1:count)*tconv+t0;
    model.fluxes=string_flux(1:count,max);
    model.wse=string_wse(1:count,max);
    model.wsemin=string_wsemin(1:count,max);
    model.wsemax=string_wsemax(1:count,max);
    model.mapping=mapping_strings(max);
    
    
    
    clear time string_flux
    
    fclose(fid);
    
    %% closes the NOAA formatted data file
    save([tempaaa '_tflx'],'model')
    csvwrite([tempaaa '_tflx_ts'],model.time, model.fluxes)
    
end