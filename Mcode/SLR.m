function SLR(basename,amount)





amount_string=num2str(amount);

amount_string(find(amount_string=='.'))='_';

fid_bc=fopen([basename '.bc'],'r');
fid_bc_slr=fopen([basename '_slr' amount_string '.bc'],'w');
slr_xy_series=zeros(100,1);

count=1;
while (~feof(fid_bc))
   
    temp=fgetl(fid_bc);
    
    card1=sscanf(temp,'%s',1);
    card2=sscanf(temp,'%*s %s',1);
    if(isequal(card1,'NB') && isequal(card2,'OTW'))
       
        temp1=sscanf(temp,'%*s %*s %*s %i',1);
        
        slr_xy_series(count)=temp1;
        count=count+1;
        
    end 
    
end

fclose(fid_bc);

fid_bc=fopen([basename '.bc'],'r');

while (~feof(fid_bc))
    
   temp=fgetl(fid_bc);
   
   card1=sscanf(temp,'%s',1);   
   
   if(isequal(card1,'XY1'))
       
       series_num=sscanf(temp,'%*s %i',1);
       fprintf(fid_bc_slr,'%s\n',temp);
       loc=length(find(series_num==slr_xy_series));

       if(loc > 0)
      
           num=sscanf(temp,'%*s %*s %i',1);
                     
           data=textscan(fid_bc,'%f %f',num);
               
           time=data{1};
           value=data{2}+amount;
           
           for i=1:num
           
               fprintf(fid_bc_slr,'%.2f %.4f\n',time(i),value(i));
               
           end
           
           
       end
    
   else
       
       fprintf(fid_bc_slr,'%s\n',temp);
       
   end
    
    
end

fclose(fid_bc);
fclose(fid_bc_slr);

clear data value time
fid_hot=fopen([basename '.hot'],'r');
fid_hot_slr=fopen([basename '_slr' amount_string '.hot'],'w');

while (~feof(fid_hot))
    
    temp=fgetl(fid_hot);
    
    card1=sscanf(temp,'%s',1);
    
    if(isequal(card1,'ND'))
        
        num_nodes=sscanf(temp,'%*s %i',1);
        fprintf(fid_hot_slr,'%s\n',temp);
        
    elseif(isequal(card1,'NAME'))
    
        para=sscanf(temp,'%*s %s',1);
        fprintf(fid_hot_slr,'%s\n',temp);
        
        if(isequal(para,'ioh'))
            temp=fgetl(fid_hot);
            fprintf(fid_hot_slr,'%s\n',temp);
            temp=fgetl(fid_hot);
            fprintf(fid_hot_slr,'%s\n',temp);
            for i=1:num_nodes

                value=sscanf(fgetl(fid_hot),'%f');
                value=value+amount;
                fprintf(fid_hot_slr,'%.4f\n',value);
            
            end
            
        end
        
    else
        
        fprintf(fid_hot_slr,'%s\n',temp);
        
    end
    
end

fclose(fid_hot);
fclose(fid_hot_slr);


