function field=read_sutrowin(filename)
%
%       This function reads a sutrowin formated text file with measured data
%
%       INPUT VARIABLES
%
%       filename = file containing the usgs data
%
%       OUTPUT VARIABLES
%
%       field = structure that contains the measurement times and values
%
%       field.time = measurement times
%
%       field.value = measurement values at the given times
%
%       Finished 3/12/2010
%
%tic

%% open the Sutrowin formatted data file
loc=find(filename=='.',1,'last');
tempaaa=filename(1:loc-1);
if exist([tempaaa '.mat'],'file')
    disp('Using pre-read grid')
    load(tempaaa)
else
    disp('Reading Data')
    fid=fopen(filename,'r');
    
    
    fgetl(fid);
    
    count=1;
    while(~feof(fid))
        
        temp2=fgetl(fid);
        
        if(strcmp(temp2(1:7),',,,,,,,'))
            fprintf(1,'END OF FILE REACHED\n');
            break;
        end
        
        
        temp=regexprep(temp2,',',' ');
        temp=regexprep(temp,'-','-900');
        clear temp2
        temp1=sscanf(temp,'%s',1);
        
        loc1=find(temp1=='/',1,'first');
        if(isempty(loc1))
            return;
        end
        loc2=find(temp1=='/',1,'last');
        if(~isempty(loc1)&&~isempty(loc2))
            date=datenum(temp1);
            clear temp1
            temp1=sscanf(temp,'%*s %s',1);
            loc1=find(temp1==':',1,'first');
            hour=str2double(temp1(1:loc1-1));
            minute=str2double(temp1(loc1+1:length(temp1)));
            clear temp1
            data.time(count)=date+hour/24.0+minute/(24.0*60.0);
            if(count>1 && data.time(count)>data.time(count-1))
                fprintf(1,'%i entry after %s time\n',count,datestr(data.time(count-1)))
            end
            temp1=sscanf(temp,'%*s %*s %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f');
            data.Cell(44).Speed(count)=temp1(1);
            data.Cell(45).Speed(count)=temp1(2);
            data.Cell(46).Speed(count)=temp1(3);
            data.Cell(47).Speed (count)=temp1(4);
            data.Cell(48).Speed(count)=temp1(5);
            data.Cell(44).Direction(count)=temp1(6);
            data.Cell(45).Direction(count)=temp1(7);
            data.Cell(46).Direction(count)=temp1(8);
            data.Cell(47).Direction(count)=temp1(9);
            data.Cell(30).Speed(count)=temp1(10);
            data.Cell(31).Speed(count)=temp1(11);
            data.Cell(32).Speed(count)=temp1(12);
            data.Cell(33).Speed(count)=temp1(13);
            data.Cell(34).Speed(count)=temp1(14);
            data.Cell(35).Speed(count)=temp1(15);
            data.Cell(36).Speed(count)=temp1(16);
            data.Cell(37).Speed(count)=temp1(17);
            data.Cell(41).Speed(count)=temp1(18);
            data.Cell(42).Speed(count)=temp1(19);
            data.Cell(43).Speed(count)=temp1(20);
            data.Cell(39).Speed(count)=temp1(21);
            data.Cell(40).Speed(count)=temp1(22);
            data.Cell(31).Direction(count)=temp1(23);
            data.Cell(32).Direction(count)=temp1(24);
            data.Cell(33).Direction(count)=temp1(25);
            data.Cell(34).Direction(count)=temp1(26);
            data.Cell(35).Direction(count)=temp1(27);
            data.Cell(36).Direction(count)=temp1(28);
            data.Cell(37).Direction(count)=temp1(29);
            data.Cell(38).Direction(count)=temp1(30);
            data.Cell(39).Direction(count)=temp1(31);
            data.Cell(40).Direction(count)=temp1(32);
            data.Cell(41).Direction(count)=temp1(33);
            data.Cell(42).Direction(count)=temp1(34);
            data.Cell(43).Direction(count)=temp1(35);
            data.Cell(18).Speed(count)=temp1(36);
            data.Cell(19).Speed(count)=temp1(37);
            data.Cell(20).Speed(count)=temp1(38);
            data.Cell(18).Direction(count)=temp1(39);
            data.Cell(19).Direction(count)=temp1(40);
            data.Cell(20).Direction(count)=temp1(41);
            data.Cell(21).Speed(count)=temp1(42);
            data.Cell(22).Speed(count)=temp1(43);
            data.Cell(23).Speed(count)=temp1(44);
            data.Cell(24).Speed(count)=temp1(45);
            data.Cell(25).Speed(count)=temp1(46);
            data.Cell(26).Speed(count)=temp1(47);
            data.Cell(27).Speed(count)=temp1(48);
            data.Cell(28).Speed(count)=temp1(49);
            data.Cell(29).Speed(count)=temp1(50);
            data.Cell(21).Direction(count)=temp1(51);
            data.Cell(22).Direction(count)=temp1(52);
            data.Cell(23).Direction(count)=temp1(53);
            data.Cell(24).Direction(count)=temp1(54);
            data.Cell(25).Direction(count)=temp1(55);
            data.Cell(26).Direction(count)=temp1(56);
            data.Cell(27).Direction(count)=temp1(57);
            data.Cell(28).Direction(count)=temp1(58);
            data.Cell(29).Direction(count)=temp1(59);
            data.Cell(30).Direction(count)=temp1(60);
            data.Cell(11).Speed(count)=temp1(61);
            data.Cell(12).Speed(count)=temp1(62);
            data.Cell(13).Speed(count)=temp1(63);
            data.Cell(14).Speed(count)=temp1(64);
            data.Cell(15).Speed(count)=temp1(65);
            data.Cell(16).Speed(count)=temp1(66);
            data.Cell(17).Speed(count)=temp1(67);
            data.Cell(11).Direction(count)=temp1(68);
            data.Cell(12).Direction(count)=temp1(69);
            data.Cell(13).Direction(count)=temp1(70);
            data.Cell(15).Direction(count)=temp1(71);
            data.Cell(16).Direction(count)=temp1(72);
            data.Cell(17).Direction(count)=temp1(73);
            data.Cell(5).Speed(count)=temp1(74);
            data.Cell(6).Speed(count)=temp1(75);
            data.Cell(7).Speed(count)=temp1(76);
            data.Cell(8).Speed(count)=temp1(77);
            data.Cell(9).Speed(count)=temp1(78);
            data.Cell(10).Speed(count)=temp1(79);
            data.Cell(5).Direction(count)=temp1(80);
            data.Cell(6).Direction(count)=temp1(81);
            data.Cell(7).Direction(count)=temp1(82);
            data.Cell(8).Direction(count)=temp1(83);
            data.Cell(9).Direction(count)=temp1(84);
            data.Cell(10).Direction(count)=temp1(85);
            data.Cell(14).Direction(count)=temp1(86);
            data.Cell(1).Speed(count)=temp1(87);
            data.Cell(2).Speed(count)=temp1(88);
            data.Cell(3).Speed(count)=temp1(89);
            data.Cell(4).Speed(count)=temp1(90);
            data.Cell(1).Direction(count)=temp1(91);
            data.Cell(2).Direction(count)=temp1(92);
            data.Cell(3).Direction(count)=temp1(93);
            data.Cell(4).Direction(count)=temp1(94);
            clear temp1
            count=count+1;
        end
        
        clear temp
    end
    
    fclose(fid);
    
    %     Find bad values...'-900' or '90' in speed
    %     Convert speed in knots to speed in feet/s (1.688 fps = 1 knot)
    
    for i=1:47
        temp1=find(data.Cell(i).Speed~=90.0);
        temp2=find(data.Cell(i).Speed~=-900.0);
        temp=intersect(temp1,temp2);
        field1.time=data.time(temp);
        field1.speed=data.Cell(i).Speed(temp)*1.688;
        field1.direction=data.Cell(i).Direction(temp);

        num=1;
        len=length(field1.time);
        for j=len:-1:1
            field.time(j)=field1.time(num);
            field.speed(j)=field1.speed(num);
            field.direction(j)=field1.direction(num);
            num=num+1;
        end  
%         ct=len;
%         for j=num:len
%             field.time(j)=field1.time(ct);
%             field.speed(j)=field1.speed(ct);
%             field.direction(j)=field1.direction(ct);
%             ct=ct-1;
%         end  
% 
%         if i==24
%          plot(field.time, field.speed)
%         end

        if (exist('field'))    
        tempbbb=['Surge_Barrier_Site_',int2str(i)]
        save(tempbbb,'field')
        end
        
        clear temp1
        clear temp2
        clear temp
        clear field
    end
    
%     save(tempaaa,'field')
    
end
    


