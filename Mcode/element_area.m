function area=element_area(x,y)
%
% This subroutine calculates the area of a triangle with vertex locatons
% x(1:3) and y(1:3)
%

%% Use to calculate the distance of three sides of triangle

dist(1)=sqrt((x(2)-x(1))^2+(y(2)-y(1))^2);
dist(2)=sqrt((x(3)-x(2))^2+(y(3)-y(2))^2);
dist(3)=sqrt((x(3)-x(1))^2+(y(3)-y(1))^2);
if(min(dist)==0.0)
    area=0;
    return
end
%% Finds the longest side of a triangle 

longest_side=find(dist==max(dist));
if(longest_side(1)==1)
   a=2;
   b=3;
   c=longest_side(1);
elseif(longest_side(1)==2)
   a=1;
   b=3;
   c=longest_side(1);
elseif(longest_side(1)==3)
    a=1;
    b=2;
    c=longest_side(1);
end

%% Calculates the area of the triangle 

temp=(dist(a)^2+dist(b)^2-dist(c)^2)/(2*dist(a)*dist(b));
theta=acos(temp);
area=dist(a)*dist(b)*sin(theta)/2;

