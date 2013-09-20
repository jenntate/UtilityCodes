
%% Use to Calculate area of all elements

for i=length(grid.ncon(i,1))
x=grid.x(grid.ncon(i,1:3));
y=grid.y(grid.ncon(i,1:3));
area(i)=element_area(x,y);
end