function plot_last_node(grid,last_node)

x=zeros(3,length(grid.ncon(:,1)));
y=zeros(3,length(grid.ncon(:,1)));
z=zeros(3,length(grid.ncon(:,1)));

for i=1:length(grid.ncon)
   
    x(1,i)=grid.x(grid.ncon(i,1));
    x(2,i)=grid.x(grid.ncon(i,2));
    x(3,i)=grid.x(grid.ncon(i,3));
    y(1,i)=grid.y(grid.ncon(i,1));
    y(2,i)=grid.y(grid.ncon(i,2));
    y(3,i)=grid.y(grid.ncon(i,3));
    z(1,i)=grid.z(grid.ncon(i,1));
    z(2,i)=grid.z(grid.ncon(i,2));
    z(3,i)=grid.z(grid.ncon(i,3));     
    
end

rect = [520 405 754 685];
M = figure('Renderer','zbuffer','Color',[1 1 1]);

% Create axes
axes1 = axes('Parent',M,'FontSize',14);
box(axes1,'on');
hold(axes1,'all');
opengl neverselect
set(M,'Backingstore','off','doublebuffer','on');
set(M,'position',rect);
hold on;
disp(' ');
%caxis([-20 0]);

%top=max(max(z'));
%caxis([0 top/100]);
%hp = patch(x,y,z);


dis(1)=sqrt((grid.x(last_node(1))-grid.x(last_node(1)-1))^2+ ...
    (grid.y(last_node(1))-grid.y(last_node(1)-1))^2);
dis(2)=sqrt((grid.x(last_node(1))-grid.x(last_node(1)+1))^2+ ...
    (grid.y(last_node(1))-grid.y(last_node(1)+1))^2);

dis(3)=sqrt((grid.x(last_node(2))-grid.x(last_node(2)-1))^2+ ...
    (grid.y(last_node(2))-grid.y(last_node(2)-1))^2);
dis(4)=sqrt((grid.x(last_node(2))-grid.x(last_node(2)+1))^2+ ...
    (grid.y(last_node(2))-grid.y(last_node(2)+1))^2);

dis=max(dis);

%dis=sqrt((grid.x(100)-grid.x(99))^2+(grid.y(100)-grid.y(99))^2);
bad_x(1)=grid.x(last_node(1))-dis/4;
bad_x(2)=grid.x(last_node(1))-dis/4;
bad_x(3)=grid.x(last_node(1))+dis/4;
bad_x(4)=grid.x(last_node(1))+dis/4;
bad_y(1)=grid.y(last_node(1))-dis/4;
bad_y(2)=grid.y(last_node(1))+dis/4;
bad_y(3)=grid.y(last_node(1))+dis/4;
bad_y(4)=grid.y(last_node(1))-dis/4;
% Create rectangle

test=patch(bad_x,bad_y,100,'EdgeColor','b','FaceColor','w','LineWidth',5);

bad_x(1)=grid.x(last_node(2))-dis/4;
bad_x(2)=grid.x(last_node(2))-dis/4;
bad_x(3)=grid.x(last_node(2))+dis/4;
bad_x(4)=grid.x(last_node(2))+dis/4;
bad_y(1)=grid.y(last_node(2))-dis/4;
bad_y(2)=grid.y(last_node(2))+dis/4;
bad_y(3)=grid.y(last_node(2))+dis/4;
bad_y(4)=grid.y(last_node(2))-dis/4;
test=patch(bad_x,bad_y,100,'EdgeColor','b','FaceColor','w','LineWidth',5);
% Create rectangle

hp = patch(x,y,z);
caxis ([min(grid.z) max(grid.z)]);
ht = title('Bathymetry Contours','FontWeight','bold','FontSize',18);
tmp = colorbar('FontSize',16);
%xlabel('Longitude (deg)','FontWeight','bold','FontSize',18);
%ylabel('Latitude (deg)','FontWeight','bold','FontSize',18);
box on;
axis equal;