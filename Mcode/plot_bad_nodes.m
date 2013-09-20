function plot_bad_nodes(grid,sorted_bad_failure,sorted_bad_occ_failure)

z_bad_nodes=zeros(length(grid.x),1);

for i=1:length(sorted_bad_failure)-1
    z_bad_nodes(sorted_bad_failure(i))=sorted_bad_occ_failure(i);
end

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
    z(1,i)=z_bad_nodes(grid.ncon(i,1));
    z(2,i)=z_bad_nodes(grid.ncon(i,2));
    z(3,i)=z_bad_nodes(grid.ncon(i,3));
    
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
axis equal;
disp(' ');
%caxis([-20 0]);

top=max(max(z'));
caxis([0 top/25]);
hp = patch(x,y,z);
ht = title('Nodal Failure Contours','FontWeight','bold','FontSize',18);
tmp = colorbar('FontSize',16);
ht2=title(tmp,'failures','FontWeight','bold','FontSize',18);
%xlabel('Longitude (deg)','FontWeight','bold','FontSize',18);
%ylabel('Latitude (deg)','FontWeight','bold','FontSize',18);
box on;


