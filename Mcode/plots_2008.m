function plots_2008(X1,Raw_X,Filtered_X,Raw_Y,Filtered_Y,name)
%CREATEFIGURE1(X1,YMATRIX1)
%  X1:  vector of x data
%  YMATRIX1:  matrix of y data

%  Auto-generated by MATLAB on 25-Apr-2010 10:33:34
temp1=datenum('01/01/2008 00:00:00');

temp2=datenum('01/01/2009 00:00:00');
% Create figure
figure1 = figure('Color',[1 1 1]);
%datetick('x',2,'keepticks')
% Create axes
axes1 = axes('Parent',figure1,'OuterPosition',[0 0.5 1 0.5],'FontSize',14);
set(axes1,'XLim',[temp1 temp2]);
set(axes1,'YLim',[-20 20]);
box(axes1,'on');
hold(axes1,'all');
% Create multiple lines using matrix input to plot
plot(X1,Raw_X,'-r','Parent',axes1,'LineWidth',0.5,'DisplayName','Raw X Component');
plot(X1,Filtered_X,'-b','Parent',axes1,'LineWidth',0.5,'DisplayName','Filtered X Component');
set(axes1,'XTick',temp1:60:temp2)
set(axes1,'YTick',-20:10:20)
% Create ylabel
ylabel('Wind Speed (m/s)','FontWeight','bold','FontSize',16);

% Create title
title('Wind Filtering','FontWeight','bold','FontSize',20);

% Create legend
legend(axes1,'show');

datetick('x',2,'keepticks')
%axis([min(X1) max(X1) min(min(Raw_X),min(Filtered_X)) max(max(Raw_X),max(Filtered_X))])
% Create axes
axes2 = axes('Parent',figure1,...
    'Position',[0.13 0.12 0.775 0.4075],...
    'FontSize',14);
%    'Position',[0.13 0.0716414523449319 0.775 0.4075],...
  
box(axes2,'on');
hold(axes2,'all');
% Create multiple lines using matrix input to plot
plot(X1,Raw_Y,'-r','Parent',axes2,'LineWidth',0.5,'DisplayName','Raw Y Component');
plot(X1,Filtered_Y','-b','Parent',axes2,'LineWidth',0.5,'DisplayName','Filtered Y Component');
set(axes2,'XLim',[temp1 temp2]);
set(axes2,'YLim',[-20 20]);
set(axes2,'XTick',temp1:60:temp2)
set(axes2,'YTick',-20:10:20)
% Create xlabel
xlabel('Time','FontWeight','bold','FontSize',16);
%feather(axes2,Raw_X,Raw_Y);
% Create ylabel
ylabel('Wind Speed (m/s)','FontWeight','bold','FontSize',16);
% Create legend
legend(axes2,'show');

datetick('x',2,'keepticks')
%axis([min(X1) max(X1) min(min(Raw_Y),min(Filtered_Y)) max(max(Raw_Y),max(Filtered_Y))])

saveas(gcf,[name '_2008'],'fig');
saveas(gcf,[name '_2008'],'png');
close(gcf);