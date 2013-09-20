function createfigure_bar_lin_it(it,fid)
%CREATEFIGURE1(YVECTOR1)
%  YVECTOR1:  bar yvector

%  Auto-generated by MATLAB on 10-Mar-2010 14:36:43

% Create figure
figure1 = figure('Name','Linear Iterations Bar Plot','Color',[1 1 1]);

max_it=max(it);
mean_it=mean(it);
min_it=min(it);

fprintf(fid,'\n');
fprintf(fid,'Maximum Number Linear Iterations    = %i\n',max_it);
fprintf(fid,'Mean Number Linear Iterations       = %f\n',mean_it);
fprintf(fid,'Minimum Number Linear Iterations    = %i\n',min_it);

small1(1)=(length(find(it<(max_it/16))));

small1(2)=(length(find(it<(max_it/8))));

small1(3)=(length(find(it<(max_it/4))));

small1(4)=(length(find(it<(max_it/2))));

small1(5)=(length(find(it<=(max_it))));

small(1)=100.0*small1(1)/length(it);
small(2)=100.0*(small1(2)-small1(1))/length(it);
small(3)=100.0*(small1(3)-small1(2))/length(it);
small(4)=100.0*(small1(4)-small1(3))/length(it);
small(5)=100.0*(small1(5)-small1(4))/length(it);

label{1}='0'; 
label{2}=num2str(int64(max_it/16));
label{3}=num2str(int64(max_it/8)); 
label{4}=num2str(int64(max_it/4)); 
label{5}=num2str(int64(max_it/2)); 
label{6}=num2str(int64(max_it)); 

labels{1}=strcat(label{1},'_to_',label{2});
labels{1}=regexprep(labels{1},'_',' ');
labels{2}=strcat(label{2},'_to_',label{3});
labels{2}=regexprep(labels{2},'_',' ');
labels{3}=strcat(label{3},'_to_',label{4});
labels{3}=regexprep(labels{3},'_',' ');
labels{4}=strcat(label{4},'_to_',label{5});
labels{4}=regexprep(labels{4},'_',' ');
labels{5}=strcat(label{5},'_to_',label{6});
labels{5}=regexprep(labels{5},'_',' ');

% Create axes
axes1 = axes('Parent',figure1,'XTick',[1 2 3 4 5], 'XTickLabel', ...
    {labels{1} labels{2} labels{3} labels{4} labels{5}});
box(axes1,'on');
hold(axes1,'all');

% Create bar
bar(small);
set(gca,'ylim',[0 50])
% Create ylabel
ylabel('Percentage of Occurrences','FontWeight','bold',...
    'FontSize',16);

xlabel('Required Number of Linear Iterations','FontWeight','bold',...
    'FontSize',16);

% Create title
title('Linear Iterations Bar Plot','FontWeight','bold','FontSize',20);

