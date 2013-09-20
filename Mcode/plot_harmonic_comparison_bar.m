function plot_harmonic_comparison_bar(model_TIDECON,field_TIDECON,harm_name)


  figure('Color',[1 1 1]);

  yyy=[model_TIDECON(:,1),field_TIDECON(:,1)];

  bar(yyy);

  set(gca,'ylim',[0,max(max(model_TIDECON(:,1)),max(field_TIDECON(:,1)))],'fontsize',8);
  
  set(gca,'XTick',1:length(model_TIDECON(:,1)));
  
  set(gca,'XTickLabel',harm_name,'FontSize',8);
  
  ylabel('Amplitude, Meters','FontSize',16,'FontName','Times New Roman');
  
  xlabel('Tidal Component','FontSize',16,'FontName','Times New Roman');
  
  title('Harmonic Constituent Comparisons','FontSize',18,'FontName', ...
      'Times New Roman');
  
%  legend('MODEL','FIELD','FontName','Times New Roman','FontSize',16);
  
  xticklabel_rotate;
  
  colormap bone;
  
  fileout=fopen('Harmonic_const.txt','a');
  
  for i=1:length(model_TIDECON(:,1))
      fprintf(fileout,'%s %5.2f %5.2f\n',harm_name(i,:),yyy(i,1),yyy(i,2));
  end
  
  fprintf(fileout,'\n');
  
  fclose(fileout);
  
  clear yyy;
  
 
