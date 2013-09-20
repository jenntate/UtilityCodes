function plot_harmonic_comparison_freq(model_FREQ,field_FREQ,harm_name)


  figure('Color',[1 1 1]);


  yyy=[model_FREQ(:),field_FREQ(:)];
  
%$  model_FREQ(:,:)
  
  bar(yyy);
  
  set(gca,'ylim',[0,max(max(model_FREQ(:)),max(field_FREQ(:)))],'fontsize',8);
  
  set(gca,'XTick',1:length(model_FREQ(:)));
  
  set(gca,'XTickLabel',harm_name,'FontSize',8);

  ylabel('Frequency,Cycles/Hour','FontSize',16,'FontName','Times New Roman');
  
  xlabel('Tidal Component','FontSize',16,'FontName','Times New Roman');
  
  title('Frequency Comparisons','FontSize',18,'FontName', ...
      'Times New Roman');
  
%  legend('MODEL','FIELD','FontName','Times New Roman','FoneSize','10');
  
  colormap bone;
  
  xticklabel_rotate;
