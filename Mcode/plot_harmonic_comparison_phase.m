function plot_harmonic_comparison_phase(model_TIDECON,field_TIDECON,harm_name)

  figure('Color',[1 1 1]);

  yyy=[model_TIDECON(:,3),field_TIDECON(:,3)];

  bar(yyy);

  set(gca,'ylim',[0,max(max(model_TIDECON(:,3)),max(field_TIDECON(:,3)))],'fontsize',8);
  
  set(gca,'XTick',1:length(model_TIDECON(:,3)));
  
  set(gca,'XTickLabel',harm_name,'FontSize',8);
  
  ylabel('Phase','FontSize',16,'FontName','Times New Roman');
  
  xlabel('Tidal Component','FontSize',16,'FontName','Times New Roman');
  
  title('Phase Comparisons','FontSize',18,'FontName', ...
      'Times New Roman');
  
%  legend('MODEL','FIELD','FontName','Times New Roman','FontSize',16);
  
  xticklabel_rotate;
  
  colormap bone;
