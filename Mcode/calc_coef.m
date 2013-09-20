function corr_coef=calc_coef(x,y)


%sigxx=calc_cov(x1,x2);
%sigx1=calc_cov(x1,x1);
%sigx2=calc_cov(x2,x2);

%corr_coef=sigxx/(sigx1*sigx2);


mean_x1=mean(x);
mean_x2=mean(y);

sum_x=sum(x);
sum_y=sum(y);
sum_xy=sum(x.*y);
sum_x_squared=sum(x.*x);
sum_y_squared=sum(y.*y);

n=length(x);

top=(n*sum_xy-sum_x*sum_y);
bottom1=sqrt(n*sum_x_squared-sum_x*sum_x);
bottom2=sqrt(n*sum_y_squared-sum_y*sum_y);
bottom=bottom1*bottom2;
corr_coef=top/bottom;