function covariance=calc_cov(x1,x2)


mean_x1=mean(x1);
mean_x2=mean(x2);
sum1=0.0;

parfor i=1:length(x1)
    sum1=sum1+(x1(i)-mean_x1)*(x2(i)-mean_x2);
end

covariance=sum1/length(x1);

