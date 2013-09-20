function Pearson=Pearson_calculation(field,model)
%
%       Function to calculate the Pearson Product-Moment
%       Correlation COefficient for a time series
%       data set for the given location
%
%       INPUT VARIABLES
%
%       field = vector of time series field data points
%
%       model = vector of time series model data points
%
%       NOTE: the field data values and the model data values should
%       correspond to the same time.
%                                
%                             SUM((Model-mean(model))*(field-mean(field)))
%       Pearson Coefficient = |---------------------------------------------|
%                             SQRT (SUM(Model-mean(model))^2)*SQRT(SUM(field-mean(field))^2)
%
%       Finished 7/15/2013
%
   %% Pearson Coefficient
   
   mean_field=mean(field);
   mean_model=mean(model);
   bottom1=0.0;
   bottom2=0.0;
   top=0.0;
   
   parfor i=1:length(model)
     top=top+(model(i)-mean_model)*(field(i)-mean_field);
     bottom1=bottom1+(model(i)-mean_model)^2;
     bottom2=bottom2+(field(i)-mean_field)^2;
   end
  
   Pearson=top/((bottom1)^0.5*(bottom2)^0.5);
   


   
   
