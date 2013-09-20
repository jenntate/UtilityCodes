function Willmott=Willmott_calculation(field,model)
%
%       Function to calculate the root mean square error for a time series
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
%                                   |         SUM((Model-field)^2)                |
%       Willmott Coefficient = SQRT |---------------------------------------------|
%                                   |SUM(((Model-mean(field)+field-mean(field))^2)|
%
%       Finished 3/12/2010
%
   %% Willmott Coefficient
   
   mean_field=mean(field);
   bottom=0.0;
   top=0.0;
   
   parfor i=1:length(model)
     top=top+(field(i)-model(i))*(field(i)-model(i));
     bottom = bottom + ((abs(model(i)-mean_field))+(abs(field(i)-mean_field)))* ...
         ((abs(model(i)-mean_field))+(abs(field(i)-mean_field)));  
   end
  
   Willmott=1-top/bottom;
   


   
   