function RMSE=RMSE_calculation(field,model)
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
%                                       |  SUM((field-model)^2) |
%       Root Mean Square Error = SQRT   |-----------------------|
%                                       |        N              |
%
%       Finished 3/12/2010
%
   %% calculate the root mean square error

   difference=field-model;
   difference_squared=difference.*difference;
   sum_diffs=sum(difference_squared);
   
   RMSE=sqrt(sum_diffs/(length(field)));
   
   %% clear all variables but the RMSE value
   
   clear difference sum_diffs field model


   
   