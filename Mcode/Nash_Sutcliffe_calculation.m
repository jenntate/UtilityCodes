function Nash_error=Nash_Sutcliffe_calculation(field,model)
%
%   function to calculate the Nash-Sutcliffe Coefficient for a set of field
%   data and corresponding model data
%
%   INPUT VARIABLES
%
%       field = vector of field data values
%
%       model = vector of model data values
%
%       Each entry in the field and model data sets should correspond to
%       the same time for both the field and the model
%
%                                             SUM((field-model)^2)
%       Nash-Sutcliffe Coefficient = 1 -  ----------------------------
%                                          SUM((field-average_field)^2)
%
%
%       Finished 3/12/2010
%

   %% Nash-Sutcliffe Coefficient for the time series data for this location

   diff=field-model;
   squared_diff=diff.*diff;
   numerator=sum(squared_diff);
   
   mean_field=mean(field);
   diff_Obs_mean_Obs=field-mean_field;
   diff_Obs_mean_Obs_squared=diff_Obs_mean_Obs.*diff_Obs_mean_Obs;
   denominator=sum(diff_Obs_mean_Obs_squared);
   
   ratio=numerator/denominator;
   Nash_error=1-ratio;
   
   %% clear all variables but the returned error value
   
   clear diff field model squared_diff numerator mean_field;
   clear diff_Obs_mean_Obs diff_Obs_mean_Obs_squared denominator ratio;



   
   