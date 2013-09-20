function analysis_output(screen_filename,adh_root_file)
%
%	This code takes the adh screen output file, boundary
%	conditions file, and grid file and does analysis to 
%   determine different convergence properties.
%	
%	Time series (model time) plots are made for the 
%	percentage completed, time step, adapted nodes,
%	required nonlinear iterations, required linear
%	iterations, normal residual, and maximum increment.
%
%	Bar plots are made to compare the convergence 
%	properties in terms of the time step size and 
% 	the required number of nonlinear iterations.
%
%   The codes also outputs two files identifying the "bad" nodes for 
%   convergence.  The "bad_nodes.out" file identifys the worst nodes for
%   each time step that converges and the z value is the number of
%   occurrences for said node.  The "bad_nodes_failure.out" file does the
%   same but only for nodes that lead to a convergence failure.  Note
%   adapted nodes are assigned to the nearest base nodal value.
%
%
%	INPUT VARIABLES
%
%	screen_filename = the filename outputted
%	from the adh run that includes the convergence
%	information and simulation information.
%	
%	example would be "adh.out"
%
%	adh_root_file = the adh root filename for the
%	adh simulation.
%
%	example would be "Mobile" 
%
%	So an example input would be:
%
%	analysis_output('adh.out','Mobile')
%
%   Finished 3/12/2010
%

%% to record code run time
tic

%% opens the screen output file

fid=fopen(screen_filename,'r');
basename=adh_root_file;

fgetl(fid);

%% pre-allocates arrays for more efficient run time

time                    = zeros(1000000,1);
dt                      = zeros(1000000,1);
percent                 = zeros(1000000,1);
non_it                  = zeros(1000000,1);
lin_it                  = zeros(1000000,1);
max_resid_norm          = zeros(1000000,1);
resid_norm_worst_node   = zeros(1000000,1);
resid_norm_worst_node_x = zeros(1000000,1);
resid_norm_worst_node_y = zeros(1000000,1);
resid_norm_worst_node_z = zeros(1000000,1);
inc_max_norm            = zeros(1000000,1);
inc_max_norm_node       = zeros(1000000,1);
inc_max_norm_node_x     = zeros(1000000,1);
inc_max_norm_node_y     = zeros(1000000,1);
inc_max_norm_node_z     = zeros(1000000,1);
nodes_after_adaption    = zeros(1000000,1);
bad_node_failure        = zeros(100000,1);


count=1;
count_bad=1;

%% read bc file
NTL_tol=0.0;
ITL_tol=0.0;

% reads the bc file to determine the tolerances used

fprintf(1,'Reading the bc file\n')

fid15=fopen([basename,'.bc'],'rt');

while(~feof(fid15))
   hold=fgetl(fid15);
   check=sscanf(hold,'%s',1);
   if(isequal(check,'IP'))
       para=sscanf(hold,'%*s %s',1);
       if(isequal(para,'NTL'))
           NTL_tol=sscanf(hold,'%*s %*s %f',1);
       elseif(isequal(para,'ITL'))
           ITL_tol=sscanf(hold,'%*s %*s %f',1);
       end
   end
end

%% closes the bc file

fclose(fid15);

fprintf(1,'Finished reading the bc file\n')

fprintf(1,'Reading the screen output file\n')

%% reads the screen output file

while(~feof(fid))

   % reads each line of the file

   hold=fgetl(fid);
   check=sscanf(hold,'%s',1);

   % checks to see if this is the end of the file

   if(isequal(check,'The'))% || isequal(check,''))
       break;
   end
   
   if(feof(fid))
       break;
   end
   
   checker=find(hold=='%', 1);
   
   if(~isempty(checker))

     % reads the data for each line of the screen output file
     %fprintf(1,'%s\n',hold)
     
     output=textscan(hold,'%f %f %*1s %f %f %f %f %f %f %f %f %f %f %f %f %f %f %s',1);

     % determines if the given time step has converged and saves all information if it has
   
     if(isempty(output{17}) && ((NTL_tol > output{6} || ITL_tol > output{11})))

     % saves the information for the for this time step

       time(count)=output{1};
      % fprintf(1,'%f\n',time(count));
       dt(count)=output{2};
       percent(count)=output{3};
       non_it(count)=output{4};
       lin_it(count)=output{5};
       max_resid_norm(count)=output{6};
       resid_norm_worst_node(count)=output{7};
       resid_norm_worst_node_x(count)=output{8};
       resid_norm_worst_node_y(count)=output{9};
       resid_norm_worst_node_z(count)=output{10};
       inc_max_norm(count)=output{11};
       inc_max_norm_node(count)=output{12};
       inc_max_norm_node_x(count)=output{13};
       inc_max_norm_node_y(count)=output{14};
       inc_max_norm_node_z(count)=output{15};
       nodes_after_adaption(count)=output{16};
       last_node(1)=output{7};
       last_node(2)=output{12};
       count=count+1;
     else
       if(output{6} > NTL_tol)
         bad_node_failure(count_bad)=output{7};
         count_bad=count_bad+1;      
       end
       if(output{11} > ITL_tol)
         bad_node_failure(count_bad)=output{12};
         count_bad=count_bad+1;
       end
       last_node(1)=output{7};
       last_node(2)=output{12};     
     end
   end
end

fclose(fid);

%% creates the figures for the time series plots

fprintf(1,'Finished reading the screen output file\n')

clear output hold check fid

figs(time(1:count-1),dt(1:count-1),'Time','Time Step','Time Step Versus Time')

saveas(gcf,'dt_vs_time','png');
saveas(gcf,'dt_vs_time','fig');

close(gcf);

figs((1:count-1),percent(1:count-1),'Time Step Number', ...
    'Percent Completed','Percent Completed Versus Time')

saveas(gcf,'percent_vs_time','png');
saveas(gcf,'percent_vs_time','fig');

close(gcf);

figs((1:count-1),non_it(1:count-1),'Time Step Number', ...
    'Nonlinear Iterations Required for Convergence', ...
    'Nonlinear Iterations Required for Convergence Versus Time')

saveas(gcf,'non_it_vs_time','png');
saveas(gcf,'non_it_vs_time','fig');

close(gcf);

figs((1:count-1),lin_it(1:count-1),'Time Step Number','Linear Iterations Required for Convergence', ...
    'Linear Iterations Required for Convergence Versus Time')

saveas(gcf,'lin_it_vs_time','png');
saveas(gcf,'lin_it_vs_time','fig');

close(gcf);

figs((1:count-1),inc_max_norm(1:count-1),'Time Step Number','Maximum Increment', ...
    'Maximum Increment Versus Time')

saveas(gcf,'max_inc_vs_time','png');
saveas(gcf,'max_inc_vs_time','fig');

close(gcf);

figs((1:count-1),max_resid_norm(1:count-1),'Time Step Number','Maximum Residual', ...
    'Maximum Residual Versus Time')

saveas(gcf,'max_resid_vs_time','png');
saveas(gcf,'max_resid_vs_time','fig');

close(gcf);

figs(time(1:count-1),nodes_after_adaption(1:count-1),'Time','Nodes after Adaption', ...
    'Nodes after Adaption Versus Time')

saveas(gcf,'adapt_nodes_vs_time','png');
saveas(gcf,'adapt_nodes_vs_time','fig');

close(gcf);

%% creates the bar plots for the time step and number of re

createfigure_bar_dt(dt(1:count-1))

saveas(gcf,'dt_bar_plot','png');
saveas(gcf,'dt_bar_plot','fig');

close(gcf);

createfigure_bar_non_it(non_it(1:count-1))

saveas(gcf,'non_it_bar_plot','png');
saveas(gcf,'non_it_bar_plot','fig');

close(gcf);

createfigure_bar_lin_it(lin_it(1:count-1))

saveas(gcf,'lin_it_bar_plot','png');
saveas(gcf,'lin_it_bar_plot','fig');

close(gcf);

%% read adh grid to get x,y nodal locations

grid=read_adh_grid([basename '.3dm']);
dt_interp=DelaunayTri(grid.x(:),grid.y(:));

fprintf(1,'Counting the Occurrences of each Worst Node\n')

significant_number_entries=1;
number_nodes=grid.nodes;
bad_node=zeros(number_nodes,1);

%% counts the number of occurrences for each "bad node" with adapted nodes
%  being attributed to the nearest mesh node

for i=1:count-1
   
   if(inc_max_norm_node(i) == -2)
     k=nearestNeighbor(dt_interp,[inc_max_norm_node_x inc_max_norm_node_y]); 
     node=k;
   else
     node=inc_max_norm_node(i);
   end
   
   bad_node(node)=bad_node(node)+1;
   
   if(resid_norm_worst_node(i) == -2)
     k=nearestNeighbor(dt_interp,[resid_norm_worst_node_x resid_norm_worst_node_y]); 
     node=k;
   else
     node=resid_norm_worst_node(i);
   end
     
   bad_node(node)=bad_node(node)+1;

end

%% sorts the number of occurrences of "bad" nodes with the highest being
%  at the top

bad=find(bad_node>significant_number_entries);
sorted_bad=zeros(length(bad),1);
sorted_bad_occ=zeros(length(bad),1);

count=1;
max_value=999999;

while(max_value > significant_number_entries)
   max_value=(max(bad_node));
   bad1=find(bad_node==max_value);
   if(length(bad1)==1)
      sorted_bad(count)=bad1;
      sorted_bad_occ(count)=bad_node(bad1);
      count=count+1;
      bad_node(bad1)=0;
   else
     for i=1:length(bad1)
        sorted_bad(count)=bad1(i);
        sorted_bad_occ(count)=bad_node(bad1(i));
        count=count+1;
        bad_node(bad1(i))=0;
     end
   end
   clear bad1;
end

%% opens and writes the "bad" node x,y,number of occurrences, node number
%  to the bad_nodes.out file.  This file can be pulled into sms as a
%  scatter set which can be pulled into SMS with the z value being the
%  number of occurrences of the "bad" node

fid1=fopen('bad_nodes.out','w');

for i=1:length(sorted_bad)
   fprintf(fid1,'%f %f %10i %10i\n',grid.x(sorted_bad(i)),grid.y(sorted_bad(i)), ...
       sorted_bad_occ(i), sorted_bad(i));
end

%% counts the number of occurrences that a "bad" nodes that lead to a 
%  time step failure

bad_failure=find(bad_node_failure>0);
bad_node_failure_occ=zeros(number_nodes,1);
sorted_bad_failure=zeros(length(bad_failure),1);
sorted_bad_occ_failure=zeros(length(bad_failure),1);

for i=1:length(bad_failure)
   
   if(bad_node_failure(i) == -2)
     k=nearestNeighbor(dt_interp,[inc_max_norm_node_x inc_max_norm_node_y]); 
     node=k;
   else
     node=bad_node_failure(i);
   end
   
   bad_node_failure_occ(node)=bad_node_failure_occ(node)+1;

end

%% sorts the number of occurrences of the "bad" nodes that lead to a time
%  step failure

count=1;
max_value=999999;

while(max_value > 0)
   max_value=(max(bad_node_failure_occ));
   if(max_value > 0)
     bad1=find(bad_node_failure_occ==max_value);
     if(length(bad1)==1)
        sorted_bad_failure(count)=bad1;
        sorted_bad_occ_failure(count)=bad_node_failure_occ(bad1);
        count=count+1;
        bad_node_failure_occ(bad1)=0;
     else
       for i=1:length(bad1)
          sorted_bad_failure(count)=bad1(i);
          sorted_bad_occ_failure(count)=bad_node_failure_occ(bad1(i));
          count=count+1;
          bad_node_failure_occ(bad1(i))=0;
       end
     end
   end
   clear bad1;
end

%% outputs an x,y,number of occurrences, node number file that can be
%  pulled into SMS to determine the location of nodes that cause time step
%  failures.

fid2=fopen('bad_nodes_failure.out','w');

fprintf(fid2,'TOTAL FAILURES = %i\n',sum(sorted_bad_occ_failure));

for i=1:count-1
   fprintf(fid2,'%f %f %10i %10i\n',grid.x(sorted_bad_failure(i)), ...
       grid.y(sorted_bad_failure(i)), sorted_bad_occ_failure(i), ...
       sorted_bad_failure(i));
end

fclose(fid1);
fclose(fid2);
fprintf(1,'Finished outputting the worst node information\n')

%% create figure showing the bad node locations

plot_bad_nodes(grid,sorted_bad_failure(1:count-1), ...
    sorted_bad_occ_failure(1:count-1))

saveas(gcf,'failures','png');
saveas(gcf,'failures','fig');

close(gcf);

plot_last_node(grid,last_node)

saveas(gcf,'last_worst_nodes','png');
saveas(gcf,'last_worst_nodes','fig');

close(gcf);

toc