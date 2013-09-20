
clear

% da files to be combined

basename{1}='MTOG_function_2004_2005_3300-4040';
basename{2}='MTOG_function_2004_2005_4040-4760';

% new da file to be created

new_da_file='basetest';

% start time and stop time for the new da file

start_time='01/01/2004 00:00:00';
stop_time='01/01/2005 00:00:00';

% attributes to put in the new da file

attributes='wse sal';

% subroutine to do the actual combining of the da files

combine_da_files(basename,new_da_file,attributes,start_time,stop_time)

