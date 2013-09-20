function field=read_data(field_data_format,field_data_file)

fprintf(1,'Reading field data from file %s\n',field_data_file);

switch lower(field_data_format)
    
    case {'usgs'}
        field = read_usgs(field_data_file);   
    case {'noaa'} 
        field = read_noaa(field_data_file); 
    case {'bush_1'}
        field = read_bush_canal_1(field_data_file);
    case {'bush_2'} 
        field = read_bush_canal_2(field_data_file);
    case {'data'}
        field = read_data_date_time_value(field_data_file);
    case {'rivergages'}
        field = read_rivergages(field_data_file);
    case {'mtog'}
        field=read_yyyy_mm_dd_hh_mm_value(field_data_file);
    case {'caer'}
        field=read_caer(field_data_file);
    case {'caer_1'}
        field=read_caer_1(field_data_file);
    case {'mat'}
        field=load(field_data_file);
    otherwise
        error('\n\n FORMAT %s NOT FOUND FOR FILE = %s \n\n', ...
            field_data_format,field_data_file)
end