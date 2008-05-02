function [serial_time, gregorian_time]=do_time(file)
%function [serial_time, gregorian_time]=do_time(ncfile)
%
%Returns an array with the serial time from the netcdf file.

serial_rel = nc_varget(file,'time');
serial_rel = serial_rel / 86400.;
time_units = nc_attget(file,'time','units');
[gregorian_base, rescale_serial_rel, serial_base_jd, serial_base] = ...
    do_parsetnc(time_units);
serial_time_jd = serial_rel + serial_base_jd;

if isempty(serial_time_jd)
    gregorian_time = [];
    serial_time = [];
else

    gregorian_time = get_calendar_date(serial_time_jd);
    serial_time = datenum(  gregorian_time(:, 1), ...
                        gregorian_time(:, 2), ...
                        gregorian_time(:, 3), ...
                        gregorian_time(:, 4), ...
                        gregorian_time(:, 5), ...
                        gregorian_time(:, 6));
end