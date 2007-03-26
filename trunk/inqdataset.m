function [s_cds] = inqdataset
%function [s_cds] = inqdataset
%
%Returns information about the space and time dimensions of the netcdf
%dataset.

s_cfg = do_config('.'); %Config defines basically the dataset
s_cds = do_coords(s_cfg); %Coords extracts the coordinates (x,y,z,t)

disp({'lat: ', s_cds.lat2d});
disp(s_cds.latlim);
disp( {'lon : ', s_cds.lon2d});
disp(s_cds.lonlim);
disp({'layers: ', s_cds.zsize});
disp( s_cds.zsize(1));
disp( {'time: ', s_cds.serial_time});
disp( s_cds.gregorian_time(1,:));

end