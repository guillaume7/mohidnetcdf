function [s_coords] = do_coords(s_cfg)
%function [s_coords] = do_coords(s_cfg)
%
%This function gets the horizontal and vertical coordinates
%
%     s_coords = struct(...
%            'lat2d', lat2d, ...
%            'lon2d', lon2d, ...
%            'units', units, ...
%            'colorlim', colorlim, ...
%            'latlim', latlim, ...
%            'lonlim', lonlim, ...
%            'zsize', zsize, ...
%            'gregorian_time', gregorian_time, ...
%            'serial_time', serial_time);

    disp('Loading coords from MOHID netcdf output file')
    pack %optimize memory for getnc

    if s_cfg.override_ll==true;
        load lat_lon_override_file
        lat = lat(:,1);
        lon = lon(1,:);
         %computes limits
        iub = size(lat,1);
        jub = size(lon,2);
    else

        switch (s_cfg.filetype)
        
            case 'cdf'
            lon = getnc(s_cfg.file,'lon', -1, -1, -1, -2, s_cfg.change_miss, s_cfg.new_miss);
            lat = getnc(s_cfg.file,'lat', -1, -1, -1, -2, s_cfg.change_miss, s_cfg.new_miss);
            
            case 'nc'
            lon = nc_varget(s_cfg.file,'lon');
            lat = nc_varget(s_cfg.file,'lat');

            otherwise
            
        end
        
        %computes limits
        iub = size(lat,1);
        jub = size(lon,1);
        
    end

    switch (s_cfg.filetype)
        
        case 'cdf'
            zsize = getnc(s_cfg.file,'depth', -1, -1, -1, -2, s_cfg.change_miss, s_cfg.new_miss);
            [gregorian_time, serial_time] = timenc(s_cfg.file);
            
        case 'nc'
            zsize = nc_varget(s_cfg.file,'depth');
            serial_rel = nc_varget(s_cfg.file,'time');
            serial_rel = serial_rel / 86400.;
            time_units = nc_attget(s_cfg.file,'time','units');
            [gregorian_base, rescale_serial_rel, serial_base_jd, serial_base] = parsetnc(time_units);
            serial_time_jd = serial_rel + serial_base_jd;

            if isempty(serial_time_jd)
                gregorian_time = [];
                serial_time = [];
            else
                
            gregorian_time = get_calendar_date(serial_time_jd);
            serial_time = datenum(gregorian_time(:, 1), gregorian_time(:, 2), ...
			gregorian_time(:, 3), gregorian_time(:, 4), ...
			gregorian_time(:, 5), gregorian_time(:, 6));
        
        end

        otherwise
            
    end
    

    disp('Computing coordinates...');
    
    if s_cfg.override_ll==true;
        lat2d=lat;
        lon2d=lon;
    else
        for i = 1:iub;
        for j = 1:jub;
            lat2d(i,j) = lat(i);
            lon2d(i,j) = lon(j);
        end
        end
    end

    left = min(lon)+0.01;
    right = max(lon)-0.01;
    bottom = min(lat)+0.01;
    top = max(lat)-0.01;
    lonlim = [left right];
    latlim = [bottom top];

    disp('Started plotting...');
    
    if s_cfg.save_coastline;
        do_coastline;
    end

    s_coords = struct( ...
            'lat2d', lat2d, ...
            'lon2d', lon2d, ...
            'latlim', latlim, ...
            'lonlim', lonlim, ...
            'zsize', zsize, ...
            'gregorian_time', gregorian_time, ...
            'serial_time', serial_time);
