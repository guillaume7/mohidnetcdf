function s_plot = do_yz( var2d, s_cfg, s_cds, s_at, s_us)
%MOHID Water Modelling System.
%Copyright (C) 1985, 1998, 2002, 2006. 
%MARETEC, Instituto Superior Técnico, Technical University of Lisbon. 
%
%   Does color plots of scalars xz cuts
%

%%%%%%%%%%%% Plot-type variables %%%%%%%%%
    type = 'yz';

    %writes the title
    if s_us.xsection > 0
        zt = num2str(s_cds.lon2d(1,s_us.xsection), '%2.2f');
        zts = [', Lon: ', zt, 'º E'];
    else
        zt = '';
        zts = '';
    end
    if s_us.instant > 0
        datet = int2str(s_us.instant);
        dates = datestr(s_cds.serial_time(s_us.instant), 'dd-mmm-yyyy HH:MM:SS');
    else
        datet = '';
        dates = '';
    end
    
    %Labels
    x_lab = 'Latitude (ºN)';
    y_lab = 'Depth (m)';
    
    %Calculates coordinates subset
    c_x = s_cds.lat2d( s_us.yrange(1):s_us.yrange(2), s_us.xsection)';
    c_z = s_cds.zsize( s_us.zrange(1):s_us.zrange(2))*(-1.);
    c_x2d = ones(size(c_z)) * c_x;
    c_y2d = c_z * ones(size(c_x));

    %if s_cfg.filetype == 'nc'
    %    c_x2d = c_x2d';
    %    c_y2d = c_y2d';
    %end

    s_plot = struct(...
            'type', type, ...
            'var2d', var2d', ...
            'zt', zt, ...
            'zts', zts, ...
            'datet', datet, ...
            'dates', dates, ...
            'x_lab', x_lab, ...
            'y_lab', y_lab, ...
            'c_x2d', c_x2d, ...
            'c_y2d', c_y2d);
%%%%%%%%%%%% End of Plot-type variables %%%%%%%%%

