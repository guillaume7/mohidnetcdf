function s_plot = do_xz( var2d, s_cfg, s_cds, s_at, s_us)
%MOHID Water Modelling System.
%Copyright (C) 1985, 1998, 2002, 2006. 
%MARETEC, Instituto Superior Técnico, Technical University of Lisbon. 
%
%   Does color plots of scalars xz cuts
%

%%%%%%%%%%%% Plot-type variables %%%%%%%%%
    type = 'xz';

    %writes the title
    if s_us.ysection > 0
        zt = num2str(s_cds.lat2d(s_us.ysection,1), '%2.2f');
        zts = [', Lat: ', zt, 'º N'];
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
    x_lab = 'Longitude (ºE)';
    y_lab = 'Depth (m)';
    
    %Calculates coordinates subset
    c_x = s_cds.lon2d( s_us.ysection, s_us.xrange(1):s_us.xrange(2));
    c_z = s_cds.zsize( s_us.zrange(1):s_us.zrange(2))*(-1.);
    c_x2d = ones(size(c_z)) * c_x;
    c_y2d = c_z * ones(size(c_x));
    
    %Is it the meridional velocity we want to plot?
    if (strcmp(s_at.varname,'v') || strcmp(s_at.varname,'Vel_Y'))
       var2d = -1 .* var2d;
    end

    if s_cfg.filetype == 'nc'
        c_x2d = c_x2d';
        c_y2d = c_y2d';
    end

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

