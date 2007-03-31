function s_plot = do_map( var2d, s_cfg, s_cds, s_at, s_us)
%MOHID Water Modelling System.
%Copyright (C) 1985, 1998, 2002, 2006. 
%MARETEC, Instituto Superior Técnico, Technical University of Lisbon. 
%
%   Does color plots of scalars
%
    
%%%%%%%%%%%% Plot-type variables %%%%%%%%%
    type = 'map';

    %writes the title
    if s_us.layer > 0
        zt = int2str(s_cds.zsize(s_us.layer));
        zts = [', Depth: ', zt, ' m'];
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
    x_lab = '';
    y_lab = '';

    c_x2d = s_cds.lon2d( s_us.yrange(1):s_us.yrange(2), s_us.xrange(1):s_us.xrange(2));
    c_y2d = s_cds.lat2d( s_us.yrange(1):s_us.yrange(2), s_us.xrange(1):s_us.xrange(2));
    
    s_plot = struct(...
            'type', type, ...
            'var2d', var2d, ...
            'zt', zt, ...
            'zts', zts, ...
            'datet', datet, ...
            'dates', dates, ...
            'x_lab', x_lab, ...
            'y_lab', y_lab, ...
            'c_x2d', c_x2d, ...
            'c_y2d', c_y2d);
%%%%%%%%%%%% End of Plot-type variables %%%%%%%%%

