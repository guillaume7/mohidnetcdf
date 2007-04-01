function [s_cfg] = do_attr(s_cfg)    
%function [s_cfg] = do_attr(s_cfg)    
%
%            'extrema', extrema,...
%            'units', units,...

    %reads variable attributes
    [units, attunits] = attnc(s_cfg.file, s_cfg.varid, 'units');
    [mincolor, minimum] = attnc(s_cfg.file, s_cfg.varid, 'minimum');
    [maxcolor, maximum] = attnc(s_cfg.file, s_cfg.varid, 'maximum');
    limits = [mincolor maxcolor];

    s_cfg.extrema = limits;

    if s_cfg.autoatt
        s_cfg.units = units;
    end
