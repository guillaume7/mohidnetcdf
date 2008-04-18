function [s_attr] = do_attr(s_cfg, varname)    
%function [s_attr] = do_attr(s_cfg, varname)    
%
%       s_attr = struct(...
%            'units', units,...
%            'colorlim', colorlim,...
%            'varname', varname);

    %reads variable attributes
    switch (s_cfg.filetype)
        
        case 'cdf'
        [units, attunits] = attnc(s_cfg.file, varname, 'units');
        [mincolor, minimum] = attnc(s_cfg.file, varname, 'minimum');
        [maxcolor, maximum] = attnc(s_cfg.file, varname, 'maximum');
    
        case 'nc'
        units = nc_attget(s_cfg.file, varname, 'units');
        mincolor = nc_attget(s_cfg.file, varname, 'minimum');
        maxcolor = nc_attget(s_cfg.file, varname, 'maximum');
            
        otherwise
            
    end

    colorlim = [mincolor maxcolor];
    
    s_attr = struct(...
            'units', units,...
            'colorlim', colorlim,...
            'varname', varname);
