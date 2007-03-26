function [s_attr] = do_attr(s_cfg, varname)    
%function [s_attr] = do_attr(s_cfg, varname)    
%
%       s_attr = struct(...
%            'units', units,...
%            'colorlim', colorlim,...
%            'varname', varname);

    %reads variable attributes
    [units, attunits] = attnc(s_cfg.file, varname, 'units');
    [mincolor, minimum] = attnc(s_cfg.file, varname, 'minimum');
    [maxcolor, maximum] = attnc(s_cfg.file, varname, 'maximum');
    colorlim = [mincolor maxcolor];

    s_attr = struct(...
            'units', units,...
            'colorlim', colorlim,...
            'varname', varname);
        
end