function [s_var] = do_var(s_cfg, s_cds, s_att, instant, layer)   
%function [s_var] = do_var(s_cfg, s_cds, s_attr, instant, layer)   
%Extracts the variable subset

if layer > 0
    var2d = getnc(s_cfg.file, s_att.varname, [instant layer -1 -1 ],  [instant layer -1 -1], -1, -2, s_cfg.change_miss, s_cfg.new_miss);
else
    var2d = getnc(s_cfg.file, s_att.varname, [instant -1 -1 ],  [instant -1 -1], -1, -2, s_cfg.change_miss, s_cfg.new_miss);
end

s_var = struct( ...
        'var2d', var2d, ...
        'instant', instant, ...
        'time', s_cds.serial_time(instant), ...
        'gtime', s_cds.gregorian_time(instant), ...
        'layer', layer, ...
        'depth', s_cds.zsize(layer) ...
        );
