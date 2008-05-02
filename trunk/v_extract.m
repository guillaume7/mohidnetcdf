function [v, v2d, vlim] = v_extract(file,variable,start,count,res)
%function [v, v2d, vlim] = v_extract(file,variable,start,count,res)

    v = nc_varget(file, variable, start, count);
    vlim = [min(min(v)) max(max(v))];
    v2d = ( ones(res,1) * [1:res] - 1 ) ./ ( res - 1 );
    v2d = vlim(1) + v2d .* ( vlim(2) - vlim(1) );