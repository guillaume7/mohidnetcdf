function [vcut, vwidth]=do_cut(file, type, cut, width)
%function cuts=do_cut(file, type, cut, width)
%
%Returns the lat/lon cut coordinate

lon = nc_varget(file,'lon');
lat = nc_varget(file,'lat');

switch type
    case 'xz'
        vcut=lat(cut);
    case 'yz'
        vcut=lon(cut);
    otherwise
end
