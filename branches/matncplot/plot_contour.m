function plot_contour(s_cfg, s_roi, var2d, type)
% function plot_contour(s_cfg, s_roi, var2d, type)
%
%%%draws the contours %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if s_cfg.autolimits
    k=10.0; %number of contour lines
    cscale = [0:k]./k * ...
    ( s_cfg.limits(end)-s_cfg.limits(1)) ...
    + s_cfg.limits(1);
else
    cscale = s_cfg.limits;
end
ss = find(cscale >= 0.);
if min(size(ss)) > 0
    switch(type)
        case 'map'
            [cs,h]=m_contour( s_roi.c_x2d, s_roi.c_y2d, var2d, cscale(ss), 'k');
        otherwise
            [cs,h]=contour( s_roi.c_x2d, s_roi.c_y2d, var2d, cscale(ss), 'k');
    end
    clabel(cs,h,'fontsize',7,'labelspacing',288);
end
    
hold on;
    
ss = find(cscale < 0.);
if min(size(ss)) > 0
    switch(type)
        case 'map'
            [cs,h]=m_contour( s_roi.c_x2d, s_roi.c_y2d, var2d, cscale(ss), 'k--');
        otherwise
            [cs,h]=contour( s_roi.c_x2d, s_roi.c_y2d, var2d, cscale(ss), 'k--');
    end    
    clabel(cs,h,'fontsize',7,'labelspacing',288);
end