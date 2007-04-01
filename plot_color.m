function plot_color(s_cfg, s_roi, var2d, type)
%function plot_color(s_cfg, s_roi, var2d)
%
%Plots a graphic with pcolor
%
    
%%%draws the colored map array %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%       
switch(type)
    case 'map'
        m_pcolor( s_roi.c_x2d, s_roi.c_y2d, var2d);
    otherwise
        pcolor( s_roi.c_x2d, s_roi.c_y2d, var2d);
end    
    
shading( s_cfg.shading_type);
colormap( s_cfg.colormap_type);
caxis( [s_cfg.limits(1) s_cfg.limits(end)]);

%draws colorbar
h = colorbar('vert');
colorbartitle = [s_cfg.varname,' (', s_cfg.units, ')', ...
' x ', num2str(1/s_cfg.scaleunits, '%0.3g')];
set(get(h,'title'),'string', colorbartitle);