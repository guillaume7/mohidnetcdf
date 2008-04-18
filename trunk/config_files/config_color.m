global plot_color;
global colorlimits;
global colormap_type;
global shading_type;
global scalecolor;
global autocolortitle;
global colorbartitle;

plot_color = true;
colorlimits = [7 18]; %Salt --> xz
scalecolor = 1.; %
colorlimits = colorlimits .* scalecolor;
colormap_type = 'jet';
colormap_inv = false;
shading_type = 'interp';
autocolortitle = false;
colorbartitle = 'Temperature (ºC)';
%colorbartitle = 'Salinity';
%colorbartitle = 'Meridional velocity (m s^{-1})';
%colorbartitle = 'Zonal velocity (m s^{-1})';

if colormap_inv
    MAP = colormap(colormap_type);
    siz = size(MAP);
    colormap_type = zeros(siz);
    n = siz(1);
    for i=1:n
        colormap_type(i,:)=MAP(n+1-i,:);
    end
end