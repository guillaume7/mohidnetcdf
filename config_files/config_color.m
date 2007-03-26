global plot_color;
global colorlimits;
global colormap_type;
global shading_type;
global scalecolor;

plot_color = true;
colorlimits = [-.5 .5]; %Put here your scale limits
scalecolor = 1.; %
colorlimits = colorlimits .* scalecolor;
colormap_type = 'jet';
colormap_inv = false;
shading_type = 'interp';

if colormap_inv
    MAP = colormap(colormap_type);
    siz = size(MAP);
    colormap_type = zeros(siz);
    n = siz(1);
    for i=1:n
        colormap_type(i,:)=MAP(n+1-i,:);
    end
end