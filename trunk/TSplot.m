function h=TSplot(dens, S , T, Sdata, Tdata, depth, titles)
%function h=TSplot(dens, S, T, Sdata, Tdata, depth, titles)

hold off;

%Draws the isopycnals.
[cs,h]=contour(S, T, dens,'k');
clabel(cs,h,'fontsize',7,'labelspacing',600);

hold on;

%Scatters the T-S data.
thick=3;
siz=size( Sdata);
for i=1:siz(2)
    h=scatter(  Sdata(:,i), ...
                Tdata(:,i), ...
                thick, ...
                -1 * depth);
end

%Computes the colorbar tick labels for "depth".
k=6; %6 is the default number of ticks in the colorbar.
zlim = [min( depth) max( depth)];
zscale = [k:-1:0.]/k * ( zlim(2) - zlim(1)) + zlim(1);

%Draws the colorbar (depth).
h=colorbar;
set(get(h,'title'), ...
    'string', 'Depth (m)');
set(h, 'YTickLabel', fix(zscale));

%Adds title to the figure and labels to the axes.
title(['\Theta - S diagram ', titles]);
xlabel('Salinity');
ylabel('Potential temperature (ºC)');