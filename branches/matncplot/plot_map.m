function plot_map(s_roi)
%function plot_map(s_roi)
%
%Plots the map
%
coastline_file = 'coastline.mat';
save_coastline = false;          
coastline_res = 'i';                %'l'=low, 'i'=intermediate, 'h'=high, 'f'=full;          
coastline_patch_color = [.5 .5 .5]; %RGB scale 0-1

projection = 'miller';
grid_fontsize = 7;
title_fontsize = 10;

lonlim = [min(min(s_roi.c_x2d)) max(max(s_roi.c_x2d))];
latlim = [min(min(s_roi.c_y2d)) max(max(s_roi.c_y2d))];
%draws projection
m_proj(projection,'long', lonlim,'lat', latlim);
    
% Draws the map frame
%draws the coordinates
m_grid('box','fancy','linestyle','none','fontsize', grid_fontsize);
%draws the coast line
m_usercoast(coastline_file,'patch', coastline_patch_color);