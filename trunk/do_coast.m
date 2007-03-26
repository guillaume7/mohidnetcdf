function do_coast(lat2d, lon2d,lonlim,latlim,batim,colorlim)
%MOHID Water Modelling System.
%Copyright (C) 1985, 1998, 2002, 2006. 
%MARETEC, Instituto Superior Técnico, Technical University of Lisbon. 
%
%   Plots the bathymetry
%
%do_coast(lat2d, lon2d,lonlim,latlim,batim,colorlim)
%
%   lat2d    -> 2D array with latitude coordinates for the center of each grid cell
%   lon2d    -> 2D array with latitude coordinates for the center of each grid cell 
%   lonlim   -> 1D array with left and right limits of the map  
%   latlim   -> 1D array with bottom and top limits of the map  
%   batim    -> 2D array with  bathymetry values
%   colorlim -> 1D array with minimum and maximum values limits of the bathymetry

    setup_plots;

    disp('Started plotting bathymetry...');

    %draws projection
    m_proj(projection,'long',lonlim,'lat',latlim);
    %m_proj('lambert','long',lonlim,'lat',latlim);

    %sets the plot invisible
    set(gcf, 'Visible', 'Off');
    
    %draws the contour 
    [cs,h]=m_contour(lon2d,lat2d,batim,batim_contour_scale,'k');
    clabel(cs,h,'fontsize',7,'labelspacing',288);
    
    hold on
    
    m_pcolor(lon2d,lat2d,batim);shading(shading_type);
    %caxis(colorlim);
    colormap(colormap_type);

    %draws colorbar
    h=colorbar('vert');
    name = 'Depth';
    if max(batim_contour_scale) < 20
        units = 'km';
    else
        units = 'm';
    end
    colorbartitle = [name,' (', units, ')'];
    set(get(h,'title'),'string',colorbartitle);
       
    %draws the grid
    m_grid('box','fancy','linestyle','none','fontsize',grid_fontsize);
    
    %this fixes the problem of the box or the countour colors disapearing 
    set(findobj('tag','m_grid_color'),'facecolor','none');    

    disp('Loading and saving coastline...');
    %draws the coast line
    
    disp('Reloading coastline...');
    %set land with grey color
    m_usercoast(coastline_file,'patch', coastline_patch_color);
    
    disp('Adding title to image...');
    title('bathymetry','fontsize',14);
    
    %do_scale(lonlim,latlim, hor_scale_x, hor_scale_y, hor_scale_size);
    
    disp('Saving bathymetry as image...');
    %outfilename = ['bathymetry.eps'];
    outfilename = [output_dir,'bathymetry','.',output_extension];
    %saves the image
    %saveas(gcf, outfilename, 'epsc');
    saveas(gcf, outfilename, output_format);
    close(gcf);

    mess = ['plotted : ',outfilename];
    disp(mess);

end 