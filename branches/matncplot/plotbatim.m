function plotbatim;
%MOHID Water Modelling System.
%Copyright (C) 1985, 1998, 2002, 2006. 
%MARETEC, Instituto Superior Técnico, Technical University of Lisbon. 
%
%   Handles the reading and contour and color plotting 
%   of a given MOHID netcdf file
%
%plotbatim

    setup_plots;
    
    disp('Loading from MOHID netcdf output file')
    pack %optimize memory for getnc

    if override_ll==true;
        load lat_lon_override_file
        lat = lat(:,1);
        lon = lon(1,:);
         %computes limits
        iub = size(lat,1);
        jub = size(lon,2);
    else
        lon = getnc(file,'lon', -1, -1, -1, -2, change_miss, new_miss);
        lat = getnc(file,'lat', -1, -1, -1, -2, change_miss, new_miss);
         %computes limits
        iub = size(lat,1);
        jub = size(lon,1);
    end

    disp('Computing coordinates...');
    
    if override_ll==true;
        lat2d=lat;
        lon2d=lon;
    else
        for i = 1:iub;
        for j = 1:jub;
            lat2d(i,j) = lat(i);
            lon2d(i,j) = lon(j);
        end
        end
    end

    left = min(lon)+0.01;
    right = max(lon)-0.01;
    bottom = min(lat)+0.01;
    top = max(lat)-0.01;
    lonlim = [left right];
    latlim = [bottom top];

    %reads batim and prepares coastline
    batim =  getnc(file,'bathymetry', -1, -1, -1, -2, change_miss, new_miss);
    [mincolor, minimum] = attnc(file, 'bathymetry', 'minimum');
    [maxcolor, maximum] = attnc(file, 'bathymetry', 'maximum');
    colorlim = [mincolor maxcolor];
    
    if save_coastline;
        do_coastline;
    end

    do_coast(lat2d, lon2d, lonlim, latlim, batim/1000., colorlim);

    disp('Done!');

end