function plotvar(varname, type, instants, zz, xx, yy)
%MOHID Water Modelling System.
%Copyright (C) 1985, 1998, 2002, 2006. 
%MARETEC, Instituto Superior T�cnico, Technical University of Lisbon. 
%
%   Handles the reading and the vector, contour and color plotting 
%   of a given MOHID netcdf file
%
%Function plotvar(varname, type, instants, zz, xx, yy)
%Remember to edit the config_*.m files prior to running the plotvar.
%
%   varname  -> the name of the variable (e.g. 'salinity')
%   type     -> 'map', 'xz', 'yz', 'xyz' type of section of data to plot
%   instants -> 1-D array containing the individual instants
%   zz   -> 1-D array containing the individual layers or the layers extrema
%   xx  -> 1-D array containing the individual x slices or the lon extrema
%   yy  -> 1-D array containing the individual y slices or the lat extrema
%
%Functions hierarchy:
%{plotvar, inqdataset} --> root functions
%   do_{config, coords, attr, user} --> configuration functions
%   do_{maps, xzs, xys, xyzs} --> loop functions sorted by plot type
%       get_{map, xz,xy, xyz} --> get the slice of data sorted by plot type
%       do_{map, xz, xy, xyz} --> plot the slice of data sorted by type
%       do_plot --> plots coastline, pcolors, contours and quivers.
%
%Configuration files:
%     config_{global, color, contour, vectors, output, coastline}.m
%
%   global  -> sets the source netcdf or mat file
%   color -> sets the colormap limits and a scaling factor
%   contour -> sets the auto or manual contour lines values
%   vectors -> sets the u and v variable names, the scale legend and
%               position
%   output  -> sets the directory output and the image file extensions
%               (eps, epsc, png ...)
%   coastline -> specifies the coastline mat file
%
%
% ex: plotvar('bathymetry','map', 0, 0, [1 117], [1 177])
%     plotvar('u','map', [23 24], 42, [1 117], [1 177])
%     plotvar('ssh','map', 25, 0, [1 117], [1 177])
%     plotvar('u','xz', 24, [10 42], [1 70], 77)
%
    s_cfg = do_config('.'); %Config defines basically the dataset
    s_cds = do_coords(s_cfg); %Coords extracts the coordinates (x,y,z,t)
    s_attr = do_attr(s_cfg, varname); %Attr extracts the variable attributes
    s_user = do_user(type, instants, zz, xx, yy); %Builds the appropriate user defined structure
    
    switch type
        
        case 'map'
            do_maps(s_cfg, s_cds, s_attr, s_user);
            
        case 'xz'
            do_xzs(s_cfg, s_cds, s_attr, s_user);
            
        case 'yz'
            do_yzs(s_cfg, s_cds, s_attr, s_user);
            
        case 'xyz'
            do_xyzs(s_cfg, s_cds, s_attr, s_user);
            
        otherwise
            disp('Error in type!');
            return
    end
    
    disp('Done!');
    
    if s_cfg.closematlab
        exit;
    end
