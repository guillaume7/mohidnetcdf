global file
global projection
global grid_fontsize
global title_fontsize
global override_ll
global lat_lon_override_file
global closematlab

file = 'http://data.mohid.com/opendap/nph-dods/mercator-ist/Portugal/20061227_Portugal_Hydrodynamic.nc';
projection = 'miller';
grid_fontsize = 7;
title_fontsize = 10;

%override latitude and longitude and load from another file
override_ll = false;
lat_lon_override_file = 'latlon-tagus-91x105.mat';

%close matlab after plotting everything
closematlab = false;