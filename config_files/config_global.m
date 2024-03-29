global filetype
global matfile
global matdims
global file
global maskit
global projection
global grid_fontsize
global title_fontsize
global override_ll
global lat_lon_override_file
global closematlab

%NOTE: use 'nc' for snctools and 'cdf' for netcdf toolbox
filetype = 'nc'; % 'nc', 'mat', 'cdf', or 'hdf5'
matfile = 'meanOpModel.mat'; % y, x, z
matdims = 3; %1d 2d 3d 4d etc ...
%file = 'mean_Portugal_Waterproperties.nc';
%file = '\\guillaume\Aplica\PreOp-Model\MATLAB\20070314_Portugal_Hydrodynamic.nc';
%file = 'http://data.mohid.com/opendap/nph-dods/mercator-ist/Portugal/20061227_Portugal_Hydrodynamic.nc';
file ='D:\projectos\GCode\MohidNetcdf\20080326_Portugal_WaterProperties.nc';
%file = 'D:\projectos\GCode\MohidNetcdf\20080130_Portugal_Hydrodynamic.nc';
maskit = 'interpol' % interpol, mask
projection = 'miller';
grid_fontsize = 7;
title_fontsize = 10;

%override latitude and longitude and load from another file
override_ll = false;
lat_lon_override_file = 'latlon-tagus-91x105.mat';

%close matlab after plotting everything
closematlab = false;