# User's Guide #

## help plotvar ##

```
%MOHID Water Modelling System.
%Copyright (C) 1985, 1998, 2002, 2006. 
%MARETEC, Instituto Superior Técnico, Technical University of Lisbon. 
%
%   Handles the reading and the vector, contour and color plotting 
%   of a given MOHID netcdf file
%
%function plotvar(varname, type, instants, zz, xx, yy)
%Remember to edit the config_*.m files prior to running the plotvar function.
%
%  * varname  -> the name of the variable (e.g. 'salinity')
%  * type     -> 'map', 'xz', 'yz', 'xyz' type of section of data to plot
%  * instants -> 1-D array containing the individual instants
%  * zz   -> 1-D array containing the individual layers or the layers extrema
%  * xx  -> 1-D array containing the individual x slices or the lon extrema
%  * yy  -> 1-D array containing the individual y slices or the lat extrema
```

### Example syntax ###
  * plotvar('bathymetry','map', 0, 0, [117](1.md), [177](1.md))
  * plotvar('u','map', [24](23.md), 42, [117](1.md), [177](1.md))
  * plotvar('ssh','map', 25, 0, [117](1.md), [177](1.md))
  * plotvar('u','xz', 24, [42](10.md), [70](1.md), 77)

### Configuration files: ###
```
     config_{global, color, contour, vectors, output, coastline}.m
```
  * global  -> sets the source netcdf or mat file
  * color -> sets the colormap limits and a scaling factor
  * contour -> sets the auto or manual contour lines values
  * vectors -> sets the u and v variable names, the scale legend and position
  * output  -> sets the directory output and the image file extensions (eps, epsc, png...)
  * coastline -> specifies the coastline mat file

### Functions hierarchy: ###
```
{plotvar, inqdataset} --> root functions
  do_{config, coords, attr, user} --> configuration functions
  do_{maps, xzs, xys, xyzs} --> loop functions sorted by plot type
  get_{map, xz,xy, xyz} --> get the slice of data sorted by plot type
  do_{map, xz, xy, xyz} --> plot the slice of data sorted by type
  do_plot --> plots coastline, pcolors, contours and quivers.
```