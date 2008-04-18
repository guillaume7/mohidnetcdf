function 2dslab = gethdf5_2d(filename, varname, bl_corner, tr_corner, stride)
%function 2dslab = gethdf5_2d(filename, varname, bl_corner, tr_corner,
%stride)
%Returns a 2D slab from a MOHID HDF5 dataset. 
%If the HDF5 dataset is not a MOHID dataset, then simply use the function
%hdf5read().
%
% *_corner - tzxy (velocity), zxy(waterpoints), xy(bathymetry), txy(level)?

%Get the type of the dimensions of the hyperslab vector
type = gettype(varname, bl_corner, tr_corner);

%Are we time dependent or are we not?
%Get the space slab
if isempty(regexpi(type,'t'));
    %x
    bl_xyz = bl_corner;
    tr_xyz = tr_corner;
    data = hdf5read(filename, varname);
    
else
    %t
    bl_xyz = bl_corner(2:end);
    tr_xyz = tr_corner(2:end);
    data = hdf5read(filename, [varname,'_',sprintf('%05d',bl_corner(1))]);
    
end

%Is it a 3D or a 2D slab?
%Extract the 2D slice from the data slab
if isempty(regexpi(type,'zxy'));    
    %2
    2dslab = data(  ...
                    bl_xyz(1):stride:tr_xyz(1), ...
                    bl_xyz(2):stride:tr_xyz(2), ...
                );

else    
    %3
    2dslab = data(  ...
                    bl_xyz(1):stride:tr_xyz(1), ...
                    bl_xyz(2):stride:tr_xyz(2), ...
                    bl_xyz(3):stride:tr_xyz(3), ...
                );

end