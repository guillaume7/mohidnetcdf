function hyperslab = gethdf5(filename, varname, bl_corner, tr_corner, stride)
%function hyperslab = gethdf5(filename, varname, bl_corner, tr_corner,
%stride)
%Returns a hyperslab from a MOHID HDF5 dataset. 
%If the HDF5 dataset is not a MOHID dataset, then simply use the function
%hdf5read().
%
% *_corner - tzxy (velocity), zxy(waterpoints), xy(bathymetry), txy(level)?

%Get the dimensions of the hyperslab vector
type = gettype(varname, bl_corner);

switch (type)
    
    case 'tzxy'
        for i = bl_corner(1):1:tr_corner(1);
            data = hdf5read(filename, [varname,'_',sprintf('%05d',i)]);
%            hyperslab(i,:,:,:) = data(...
%                    bl_corner(2):stride:tr_corner(2), ...
%                    bl_corner(3):stride:tr_corner(3), ...
%                    bl_corner(4):stride:tr_corner(4), ...
%            );
        end
        
    case 'txy'
        for i = bl_corner(1):1:tr_corner(1);
            data = hdf5read(filename, [varname,'_',sprintf('%05d',i)]);
%            hyperslab(i,:,:,:) = data(...
%                    bl_corner(2):stride:tr_corner(2), ...
%                    bl_corner(3):stride:tr_corner(3), ...
%            );
        end
        
    case 'zxy'
        data = hdf5read(filename, varname);
        hyperslab = data;        
        
    case 'xy'
        data = hdf5read(filename, varname);
        hyperslab = data;
        
    otherwise
        disp 'gethdf5 - Wrong size in variable bl_corner - ERR1';
        
end