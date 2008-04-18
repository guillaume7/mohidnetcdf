function type = gettype (varname, bl_corner, tr_corner)
%function type = gettype (varname, len)
%
%Returns 'tzxy', 'txy', 'zxy' or 'xy'
%by analysis of len and varname of a MOHID HDF5 dataset
%

len = length(bl_corner);

switch(len)
    
    %tzxy
    case 4
        type = 'tzxy';
    
    %txy or zxy?
    case 3
        if      isempty(regexpi(varname,'/Grid'));
            type = 'txy';
        elseif  isempty(regexpi(varname,'Openpoints')) && ...
                isempty(regexpi(varname,'VerticalZ'));
            type = 'zxy';
        else
            type = 'txy';
        end
    
    %xy
    case 2
        type = 'xy';
        
    otherwise
        disp 'gettype - Wrong value of len - ERR1';
        type = 'error';
        
end