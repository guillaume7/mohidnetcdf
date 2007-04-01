function var2d = get_var(s_cfg, s_roi)
%function var2d = get_var(s_cfg, s_roi)
%
% Extracts the MOHID netcdf data with the landmask
%
    if s_cfg.filetype == 'mat'
        var2d = getmat(s_cfg.matfile, ...
                    s_cfg.varid, ...
                    s_roi.m_bl_corner, ...
                    s_roi.m_tr_corner, ...
                    1 ...  %stride
                    );
    else
        var2d = getnc(s_cfg.file, ...
                      s_cfg.varid, ...
                      s_roi.bl_corner,  ... % bl_corner
                      s_roi.tr_corner, ... %tr_corner
                      -1, -2, ... %stride, order
                      s_cfg.change_miss, ...
                      s_cfg.new_miss ...
                      );
    end
    mask =  getnc(s_cfg.file, ...
                'mask', ...
                s_roi.m_bl_corner,  ... % bl_corner (tzxy)
                s_roi.m_tr_corner, ... %tr_corner (tzxy)
                -1, -2, ... %stride, order
                s_cfg.change_miss, ...
                s_cfg.new_miss ...
                );
                  
    nn = find(mask == 0); % Find land
    mask(nn) = NaN; % and NaN it...
    var2d = var2d .* mask; % Apply the landmask to our data.    