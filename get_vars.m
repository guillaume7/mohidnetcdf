function list = get_vars(s_cfg, s_cds, s_at, s_us, ...
                          bl_corner, tr_corner, ...
                          m_bl_corner, m_tr_corner, ...
                          bat_bl_corner, bat_tr_corner)
%function var2d = get_var(s_cfg, s_at, bl_corner, tr_corner, m_bl_corner, m_tr_corner...
%                           bat_bl_corner, bat_tr_corner)
%
% Extracts the MOHID netcdf data with the landmask and the bathymetry
%

%Gets the typical variable called by the user
[var2d mask batim] = get_var(s_cfg, s_at, bl_corner, ...
                                tr_corner, m_bl_corner, m_tr_corner,...
                                bat_bl_corner, bat_tr_corner);

switch(s_us.type)
    case 'map'
               
    case 'xz'

        %Is it the meridional velocity we want to plot?
        if (strcmp(s_at.varname,'v') || strcmp(s_at.varname,'Vel_Y'))
           var2d = -1 .* var2d;
        end

    case 'yz'
        
    otherwise
end

%Fill the gaps
switch (s_cfg.maskit)
    case 'mask'
        var2d = var2d .* mask;    
    case 'interpol'
        var2d = interpol(var2d);    
end

var2d = var2d * s_cfg.scalecolor;

%Do we want to plot a vector field? 
%If yes, then we also want to plot the bathymetry contour
if s_cfg.plot_vectors
    s_ataux = s_at;
    %Get u_component
    s_ataux.varname = s_cfg.u_vec;
    u_var = get_var(s_cfg, s_ataux, bl_corner, tr_corner, ...
                    m_bl_corner, m_tr_corner,...
                    bat_bl_corner, bat_tr_corner);
    u_var = u_var * s_cfg.scalecolor;
    %Get v_component
    s_ataux.varname = s_cfg.v_vec;
    v_var = get_var(s_cfg, s_ataux, bl_corner, tr_corner, ...
                    m_bl_corner, m_tr_corner,...
                    bat_bl_corner, bat_tr_corner);
    v_var = v_var * s_cfg.scalecolor;
    list = {var2d, mask, batim, u_var, v_var};
%No? Then we simply add the user-given variable to our list
else
    list = {var2d, mask, batim};
end
