function s_nc = do_roi( type, instants, zz, xx, yy, s_cfg)
%function s_nc = do_roi( type, instants, zz, xx, yy, s_cfg)
%
%Constructs the user structure for map, xz, yz or xyz.
%
    ninstants = length(instants);
    switch type
        
        case 'map'            

            nlayers = length(zz);
            for n = 1 : ninstants
            for k = 1 : nlayers
                layer = zz(k);
                instant = instants(n);
                %coords
                s_nc(n,k).x_lab = '';
                s_nc(n,k).y_lab = '';             
                s_nc(n,k).c_x2d = s_cfg.lon2d( yy(1):yy(2), xx(1):xx(2));
                s_nc(n,k).c_y2d = s_cfg.lat2d( yy(1):yy(2), xx(1):xx(2));
                if layer > 0
                    zt = int2str(s_cfg.zsize(layer));
                    s_nc(n,k).zts = [', Depth: ', zt, ' m'];
                    if instant > 0
                        datet = int2str(instant);
                        s_nc(n,k).dates = datestr(s_cfg.serial_time(instant), 'dd-mmm-yyyy HH:MM:SS');
                        s_nc(n,k).bl_corner = [instant, layer, xx(1), yy(1)]; % bl_corner (tzxy)
                        s_nc(n,k).tr_corner = [instant, layer, xx(2), yy(2)]; % tr_corner (tzxy)
                    else
                        datet = '';
                        s_nc(n,k).dates = '';
                        s_nc(n,k).bl_corner = [layer, xx(1), yy(1)]; % bl_corner (tzxy)
                        s_nc(n,k).tr_corner = [layer, xx(2), yy(2)]; % tr_corner (tzxy)
                    end
                else
                    zt = '';
                    s_nc(n,k).zts = '';
                    if instant > 0
                        datet = int2str(instant);
                        s_nc(n,k).dates = datestr(s_cfg.serial_time(instant), 'dd-mmm-yyyy HH:MM:SS');
                        s_nc(n,k).bl_corner = [instant, xx(1), yy(1)]; %bl_corner (tzxy)
                        s_nc(n,k).tr_corner = [instant, xx(2), yy(2)]; %tr_corner (tzxy)
                    else
                        datet = '';
                        s_nc(n,k).dates = '';
                        s_nc(n,k).bl_corner = [xx(1), yy(1)]; %bl_corner (tzxy)
                        s_nc(n,k).tr_corner = [xx(2), yy(2)]; %tr_corner (tzxy)
                    end
                end
                s_nc(n,k).m_bl_corner = [layer xx(1) yy(1)]; %z x y
                s_nc(n,k).m_tr_corner = [layer xx(2) yy(2)];
            end
            end        
                            
        case 'xz'
                       
            jsections = length(yy);
            for n = 1 : ninstants
            for j = 1 : jsections
                ysection = yy(j);
                instant = instants(n);
                %coords
                c_x = s_cfg.lon2d( ysection, xx(1):xx(2));
                c_z = s_cfg.zsize( zz(1):zz(2))*(-1.);
                s_nc(n,j).c_x2d = ones(size(c_z)) * c_x;
                s_nc(n,j).c_y2d = c_z * ones(size(c_x));                
                if ysection > 0
                    zt = num2str(s_cfg.lat2d(ysection,1), '%2.2f');
                    s_nc(n,j).zts = [', Lat: ', zt, 'º N'];
                    s_nc(n,j).m_bl_corner = [zz(1) xx(1) ysection];
                    s_nc(n,j).m_tr_corner = [zz(2) xx(2) ysection];
                    if instant > 0
                        datet = int2str(instant);
                        s_nc(n,j).dates = datestr(s_cfg.serial_time(instant), 'dd-mmm-yyyy HH:MM:SS');
                        s_nc(n,j).bl_corner = [instant zz(1) xx(1) ysection]; %bl_corner (tzxy)
                        s_nc(n,j).tr_corner = [instant zz(2) xx(2) ysection]; %tr_corner (tzxy)
                    else
                        datet = '';
                        s_nc(n,j).dates = '';
                        s_nc(n,j).bl_corner = [zz(1) xx(1) ysection]; %bl_corner (zxy)
                        s_nc(n,j).tr_corner = [zz(2) xx(2) ysection]; %tr_corner (zxy)
                    end
                else
                    disp('Warning: no sections were chosen!');
                    return
                end        
            end
            end
            
        case 'yz'
            
            isections = length(xx);
            for n = 1 : ninstants
            for i = 1 : isections
                xsection = xx(i);
                instant = instants(n);
                %coords
                c_x = s_cfg.lat2d( yy(1):yy(2), xsection)';
                c_z = s_cfg.zsize( zz(1):zz(2))*(-1.);
                s_nc(n,i).c_x2d = ones(size(c_z)) * c_x;
                s_nc(n,i).c_y2d = c_z * ones(size(c_x));
                if xsection > 0
                    zt = num2str(s_cds.lon2d(1,s_us.xsection), '%2.2f');
                    s_nc(n,i).zts = [', Lon: ', zt, 'º E'];
                    s_nc(n,i).m_bl_corner = [zz(1) xsection yy(1)];
                    s_nc(n,i).m_tr_corner = [zz(2) xsection yy(2)];
                    if instant > 0
                        datet = int2str(s_us.instant);
                        s_nc(n,i).dates = datestr(s_cds.serial_time(s_us.instant), 'dd-mmm-yyyy HH:MM:SS');
                        s_nc(n,i).bl_corner = [instant zz(1) xsection yy(1)]; %bl_corner (tzxy)
                        s_nc(n,i).tr_corner = [instant zz(2) xsection yy(2)]; %tr_corner (tzxy)
                    else
                        datet = '';
                        s_nc(n,i).dates = '';
                        s_nc(n,i).bl_corner = [zz(1) xsection yy(1)]; %bl_corner (zxy)
                        s_nc(n,i).tr_corner = [zz(2) xsection yy(2)]; %tr_corner (zxy)
                    end
                else
                    disp('Warning: no sections were chosen!');
                    return
                end        
            end
            end
            
        case 'xyz'
            for n = 1 : ninstants
                s_user(n) = struct(...
                    'instant', instants(n), ...
                    'zrange', zz, ...
                    'xrange', xx, ...
                    'yrange', yy);
            end
            
        otherwise
            disp('Error in type!');
            return
    end