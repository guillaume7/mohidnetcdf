function s_nc = do_nc( type, instants, zz, xx, yy)
%function s_nc = do_nc( type, instants, zz, xx, yy)
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
                if layer > 0           
                    if instant > 0
                        s_nc(n,k).bl_corner = [instant, layer, xx(1), yy(1)]; % bl_corner (tzxy)
                        s_nc(n,k).tr_corner = [instant, layer, xx(2), yy(2)]; % tr_corner (tzxy)
                    else
                        s_nc(n,k).bl_corner = [layer, xx(1), yy(1)]; % bl_corner (tzxy)
                        s_nc(n,k).tr_corner = [layer, xx(2), yy(2)]; % tr_corner (tzxy)
                    end
                else
                    if instant > 0
                        s_nc(n,k).bl_corner = [instant, xx(1), yy(1)]; %bl_corner (tzxy)
                        s_nc(n,k).tr_corner = [instant, xx(2), yy(2)]; %tr_corner (tzxy)
                    else
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
                if ysection > 0
                    s_nc(n,j).m_bl_corner = [zz(1) xx(1) ysection];
                    s_nc(n,j).m_tr_corner = [zz(2) xx(2) ysection];
                    if instant > 0
                        s_nc(n,j).bl_corner = [instant zz(1) xx(1) ysection]; %bl_corner (tzxy)
                        s_nc(n,j).tr_corner = [instant zz(2) xx(2) ysection]; %tr_corner (tzxy)
                    else
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
                if xsection > 0
                    s_nc(n,i).m_bl_corner = [zz(1) xsection yy(1)];
                    s_nc(n,i).m_tr_corner = [zz(2) xsection yy(2)];
                    if instant > 0
                        s_nc(n,i).bl_corner = [instant zz(1) xsection yy(1)]; %bl_corner (tzxy)
                        s_nc(n,i).tr_corner = [instant zz(2) xsection yy(2)]; %tr_corner (tzxy)
                    else
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