function s_user = do_user( type, instants, zz, xx, yy)
%function s_user = do_user( type, instants, zz, xx, yy)
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
                        bl_corner = [instant, layer, xx(1), yy(1)]; % bl_corner (tzxy)
                        tr_corner = [instant, layer, xx(2), yy(2)]; % tr_corner (tzxy)
                    else
                        bl_corner = [layer, xx(1), yy(1)]; % bl_corner (tzxy)
                        tr_corner = [layer, xx(2), yy(2)]; % tr_corner (tzxy)
                    end
                else
                    if instant > 0
                        bl_corner = [instant, xx(1), yy(1)]; %bl_corner (tzxy)
                        tr_corner = [instant, xx(2), yy(2)]; %tr_corner (tzxy)
                    else
                        bl_corner = [xx(1), yy(1)]; %bl_corner (tzxy)
                        tr_corner = [xx(2), yy(2)]; %tr_corner (tzxy)
                    end
                end
                m_bl_corner = [layer xx(1) yy(1)]; %z x y
                m_tr_corner = [layer xx(2) yy(2)];
            end
            end        
                            
        case 'xz'
            jsections = length(yy);
            for n = 1 : ninstants
            for j = 1 : jsections
                s_user(n,j) = struct(...
                    'instant', instants(n), ...
                    'zrange', zz, ...
                    'xrange', xx, ...
                    'ysection', yy(j));
            end
            end
            
        case 'yz'
            isections = length(xx);
            for n = 1 : ninstants
            for i = 1 : isections
                s_user(n,i) = struct(...
                    'instant', instants(n), ...
                    'zrange', zz, ...
                    'xsection', xx(i), ...
                    'yrange', yy);
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