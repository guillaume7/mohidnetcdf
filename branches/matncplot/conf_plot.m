function s_plot = conf_plot( type, instants, zz, xx, yy)
%function s_plot = conf_plot( type, instants, zz, xx, yy)
%
%Constructs the user structure for map, xz, yz or xyz.
%
    ninstants = length(instants);

    switch type
        
        case 'map'
            nlayers = length(zz);
            for n = 1 : ninstants
            for k = 1 : nlayers
                s_plot(n,k) = struct(...
                    'instant', instants(n), ...
                    'layer', zz(k), ...
                    'xrange', xx, ...
                    'yrange', yy);
            end
            end
            
        case 'xz'
            jsections = length(yy);
            for n = 1 : ninstants
            for j = 1 : jsections
                s_plot(n,j) = struct(...
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
                s_plot(n,i) = struct(...
                    'instant', instants(n), ...
                    'zrange', zz, ...
                    'xsection', xx(i), ...
                    'yrange', yy);
            end
            end
            
        case 'xyz'
            for n = 1 : ninstants
                s_plot(n) = struct(...
                    'instant', instants(n), ...
                    'zrange', zz, ...
                    'xrange', xx, ...
                    'yrange', yy);
            end
            
        otherwise
            disp('Error in type!');
            return
    end
