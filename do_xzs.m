function do_xzs(s_cfg, s_cds, s_at, s_us)
%function do_xzs(s_cfg, s_cds, s_at, s_us)
%
%Plots maps per instant and per layer
    siz = size(s_us);
    ninstants = siz(1);
    nsections = siz(2);
    
    for n = 1 : ninstants
    for j = 1 : nsections
        
        if s_us(n,j).ysection > 0
            m_bl_corner = [s_us(n,j).zrange(1) s_us(n,j).xrange(1) s_us(n,j).ysection];
            m_tr_corner = [s_us(n,j).zrange(2) s_us(n,j).xrange(2) s_us(n,j).ysection];
            if s_us(n,j).instant > 0
                bl_corner = [...
                            s_us(n,j).instant ...
                            s_us(n,j).zrange(1) ...
                            s_us(n,j).xrange(1) ...
                            s_us(n,j).ysection ...
                            ]; %bl_corner (tzxy)
                tr_corner = [...
                            s_us(n,j).instant ...
                            s_us(n,j).zrange(2) ...
                            s_us(n,j).xrange(2) ...
                            s_us(n,j).ysection ...
                            ]; %tr_corner (tzxy)
            else
                bl_corner = [...
                            s_us(n,j).zrange(1) ...
                            s_us(n,j).xrange(1) ...
                            s_us(n,j).ysection ...
                            ]; %bl_corner (zxy)
                tr_corner = [...
                            s_us(n,j).zrange(2) ...
                            s_us(n,j).xrange(2) ...
                            s_us(n,j).ysection ...
                            ]; %tr_corner (zxy)
            end
        else
            disp('Warning: no sections were chosen!');
            return
        end        

        list = get_vars(s_cfg, s_cds, s_at, s_us(n,j), ...
                          bl_corner, tr_corner, m_bl_corner, m_tr_corner);
        s_plot = do_xz( list, s_cfg, s_cds, s_at, s_us);
        do_plot(s_cfg, s_at, s_plot);
       
    end
    end