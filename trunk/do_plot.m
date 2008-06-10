function do_plot(s_cfg, s_at, s_pl)
%function do_plot(s_cfg, s_at, s_pl)
%
%Plots a graphic with contour and pcolor
%
    switch(s_pl(1).type)

        case 'map'
        lonlim = [min(min(s_pl(1).c_x2d)) max(max(s_pl(1).c_x2d))];
        latlim = [min(min(s_pl(1).c_y2d)) max(max(s_pl(1).c_y2d))];
        %draws projection
        m_proj(s_cfg.projection,'long', lonlim,'lat', latlim);

    end
    
    %sets the plot invisible
    set(gcf, 'Visible', s_cfg.output_visible);
    
    %Define correct renderers
    set(gcf, 'RendererMode', 'Manual');
    set(gcf, 'Renderer', s_cfg.output_renderer);

    %%%draws the colored map array %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    if s_cfg.plot_color
        
        switch(s_pl(1).type)

            case 'map'
            m_pcolor( s_pl(1).c_x2d, s_pl(1).c_y2d, s_pl(1).var2d);
            
            otherwise
            pcolor( s_pl(1).c_x2d, s_pl(1).c_y2d, s_pl(1).var2d);
            
        end    
    
        shading( s_cfg.shading_type);
        colormap( s_cfg.colormap_type);
        caxis( s_cfg.colorlimits);
        
        %draws colorbar
        h = colorbar('vert');
        
        if s_cfg.autocolortitle        
            colorbartitle = [s_at.varname,' (', s_at.units, ')', ...
                ' x ', num2str(1/s_cfg.scalecolor, '%0.3g')];        
        else
            colorbartitle = s_cfg.colorbartitle;        
        end
        
        set(get(h,'title'),'string', colorbartitle);

        hold on;
        
    end %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    %%%draws the contours %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    if s_cfg.plot_contour
        
        if s_cfg.autoscale == true
            k=10.0; %number of contour lines
            cscale = [0:k]./k * ...
                ( s_cfg.colorlimits(2) - s_cfg.colorlimits(1)) ...
                + s_cfg.colorlimits(1);
        else
            cscale = s_cfg.contour_scale * s_cfg.scalecolor;
        end
        ss = find(cscale >= 0.);
        if min(size(ss)) > 0
            switch(s_pl(1).type)
                case 'map'
                [cs,h]=m_contour( s_pl(1).c_x2d, s_pl(1).c_y2d, s_pl(1).var2d, cscale(ss), 'k');
                otherwise
                [cs,h]=contour( s_pl(1).c_x2d, s_pl(1).c_y2d, s_pl(1).var2d, cscale(ss), 'k');
            end
            clabel(cs,h,'fontsize',7,'labelspacing',288);
        end
    
        hold on;
            
        ss = find(cscale < 0.);
        if min(size(ss)) > 0
            switch(s_pl(1).type)
                case 'map'
                [cs,h]=m_contour( s_pl(1).c_x2d, s_pl(1).c_y2d, s_pl(1).var2d, cscale(ss), 'k--');
                otherwise
                [cs,h]=contour( s_pl(1).c_x2d, s_pl(1).c_y2d, s_pl(1).var2d, cscale(ss), 'k--');
            end    
            clabel(cs,h,'fontsize',7,'labelspacing',288);
        end
    
    end %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
          
    %%%draws the vector field%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    if s_cfg.plot_vectors

        %draws the arrows
        switch(s_pl(1).type)
            case 'map'
                m_quiver(s_pl(1).c_x2d( 1:s_cfg.inc:end, 1:s_cfg.inc:end), ...
                 s_pl(1).c_y2d( 1:s_cfg.inc:end, 1:s_cfg.inc:end), ...
                 s_pl(4).var2d( 1:s_cfg.inc:end, 1:s_cfg.inc:end), ...
                 s_pl(5).var2d( 1:s_cfg.inc:end, 1:s_cfg.inc:end), 0, 'k');
            otherwise
                quiver(s_pl(1).c_x2d( 1:s_cfg.inc:end, 1:s_cfg.inc:end), ...
                 s_pl(1).c_y2d( 1:s_cfg.inc:end, 1:s_cfg.inc:end), ...
                 s_pl(4).var2d( 1:s_cfg.inc:end, 1:s_cfg.inc:end), ...
                 s_pl(5).var2d( 1:s_cfg.inc:end, 1:s_cfg.inc:end), 0, 'k');
        end
   
        hold on
        
    end %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    % Draws the map frame
    switch(s_pl(1).type)
        
        case 'map'
        %draws the coordinates
        m_grid('box','fancy','linestyle','none','fontsize', s_cfg.grid_fontsize);
        
        %draws the isobath
        [isobatc,h]=m_contour(...
                            s_pl(3).c_x2d, ...
                            s_pl(3).c_y2d, ...
                            s_pl(3).var2d, ...
                            s_pl(3).depth, ...
                            'k');
        %Draws a patched contour --> doesn't works well with maps                
        %m_patchcontour(isobatc);
        
        %draws the coast line
        m_usercoast(s_cfg.coastline_file,'patch', s_cfg.coastline_patch_color);       
        
        case 'xz'
        %Draws a patched contour --> doesn't works well with map
        contourx = [ max( s_pl(3).c_x2d(1,1) ) ...
                    s_pl(3).c_x2d(1,:) ...
                    max( s_pl(3).c_x2d(1,end) ) ];
        contoury = -1 .* [ 5500  ...
                    s_pl(3).var2d' ...
                    5500  ];
        patch(contourx,contoury, [.5 .5 .5]);
        
        case 'yz'
        contourx = [ max( s_pl(3).c_x2d(1,1) ) ...
                    s_pl(3).c_x2d(1,:) ...
                    max( s_pl(3).c_x2d(1,end) ) ];
        contoury = -1 .* [ 5500 ...
                    s_pl(3).var2d ...
                    5500 ];
        patch(contourx,contoury, [.5 .5 .5]);
        
        otherwise
            
    end

    %%%draws the vector scale arrow%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    if s_cfg.plot_vectors

        % Draws the arrows scale 
       switch(s_pl(1).type)
            case 'map'
                m_quiver(s_cfg.legpos(1), ...
                        s_cfg.legpos(2), ...
                        s_cfg.legpos(3), ...
                        s_cfg.legpos(4), ...
                        0, ...
                        s_cfg.legcol);
            otherwise
                quiver(s_cfg.legpos(1), ...
                        s_cfg.legpos(2), ...
                        s_cfg.legpos(3), ...
                        s_cfg.legpos(4), ...
                        0, ...
                        s_cfg.legcol);
        end

        %Draws the legend text( .015, .69, s_cfg.legkey)
        %htv5 = text( s_cfg.legpos(1), s_cfg.legpos(2), s_cfg.legkey);
        htv5 = text(.015, .69, s_cfg.legkey);
        set(htv5,'FontSize',8);

        %sets blue sea
        %set(gca,'color',[.9 .99 1]);     % Trick is to set this *before* the patch call.
    end %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        
    title([ ...
        s_pl(1).dates, ...
        s_pl(1).zts(3:end)], ...
        'fontsize', s_cfg.title_fontsize);
    xlabel(s_pl(1).x_lab);
    ylabel(s_pl(1).y_lab);
    
    outfilename = [s_cfg.output_dir,'/',s_at.varname, ...
                '_', s_pl(1).type,'_',s_pl(1).datet, ...
                '_', s_pl(1).zt, ...
                '.', s_cfg.output_extension];

    %saves the image
    %print(gcf, '-dpng','-painters', outfilename);
    saveas(gcf, outfilename, s_cfg.output_format);
    close(gcf);

    mess = ['plotted : ',outfilename];
    disp(mess);