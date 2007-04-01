function plot_out(s_cfg,s_roi,type)
%function plot_out(s_cfg,s_roi,type)

    output_format = 'png';
    output_extension = 'png';
    output_dir = 'png';
    output_visible = 'on';

    outfilename = [output_dir,'/',s_cfg.varname, ...
                '_',type,'_',s_roi.datet, ...
                '_', s_roi.zt, ...
                '.', output_extension];

    %saves the image
    saveas(gcf, outfilename, output_format);
    close(gcf);

    mess = ['plotted : ',outfilename];
    disp(mess);