function matncplot
%matncplot User's handbook
%
%Edit the config_var.m function first!
%Also, edit the plot_out.m function before saving images.
%
%Then, here's the user's functions:
%
% s_cfg = conf_var('setup_dir');
% s_roi = do_roi( type, instants, zz, xx, yy, s_cfg);
% var2d = get_var(s_cfg, s_roi);
%
% plot_map(s_roi);
% hold on;
% plot_color(s_cfg, s_roi, var2d, type)
% hold on;
% plot_contour(s_cfg, s_roi, var2d, type)
% hold on;
% plot_vectors(s_cfg, s_roi, uvar, vvar, type)
% hold on;
% plot_title(s_roi);
% plot_out(s_cfg,s_roi,type);