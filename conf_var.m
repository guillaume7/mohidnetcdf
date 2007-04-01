function s_cfg = conf_var(setup_dir)
%function s_cfg = conf_var(setup_dir)

    cd(setup_dir);   
    s_cfg = config_var; %Config defines basically the dataset    
    s_cfg = do_cds(s_cfg); %Coords extracts the coordinates (x,y,z,t)    
    s_cfg = do_attr(s_cfg); %Attr extracts the variable attributes
