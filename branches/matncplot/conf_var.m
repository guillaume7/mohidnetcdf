function s_cfg = conf_var(setup_dir)
%function s_cfg = conf_var(setup_dir)

    wd = cd;
    cd(setup_dir);   
    s_cfg = confi_var; %Config defines basically the dataset    
    cd(wd);
    s_cfg = do_cds(s_cfg); %Coords extracts the coordinates (x,y,z,t)    
    s_cfg = do_attr(s_cfg); %Attr extracts the variable attributes
