global inc;
global vec_headlength;
global vec_headwidth;
global vec_shaftwidth;
global plot_vectors;
global u_vec;
global v_vec;
global scalevec;

plot_vectors = true;
u_vec = 'u';
v_vec = 'v';
inc            = 3;  %increment value to plot the arrows
vec_headlength = 5;
vec_headwidth  = 2;
vec_shaftwidth = 1;
scalevec = {100, -8, 40, ...
            .1, 0, 'k', ...
           'headlength', vec_headlength, ...
           'headwidth', vec_headwidth,...
           'shaftwidth', vec_shaftwidth,...
           'key', '50 cm s^{-1}'}
