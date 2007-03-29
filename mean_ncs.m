function vars3d = mean_ncs(s_nc)
%function vars2d = get_ncs(s_nc)

n = length(s_nc);
vars3d = 0.;
for i = 1:n
    vars3d = vars3d + getnc(s_nc(i));
    disp(i);
end

vars3d = vars3d ./ n;
