function vars3d = ad_ncs(s_nc)
%function vars2d = get_ncs(s_nc)

n = length(s_nc);
var3d = getnc(s_nc(1));
vars3d = zeros( cat( 2, n, size(var3d) ) );
vars3d(1,:,:,:) = var3d;
for i = 2:n
    vars3d(i,:,:,:) = getnc(s_nc(i));
end
