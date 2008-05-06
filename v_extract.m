function [vv, v2d, vlim] = v_extract(file,variable,start,count,res)
%function [v, v2d, vlim] = v_extract(file,variable,start,count,res)

    v = nc_varget(file, variable, start, count);
    
    sizev=size(v);
    
    v_av=zeros(sizev(2),sizev(3));

    for k=1:sizev(3)
    for j=1:sizev(2)
    for i=1:sizev(1)
        v_av(j,k)=v_av(j,k) + v(i,j,k)/sizev(1);
    end
    end
    end
    
    vv=v;
    %vv=v_av;
    
    %ajuster v2d en ftion de la valeur moyenne ou la valeur instantanée
    
    vlim = [min(min(min(vv))) max(max(max(vv)))];
    v2d = ( ones(res,1) * [1:res] - 1 ) ./ ( res - 1 );
    v2d = vlim(1) + v2d .* ( vlim(2) - vlim(1) );
