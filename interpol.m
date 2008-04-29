function y = interpol(x)
%function y = interpol(x)
%
%Interpolates x-matrix values on 'NaN' zones.

y=x;
[ion jon] = find (isnan(x));

for m=1:length(ion)
           
        io=ion(m);
        jo=jon(m);

        %dip calculation
        i=io;
        while i > 1  
            if ~isnan(x(i,jo))
                break;
            end
            i=i-1;
        end
        dim=io-i;
        xim=x(i,jo);
        if dim ~= 0
            dim = 1. / dim;
        end
        if isnan(xim)
            xim=0;
            dim=0;
        end
        
        %dim calculation
        i=io;
        while i < length(x(:,jo)) 
            if ~isnan(x(i,jo))
                break;
            end
            i=i+1;
        end
        dip=i-io;
        xip=x(i,jo);
        if dip ~= 0
            dip = 1. / dip;
        end
        if isnan(xip)
            xip=0;
            dip=0;
        end

        %djm calculation
        j=jo;
        while j > 1 
            if ~isnan(x(io,j))
                break;
            end
            j=j-1;
        end
        djm=jo-j;
        xjm=x(io,j);
        if djm ~= 0
            djm = 1. / djm;
        end
        if isnan(xjm)
            xjm=0;
            djm=0;
        end

        %djp calculation
        j=jo;
        while j < length(x(io,:)) 
            if ~isnan(x(io,j))
                break;
            end
                j=j+1;
        end
        djp=j-jo;
        xjp=x(io,j);
        if djp ~= 0
            djp = 1. / djp;
        end
        if isnan(xjp)
            xjp=0;
            djp=0;
        end
        
        y(io,jo)= (dim * xim + dip * xip + djm * xjm + djp * xjp) ...
                   / (dim + dip + djm + djp);

end