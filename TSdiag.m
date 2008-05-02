function [dens S T Sdata Tdata depth] = TSDiag(file, start, count)
%function [dens S T Sdata Tdata] = TSDiag(file, start, count)
%
%start/count(1) time start/count
%start/count(2) layer start/count
%start/count(3) lon index start/count
%start/count(4) lat index start/count

%Isocontour resolution
res=200;

%Get data from netcdf file
mask = nc_varget(file, 'mask', start(2:end), count(2:end));

%X & Y
i=0;
for var={'salinity','temperature'}
    i=i+1;
    [v{i} v2d{i} vlim{i}]= v_extract( file,var{1}, ...
                  start, count, ...
                  res);
end
S = v2d{1};
T = v2d{2}';
Sdata = v{1};
Tdata = v{2};

%Z
var='depth';
depth=nc_varget(file,var,start(2),count(2));

%Val
dens=zeros(res,res);
dens=densUNESCO(S,T,zeros(size(T)), false);
