%makeTS.m
%
%Plots and saves image of TS diagram. Edit the m-file settings prior to 
%usage.
%
%Example syntax:
% >> makeTS

%TS ROI
%file='D:\projectos\GCode\MohidNetcdf\20080326_Portugal_WaterProperties.nc';     
file='D:\Aplica\PreOp-Model\WestIberia_ET2_2K4\Portugal\res\20080528_Portugal_WaterProperties.nc';
type='xz'; %xz, yz only.
time=4;
cut=70; %p=lat(xz)/lon(yz)
width=[1 117]; %xz=[1 117] yz=[1 177] p=lon(xz)/lat(yz)
depthl=[1 42];

for t=1:length(time)
for l=1:length(cut)

    %start/count = [time depth-layer lon-index lat-index]
    switch type

        case 'xz'
        start=[time depthl(1)   width(1)    cut] - 1;
        count=[2    depthl(2)   width(2)    1];

        case 'yz'
        start=[time depthl(1)   cut width(1)] - 1;
        count=[1    depthl(2)   1   width(2)];
        
        case 'xzt'
        start=[time(1) depthl(1)   width(1)    cut] - 1;
        count=[time(2) depthl(2)   width(2)    1];

        case 'yzt'
        start=[time(1) depthl(1)   cut width(1)] - 1;
        count=[time(2) depthl(2)   1   width(2)];
        
        case 'p'
        start=[time depthl(1)   width   cut] - 1;
        count=[1    depthl(2)   1       1];
        
        case 'pt'
        start=[time(1)  depthl(1)   width   cut] - 1;
        count=[time(2)  depthl(2)   1       1];
        
        otherwise
            
    end

    %Get the data
    [dens S T Sdata Tdata depth]=TSDiag(file,start,count);

    %Get metadata
    format='png'; %png, jpg, bmp, eps, pdf, ...
    [outfilename, dates, cuts]=do_filename(file, type, time, cut, 'png', width);

    %Plot the data
    h=TSplot(dens-1000, S, T, Sdata, Tdata, depth, [dates, ' ', cuts]);

    %Save image
    saveas(gcf, outfilename, format);
    close(gcf);

end
end