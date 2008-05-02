function [outfilename, dates, cuts]=do_filename(file, type, time, cut, format, width)
%function [outfilename, dates]=do_filename(file, type, time, cut, format, width)
%
%Returns a proper filename from input data
%format={png, jpg, bmp, eps, pdf ...}

model='Portugal'; % Users, change this manually

%Transform date into string
serial_time = do_time(file);
datet = int2str(time);
dates = datestr(serial_time(time), 'dd-mmm-yyyy HH:MM');
datets = datestr(serial_time(time), 'yyyy-mm-dd');

%Transform lat/lon cut into string
vcut=do_cut(file, type, cut, width);
cuts=num2str(vcut, '%2.2f');

switch type
    case 'xz'
        cuts = [cuts, 'N'];
    case 'yz'
        cuts = [cuts, 'E'];
    otherwise
end

outfilename=[   'TS_',...
                 model,'_',...
                 datets,'_',...
                 type,'_',...
                 cuts,'_',...
                 '.', format];
