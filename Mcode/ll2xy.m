function [x,y]=ll2xy(lat,lon,reflat,reflong,projflag)
%LL2XY performs either a Carte Parallelogrammatique
% projection (CPP) or a Mercator projection and returns
% X and Y vectors (as distance from arbitrary
% origin REFLAT and REFLON in degrees) for vectors
% of latitudes and longitudes, LAT and LON in degrees.
% The CPP and Mercator projection are cylindrical projection,
% and in addition the Mercator projection represents a type
% of conformal projection.
%
% Variables:
% REFLAT and REFLON -- Reference latitude and longitude
%                      (in degrees) default to Boston
% LAT and LON -- latitude and longitude values,
%                may be specified as LON+i*LAT (in degrees)
% projflag -- an optional flag indicating which projection
%             to use,
%             Mercator -- projflag = 1
%             CPP -- projflag = 0 (default (i.e., no flag))
% x and y -- transformed values in Cartesian coordinates (meters)
%            Specifying only a single output yields X+i*Y
%
% Calls: none
%
% Usage:
%  [x,y] = ll2xy(lat,lon,reflat,reflong,projflag);
%                or
%  [x,y] = ll2xy(lat,lon); %(reflat,reflong) = Boston Default
%                          and CPP used
%
%
% FileName:  ll2xy.m
% Written By:  CVL -- Hacked from mercgom2 from C. Naimie.
% Date: 7-10-97      -- Mercator Projection part
% Last Modified:
%    01/12/2007 -- Chris Massey NRL Code 7322 Stennis Space Center
%                  Modified and added to documentation.
%    01/12/2007 -- Chris Massey NRL Code 7322 Stennis Space Center
%                   Fixed Bug related to default (reflat,reflong).
%    03/12/2007 -- Chris Massey NRL Code 7322
%                  Corrected the documentation to reflect that this
%                  is a Mercator projection, not a CPP.
%    03/13/2007 -- Added optional flag "projflag" to indicate which
%                  projection to use (def. is CPP); added true CPP
%                  projection conversion, updated value of r.
%                  R.S. Linzell, PSI/NRL-SSC Code 7322
%    03/14/2007 -- Fixed bug in line 64.  (R.S. Linzell)
%    03/14/2007 -- Modified error detection and input option detection
%                  rules to fix incorrect logic
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Keywords:  map conversion, latitude, longitude x/y
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

r=6378206.40;
degtorad=pi/180;

%Default lat/long is Boston, MA
default_long = -71.03;
default_lat = 42.35;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%         Error detection and input option determining

mesg1 = 'Latitude and Longitude input as complex variable.';
mesg2 = 'Latitude and Longitude must both be supplied.';
mesg3 = 'Second input argument being ignored.';
mesg4 = 'Incorrect input arguments. Check and try again.';
mesg5 = 'Default reference lat/lon of Boston, MA being used.';

if nargin < 3
    % Boston, MA longitude latitude
    reflong=default_long;
    reflat=default_lat;
    %CPP being used
    flag = 0;

    if nargin == 1
        if ~isreal(lat) %Latitude and longitude expressed in complex form
            lon = real(lat);
            lat = imag(lat);
            disp(mesg1);
        else
            error(mesg2)
        end
    end

    if nargin ==2
        if (length(lon)~=length(lat))
            if ~isreal(lat) %Latitude/Longitude expressed in complex form
                lon = real(lat);
                lat = imag(lat);
                disp(mesg1);
                disp(mesg3);
            else
                error(mesg4)
            end
        else %lon and lat same length
            if ~isreal(lat) %Latitude/Longitude expressed in complex form
                lon = real(lat);
                lat = imag(lat);
                disp(mesg1);
                disp(mesg3);
            end
        end
    end
    disp(mesg5);
end


if (nargin <= 4)
    if (length(lon)~=length(lat))
        if ~isreal(lat)
            if nargin == 4
                flag = reflong;
            else
                flag = 0;
            end
            reflong = reflat;
            reflat = lon;
            lon = real(lat);
            lat = imag(lat);
            disp(mesg1);
        else
            error(mesg4)
        end
    else
        if ~isreal(lat)
            if nargin == 4
                flag = reflon;
            else
                flag = 0;
            end
            reflong = reflat;
            reflat = lon;
            lon = real(lat);
            lat = imag(lat);
            disp(mesg1);
        else
            flag = 0; %using CPP
            if nargin == 3
                reflong = default_long;
                reflat = default_lat;
                disp('Not enough input arguments supplied.');
                disp(mesg5);
            end
        end
    end
end

if(nargin == 5)
    flag = projflag;
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                   Actual Projection Routines
%

% Convert reference lat/lon (reflat,relong) from degrees to radians
reflong = reflong*degtorad;
reflat = reflat*degtorad;


if flag == 1
    disp('Mercator projection used for coordinate transform.');
    xo=r*cos(reflat)*reflong;
    yo=r*cos(reflat)*log((1.0+sin(reflat))/cos(reflat));

    rlong=lon*degtorad;
    rlat=lat*degtorad;
    x=r*cos(reflat).*rlong-xo;
    y=r*cos(reflat).*log((1.0+sin(rlat))./cos(rlat))-yo;
else
    disp('CPP projection used for coordinate transform.');
    rlong = lon*degtorad;
    rlat = lat*degtorad;
    x = r.*(rlong - reflong).*cos(reflat);
    y = rlat.*r;

end

if nargout<=1
    x=x+i*y;
end

return;
