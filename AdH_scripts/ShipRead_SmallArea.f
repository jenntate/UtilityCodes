      PROGRAM SHIPREAD 
      INTEGER NN, NPNTS, SAVE_TM, i, j, n
      INTEGER Lat_int, Lon_int, units, nid, sid, wid, eid
      REAL North, South, East, West, sign
      REAL u(20), v(20), Lat, Lon, speed(20), direction(20)   
      REAL MIN, MINLon, nmin, smin, wmin, emin
      CHARACTER filename*80,filename1*80,file(20)*80 
      CHARACTER string*2

      sign = 1.0

      WRITE(*,*) 'Enter the input filename'
      WRITE(*,*) 'This file starts with the number of points'
      WRITE(*,*) 'and the number of times on separate rows'
      WRITE(*,*) 'Velocity data for multiple times at a single'
      WRITE(*,*) 'lat/lon are provided on the same row.'
      WRITE(*,*) 'lat, lon, u(1), v(1), u(2), v(2),...,u(i), v(i)'
      READ (*,'(A)')filename

      WRITE (*,*) ' Enter 0 if velocities are in English units'
      WRITE (*,*) ' Enter 1 if velocities are in Metric units'
      READ (*,*) units

      WRITE (*,*) '  '
      WRITE (*,*) '  '
      WRITE (*,*) ' All Western Longitudes will be converted to a '
      WRITE (*,*) ' positive sign and a W added to the values.'
      WRITE (*,*) ' For the tile, add the negative sign for Western',
     >              ' Longitude'
      WRITE (*,*) '  '
      WRITE (*,*) '  '
      WRITE (*,*) ' Velocity Tile boundaries '
      WRITE (*,*) '  '
      WRITE (*,*) ' Enter the Northern Latitude (degrees, space,',
     >            ' decimal minutes)'
      READ (*,*) nid, nmin
      temp = nmin / 60.0
      North = nid + temp
      WRITE(*,*) North
      WRITE (*,*) '  '
      WRITE (*,*) ' Enter the Southern Latitude (degrees, space,',
     >            ' decimal minutes)'
      READ (*,*) sid, smin
      temp = smin / 60.0
      South = sid + temp
      WRITE(*,*) South
      WRITE (*,*) '  '
      WRITE (*,*) ' Enter the Western Longitude (degrees, space,',
     >            ' decimal minutes), include the negative sign ',
     >            'for western hemisphere'
      READ (*,*) wid, wmin
      temp = wmin / 60.0
      if(wid .LT. 0.0) sign=-1.0
      West = (ABS(wid) + temp)*sign
      WRITE(*,*) West 
      WRITE (*,*) '  '
      WRITE (*,*) ' Enter the Eastern Longitude (degrees, space,',
     >            ' decimal minutes), include the negative sign ',
     >            ' for western hemisphere'
      READ (*,*) eid, emin
      temp = emin / 60.0
      if(eid .LT. 0.0) sign=-1.0
      East = (ABS(eid) + temp)*sign
      WRITE(*,*) East 
      WRITE (*,*) '  '
      WRITE (*,*) '  '
      WRITE (*,*) '  '


      OPEN (100,file=filename,form='formatted',status='old')
      REWIND (100)

      READ (100,*) NPNTS
      READ (100,*) SAVE_TM 
      READ (100,*)

C      WRITE (*,*) ' Enter the output filename root'
C      READ(*,'(A)')filename1
      DO j=1,SAVE_TM
        n = 10+j
        write(string,'(i2.2)') j
        file(j)='TidalDiamond'//string//'.etd'
        OPEN (UNIT=j+10,FILE=file(j),form='formatted',status='unknown')
        REWIND(n)
        write(n,2020)
        write(n,2021)
        write(n,2022)
        write(n,2023)
        write(n,2024)
        write(n,2025)nid,nmin
        write(n,2026)sid,smin
        wid=abs(wid)
        wmin=abs(wmin)
        write(n,2027)ABS(wid),wmin
        eid=abs(eid)
        emin=abs(emin)
        write(n,2028)ABS(eid),emin
      ENDDO

      DO NN = 1,NPNTS
        READ (100,*) Lat, Lon, (u(i), v(i),i=1,SAVE_TM)
C
C   SECTION TO REMOVE PART OF GRID NOT NEEDED
C
        IF ((Lat.GT.North).or.(Lat.LT.South))THEN
        WRITE(*,*) ' Skipping node #', NN
           GO TO 910
C 
C
        ELSEIF ((Lon.GT.East).or.(Lon.LT.West))THEN
        WRITE(*,*) 'Skipping node #', NN
           GO TO 910
        ENDIF
C
C
        WRITE(*,*) ' Processing node #', NN
        DO i=1,SAVE_TM
          IF(units .EQ. 0) THEN
            u(i)=u(i)*0.59248
            v(i)=v(i)*0.59248
          ELSE IF(units .EQ. 1) THEN
            u(i)=u(i)*1.94384
            v(i)=v(i)*1.94384
          ELSE
            WRITE(*,*)' Unit specification is bad, start over!'
            GO TO 900
          ENDIF
          speed(i) = SQRT(u(i)*u(i)+v(i)*v(i))
          direction(i) = 450.0-(ATAN2(v(i),u(i))*(180./3.14159))
          IF(direction(i) .GT. 360.0) THEN
            direction(i)=direction(i)-360.0
          ENDIF
          n = 10+i
          Lat_int=INT(Lat)
          MIN=(Lat-Lat_int)*60.0
          Lon_int=INT(Lon)
          MINLon=(Lon-Lon_int)*60.0
          WRITE(n,2003)
          WRITE(n,2000) Lat_int, MIN
          WRITE(n,2001) Lon_int/-1, MINLon/-1.0
          WRITE(n,2004)
          WRITE(n,2002)speed(i),direction(i),speed(i),
     #          direction(i) 
        ENDDO
  910     CONTINUE
      ENDDO

  900   CONTINUE
      CLOSE(100)
      DO j=1,SAVE_TM
        n=10+j
        CLOSE (n)
      ENDDO

 2000 FORMAT('Lat=N', I4,'.',F7.4)
 2001 FORMAT('Lon=W', I4,'.',F7.4)
 2002 FORMAT('SpringCurrent=  0,',F10.4,',',F10.4,
     #    ',  1,',F10.4,',',F10.4)
 2003 FORMAT('[Tidal Diamond]')
 2004 FORMAT('Depth=0.0')
C
C
 2020 FORMAT('Explicit Time Varying Current')
 2021 FORMAT('Version 1')
 2022 FORMAT('[Cycle]')
 2023 FORMAT('Time Length=1')
 2024 FORMAT('[Tile]')
 2025 FORMAT('North Lat=N', I4,'.',F7.4)
 2026 FORMAT('South Lat=N', I4,'.',F7.4)
 2027 FORMAT('West Lon=W', I4,'.',F7.4)
 2028 FORMAT('East Lon=W', I4,'.',F7.4)

      END
