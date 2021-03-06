      PROGRAM SHIPREAD 
      INTEGER NN, NPNTS, SAVE_TM, i, j, n
      INTEGER Lat_int, Lon_int, units
      REAL u(20), v(20), Lat, Lon, speed(20), direction(20)   
      REAL MIN, MINLon
      CHARACTER filename*80,filename1*80,file(20)*80 
      CHARACTER string*2

      WRITE (*,*) 'Enter the input filename'
      WRITE(*,*) 'This file starts with the number of points'
      WRITE(*,*) 'and the number of times on separate rows'
      WRITE(*,*) 'Velocity data for multiple times at a single'
      WRITE(*,*) 'lat/lon are provided on the same row.'
      WRITE(*,*) 'lat, lon, u(1), v(1), u(2), v(2),...,u(i), v(i)'
      READ (*,'(A)')filename

      WRITE(*,*) 'Enter 0 if velocities are in English units'
      WRITE(*,*) 'Enter 1 if velocities are in Metric units'
      READ(*,*) units


      OPEN(100,file=filename,form='formatted',status='old')
      REWIND(100)

      READ (100,*) NPNTS
      READ (100,*) SAVE_TM 
      READ (100,*)

C      WRITE (*,*) 'Enter the output filename root'
C      READ(*,'(A)')filename1
      DO j=1,SAVE_TM
        n = 10+j
        write(string,'(i2.2)') j
        file(j)='TidalDiamond'//string//'.etd'
        OPEN (UNIT=j+10,FILE=file(j),form='formatted',status='unknown')
        REWIND(n)
      ENDDO

      DO NN = 1,NPNTS
        READ (100,*) Lat, Lon, (u(i), v(i),i=1,SAVE_TM) 
        WRITE(*,*) 'Processing node #', NN
        DO i=1,SAVE_TM
          IF(units .EQ. 0) THEN
            u(i)=u(i)*0.59248
            v(i)=v(i)*0.59248
          ELSE IF(units .EQ. 1) THEN
            u(i)=u(i)*1.94384
            v(i)=v(i)*1.94384
          ELSE
            WRITE(*,*)'Unit specification is bad, start over!'
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

      END
