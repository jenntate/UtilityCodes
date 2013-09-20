      PROGRAM CONVERTSAL
      PARAMETER (MAXNE = 500000, MAXN = 1000000)
      DIMENSION VALX(MAXN), VALY(MAXN), SUMX(MAXN), SUMY(MAXN)
      INTEGER NPTOT, NETOT, NN, ITIME
      REAL S_TIME, E_TIME, VALX, VALY, SUMX, SUMY, TIME
      REAL TIME_TOT, P_TIME, DEL_T
      CHARACTER filename*80, junk1*2, junk2*2


      WRITE(*,*) 'enter the salinity data filename'
      READ(*,'(A)') filename
      OPEN(15,file=filename,form='formatted',status='old')
      REWIND(15)
      OPEN(16,file='salinity.out',form='formatted',status='unknown')
      WRITE(16,*) 'Date  Latitude  Longitude  Depth(m)  Salinity(ppt)'

        DO ITIME = 1, 9999999
          READ(15,1000,END=900) YR, MO, DA, Deg1, Min1, Sec1, Deg2, 
     # Min2, Sec2, Depth, Sal 
          Lat = Deg1+Min1/60.0+Sec1/60.0/60.0
          Lon = Deg2+Min2/60.0+Sec2/60.0/60.0
          WRITE(16,1001) MO,'/',DA,'/',YR,Lat,Lon,Depth,Sal
        ENDDO
  900   CONTINUE
 1000   FORMAT(I2,I2,I2,1X,I2,I2,I2,1X,I2,I2,I2,1X,F4.1,E9.3E3)
 1001   FORMAT(I2,I2,I2,1X,F9.6,1X,F9.6,1X,F4.1,1X,F5.2)
        CLOSE(15)
        CLOSE(16)

      END
