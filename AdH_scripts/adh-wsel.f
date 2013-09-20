      PROGRAM ADHWSEL 
      INTEGER NPTOT, NETOT,  NN, ITIME
      REAL DEPTH, WSEL, TIME, VALX, VALY, VALZ
      CHARACTER filename*80,filename2*80,junk1*2,junk2*2,filename1*20
      CHARACTER junk3*9, TUNITS*20



      WRITE(*,*) 'enter the ADH depth solution filename'
      READ(*,'(A)') filename
      OPEN(15,file=filename,form='formatted',status='old')
      REWIND(15)
      READ(15,*)
      READ(15,*)
      READ(15,*)
      READ(15,*) junk1, NPTOT
      READ(15,*) junk2, NETOT 
      READ(15,*)
      READ(15,*) junk3, TUNITS

      WRITE(*,*) 'enter the ADH geometry filename'
      READ(*,'(A)') filename1
      OPEN(16,file=filename1,form='formatted',status='old')
      READ(16,*)
      DO NE=1, NETOT
        READ(16,*)
      END DO


      WRITE(*,*) 'enter the desired ADH water surface elevation
     & solution filename'
      READ(*,'(A)') filename2
      OPEN(68,file=filename2,form='formatted',status='unknown')
      WRITE(68,*)'DATASET' 
      WRITE(68,*)'OBJTYPE "mesh2d"' 
      WRITE(68,*)'BEGSCL'
      WRITE(68,*)'ND ', NPTOT
      WRITE(68,*)'NC ', NETOT
      WRITE(68,*)'NAME "Water Surface Elevation"'
      WRITE(68,*)'TIMEUNITS ', TUNITS

        DO ITIME = 1, 9999999
          READ(15,*,END=900) junk1, junk2, TIME
          WRITE(68,*) 'TS  0  ', TIME 
            DO NN=1,NPTOT
              READ(15,*,END=900) DEPTH
              READ(16,*) junk1, nnode, VALX, VALY, VALZ 
              WSEL = DEPTH + VALZ
              WRITE(68,*) WSEL 
            ENDDO
        ENDDO
  900   CONTINUE
        WRITE(68,*)'ENDDS' 
        CLOSE(15)
        CLOSE(16)
        CLOSE(68)

      END