      PROGRAM ADHNODRY 
      PARAMETER (MAXNE = 5000000, MAXN = 10000000)
      INTEGER NPTOT1, NETOT1, COUNT, NN, ITIME
      INTEGER NPTOT2, NETOT2
      REAL VALD, VEL, VALX, VALY, VALZ, VALH, TIME, VELLIM
      CHARACTER filename*80,junk1*2, junk2*2
      CHARACTER filename2*80



      WRITE(*,*) 'This routine with set the depth value to zero'
      WRITE(*,*) 'when the velocity is less than some limit at a node.'
      WRITE(*,*) 'enter the ADH depth solution file name'
      READ(*,'(A)') filename
      OPEN(15,file=filename,form='formatted',status='old')
      REWIND(15)
      WRITE(*,*) 'enter the ADH velocity solution file name'
      READ(*,'(A)') filename2
      OPEN(16,file=filename2,form='formatted',status='old')
      REWIND(16)
      WRITE(*,*) 'enter the velocity limit for wet nodes'
      READ(*,*) VELLIM 
      READ(15,*)
      READ(15,*)
      READ(15,*)
      READ(15,*) junk1, NPTOT1
      READ(15,*) junk2, NETOT1 
      READ(15,*) 
      READ(16,*)
      READ(16,*)
      READ(16,*)
      READ(16,*) junk1, NPTOT2
      READ(16,*) junk2, NETOT2 
      READ(16,*) 

        IF(NPTOT1 .NE. NPTOT2 .OR. NETOT1 .NE. NETOT2)THEN
          WRITE(*,*) 'ADH solution files do not match.' 
           GOTO 1000
        ENDIF


        OPEN(68,file='Depth_nodry.dat',form='formatted',
     # status='unknown')
          WRITE(68,*)'DATASET' 
          WRITE(68,*)'OBJTYPE "mesh2d"' 
          WRITE(68,*)'BEGSCL' 
          WRITE(68,*)'ND ', NPTOT1
          WRITE(68,*)'NC ', NETOT1
          WRITE(68,*)'NAME "DEPTH no-dry"'
 
        DO ITIME = 1, 9999999
          READ(15,*,END=900) junk1, junk2, TIME
          READ(16,*,END=900)
          WRITE(68,*)'TS 0  ', TIME 
          DO NN=1,NPTOT1
             READ(15,*,END=900) VALH
             READ(16,*,END=900) VALX, VALY, VALZ
             VEL = SQRT(VALX*VALX+VALY*VALY+VALZ*VALZ)
             VALD = MAX(VALH,0.0)
             IF(VEL .LT. VELLIM) VALD = 0.0 
             WRITE(68,*) VALD 
          ENDDO
        ENDDO
  900   CONTINUE
        WRITE(68,*)'ENDDS' 

 1000   CLOSE(15)
        CLOSE(16)
        CLOSE(68)

      END
