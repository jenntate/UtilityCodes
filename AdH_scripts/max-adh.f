      PROGRAM ADHDATA 
      PARAMETER (MAXNE = 500000, MAXN = 1000000, MAXNDF = 6)
      DIMENSION VAL(MAXN), VALM(MAXN), COUNT(MAXN)
      INTEGER NPTOT, NETOT, COUNT, NN, ITIME
      REAL SH_LIM, VALM, VAL
      CHARACTER filename*80,CHND*6, junk1*2, junk2*2



      WRITE(*,*) 'enter the ADH ascii solution file name'
      READ(*,'(A)') filename
      OPEN(15,file=filename,form='formatted',status='old')
      REWIND(15)
      WRITE(*,*) 'enter the critical shear for erosion'
      READ(*,*) SH_LIM 
      READ(15,*)
      READ(15,*)
      READ(15,*)
      READ(15,*) junk1, NPTOT
      READ(15,*) junk2, NETOT 
      READ(15,*) 

        DO NN=1, NPTOT
          VALM(NN) = 0.0
          COUNT(NN) = 0
        ENDDO
 
        DO ITIME = 1, 9999999
          READ(15,*,END=900)
          DO NN=1,NPTOT
             READ(15,*,err=900) VAL(NN)
             IF(VAL(NN) .GT. SH_LIM) COUNT(NN) = COUNT(NN)+1
             VALM(NN) = MAX(VALM(NN),VAL(NN)) 
          ENDDO
        ENDDO
  900   CONTINUE
        OPEN(68,file='maxvalues.out',form='formatted',status='unknown')
        WRITE(68,*)'DATASET' 
        WRITE(68,*)'OBJTYPE "mesh2d"' 
        WRITE(68,*)'BEGSCL' 
        WRITE(68,*)'ND ', NPTOT
        WRITE(68,*)'NC ', NETOT
        WRITE(68,*)'NAME "Max Value"'
        WRITE(68,*)'TS 0  0.0' 
        OPEN(69,file='exceedence.out',form='formatted',status='unknown')
        WRITE(69,*)'DATASET' 
        WRITE(69,*)'OBJTYPE "mesh2d"' 
        WRITE(69,*)'BEGSCL' 
        WRITE(69,*)'ND ', NPTOT
        WRITE(69,*)'NC ', NETOT
        WRITE(69,*)'NAME "Exceedence Fraction"'
        WRITE(69,*)'TS 0  0.0' 
          DO NN=1,NPTOT
             WRITE(68,*) VALM(NN) 
             WRITE(69,*) REAL(COUNT(NN))/REAL(ITIME-1.0) 
          ENDDO
        WRITE(68,*)'ENDDS' 
        WRITE(69,*)'ENDDS' 
        CLOSE(15)
        CLOSE(68)
        CLOSE(69)

      END
