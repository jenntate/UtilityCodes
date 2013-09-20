      PROGRAM ADHRESID
      PARAMETER (MAXNE = 500000, MAXN = 1000000)
      DIMENSION VALX(MAXN), VALY(MAXN), SUMX(MAXN), SUMY(MAXN)
      INTEGER NPTOT, NETOT, NN, ITIME
      REAL S_TIME, E_TIME, VALX, VALY, SUMX, SUMY, TIME
      REAL TIME_TOT, P_TIME, DEL_T
      CHARACTER filename*80, junk1*2, junk2*2



      WRITE(*,*) 'enter the ADH ascii solution file name'
      READ(*,'(A)') filename
      OPEN(15,file=filename,form='formatted',status='old')
      REWIND(15)
      WRITE(*,*) 'enter the start and end times for the calculation'
      READ(*,*) S_TIME
      READ(*,*) E_TIME 
      READ(15,*)
      READ(15,*)
      READ(15,*)
      READ(15,*) junk1, NPTOT
      READ(15,*) junk2, NETOT 
	READ(15,*)

        DO NN=1, NPTOT
          SUMX(NN) = 0.0
          SUMY(NN) = 0.0
        ENDDO
        P_TIME = 0.0
        TIME_TOT = E_TIME - S_TIME
 
        DO ITIME = 1, 9999999
          READ(15,*,END=900) junk1, junk2, TIME
          DEL_T = TIME - P_TIME
          P_TIME=TIME
          DO NN=1,NPTOT
             READ(15,*,END=900) VALX(NN), VALY(NN)
             IF(TIME .GE. S_TIME .AND. TIME .LE. E_TIME) THEN
               SUMX(NN) = SUMX(NN)+VALX(NN)*DEL_T
               SUMY(NN) = SUMY(NN)+VALY(NN)*DEL_T 
             ENDIF
          ENDDO
        ENDDO
  900   CONTINUE
        OPEN(68,file='residual.out',form='formatted',status='unknown')
        WRITE(68,*)'DATASET' 
        WRITE(68,*)'OBJTYPE "mesh2d"' 
        WRITE(68,*)'BEGVEC' 
        WRITE(68,*)'ND ', NPTOT
        WRITE(68,*)'NC ', NETOT
        WRITE(68,*)'NAME "RESIDUAL"'
        WRITE(68,*)'TS 0  0.0' 
          DO NN=1,NPTOT
             WRITE(68,*) SUMX(NN)/(TIME_TOT), SUMY(NN)/(TIME_TOT) 
          ENDDO
        WRITE(68,*)'ENDDS' 
        CLOSE(15)
        CLOSE(68)

      END
