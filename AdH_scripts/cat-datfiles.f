      PROGRAM ADHCATDAT 
      PARAMETER (MAXNE = 500000, MAXN = 1000000, MAXNDF = 6)
      INTEGER NPTOT, NETOT, N, NN, ITIME, TIME_INT
      REAL SAVE_TM1, SAVE_TM2, VAL, TIME, VALX, VALY, VALZ, FACTOR
      CHARACTER filename*80,filename2*80,junk1*2,junk2*2,EXTENSION*20



      WRITE(*,*) 'This routine will merge several solution files'
      WRITE(*,*) 'enter the number of solution files to be merged'
      READ(*,*) NUMBER

      WRITE(*,*) 'what is the variable extension for this solution.' 
      READ(*,'(A)') EXTENSION

      WRITE(*,*) 'enter the merged ADH solution filename'
      READ(*,'(A)') filename2
      OPEN(68,file=filename2,form='formatted',status='unknown')

      DO NN = 1, NUMBER
        WRITE(*,*) 'enter the ADH ascii solution filename'
        READ(*,'(A)') filename
        OPEN(15,file=filename,form='formatted',status='old')
        REWIND(15)
        WRITE(*,*) 'enter the first time to save'
        READ(*,*) SAVE_TM1 
        WRITE(*,*) 'enter the last time to save'
        READ(*,*) SAVE_TM2 
        WRITE(*,*) 'enter the solution save increment'
        READ(*,*) TIME_INT 
        WRITE(*,*) 'Enter the data conversion factor'
        READ(*,*) FACTOR
        READ(15,*)
        READ(15,*)
        READ(15,*)
        READ(15,*) junk1, NPTOT
        READ(15,*) junk2, NETOT 
        READ(15,*) 
        READ(15,*) 
       
        IF(NN .EQ. 1) THEN
          WRITE(68,*)'DATASET' 
          WRITE(68,*)'OBJTYPE "mesh2d"' 
          IF(EXTENSION .EQ. 'ovl')THEN
             WRITE(68,*)'BEGVEC' 
          ELSE
            WRITE(68,*)'BEGSCL'
          ENDIF
          WRITE(68,*)'ND ', NPTOT
          WRITE(68,*)'NC ', NETOT
          WRITE(68,*)'NAME ', EXTENSION
          WRITE(68,*)'TIMEUNITS SECONDS'
        ENDIF  

        K=TIME_INT

        DO ITIME = 1, 9999999
          READ(15,*,END=900) junk1, junk2, TIME
          IF(K .EQ. TIME_INT) THEN
            K = 1
            IF(TIME .LE. SAVE_TM2) THEN
              IF(TIME .GE. SAVE_TM1) THEN
                WRITE(68,*) 'TS  0  ', TIME 
                IF(EXTENSION .EQ. 'ovl') THEN
                  DO N=1,NPTOT
                    READ(15,*,END=900) VALX, VALY, VALZ
                    WRITE(68,950) VALX*FACTOR, VALY*FACTOR, VALZ*FACTOR 
                  ENDDO
                ELSE
                  DO N=1,NPTOT
                    READ(15,*,END=900) VAL
                    WRITE(68,*) VAL*FACTOR 
                  ENDDO
                ENDIF 
              ELSE
                DO N=1,NPTOT
                  READ(15,*,END=900)
                ENDDO
              ENDIF
              ELSE
               GOTO 900
            ENDIF
          ELSE
            DO N=1,NPTOT
              READ(15,*,END=900)
            ENDDO
            K = K+1
          ENDIF
        ENDDO
  900   CONTINUE
        CLOSE(15)
      ENDDO
  950   FORMAT(3E15.6)
        WRITE(68,*)'ENDDS' 
        CLOSE(68)

      END
