      PROGRAM ADHENDDAT 
      PARAMETER (MAXNE = 500000, MAXN = 1000000, MAXNDF = 6)
      INTEGER NPTOT, NETOT,  NN, ITIME, INT_TM
      REAL SAVE_TM, VAL, TIME, VALX, VALY, VALZ
      CHARACTER filename*80,filename2*80,junk1*2,junk2*2,EXTENSION*20



      WRITE(*,*) 'enter the ADH ascii solution filename'
      READ(*,'(A)') filename
      OPEN(15,file=filename,form='formatted',status='old')
      REWIND(15)
      WRITE(*,*) 'what is the variable extension on this
     #filename(ovl,ohd,err)?'
      READ(*,'(A)') EXTENSION
      WRITE(*,*) 'enter the last time to save'
      READ(*,*) SAVE_TM 
      WRITE(*,*) 'enter the output interval to save'
      READ(*,*) INT_TM 
      READ(15,*)
      READ(15,*)
      READ(15,*)
      READ(15,*) junk1, NPTOT
      READ(15,*) junk2, NETOT 
      READ(15,*)  

      WRITE(*,*) 'enter the corrected ADH ascii solution filename'
      READ(*,'(A)') filename2
      OPEN(68,file=filename2,form='formatted',status='unknown')
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

      COUNT=INT_TM

        DO ITIME = 1, 9999999
          READ(15,*,END=900) junk1, junk2, TIME
          IF(COUNT .EQ. INT_TM) THEN
            COUNT = 0
           IF(TIME .LE. SAVE_TM) THEN
            WRITE(68,*) 'TS  0  ', TIME 
             IF(EXTENSION .EQ. 'ovl') THEN
              DO NN=1,NPTOT
                READ(15,*,END=900) VALX, VALY, VALZ
                WRITE(68,950) VALX, VALY, VALZ 
              ENDDO
            ELSE
              DO NN=1,NPTOT
                READ(15,*,END=900) VAL
                WRITE(68,*) VAL 
              ENDDO
            ENDIF 
           ELSE
            GOTO 900
           ENDIF
          ELSE
           DO NN=1,NPTOT
            READ(15,*,END=900) 
           ENDDO
          ENDIF
          COUNT=COUNT+1
        ENDDO
  900   CONTINUE
  950   FORMAT(3E15.6)
        WRITE(68,*)'ENDDS' 
        CLOSE(15)
        CLOSE(68)

      END
