      PROGRAM GRAB_R10SOL_CHUNK
C
C  This program grabd a user specified chunk of an RMA10 solution
C  file and copies it to a user speficied file.
C
      PARAMETER (MNP=100000,MEL=50000)
      REAL*4 TET,VEL,WSEL,DELBED,BSHR,VVEL,DFCT 
      DIMENSION VEL(6,MNP),WSEL(MNP),VVEL(MNP)
      DIMENSION DELBED(MNP),BSHR(MNP),DFCT(MEL)
      DIMENSION IRESAV(9),IMAT(MEL),NDRY(MNP)
      CHARACTER*80 FNAME1, FNAME2
C
C
      WRITE(*,*)
      WRITE(*,*) '      PROGRAM GRAB_R10SOL_CHUNK'
      WRITE(*,*)
      WRITE(*,*) 'This program grabs a user specified chunk of an'
      WRITE(*,*) 'RMA10 solution file and copies it to a user'
      WRITE(*,*) 'specified file'
      WRITE(*,*)
      WRITE(*,*) 'What is the RMA10 solution file name?'
      READ(*,5) FNAME1
 5    FORMAT (A)
      OPEN(10,FILE=FNAME1,STATUS='OLD',FORM='UNFORMATTED')
      WRITE(*,*) 'What do you want the output file name to be?'
      READ(*,5) FNAME2
      OPEN(20,FILE=FNAME2,STATUS='UNKNOWN',FORM='UNFORMATTED')
      WRITE(*,*) 'What is the time window you want to extract'
      WRITE(*,*) 'From the RMA10 solution file (i.e. the minimum'
      WRITE(*,*) 'and maximum times)?'
 10   READ(*,*) TMIN, TMAX
      IF (TMIN .GE. TMAX) THEN
        WRITE(*,*) 'ERROR: TMIN MUST BE LESS THAN TMAX'
        WRITE(*,*) 'Reenter the minimum and maximum times'
        GO TO 10
      END IF
C
      IFIRST = 0
      NDFP1 = 7
 100  READ(10,END=200) TET, NP, NDF, NE,
     &       NDFS,(IRESAV(K),K=1,NDFS),
     &       ((VEL(K,J),J = 1, IRESAV(K)),K=1,NDF),
     &       (WSEL(J),J = 1, IRESAV(3)),
     &       (IMAT(J), J = 1, NE), (NDRY(J), J = 1, NP),
     &       (DELBED(J),J=1,IRESAV(NDFP1)),(BSHR(J),J=1,IRESAV(NDFS)),
     &       (VVEL(J), J = 1, NP), (DFCT(J),
     &       J = 1, NE)
      IF (TET .GE. TMIN .AND. TET .LE. TMAX) THEN
        WRITE(20) TET, NP, NDF, NE,
     &       NDFS,(IRESAV(K),K=1,NDFS),
     &       ((VEL(K,J),J = 1, IRESAV(K)),K=1,NDF),
     &       (WSEL(J),J = 1, IRESAV(3)),
     &       (IMAT(J), J = 1, NE), (NDRY(J), J = 1, NP),
     &       (DELBED(J),J=1,IRESAV(NDFP1)),(BSHR(J),J=1,IRESAV(NDFS)),
     &       (VVEL(J), J = 1, NP), (DFCT(J),
     &       J = 1, NE)
        TEND = TET
        IF (IFIRST .EQ. 0) THEN
          TBEG = TET
          IFIRST = 1
        END IF
      END IF
      GO TO 100
 200  IF (IFIRST .EQ. 0) THEN
        WRITE(*,*) 'The solution file contained no data for the'
        WRITE(*,*) 'time window you specified.  Enter different'
        WRITE(*,*) 'values for the minimum and maximum times' 
        REWIND 10
        GO TO 10
      ELSE
        WRITE(*,*) 'The new file contains RMA10 solution data'
        WRITE(*,*) 'for all time steps between (and including)'
        WRITE(*,*) 'the following 2 times:' 
        WRITE(*,300) TBEG,TEND 
 300    FORMAT(2F12.4)
      END IF
      STOP
      END
