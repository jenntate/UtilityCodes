      PROGRAM PCTGEN
C
C WRITTEN BY KEITH MARTIN (DYNTEL) JANUARY 1996
C PURPOSE:  TO CALCULATE PERCENTILES FOR RMA2 OR CH3D OUTPUT AND 
C           PUT RESULTING PERCENTILES INTO RMA4 FORMAT, IN ORDER
C           THAT THE PERCENTILES CAN BE VIEWED IN FASTTABS.  IN 
C           ADDITION, THE SHEAR STRESS COEFFICIENTS ARE 
C           CALCULATED FOR THE 50TH AND 100TH PERCENTILE.
C
C LAST MODIFICATION:  OCTOBER 25, 1996 BY KEITH MARTIN
C
C      INCLUDE 'bin2asc.inc'
C
C
      INTEGER count,TRMLEN,I, dummy, TIME_STEPS,nn,ne  
      real*8 TIME,v1,v2
      REAL*8, DIMENSION(:), ALLOCATABLE :: x,y,elev,WSE
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ncon      
      LOGICAL AROUND, HERE       
C ---
C      DATA (IMATX(I),I=1,MAXE)/ MAXE*0/
C      DATA (IMATG(I),I=1,MAXE)/ MAXE*0/
C      DATA ITRACE /0/
C      DATA TIME/ ITMAX*-1.0E+10/
C      DATA TMIN, TMAX/1.0E+10, -1.0E+10/
C ---
      PARAMETER(MNN=2500000,IPC=21,ICOL=3)
      COMMON /LOOPS/ILOOP
      COMMON /PERCENT/PCT,PCTILES(MNN,IPC,ICOL),VOUT(MNN),NFILE,
     *        NT3(10),VMIN(MNN),VMAX(MNN),NT2
      DIMENSION OPCT(50)
      DIMENSION TETEND(15),TETHRS(15)
      DIMENSION IMAT(10000),DIV(MNN)
      CHARACTER FNAME*80,ZDUM*80,DUMA*80,junk*80,label*80,grid_file*80
      DATA OPCT/0.,1.,2.,5.,10.,20.,30.,40.,50.,60.,70.,80.,90.,
     *          95.,98.,99.,100.,33*100./
      CHARACTER junk1*80,junk2*80,junk3*80,temp*3
      DATA VMIN/MNN*9999./
      DATA VMAX/MNN*-9999./
      DATA VOUT/MNN*0/
      DATA NT3/10*0/
      NT2 = 0
      count=0
      NINTERP = 17
      DUM1 = 10.
      RHO = 515.378
      Z0 = 0.0009
      KSQ = 0.16
      IREND = 0

C**********************************
C  OPEN UP DATA FILES
C**********************************   
        PRINT *,'ENTER INPUT FILENAME:'
        READ (*,1001) FNAME
        OPEN(73,FILE=FNAME,STATUS='OLD')
          L12 = 40
      PRINT *,'ENTER OUTPUT FILENAME:'
      READ (*,1001) FNAME
      OPEN(63,FILE=FNAME,STATUS='UNKNOWN',FORM='FORMATTED')
C********************************************************
C     READ I,J COORDINATES FOR EACH NODE INTO AN ARRAY

C********************************************************
C234567        1         2         3         4         5         6         712

        READ (73,'a80')junk
        WRITE(*,*)junk
        READ (73,'a80')junk1
        WRITE(*,*)junk1
        READ (73,'a80')junk2
        WRITE(*,*)junk2
        READ (73,'a3,i30')temp,NP2
        WRITE(*,*)temp,NP2
        READ (73,'a3,i30')temp,NE2
        WRITE(*,*)temp,NE2
        READ (73,'a80')junk3
        WRITE(*,*)junk3
        allocate(WSE(NP2))
        PRINT *,'Number of nodes = ',NP2 ,',Number of elements = ',NE2
          NT3(II)=50000
          DO 400 I = 1,NT3(II)
              count=count+1
              read(73,*)temp
              backspace(73)
              if(temp .eq. 'END')then
                WRITE(*,*)'End of solution file'
                goto 527
              else
                READ(73,'a3,f') temp,TIME
              endif
              PRINT *,'READING DATA FOR TIME =',TIME
              DO 300 J=1,NP2
                READ(73,*) v1,v2
                WSE(J)=(v1*v1+v2*v2)**0.5
  300         CONTINUE

C****************************************************
C     FIND THE MAXIMUM VELOCITY AT EACH NODE
C****************************************************
           PRINT *,'TIME TO FIND MAXIMUMS!!',I,IREND
            DO 350 J=1,NP2
              VELMAG = WSE(J)
              IF (VELMAG .GT. VMAX(J)) THEN
                VMAX(J) = VELMAG
              ENDIF
              IF (VELMAG .LT. VMIN(J)) THEN
                VMIN(J) = VELMAG
              ENDIF
  350       CONTINUE
  400     CONTINUE
  527     continue
          NT3(II)=count-1
        REWIND(73)
      PRINT *,'I FOUND ALL THE MAXIMUMS!!'
C***************************************************************
C REREAD FILE, CALCULATE VELOCITY MAGNITUDES, AND
C SEPARATE THE VELOCITIES INTO PERCENTILE BINS FOR EACH NODE
C***************************************************************
C        1         2         3         4         5         6         712
        READ (73,'a80')junk
        WRITE(*,*)junk
        READ (73,'a80')junk1
        WRITE(*,*)junk1
        READ (73,'a80')junk2
        WRITE(*,*)junk2
        READ (73,'a3,i30')temp,NP2
        WRITE(*,*)temp,NP2
        READ (73,'a3,i30')temp,NE
        WRITE(*,*)temp,NE
        READ (73,'a80')junk3
        WRITE(*,*)junk3
          NT2 = NT2 + NT3(II)
          print *,'NT2 = ',NT2
          print *,'NP2 = ',NP2
        DO 700 I = 1,NT3(II)
            READ(73,'a3,f') temp,TIME
            PRINT *,'READING DATA FOR TIME = ',TIME
            DO 600 J=1,NP2
              READ(73,*) v1,v2
              WSE(J)=(v1*v1+v2*v2)**0.5
  600       CONTINUE
            print *,'FINISHED READING TIME = ',TIME
          IF (I .EQ. 1) THEN
            DO 621 NN = 1,NP2
              print *,'INSIDE LOOP, NN = ', NN
              DIV(NN)= ( VMAX(NN) - VMIN(NN) ) / 20
              print *,'DIV = ', DIV(NN)
  621       CONTINUE
            print *,'FINISHED DETERMINING SECTIONS'
          ENDIF
            DO 650 J=1,NP2
              VELMAG = WSE(J)
  623         FORMAT(3F12.4)
              DO 625 M = 1,21
                IF (M .EQ. 1) THEN
                  PCTILES(J,M,1) = VMIN(J)
                ELSEIF (M .GT. 1) THEN
                  PCTILES(J,M,1) = float(M-1) * DIV(J) + VMIN(J)
                ENDIF
                IF (VELMAG .LE. PCTILES(J,M,1)) THEN
                  PCTILES(J,M,2) = PCTILES(J,M,2) + 1. 
                ENDIF
  625         CONTINUE
  650       CONTINUE
  700   CONTINUE
        REWIND(73)
      DO 1100 K = 1,NP2
        DO 1000 J = 1,21
          PCTILES(K,J,3) = PCTILES(K,J,2) / float(NT2)
          IF (PCTILES(K,J,3) .GT. 1) THEN
             print *,PCTILES(K,J,3),NT2
          ENDIF
 1000   CONTINUE
 1100 CONTINUE
      print *,'NT2 = ',NT2
      print *,'NP2 = ',NP2
      
C
C INTERPOLATE PERCENTILES
C
      WRITE (63,'a80')junk
      WRITE (63,'a80')junk1
      WRITE (63,'a80')'BEGSCL'
      WRITE (63,'a3,i30')'ND ',NP2
      WRITE (63,'a3,i30')'NC ',NE
      WRITE (63,*)'NAME "Depth"'
     
      NINT = NINTERP + 1
      ZERO = 0.
      WRITE(63,*)'TS 0 ',zero
      DO 1700 KM = 1,NP2
           WRITE(63,*)VMIN(KM)
 1700 CONTINUE       
      DO 1900 I = 2,NINTERP-1
        PRINT *,'Node 1 has these values: ',PCTILES(1,I,1),
     *     PCTILES(1,I,2),PCTILES(1,I,3)
        PCT = OPCT(I) / 100.
        PRINT *,'Entering INTERP with ',PCT,OPCT(I)
        CALL INTERP(NP2)
        WRITE(63,*)'TS 0 ',OPCT(I) 
        DO 1800 J = 1,NP2
            WRITE(63,*)VOUT(J)
 1800   CONTINUE     
 1900 CONTINUE

      WRITE(63,*)'TS 0 ',100.0
      DO 1750 KM = 1,NP2
           WRITE(63,*)VMAX(KM)
 1750 CONTINUE       

      WRITE(63,*)'ENDDS'

 1001 FORMAT(A80)
 1200 FORMAT(3I10)
 2001 FORMAT(I5,21F6.2)
 3001 FORMAT(16I5)
 3301 FORMAT('PPM  CONC')
 4001 FORMAT(2F10.4)

      STOP
      END
      
C----------------------------------------------------------------------
      SUBROUTINE INTERP(NP2)
C
C WRITTEN BY KEITH MARTIN (DYNTEL) JANUARY 1996
C PURPOSE:  INTERPOLATE CALCULATED PERCENTILES TO EXACT PERCENTILES 
C           (I.E 5,10,20,...)
C
C
C      INCLUDE 'bin2asc.inc'
C
      INTEGER NP2
      PARAMETER(MNN=2500000,IPC=21,ICOL=3)
      COMMON /PERCENT/PCT,PCTILES(MNN,IPC,ICOL),VOUT(MNN),NFILE,
     *                NT3(10),VMIN(MNN),VMAX(MNN),NT2
      COMMON /SUB/ START,END,NODE
C
      DO 1800 J = 1,NP2
        NODE = J
        ICHK = -1
        DO 1700 K = 2,21
          IF (J .EQ. 7861) THEN
C            WRITE (81,1702) J,K
C            WRITE (81,1701) PCTILES(J,K,1),PCTILES(J,K,2),
C     *                      PCTILES(J,K,3)
C 1701       FORMAT(3F10.4)
C 1702       FORMAT(2I5)
          ENDIF
          IF ( VMAX(J) .LT. 0.00001 ) THEN
            VOUT(J) = 0.
          ELSEIF ((PCTILES(J,K,3) .GT. PCT) .AND. (ICHK .LT. 0)) THEN
            KM = K - 1
            DEN = (PCTILES(J,K,3) - PCTILES(J,KM,3))
            if (den .eq. 0.) then
              print *,'Values:',PCTILES(J,K,3),PCTILES(J,K-1,3),J,K
              print *,'Freqs: ',PCTILES(J,K,2),PCTILES(J,K-1,2),NT2
              KM = KM - 1
              IF( KM .LE. 0 ) KM = 1
               VOUT(J) = PCTILES(J,K,1)
                 ICHK = ICHK + 1
               GOTO 1700
            endif
            PNUM1 = (PCT - PCTILES(J,KM,3))
            PNUM2 = (PCTILES(J,K,3) - PCT)
            TERM1 = (PNUM1/DEN) * PCTILES(J,K,1)
            TERM2 = (PNUM2/DEN) * PCTILES(J,KM,1)
            VOUT(J) = TERM1 + TERM2
            ICHK = ICHK + 1
          ENDIF
 1700   CONTINUE
 1800 CONTINUE
      RETURN
      END
C-------------------------------------------------------------------------------

      INTEGER FUNCTION TRMLEN (STRING)
      CHARACTER *(*) STRING
      ILAST = LEN(STRING)
      DO 10 I = ILAST, I, -1
         IVAL = ICHAR(STRING(I:I))
         IF( IVAL.GE.33 .AND. IVAL.LE.126) GO TO 20
  10  CONTINUE
      TRMLEN = ILAST
      RETURN
  20  TRMLEN = I
      RETURN
      END      
C--------------------------------------------------------------------

      SUBROUTINE READ_GRID(nn,ne,x,y,elev,ncon)

      implicit none

      INTEGER i, j, nn, ne, dummy
      INTEGER ncon(ne,3)
      DOUBLE PRECISION x(nn),y(nn),elev(nn)
      CHARACTER*80 elevation_grid_file, label

      do i=1,nn
        read(14,*)j,x(i),y(i),elev(i)
        if(j.ne.i)then
          write(*,*)' Node table inconsistency in grid file'
          write(*,*)'  i= ',i,' but j= ',j
          stop
        endif
      enddo

      do i=1,ne
        read(14,*)j,dummy, ncon(j,1), ncon(j,2), ncon(j,3)
        if(j.ne.i)then
          write(*,*)' Element inconsistency in grid file'
          write(*,*)'  i= ',i,' but j= ',j
          stop
        endif
      enddo

      return
      end
