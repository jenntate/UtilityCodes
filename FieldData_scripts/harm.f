      PROGRAM HARMONY
C   **************************************************************
C   *    HARMONIC ANALYSIS BY D.L. DURHAM, LIBRARY PLOT2D        *
C   *    SUPPLIED BY STEPHEN A. ADAMEC, SUBROUTINES PLOTTER AND  *
C   *    COMPARE AND MODIFICATIONS BY MCANALLY + ADAMEC,1977.    *
C   *    CONVERTED TO BCS CRAY BY DONNELL DEC 1982               *
C   *    CONVERTED TO CDC CYBERNET NOS BY LETTER 6-1984          *
C   *    CONVERTED TO CRAY-YMP DISSPLA CALLS BY DONNELL 10-1990  *
C   **************************************************************
      PARAMETER (NDATAP = 30000, NCOMP = 15, NCOMP2 = 2 * NCOMP, 
     *           NCOMPP = NCOMP2+1, NT1 = 150)
      COMMON /COMP/   PERIOD(NCOMP), OMEGA(NCOMP)
      COMMON /SERIES/ X(NDATAP), XBAR, KOUNT, RMS, NCOMRD, DELT, NPTS,
     *                TMIN, TMAX, YMIN, YMAX, NTITLE, NVPR, NRSKIP
      COMMON /WORD/   TITLE, RUNLAB,XAXL,YAXL,DATE,TIME,ECHO1,ECHO2
      COMMON /ZPLOT/  Y(NDATAP), R(NDATAP), T(NDATAP),
     *                IPLTNTH, IDRUM
      CHARACTER *80   TITLE,FNAME1,FNAME2
      CHARACTER *40   RUNLAB, XAXL, YAXL, LETTERS
      CHARACTER       DATE*6, TIME*4, ECHO1*6, ECHO2*4
      CHARACTER *80   RECORD
C
      REAL C(NCOMPP, NCOMPP)
C     DOUBLE PRECISIONC(NCOMPP,NCOMPP),A1,A2
      DIMENSION INDEX(NCOMP2), AMP(NCOMP), PHS(NCOMP), HOURS(NCOMP)
      DIMENSION AMPM(NCOMP)
      CHARACTER PRNT(NCOMP)*8
      DATA PRNT/NCOMP*'++++++++'/
      DATA PERIOD/NCOMP*0.0/
C                     123456789-123456789-123456789-123456789-
      RUNLAB(1:40) = 'Harmony Program ...       Test Run 1    '
      XAXL(1:40)   = 'TIME IN HOURS                           '
      YAXL(1:40)   = 'AMPLITUDE IN FEET                       '
      IDRUM     =  0 
      TMIN      =  1.E20
      YMIN      =  1.E20
      TMAX      = -1.E20
      YMAX      = -1.E20
      IPLTNTH   =  1
      NPTS      =  99999
      IREST     =  0
      DELT      =  0.25
      NCOMRD    =  3
      NRSKIP    =  0
      NTITLE    = 99
C
      IERROR = 0
C ...
      PRINT *,' ============== HARMONIC ANALYSIS ============ '
      PRINT *,' RUN CONTROL INPUT EXPECTED ON LOGICAL UNIT=5  '
      PRINT *,' ============================================= '
      PRINT *,' PROGRAM DIMENSIONED To ', NDATAP, ' POINTS'
      PRINT *,'                 and To ', NCOMP, ' COMPONENTS'
      PRINT *,' ============================================='
 1    continue
C    1 PRINT *,' Enter Run/Plot Title (<= to 40 Characters)'
C      READ (5,100,END =180)        RECORD
      RECORD = 'gomer'
      WRITE(15,'(A)')              RECORD      
      IF(RECORD(1:1).NE. ' ')      RUNLAB(1:40) = RECORD(1:40)
    5 WRITE (6,107)                RUNLAB
C      PRINT *,'Enter file name for input parameters'
C      READ(*,'(A80)') FNAME1
      FNAME1 = 'periods.txt'
      PRINT *,'Enter file name with columnar data'
      READ(*,'(A80)') FNAME2
      OPEN(UNIT=13,FILE=FNAME1,STATUS='OLD')  
      OPEN(UNIT=40,FILE=FNAME2,STATUS='OLD')  
C
C---Read up Input Parameters
C
      PRINT *,'Read up input parameters from',FNAME
      READ(13,*) NPTS
      READ(13,*) NCOMRD
      READ(13,*) IREST
      READ(13,*) DELT
C
C
      PRINT *,'Number Data Points =',NPTS
      PRINT *,'Number Requested Constituents =',NCOMRD
      PRINT *,'Residual Switch =', IREST
      PRINT *,'Time Interval =',DELT
C
C
      write(*,*)'Read the Periods of Interest'
      do i = 1, ncomrd
        read(13,*)period(i)
      enddo
      write(*,*)'The periods are:'
      do i = 1, ncomrd
        write(*,*)period(i)
      enddo
      READ(13,*) NRSKIP
      READ(13,*) NVPR
      READ(13,*) NTITLE
      PRINT *,'Number Points to Skip =',NRSKIP
      PRINT *,'Number of Data Pts. per Line =',NVPR
      PRINT *,'Number of Title Lines =',NTITLE
      CLOSE(13)
C
C
C
   90 FORMAT(' Default =',A)
  100 FORMAT (A)
  105 FORMAT(' Default =', 3I8,F8.4)
  106 FORMAT(' Default =', (1X,6F7.4,/))
  107 FORMAT(' RUNLAB TITLE=',A ,/)
  108 FORMAT(' IDRUM PLOT OPTION=',I10)
  110 FORMAT(' X AXIS PLOT LABEL=', A,/,9X,'TMIN-TMAX=', 2F25.2)
  120 FORMAT(' Y AXIS PLOT LABEL=', A,/,9X,'YMIN-YMAX=', 2F25.2,/)
  130 FORMAT(' IPLTNTH  Nth     =', I10)
  140 FORMAT(' NPTS     NO OF POINTS    =',I10,/,
     *       ' NCOMRD   NO OF PERIODS   =',I10,/,
     *       ' IREST    RESIDUAL ON-OFF =',I10,/,
     *       ' DELTA    DECIMAL HR INC  =',F10.4,/)
  150 FORMAT(' PERIODS = ', (1X,6F10.5,/))
  160 FORMAT(' NRSKIP   RECORDS TO SKIP =',I10,/,
     *       ' NVPR     VARIBLES/LINE   =',I10,/,
     *       ' NTITLE   LINES OF TITLES =',I10, ' ** ENDBANNER flag',/)
C
      GO TO 200
  170 PRINT *,' +++++ AN ERROR HAS OCCURRED DURING READ +++++'
      IERROR = IERROR + 1
      STOP 'READ-ERROR'
  180 PRINT *,' +++++ END-OF-FILE HIT, ... Try Again ...'
      GO TO 1
  190 PRINT *, ' Could NOT Find all ',NCOMRD,' Harmonic Constituents'
      PRINT *, ' Stop ... '
      GO TO 500
C
  200 CALL FILERD 
C 
      REC = (NPTS - 1) * DELT
      WRITE (6,210) RUNLAB, NPTS, DELT, REC
  210 FORMAT ( //, 5X, 'HARMONIC ANALYSIS BY PROGRAM HARMONY', 
     *         //, 5X, A, 
     *         //, 5X, I6, ' DATA PTS. AT ', F10.5, 
     *            ' HOUR INTERVALS. RECORD LENGTH=', F10.2)
      WRITE (6,220) XBAR
  220 FORMAT ( /, 10X, 'INPUT DATA MEAN (REMOVED)', G12.5)
      DO 240 I = 1, NPTS
         X(I) = X(I) - XBAR
         IF (ABS(X(I)) .GT. 30.0) X(I) = 0.00
  240 CONTINUE
      DO 250 I = 1, NCOMRD
        OMEGA(I) = 6.2831853072 / PERIOD(I)
  250 CONTINUE
      NC1 = NCOMRD + 1
      NC2 = 2 * NCOMRD
      NCP = NC2 + 1
      DO 280 I = 1, NCP
        DO 270 J = 1, NCP
          C(I,J) = 0.
  270   CONTINUE
  280 CONTINUE
C-
      DO 340 I = 1, NPTS
         TT = FLOAT(I - 1) * DELT
         DO 310 J = 1, NCOMRD
           TBPD = OMEGA(J) * TT
           A1 = COS( TBPD )
           DO 290 K = J, NCOMRD
              TBPD = OMEGA(K) * TT
              A2 = COS( TBPD )
              C(J,K) = C(J,K) + A1 * A2
  290      CONTINUE
           DO 300 K = NC1, NC2
              TBPD = OMEGA(K-NCOMRD) * TT
              A2 = SIN( TBPD )
              C(J,K) = C(J,K) + A1 * A2
  300      CONTINUE
           C(J,NCP) = C(J,NCP) + A1 * X(I)
  310   CONTINUE
        DO 330 J = NC1, NC2
           TBPD = OMEGA(J-NCOMRD) * TT 
           A1 = SIN( TBPD )
           DO 320 K = J, NC2
              TBPD = OMEGA(K-NCOMRD) * TT 
              A2 = SIN( TBPD )
              C(J,K) = C(J,K) + A1 * A2
  320      CONTINUE
           C(J,NCP) = C(J,NCP) + A1 * X(I)
  330   CONTINUE
  340 CONTINUE
C 
      DO 350 J = 2, NC2
        DO 345 K = 1, J - 1
          C(J,K) = C(K,J)
  345   CONTINUE
  350 CONTINUE
C 
      CALL INVERT(C, -NC2, NC2, NCOMPP, NCOMPP, INDEX)
C 
      DO 360 I = 1, NCOMRD
         AMP(I) = SQRT( C(I,NCP)**2 + C(I+NCOMRD,NCP)**2 )
         AMPM(I) = AMP(I)*0.3048
         PHS(I) = ATAN2( C(I+NCOMRD,NCP),C(I,NCP) ) * 57.2957795131
         HOURS(I) = PHS(I) * PERIOD(I) / 360.
  360 CONTINUE
      WRITE (6,365) (PERIOD(I),AMP(I),AMPM(I)
     *     ,HOURS(I),PHS(I),I=1,NCOMRD)
  365 FORMAT ( /, 10X, 'CALCULATED COMPONENTS',  //, 12X, 
     *     'PERIOD   AMPLITUDE(ft)      (m)        PHASE (hr)  (deg)'
     *     ,  //, (10x,5G12.5)) 
C 
      CALL COMPAR ( PERIOD, AMP, PHS, IREST)
C 
      IF( IDRUM.GE.1 ) THEN
C ...     PLOT THE DATA
c.....jhs 5-93
c          CALL PLOTTER
      ENDIF
C
      DO 370 I = 1, NCOMRD
        IF (ABS(PERIOD(I)-12.4206) .LT. .0001) PRNT(I) = '    M2 ='
        IF (ABS(PERIOD(I)-12.6584) .LT. .0001) PRNT(I) = '    N2 ='
        IF (ABS(PERIOD(I)-12.0165) .LT. .0001) PRNT(I) = '    T2 ='
        IF (ABS(PERIOD(I)-12.1916) .LT. .0001) PRNT(I) = '    L2 ='
        IF (ABS(PERIOD(I)-12.00) .LT. .0001) PRNT(I) = '    S2 ='
        IF (ABS(PERIOD(I)-11.9672) .LT. .0001) PRNT(I) = '    K2 ='
        IF (ABS(PERIOD(I)-23.9345) .LT. .0001) PRNT(I) = '    K1 ='
        IF (ABS(PERIOD(I)-25.8193) .LT. .0001) PRNT(I) = '    O1 ='
        IF (ABS(PERIOD(I)-24.0659) .LT. .0001) PRNT(I) = '    P1 ='
        IF (ABS(PERIOD(I)-23.0985) .LT. .0001) PRNT(I) = '    J1 ='
        IF (ABS(PERIOD(I)-24.8412) .LT. .0001) PRNT(I) = '    M1 ='
        IF (ABS(PERIOD(I)-26.8684) .LT. .0001) PRNT(I) = '    Q1 ='
        IF (ABS(PERIOD(I)-8.1924) .LT. .0001) PRNT(I) = '   SO3 ='
        IF (ABS(PERIOD(I)-6.2103) .LT. .0001) PRNT(I) = '    M4 ='
        IF (ABS(PERIOD(I)-4.1402) .LT. .0001) PRNT(I) = '    M6 ='
        IF (ABS(PERIOD(I)-3.1052) .LT. .0001) PRNT(I) = '    M8 ='
        IF (ABS(PERIOD(I)-327.859) .LT. .0001) PRNT(I) = '    MF ='
        IF (ABS(PERIOD(I)-661.3092) .LT. .0001) PRNT(I) = '    MM ='
        IF (ABS(PERIOD(I)-6.0) .LT. .0001) PRNT(I) = '    S4 ='
        IF (ABS(PERIOD(I)-8765.821) .LT. .0001) PRNT(I) = '    SA ='
        IF (ABS(PERIOD(I)-4382.905) .LT. .0001) PRNT(I) = '   SSA ='
        IF (ABS(PERIOD(I)-24.0) .LT. .0001) PRNT(I) = '    S1 ='
  370 CONTINUE
C 
C     .....................................................
C     .          PRINT OUT TABLE SUMMARY                  .
C     .....................................................
C 
      PRCT = FLOAT(KOUNT) / FLOAT(NPTS) * 100.
      IPRNTR = 0
      PRINT *,' OUTPUT Summary Table found on Logical Unit=50'
      LP = 50 
  380 WRITE (LP,400) TITLE
      WRITE (LP,410) RUNLAB, NPTS, DELT, REC, PRCT, XBAR, ECHO1, ECHO2
      WRITE (LP,420) (PRNT(J), PERIOD(J), AMP(J), AMPM(J), HOURS(J)
     *,  PHS(J), J=1,NCOMRD)
      WRITE (LP,430) RMS
C &&&
      write(55,2112) (amp(j),j=1,5)
 2112 format(5f12.7)
      write(56,2112) (hours(j),j=1,5)
C &&&
         IF( IREST.GT.0 ) WRITE (6,390) NPTS
  390    FORMAT (/,I8, ' RESIDUAL POINTS WRITTEN TO= Unit 2', /,
     *   80('-'))
         GO TO 500
  400 FORMAT (//, 23X, 'HARMONIC ANALYSIS BY PROGRAM HARMONY', 
     *      //, (1X,A,/),  / )
  410 FORMAT (3X, 'RUN TITLE=', A,  /, 
     *     1X, I7, ' DATA POINTS AT', F10.5, 
     *     ' HOUR INTERVALS.  RECORD LENGTH=', F10.2, ',',/,
     *    3X, 'PERCENT OF BAD PTS=', F12.7, '   INPUT DATA ',
     *   'MEAN (REMOVED)=', F10.5,  //, 32X, A5, 1X, A4, 25X, 
     *   '  -  -      ',  /, 7X, 'PERIOD', 7X, 'AMPLITUDE (ft)', 3X,
     *   '(m)',3x,'PHASE (HOURS)',3X,'DEGREES   CALCULATED PHASE (HRS)',/,
     *   4X, 12('-'), 4X, 9('-'), 3X, 13('-'), 3X,  32('-'),  // )
  420 FORMAT (1X,A10,1X,F12.5,1X,F12.7,1x,F12.7,3X,F12.5,1x,F12.5)
  430 FORMAT ( /, 30X, 'RMS = ', G12.4)
C 
  440 WRITE (6,450) 
  450 FORMAT ('END-OF-FILE ON LOGICAL UNIT=5 -- FIRST READ ')
  500 STOP 'AOK'
      END
      SUBROUTINE COMPAR( P, A, PH, IREST)
C-
      PARAMETER (NDATAP = 30000, NCOMP = 15, NCOMP2 = 2 * NCOMP, 
     *           NCOMPP = NCOMP2+1, NT1 = 150)
      COMMON /COMP/   PERIOD(NCOMP), OMEGA(NCOMP)
      COMMON /SERIES/ X(NDATAP), XBAR, KOUNT, RMS, NCOMRD, DELT, NPTS,
     *                TMIN, TMAX, YMIN, YMAX, NTITLE, NVPR, NRSKIP
      COMMON /WORD/   TITLE, RUNLAB,XAXL,YAXL,DATE,TIME,ECHO1,ECHO2
      COMMON /ZPLOT/  Y(NDATAP), R(NDATAP), T(NDATAP),
     *                IPLTNTH, IDRUM
      CHARACTER *80   TITLE
      CHARACTER *40   RUNLAB, XAXL, YAXL
      CHARACTER       DATE*6, TIME*4, ECHO1*6, ECHO2*4
C
      DIMENSION  A(NCOMP), P(NCOMP), PH(NCOMP)
      D = 57.2957795
      P2 = 6.2831853
      SUM = 0.
      REWIND 2
  100 DO 120 I = 1, NPTS
        T(I) = DELT * (I - 1)
        Y(I) = 0.0
        DO 110 M = 1, NCOMRD
  110   Y(I) = Y(I) + A(M) * COS((T(I)*P2/P(M))-(PH(M)/D))
        R(I) = X(I) - Y(I)
        SUM = SUM + R(I) * R(I)
  120 CONTINUE
      RMS = SQRT(SUM/NPTS)
      WRITE (42,130) RMS
  130 FORMAT ( ///, 13X, 'RMS ERROR =', G12.5)
      IF (IREST .LE. 0) GO TO 190
C ... ---------------------------------------------------------
C ...          WRITE RESIDUAL OUTPUT FILE TO TAPE 2
C ... ---------------------------------------------------------
      WRITE (2,140) NCOMRD, RMS
  140 FORMAT (' HARMONIC RESIDUAL. USED', I5, ' CONSTITUANTS.',
     *        ' RMS=', F8.4)
      WRITE (2,160) TITLE
      WRITE (2,170) (R(I), I = 1, NPTS)
      REWIND 2
  160 FORMAT (A)
  170 FORMAT (10X, F7.2)
  190 RETURN 
      END
      SUBROUTINE FILERD
      PARAMETER (NDATAP = 30000, NCOMP = 15, NCOMP2 = 2 * NCOMP, 
     *           NCOMPP = NCOMP2+1, NT1 = 150)
      COMMON /COMP/   PERIOD(NCOMP), OMEGA(NCOMP)
      COMMON /SERIES/ X(NDATAP), XBAR, KOUNT, RMS, NCOMRD, DELT, NPTS,
     *                TMIN, TMAX, YMIN, YMAX, NTITLE, NVPR, NRSKIP
      COMMON /WORD/   TITLE, RUNLAB,XAXL,YAXL,DATE,TIME,ECHO1,ECHO2
      CHARACTER *80   TITLE
      CHARACTER *40   RUNLAB, XAXL, YAXL
      CHARACTER       DATE*6, TIME*4, ECHO1*6, ECHO2*4, TDUM*80
C
      PRINT *,' ============================================'
      PRINT *,' TIME SERIES FILE EXPECTED ON LOGICAL UNIT=40'
      REWIND 40
      KOUNT = 0
      MMM   = NT1
      NN    = NTITLE
  100 IF (NN .EQ. 0) GO TO 120
      KOUNT = KOUNT + 1
      READ (40,220,END = 280)  TDUM
      IF( KOUNT .EQ. 1) TITLE(1:80) = TDUM(1:80)
      IF( TDUM(1:9).EQ.'ENDBANNER') GO TO 120
      NN = NN - 1
      GO TO 100
C ... FINISHED READING TITLE RECORDS
  120 PRINT *,' JUST READ ALL TITLE LINES ... Total = ',KOUNT
      IF (NRSKIP .EQ. 0) GO TO 140
      IILINE = 0
      DO 130 I = 1, NRSKIP
        READ (40,'(A80)')  TDUM
        IILINE = IILINE + 1
  130 CONTINUE
  140 PRINT *,' JUST SKIPPED ', IILINE, ' RECORDS ON fort.40'
      KOUNT = 0
      SUM = 0.0
      KK = 0
      NLINE = (NPTS / NVPR) + 1
      PRINT *,' READING TIME SERIES DATA ON fort.40 (A6,A4,24F5.2)'
      PRINT *,' ===================================== | Format | '
      DO 160 J = 1, NLINE
        K = KK + 1
        IF (K .GE. NPTS) GO TO 180
        KK = J * NVPR
c        READ (40,230,END = 200) TIME, (X(M), M = K, KK)
        READ (40,*,END = 200)  TIME,(X(M), M = K, KK)
        print *,x(j)
        IF (J .EQ. 1) ECHO1 = DATE
        IF (J .EQ. 1) ECHO2 = TIME
        DO 150 MM = K, KK
          ABXOFX = ABS(X(MM))
          IF (ABXOFX .GT. 30.) KOUNT = KOUNT + 1
          IF (ABXOFX .LE. 30.) SUM = SUM + X(MM)
  150   CONTINUE
  160 CONTINUE
  170 NPTS = K
  180 REWIND 40
      WRITE (6,190) X(1), ECHO1, ECHO2, X(NPTS), DATE, TIME
  190 FORMAT ( //, 10X, 'FIRST PT. READ IS', G12.5, 
     *       ' CORRESPONDING TO ', A,2X,A, /,
     *       10X, 'LAST PT. READ WAS', G12.5, 
     *       ' AT APPROXIMATELY ', A, 2X, A,  //,
     *       10X, ' NOTE IF ABS(POINT) GT 30.00, POINT ASSIGNED 0.00')
      GO TO 240
  200 NPTS = K - 1
      WRITE (6,210) NPTS
  210 FORMAT ( //, '*** OUT OF DATA AT', I7, ' PTS., ****WARNING****'
     *    ,  /, 4X, 'IN SUBROUTINE FILERD',  // )
      GO TO 180
  220 FORMAT (A)
c  230 FORMAT (A6, A4, 24F5.2)
C  230 FORMAT (24F5.2)
  240 XBAR = SUM / (NPTS - KOUNT)
      WRITE (6,250) NPTS, KOUNT, SUM, XBAR
  250 FORMAT ( /, 1X, 'NPTS=', I7, ' NO. BAD PTS=', I5, ' ACTIVE SUM='
     *    , F12.3, ' XBAR=', F12.4,  // )
      CLOSE(40)
      RETURN 
  280 WRITE (6,290) 
  290 FORMAT (' END-OF-FILE TAPE40 WHILE READING TITLES')
      STOP 'ERROR'
      END
      SUBROUTINE INVERT(ROWCOL, NR, NC, NNR, NNC, INDEX)
C BIGGEST-RESIDUAL-PIVOT ONE-SIDED SINGLE-SWEEP COMPLEX MATRIX INVERSION
      COMMON /WOE/ WEE
      REAL FLIP, ROWCOL(NNR, NNC), WEE
C     DOUBLE PRECISION FLIP,ROWCOL(NNR,NNC),WEE
C     COMPLEX FLIP,ROWCOL(NNR,NNC),WEE
      INTEGER INDEX(NC), MODULO
      DATA MODULO /262144/
C-        DATA MODULO/1000000B/   suggested by CrayYMP consultant
C-        DATA MODULO/O"1000000"/ was used on CDC NOS machine
C-   THIS DATA STATEMENT WAS CHANGED FROM FTN4 SYNTAX TO FTN5 SYNTAX
C-   WHEN CODE WAS MOVED FROM BCS CRAY TO CYBERNET CDC, WITHOUT ANY
C-   CORRECTION FOR WORD LENGTH. I'M NOT SURE WHETHER THIS IS GOING TO
C-   AFFECT CODE PERFORMANCE . --- JOE LETTER, JUNE 1984
C
C   INPUT  QUASI-SINGULAR RECTANGULAR MATRIX (ROWCOL(I,J),I=1,NR),J=1,NC
C          COLUMN VECTORS (V(I,J)=ROWCOL(I,J),I=1,NR),J=NC+1,NNC  IF ANY
C     TRIPLE-SPEED MATRIX-DIVISION OPTION  -NR BYPASSES MATRIX INVERSION
C     DEGENERACY    NNC.LE.NC GENERATES EIGENVECTORS SPANNING DEGENERACY
C     ROW-OPERATION INVERSION SEQUENCE IN BIGGEST-RESIDUAL-PIVOT BASIS
C     J
C   I MMMMV1   WMMMV1  WWMMV1  WWWMV1  WWWWV1
C     MMMMV2   WMMMV2  WWMMV2  WWWMV2  WWWWV2
C     MMMMV3   WMMMV3  WWMMV3  WWWMV3  WWWWV3
C     MMMMV4   WMMMV4  WWMMV4  WWWMV4  WWWWV4
C     MMMMV5   0MMMV5  00MMV5  000MV5  0000V5
C     MMMMV6   0MMMV6  00MMV6  000MV6  0000V6
C     MMMMV7   0MMMV7  00MMV7  000MV7  0000V7
C              B       BB      BBB     BBBB
      MN = 1
      INR = IABS(NR)
      NN = MIN0(INR,NC)
      NP = MIN0(NN,NNC)
      MNC = MAX0(NC,NNC)
C     SET UP BIGGEST-RESIDUAL-PIVOT INDEX
      DO 100 N = 1, NN
  100 INDEX(N) = N
C     QUASI-SINGULAR MATRIX-INVERSION LOOP
      DO 190 N = 1, NP
C     LOCATE BIGGEST RESIDUAL PIVOT
        WEE = 0.
        DO 130 M = N, NN
          J = INDEX(M)
          DO 120 L = N, INR
            I = L
            IF (I .LE. NN) I = INDEX(L)
            IF (ABS(WEE) - ABS(ROWCOL(I,J)) .GT. 0.) GO TO 120
C     IF(CABS(WEE)-CABS(ROWCOL(I,J))) 20,20,30
C     IF(DABS(WEE)-DABS(ROWCOL(I,J))) 20,20,30
  110       WEE = ROWCOL(I,J)
            MC = M
            IR = I
  120     CONTINUE
  130   CONTINUE
        ROWCOL(INR+1,N) = WEE
C DEGENERACY  RETURN PIVOTS, SUB-MATRIX INVERSE, DEGENERACY EIGENVECTORS
        IF (WEE .LT. 0.) GO TO 140
        IF (WEE .LE. 0.) GO TO 190
C     IF(CABS(WEE)) 50,100,50
C     PERMUTE PIVOT TAGS  INDEX(N),INDEX(MC)
  140   K = INDEX(MC)
        INDEX(MC) = INDEX(N)
        INDEX(N) = IR * MODULO + K
        IF (NR .LT. 0) MN = N
        DO 150 M = MN, MNC
          J = M
          IF (J .LE. NN) J = MOD(INDEX(M),MODULO)
C     PERMUTE ROWS (K,IR) TO DIAGONALIZE PIVOT
          FLIP = ROWCOL(IR,J)
C     SIGN FLIP FOR K.NE.IR TO PRESERVE DETERMINANT
          ROWCOL(IR,J) =  - ROWCOL(K,J)
C     REDUCE PIVOT TO UNITY BY ROW DIVISION
  150   ROWCOL(K,J) = FLIP / WEE
C     REDUCE OFF-PIVOT COLUMN ELEMENTS TO ZERO BY PIVOT-ROW SUBTRACTION
        DO 170 M = MN, MNC
          IF (M .EQ. N) GO TO 170
          J = M
          IF (J .LE. NN) J = MOD(INDEX(M),MODULO)
          DO 160 L = 1, INR
            IF (L .EQ. N) GO TO 160
            I = L
            IF (I .LE. NN) I = MOD(INDEX(L),MODULO)
            ROWCOL(I,J) = ROWCOL(I,J) - ROWCOL(I,K) * ROWCOL(K,J)
  160     CONTINUE
  170   CONTINUE
C     ROW-OPERATION GENERATOR VECTOR
        IF (NR .LT. 0) GO TO 190
        ROWCOL(K,K) =  - 1.
        DO 180 L = 1, INR
  180   ROWCOL(L,K) =  - ROWCOL(L,K) / WEE
  190 CONTINUE
      IF (NR .LT. 0) GO TO 220
C     PERMUTE MATRIX-INVERSE COLUMNS
      N = NN
  200 N = N - 1
      IF (N .LT. 1) GO TO 220
      J = MOD(INDEX(N),MODULO)
      I = INDEX(N) / MODULO
      IF ((I .EQ. J) .OR. (I .EQ. 0)) GO TO 200
      DO 210 L = 1, INR
        FLIP = ROWCOL(L,J)
        ROWCOL(L,J) =  - ROWCOL(L,I)
  210 ROWCOL(L,I) = FLIP
      GO TO 200
  220 CONTINUE
C     TEST FOR SPILL   DIVISION BY ZERO IMPOSSIBLE  TINIEST PIVOT IN WEE
      RETURN 
C     OUTPUT  MATRIX INVERSE ((1/ROWCOL(I,J),I=1,NR),J=1,NC)      IF ANY
C     COLUMN VECTORS (((1/ROWCOL).V(I,J),I=1,NR),J=NC+1,NNC)      IF ANY
C     EIGENVECTORS  (ROWCOL(I,RHALF(INDEX(J))),I=1,NC),J=NNC,NC   IF ANY
C     DETERMINANT PIVOTS IN (ROWCOL(NR+1,J),J=1,NP) IN DECREASING SIZE
C     PIVOT ROW-COLUMNS (I(N) J(N)) PACKED LEFT-RIGHT IN INDEX(N),N=1,NP
C     TIME T=(2*NP-1)*NR*NNC (ADD, MULTIPLY, OR DIVIDE) OPERATIONS
      END
