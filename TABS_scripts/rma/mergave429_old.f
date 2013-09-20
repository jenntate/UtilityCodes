       PROGRAM MERGAVE
C
C     THIS PROGRAM READS GFGEN-RMA2-RMA4-RMA10 BINARY DATA FILES
C     MERGES SEVERAL RMA2/RMA4/RMA10 FILES TOGETHER   AND
C     AVERAGES OVER A SPECIFIED PERIOD FOR HYDRO AND SALINITY
C
C     OUTPUT IS THE COMPLETE MERGED FILE AND AN AVERAGE FILE
C
C ... WRITTEN BY BARBARA P. DONNELL  WESHE-S   AUGUST 14, 1986
C ... CONVERTED TO FORTRAN-77 BY SIS COMPUTER SERVICES JUNE 1987
C ... THIS PROGRAM WAS ONCE CALLED R23MERG, NOW MULTI-PURPOSE
C
****************************************************************
*
*     MODIFIED BY BOB EVANS / B.P.DONNELL TO RUN WITH THE LATEST
*     GFGEN & RMA2 (VERSION 4.25) & RMA4 & RMA10    
*                             LAST MODIFICATION DATE: 01-27-1992
*     DONNELL RE-MODIFIED TO ACCEPT VERSION 4.27    : 11-08-1993
****************************************************************
*
C       UNIT  3    = SUMMARY PRINT LISTING FROM THIS PROGRAM (NEW)
C       UNIT 11-20 = INPUT RMA2 FILES (PIECES)               (OLD)
C       UNIT 40    = BINARY GFGEN GEOMETRY                   (OLD)
C       UNIT 50    = MERGED RMA2 PIECES TO MAKE A WHOLE      (NEW/OLD)
C       UNIT 60    = RMA2 LOOK-A-LIKE AVERAGED OVER A PERIOD (NEW)
C       UNIT 70    = MERGED RMA4 PIECES TO MAKE A WHOLE      (NEW/OLD)
C       UNIT 71-79 = INPUT RMA4 FILES (PIECES)               (OLD)
C       UNIT 80    = RMA4 LOOK-A-LIKE AVERAGED OVER A PERIOD (NEW)
C       INIT 91-93 = INPUT RMA10 FILES (PIECES)              (OLD)
C       INIT 95    = MERGED RMA10 PIECES TO MAKE A WHOLE     (NEW/OLD)
C       UNIT 90    = RMA10 LOOK-A-LIKE AVERAGED OVER A PERIOD(NEW)
C
C ... VARIABLE DESCRIPTION
C          BE      DEPTH TAKEN FROM GFGEN GEOMETRY FILE
C          CORD    X AND Y COORDINATES FROM GFGEN GEOMETRY FILE
C          VEL     ARRAY CONTAINING X-VELOCITY,Y-VELOCITY AND DEPTHS
C                    TAKEN FROM RMA2 FILE
C          TIME    MODEL DELTA TIME BETWEEN TIME STEPS
C          NP      NUMBER OF GRID POINTS USED
C          WSELV   GRID DEPTH + RMA2 DEPTH AT THE SAME LOCATION
C          WSMLW   WSELV - MEAN LOW WATER REFERENCE
C
      PARAMETER(MME=25000,MMN=70000,NTS=3000)
C
      INTEGER TRMLEN
      LOGICAL HERE
      CHARACTER*80   DMSBAN
      CHARACTER*10   FLAG
      CHARACTER*50   IGEO, IFILE1, IFILE2, NAMEF, IFILEM
      CHARACTER*77   TITLEGF, TITLER10, TITLER2, TITLER4
C
      COMMON /DMS /  DMSBAN(15),
     *               IREC(40),FREC(40),TITLER2,TITLER4,
     *               TITLEGF, TITLER10, FLAG, IFILEM,
     *               IPACKT(77), IPACKB(1200)
      COMMON /BLKR2/  
     *               VEL(3,MMN),NDRY(MMN),IMAT(MME),VMAG(MMN),
     *               WSEL(MMN), WIDTH(MMN),SS1(MMN),SS2(MMN),
     *               WIDS(MMN), CORD(MMN,2),ALPHA(MMN),BE(MMN),
     *               NOP(MME,20),TH(MME),NFIXH(MME),
     *               NEG,NPG,NE2,NP2
C
      COMMON /BLKR4/ TIMER4, NQAL, NPR4, CON(MMN,6), UL(MMN), VL(MMN)
C
C GLB-R10 change VELR10 dimension from 4 to 6
      COMMON /BLKR10/ TIMER10, NP10, NE10, NDF, VELR10(6,MMN),
     *               VERTVEL(MMN),NDEP(MMN),NREF(MMN),
     *               NOPR10(MME,20),CORDR10(MMN,3),SPEC(MMN,3),
     *               ALFA(MMN),NFIX(MMN),AO(MMN),NSURF(MMN),
     *               NCORN(MME),IMATR10(MME),
     *               THR10(MME),WIDTHR10(MMN)
C
      HERE = .FALSE.
C
      DO 1 I= 1, MMN
         VEL(1,I)  = 0.
         VEL(2,I)  = 0.
         VEL(3,I)  = 0.
         WSEL(I)   = 0.
         NDRY(I)   = 0
    1 CONTINUE         
      IFILEM = 'NULL'
C
      PRINT *,' ********************************************'
      PRINT *,' ******* PROGRAM MERGE AND/OR AVERAGE *******'
      PRINT *,' *** Manipulates GFGEN, RMA2, RMA4, RMA10 ***'
      PRINT *,' ********************************************'  
      PRINT 8, MME, MMN
    8 FORMAT(' Dimensioned for ',I6,' Elements, and ',I6,' Nodes',/,
     *       ' Program MERGAVE last modified 11-08-1993')
      PRINT *,' ********************************************'
      PRINT *,' Enter Printed Output Summary File Name '
      READ(5,300) IFILE1
      LASTC = TRMLEN( IFILE1 )
      OPEN( 3, FILE=IFILE1,FORM='FORMATTED',STATUS='NEW' )
C
C
C ... MERGE TOGETHER RMA2 FILES, FINAL RESULT ON UNIT IHYDRO=50 
      PRINT *,' Do you need to merge together RMA-2 files? '
      READ(5,300) FLAG
      IF( FLAG(1:1).EQ.'N' .OR. FLAG(1:1).EQ.'n') GO TO 10
      CALL R2MERG
C
C ... MERGE TOGETHER RMA4 FILES, FINAL RESULT ON UNIT NSAL=70
  10  PRINT *,' Do you need to merge together RMA-4 files? '
      READ(5,300) FLAG
      IF( FLAG(1:1).EQ.'N' .OR. FLAG(1:1).EQ.'n') GO TO 20
      CALL R4MERG
C
C ... MERGE TOGETHER RMA10 FILES, FINAL RESULT ON UNIT NRMA10=90
   20 PRINT *,' Do you need to merge together RMA-10 files? '
      READ(5,300) FLAG
      IF( FLAG(1:1).EQ.'N' .OR. FLAG(1:1).EQ.'n') GO TO 30
      PRINT *,' I HOPE you know this will be lots of I/O'
      CALL R10MERG
C
   30 PRINT *,' Do you want to read GFGEN binary file'
      READ(5,40,END=10) FLAG 
   40 FORMAT(A)
      IF (FLAG(1:1).EQ.'N' .OR. FLAG(1:1).EQ.'n' ) GO TO 250
   45    PRINT *,' Enter GFGEN Binary file name'
         READ(5,300) IGEO
         LASTC = TRMLEN( IGEO )
         INQUIRE (FILE=IGEO, EXIST=HERE)
         IF (HERE) THEN
             OPEN (40,FILE=IGEO, FORM='UNFORMATTED',STATUS='OLD')
         ELSE
             PRINT *,' Sorry ... no such GFGEN file name ... try again'
             GO TO 45
         ENDIF
         OPEN (40,FILE=IGEO,FORM='UNFORMATTED',STATUS='OLD')
         PRINT *,' ENTER DATUM (Make it 0.0 for WSELEV array to be OK)'
         READ(5,*)         DATUM
C ---
C-       Read GFGEN geometry file              
         REWIND (40)
         READ (40, END=2000, ERR=2100) ITESTG
         REWIND (40)
         IF (ITESTG .GT. 200) THEN
C            true character banner read
             READ (40, END=2000, ERR=2100) (DMSBAN(I),I = 1,15)
             READ (40) (IREC(I), I = 1, 40), (FREC(I), I = 1, 40)
             IF (IREC(1) .GE. 425) READ (40) TITLEGF
         ELSE 
C            integer representation of charactered variables
             READ (40, END=2000, ERR=2100) MFLGEO, IVERID, NPG, NEG
             READ (40) IWRT1, (IPACKB(I), I = 1, IWRT1)
             READ (40) IWRT2, IWRT3, (IREC(I), I = 1, IWRT2), 
     &                 (FREC(I), I = 1, IWRT3)
             READ (40) IWRT4, (IPACKT(I), I = 1, IWRT4)
C-           convert from integer to character data
             CALL CONVRT ( DMSBAN, 15, IPACKB, 80, 2)
             CALL CONVRT ( TITLEGF, 1, IPACKT, 77, 2)
         END IF 
         READ (40) NPG, NEG, 
     *            ((CORD(J,K),K=1,2), ALFA(J), BE(J), J =1,NPG),
     *            ((NOP(J,K),K=1,8), IMAT(J), TH(J), NFIXH(J),J =1,NEG)
         IF(IREC(1).GT.420) 
     *            READ(40) (WIDTH(J),SS1(J),SS2(J),WIDS(J),J=1,NPG)
         WRITE (*,*) ' --> GFGEN geometry file read AOK'
         WRITE (*,'(/,'' GFGEN='',A77,/,6X,2I8)') TITLEGF, NPG, NEG
         CLOSE(40)
C ---
   50 ISKIP = 1
C
      IF(NE2.NE.NEG .OR. NP2.NE.NPG) WRITE(6,200) NEG,NPG,NE2,NP2
  200 FORMAT(' WARNING inconsistant node/elem... NE-NP-NE2-NP2=',4I7,/)
C
C ... INTEGRATE VELOCITY BETWEEN DESIGNATED HOURS
  250 PRINT *,' Do you want to average (INTEGRATE) the RMA-2 data?'
      READ(5,300) FLAG
      IF( FLAG(1:1) .EQ. 'N' .OR. FLAG(1:1).EQ.'n') GO TO 400
  260    PRINT *,' Enter RMA2 integrated file name to be created'
         READ(5,300,END=250) IFILE2
         LASTC = TRMLEN( IFILE2 )
         INQUIRE (FILE=IFILE2, EXIST=HERE)
         IF (HERE) THEN
             PRINT *,' That file name already exists   OverWrite? '
             FLAG= 'N'
             READ(5,'(A)')  FLAG
             IF (FLAG(1:1).EQ.'N' .OR. FLAG(1:1).EQ.'n') FLAG(1:1) = 'N'
             IF( FLAG(1:1).EQ.'N') GO TO 260
             OPEN (60,FILE=IFILE2,FORM='UNFORMATTED',STATUS='OLD')
         ELSE
             OPEN (60,FILE=IFILE2,FORM='UNFORMATTED',STATUS='NEW')
         ENDIF
C
  300    FORMAT(A)
  310    PRINT *,' Enter name of RMA2 file to read from' 
         READ(5,40,END=310) IFILEM
         LASTC = TRMLEN( IFILEM )
         INQUIRE (FILE=IFILEM, EXIST=HERE)
         IHYDRO = 50
         IF (HERE) THEN
             OPEN (IHYDRO,FILE=IFILEM,FORM='UNFORMATTED',STATUS='OLD')
          ELSE
             PRINT *,' Sorry ... no such file name ... try again'
             GO TO 310
          ENDIF
C
         CALL R2INT
         CLOSE (50)
         CLOSE (60)
  400 ISKIP = 1
C -----------------------------------------------------------------
C ... INTEGRATE RMA10 VELOCITY BETWEEN DESIGNATED HOURS
  450 PRINT *,' Do you want to average (INTEGRATE) the RMA-10 data?'
      READ(5,300) FLAG
      IF( FLAG(1:1) .EQ. 'N' .OR. FLAG(1:1).EQ.'n') GO TO 500
  460    PRINT *,' Enter RMA10 integrated file name to be created'
         READ(5,300) IFILE2
         LASTC = TRMLEN( IFILE2 )
         INQUIRE (FILE=IFILE2, EXIST=HERE)
         IF (HERE) THEN
             PRINT *,' That file name already exists   OverWrite? '
             FLAG= 'N'
             READ(5,'(A)')  FLAG
             IF (FLAG(1:1).EQ.'N' .OR. FLAG(1:1).EQ.'n') FLAG(1:1) = 'N'
             IF( FLAG(1:1).EQ.'N') GO TO 460
             OPEN (90,FILE=IFILE2,FORM='UNFORMATTED',STATUS='OLD')
         ELSE
             OPEN (90,FILE=IFILE2,FORM='UNFORMATTED',STATUS='NEW')
         ENDIF
C
  470    PRINT *,' Enter name of RMA10 file to read from' 
         READ(5,40,END=470) IFILEM
         LASTC = TRMLEN( IFILEM )
         INQUIRE (FILE=IFILEM, EXIST=HERE)
         NRMA10 = 95
         IF (HERE) THEN
             OPEN (NRMA10,FILE=IFILEM,FORM='UNFORMATTED',STATUS='OLD')
          ELSE
             PRINT *,' Sorry ... no such file name ... try again'
             GO TO 470
          ENDIF
C
         CALL R10INT
         CLOSE (95)
         CLOSE (90)
  500 ISKIP = 1
C -----------------------------------------------------------------
  650 PRINT *,' Do you want to average (INTEGRATE) the RMA-4 data?'
      READ(5,300) FLAG
      IF( FLAG(1:1) .EQ. 'N' .OR. FLAG(1:1).EQ.'n') GO TO 800
  670    PRINT *,' Enter RMA4 averaged file name to be created'
         READ(5,300) IFILE2
         LASTC = TRMLEN( IFILE2 )
         INQUIRE (FILE=IFILE2, EXIST=HERE)
         IF (HERE) THEN
             PRINT *,' That file name already exists   OverWrite? '
             FLAG= 'N'
             READ(5,'(A)')  FLAG
             IF (FLAG(1:1).EQ.'N' .OR. FLAG(1:1).EQ.'n') FLAG(1:1) ='N'
             IF( FLAG(1:1).EQ.'N') GO TO 670
             OPEN (80,FILE=IFILE2,FORM='UNFORMATTED',STATUS='OLD')
         ELSE
             OPEN (80,FILE=IFILE2,FORM='UNFORMATTED',STATUS='NEW')
         ENDIF
C
  700    PRINT *,' Enter name of RMA10 file to read from' 
         READ(5,40,END=470) IFILEM
         LASTC = TRMLEN( IFILEM )
         INQUIRE (FILE=IFILEM, EXIST=HERE)
         NSAL = 70
         IF (HERE) THEN
             OPEN (NSAL,FILE=IFILEM,FORM='UNFORMATTED',STATUS='OLD')
         ELSE
             PRINT *,' Sorry ... no such file name ... try again'
             GO TO 700
         ENDIF
C
         CALL R4INT
         CLOSE (70)
         CLOSE (80)
  800 ISKIP = 1
C ------------------------------------------------------------------
C
 1000 PRINT *,' ****** Program Finished ******'
      STOP
 2000 PRINT *,' --> Unexpected end-of-file on GFGEN read'
      STOP
 2100 PRINT *,' --> ERROR during read of GFGEN binary file'
      STOP
      END
C
      SUBROUTINE R2MERG
C
C ... MERGE INFORMATION OF UNITS 11-20 ONTO UNIT 50 (IHYDRO)
C
      PARAMETER(MME=25000,MMN=70000,NTS=3000)
C
      INTEGER TRMLEN
      CHARACTER*80   DMSBAN
      CHARACTER*10   FLAG
      CHARACTER*50   IFILE(10), NAMEF, IFILEM
      CHARACTER*77   TITLEGF, TITLER10, TITLER2, TITLER4
C
      COMMON /DMS /  DMSBAN(15),
     *               IREC(40),FREC(40),TITLER2,TITLER4,
     *               TITLEGF, TITLER10, FLAG, IFILEM,
     *               IPACKT(77), IPACKB(1200)
      COMMON /BLKR2/  
     *               VEL(3,MMN),NDRY(MMN),IMAT(MME),VMAG(MMN),
     *               WSEL(MMN), WIDTH(MMN),SS1(MMN),SS2(MMN),
     *               WIDS(MMN), CORD(MMN,2),ALPHA(MMN),BE(MMN),
     *               NOP(MME,20),TH(MME),NFIXH(MME),
     *               NEG,NPG,NE2,NP2
      COMMON /BLKR4/ TIMER4, NQAL, NPR4, CON(MMN,6), UL(MMN), VL(MMN)
C
      DIMENSION TSTART(10), TEND(10)
      LOGICAL HERE
C
      WRITE(3,10)
   10 FORMAT(1H1,/,'  MERGE TOGETHER - MULTIPLE RMA2 FILES')
      IUNIT  =  11
      IHYDRO =  50
      NREC   =   0
      TFIRST = -99.
      TLAST  = -99.
C
      PRINT *,'  PURPOSE -->  MERGE together - MULTIPLE RMA2 files'
      PRINT *,' '
   20 PRINT *,' Enter name of RMA2 MERGED file to be created' 
      READ(5,'(A)',END=2300) IFILEM
      LASTC = TRMLEN( IFILEM )
      INQUIRE (FILE=IFILEM, EXIST=HERE)
      IHYDRO = 50
      IF (HERE) THEN
         PRINT *,' That file name already exists   OverWrite? '
         FLAG= 'N'
         READ(5,'(A)')  FLAG
         IF (FLAG(1:1).EQ.'N' .OR. FLAG(1:1).EQ.'n') FLAG(1:1) = 'N'
         IF( FLAG(1:1).EQ.'N') GO TO 20
         OPEN (IHYDRO,FILE=IFILEM,FORM='UNFORMATTED',STATUS='OLD')
      ELSE
         OPEN (IHYDRO,FILE=IFILEM,FORM='UNFORMATTED',STATUS='NEW')
      ENDIF
C
      DO 1000 I = 1, 10
   25    PRINT *,' Enter flag identifier or (?) for menu'
         READ(5,'(A)',END=25) FLAG
         IF( FLAG(1:1).EQ.'?') THEN
             PRINT 30
   30        FORMAT(' VALID FLAG OPTIONS FOR RMA2 MERGE ARE:',/,
     *              '   To read first file enter     -->  1ST RMA2',/,
     *              '   To read second file enter    -->  2ND RMA2',/,
     *              '   To read third file enter     -->  3RD RMA2',/,
     *              '   To read fourth file enter    -->  4TH RMA2',/,
     *              '   To read fifth file enter     -->  5TH RMA2',/,
     *              '   To leave this routine enter  -->  EXIT    ',/)
             READ(5,'(A)') FLAG
         ENDIF
         IF (FLAG(1:3) .EQ. '1ST' .OR. FLAG(1:3) .EQ. '1st') THEN
   50         PRINT *,' Enter name of 1st RMA2 binary file'
              READ(5,'(A)',END=2300) IFILE(1)
              PRINT *,' Enter starting and ending decimal hour'
              READ(5,*,END=2300)           TSTART(1), TEND(1)
              MERGTOT = 1
              IUNIT = 11
              LASTC = TRMLEN( IFILE(1) )
              INQUIRE (FILE=IFILE(1), EXIST=HERE)
              IF (HERE) THEN
                OPEN (IUNIT,FILE=IFILE(1),
     *                      FORM='UNFORMATTED',STATUS='OLD')
              ELSE
                PRINT *,' Sorry ... no such file name ... try again'
                GO TO 50
              ENDIF
         ELSEIF (FLAG(1:3) .EQ. '2ND' .OR. FLAG(1:3) .EQ. '2nd') THEN
   60         PRINT *,' Enter name of 2ND RMA2 binary file'
              READ(5,'(A)',END=2300) IFILE(2)
              PRINT *,' Enter starting and ending decimal hour'
              READ(5,*,END=2300)           TSTART(2), TEND(2)
              MERGTOT = 2
              IUNIT = 12
              LASTC = TRMLEN( IFILE(2) )
              INQUIRE (FILE=IFILE(2), EXIST=HERE)
              IF (HERE) THEN
                OPEN (IUNIT,FILE=IFILE(2),
     *                      FORM='UNFORMATTED',STATUS='OLD')
              ELSE
                PRINT *,' Sorry ... no such file name ... try again'
                GO TO 60
              ENDIF
         ELSEIF (FLAG(1:3) .EQ. '3RD' .OR. FLAG(1:3) .EQ. '3rd') THEN
   70         PRINT *,' Enter name of 3rd RMA2 binary file'
              READ(5,'(A)',END=2300) IFILE(3)
              PRINT *,' Enter starting and ending decimal hour'
              READ(5,*,END=2300)           TSTART(3), TEND(3)
              MERGTOT = 3
              IUNIT = 13
              LASTC = TRMLEN( IFILE(3) )
              INQUIRE (FILE=IFILE(3), EXIST=HERE)
              IF (HERE) THEN
                OPEN (IUNIT,FILE=IFILE(3),
     *                      FORM='UNFORMATTED',STATUS='OLD')
              ELSE
                PRINT *,' Sorry ... no such file name ... try again'
                GO TO 70
              ENDIF
         ELSEIF (FLAG(1:3) .EQ. '4TH' .OR. FLAG(1:3).EQ. '4th') THEN
   80         PRINT *,' Enter name of 4th RMA2 binary file'
              READ(5,'(A)',END=2300) IFILE(4)
              PRINT *,' Enter starting and ending decimal hour'
              READ(5,*,END=2300)           TSTART(4), TEND(4)
              MERGTOT = 4
              IUNIT = 14
              LASTC = TRMLEN( IFILE(4) )
              INQUIRE (FILE=IFILE(4), EXIST=HERE)
              IF (HERE) THEN
                OPEN (IUNIT,FILE=IFILE(4),
     *                      FORM='UNFORMATTED',STATUS='OLD')
              ELSE
                PRINT *,' Sorry ... no such file name ... try again'
                GO TO 80
              ENDIF
         ELSEIF (FLAG(1:3) .EQ. '5TH' .OR. FLAG(1:3) .EQ. '5th') THEN
   90         PRINT *,' Enter name of 5th RMA2 binary file'
              READ(5,'(A)',END=2300) IFILE(5)
              PRINT *,' Enter starting and ending decimal hour'
              READ(5,*,END=2300)           TSTART(5), TEND(5)
              MERGTOT = 5
              IUNIT = 15
              LASTC = TRMLEN( IFILE(5) )
              INQUIRE (FILE=IFILE(5), EXIST=HERE)
              IF (HERE) THEN
                OPEN (IUNIT,FILE=IFILE(5),
     *                      FORM='UNFORMATTED',STATUS='OLD')
              ELSE
                PRINT *,' Sorry ... no such file name ... try again'
                GO TO 90
              ENDIF
         ELSE
              GO TO 3000
         ENDIF
C
C ...    READ RMA2 BANNERS
C * * * * * * * * * * * * *
C ...    determine type of binary file and read 
         READ (IUNIT,END=2050,ERR=2050) ITEST2
         PRINT *,' -->  1st integer on RMA2 binary=',ITEST2
         REWIND (IUNIT)
         IF (ITEST2.GT.200) THEN
C ...        This must be a character banner style binary file
             READ (IUNIT, END=2050,ERR=2050) (DMSBAN(K),K=1,15)
             READ (IUNIT) (IREC(K),K=1,40), (FREC(K),K=1,40)
             IF (ISWREC.GT.425) READ (IUNIT) TITLER2
             IF( I.EQ.1 ) THEN
                 PRINT *,' --> WRITE CHARACTER BANNERS TO UNIT=50'
                 WRITE(IHYDRO) (DMSBAN(K),K=1,15)
                 WRITE(IHYDRO) (IREC(K),K=1,40), (FREC(K),K=1,40)
                 WRITE(IHYDRO) TITLER2
             ENDIF
         ELSE
             READ (IUNIT, END=2050,ERR=2050) MFLG, ISWREC, NP, NE
             IF (NP .GT. MMN .OR. NE .GT. MME) THEN
                WRITE(*,*)' *** Array dimension over-run problem *** '
                WRITE(*,*)' --> Maximum allowed # of nodes =',MMN
                WRITE(*,*)' --> Your file has ',NP,' nodes'
                WRITE(*,*)' --> Maximum allowed # of elements =',MME
                WRITE(*,*)' --> Your file has ',NE,' elements'
                STOP'DIM'
             ENDIF
             NP2 = NP
             NE2 = NE
C ...        INTEGER STYLE CHARACTER VALUES
             READ (IUNIT) IWRT1,(IPACKB(J),J = 1, IWRT1)
             READ (IUNIT) IWRT2,IWRT3,
     *                    (IREC(J),J=1,IWRT2),(FREC(J),J=1, IWRT3)
             READ (IUNIT) IWRT4, (IPACKT(J),J=1,IWRT4)
             CALL CONVRT ( DMSBAN, 15, IPACKB, 80, 2)
             CALL CONVRT ( TITLER2, 1, IPACKT, 77, 2)
C-
             IF( I.EQ.1 ) THEN
                 PRINT *,' --> WRITE INTEGER/CHAR BANNERS TO UNIT=50'
                 WRITE(IHYDRO) MFLG, ISWREC, NP, NE
                 WRITE(IHYDRO) IWRT1,(IPACKB(J),J = 1, IWRT1)
                 WRITE(IHYDRO) IWRT2,IWRT3,
     *                         (IREC(J),J=1,IWRT2),(FREC(J),J=1, IWRT3)
                 WRITE(IHYDRO) IWRT4, (IPACKT(J),J=1,IWRT4)
             ENDIF
         ENDIF
         WRITE(*,*) ' --> Your RMA2 has a version # of ',ISWREC
C-
C * * * * * * *
C
         WRITE(3,110) IFILE(I), IUNIT, (DMSBAN(J),J=1,15)
         WRITE(3,120) IREC(1), TSTART(I), TEND(I)
  110    FORMAT(' FILE=',A50,' UNIT =',I2,' BANNERS',/,
     *       '  1)',A80,/,4(5X,A80,/),/,
     *       '  2)',A80,/,4(5X,A80,/),/,
     *       '  3)',A80,/,4(5X,A80,/),//)
C
  120    FORMAT(9X,'IREC(1)=',I3,5X,'EXTRACT HRS',F12.6,' TO',F12.6,/)
C
         INSIDE = 0
         DO 900 JJ = 1, NTS
            IF( IREC(1).GE.40 ) THEN
            READ(IUNIT,END= 910) TIMER2,NP2,((VEL(J,K),J=1,3),K=1,NP2),
     *                           (NDRY(K),K=1,NP2),
     *                           NE2,(IMAT(K),K=1,NE2),
     *                           (WSEL(K),K=1,NP2)
            ELSE
            READ(IUNIT,END= 910) TIMER2,NP2,((VEL(J,K),J=1,3),K=1,NP2),
     *                           (NDRY(K),K=1,NP2),
     *                           NE2,(IMAT(K),K=1,NE2)
            ENDIF
            IF (JJ.EQ.1) T1ST = TIMER2
            IF (JJ.EQ.2) T2ND = TIMER2
C
            IF( TIMER2.GE.TSTART(I) .AND. TIMER2.LE.TEND(I) ) THEN
                INSIDE = INSIDE + 1
                IF(IREC(1).LT.40)  THEN
                   WRITE(IHYDRO) TIMER2,NP2,((VEL(J,K),J=1,3),K=1,NP2),
     *                           (NDRY(K),K=1,NP2),
     *                           NE2,(IMAT(K),K=1,NE2)
                ELSE
                   WRITE(IHYDRO) TIMER2,NP2,((VEL(J,K),J=1,3),K=1,NP2),
     *                           (NDRY(K),K=1,NP2),
     *                           NE2,(IMAT(K),K=1,NE2),
     *                           (WSEL(K),K=1,NP2)
                ENDIF
                IF( INSIDE.EQ.1 ) TFIRST = TIMER2
                NREC = NREC + 1
                TLAST = TIMER2
            ENDIF
  900   CONTINUE
  910   IUNIT = IUNIT + 1
        REWIND IUNIT
        TDELT = T2ND - T1ST
 1000 CONTINUE
C 
 2000 WRITE(3,2010) IFILEM,
     *              (IFILE(JJ),TSTART(JJ),TEND(JJ),JJ=1,MERGTOT)
 2010 FORMAT(/,' THE MERGED FILE IS NAMED=',A50,
     *       ' AND THESE DATA FILES AND TIMES WERE USED TO CREATE IT',/,
     *         3X,10(A50,5X,2F12.6),//)
      WRITE(3,2030) NREC, TFIRST, TLAST
 2030 FORMAT(/,I5,' TIME STEPS MERGED TO UNIT 50 ... FIRST=',F12.6,/,
     *                                39X,'LAST=',F12.6,/)
      GO TO 3000
 2050 WRITE(3,2060) IUNIT
 2060 FORMAT(' END OF FILE WHILE READING UNIT =',I2,' SUB R2MERGE')
      GO TO 3000
 2100 WRITE(3,2200) IUNIT
 2200 FORMAT(' ERROR WHILE READING UNIT =',I2,' SUBROUTINE R2MERGE')
      GO TO 3000
 2300 WRITE(3,2310)
 2310 FORMAT(' END OF FILE UNIT  5 INPUT')
 3000 DO 3050 II = 11, IUNIT
         CLOSE (II)
 3050 CONTINUE
      PRINT *,' ========= Closing active files from R2MERGE ========='
      CLOSE(IHYDRO)
      PRINT 3060, IFILEM, INSIDE
 3060 FORMAT('  ==== MERGED RMA2 file named -->',A,/,
     *       '  ==== Closed with',I6,' time steps',/)
      RETURN
      END
C --------
      SUBROUTINE R2INT 
C
      PARAMETER(MME=25000,MMN=70000,NTS=3000)
C
      INTEGER TRMLEN
      CHARACTER*80   DMSBAN
      CHARACTER*10   FLAG
      CHARACTER*50   IGEO, IFILE1, IFILE2, NAMEF, IFILEM
      CHARACTER*77   TITLEGF, TITLER10, TITLER2, TITLER4
C
      COMMON /DMS /  DMSBAN(15),
     *               IREC(40),FREC(40),TITLER2,TITLER4,
     *               TITLEGF, TITLER10, FLAG, IFILEM,
     *               IPACKT(77), IPACKB(1200)
      COMMON /BLKR2/  
     *               VEL(3,MMN),NDRY(MMN),IMAT(MME),VMAG(MMN),
     *               WSEL(MMN), WIDTH(MMN),SS1(MMN),SS2(MMN),
     *               WIDS(MMN), CORD(MMN,2),ALPHA(MMN),BE(MMN),
     *               NOP(MME,20),TH(MME),NFIXH(MME),
     *               NEG,NPG,NE2,NP2
      COMMON /BLKR4/ TIMER4, NQAL, NPR4, CON(MMN,6), UL(MMN), VL(MMN)
C
C
      DIMENSION      IMATI(MME),WSELV(MMN), WSMLW(MMN), DSUM(MMN),
     *               VXMAX(MMN),VYMAX(MMN),VXMIN(MMN),VYMIN(MMN),
     *               WSMAX(MMN),WSMIN(MMN),VXSUM(MMN),VYSUM(MMN),
     *               WESUM(MMN),VXAVG(MMN),VYAVG(MMN), WEAVG(MMN),
     *               NDRYI(MMN), TR2(NTS)
C
C ... INITIALIZE MINIMUM AND MAXIMUM COUNTERS
C
      DATA VXMAX/MMN*-1.0E+20/, VYMAX/MMN*-1.0E+20/, WSMAX/MMN*-1.0E+20/
     *     VXMIN/MMN* 1.0E+20/, VYMIN/MMN* 1.0E+20/, WSMIN/MMN* 1.0E+20/
      DATA VXSUM/MMN* 0./, VYSUM/MMN* 0./, WESUM/MMN* 0./
      DATA IMATI/MME*0/, NDRYI/MMN*2/
      DATA IDOFLUX /0/
C
C
      PRINT *,' ************** HYDRODYNAMIC AVERAGE ***************'
      T2START = -9999
      PRINT *,' Enter the RMA2 decimal time to start in HOURS'
      PRINT *,'                Default=last time minus 25 TIME STEPS'
      CALL DCODE (T2START,T2START)
      T2STOP  = -9999
      PRINT *,' Enter the RMA2 decimal time to stop in HOURS'
      PRINT *,'                Default=last time'
      CALL DCODE (T2STOP,T2STOP)
      DATUM = 100
      PRINT *,' Enter the datum reference in feet'
      PRINT *,'                Default=',DATUM
      CALL DCODE (DATUM,DATUM)
      IDEBUG = 4
      PRINT *,' DO you want to turn full prints on? (Enter 1 of yes)'
      READ(5,*,END=10) ITEST
      IDEBUG = ITEST
   10 FLAG = 'N'
      PRINT *,' Do you want to weight the average current by depth?'
      PRINT *,'     (ie. suitable for FLUX computation  DEFAULT=NO)'
      READ(5,15,END=20) FLAG
   15 FORMAT(A)
   20 IF(FLAG(1:1).EQ.'Y' .OR. FLAG(1:1).EQ.'y') THEN
         IDOFLUX = 1
         FLAG(1:1) = 'Y'
      ENDIF
C
C  READ RMA2-TYPE BINARY DATA AND BANNERS
C
      IRECN = 0 
      IHYDRO = 50
      REWIND IHYDRO
      PRINT *,' INSIDE R2INT, rewinding unit=50 to read RMA2 binary'
C * * * * * * * * * * * * *
C ... determine type of binary file and read 
      READ (IHYDRO,END=50,ERR=50) ITEST2
      PRINT *,' -->  1st integer on RMA2 binary=',ITEST2
      REWIND (IHYDRO)
      IF (ITEST2.GT.200) THEN
C ...     This must be a character banner style binary file
          READ (IHYDRO, END=50,ERR=50) (DMSBAN(K),K=1,15)
          IRECN = 1
          READ (IHYDRO) (IREC(K),K=1,40), (FREC(K),K=1,40)
          IRECN = 2
          IF (ISWREC.GT.425) READ (IHYDRO) TITLER2
          IRECN = 3
          IF( I.EQ.1 ) THEN
              PRINT *,' --> WRITE CHARACTER BANNERS TO UNIT=50'
              WRITE(60) (DMSBAN(K),K=1,15)
              WRITE(60) (IREC(K),K=1,40), (FREC(K),K=1,40)
              WRITE(60) TITLER2
          ENDIF
      ELSE
          READ (IHYDRO, END=50,ERR=50) MFLG, ISWREC, NP, NE
          IRECN = 1
          IF (NP .GT. MMN .OR. NE .GT. MME) THEN
             WRITE(*,*)' *** Array dimension over-run problem *** '
             WRITE(*,*)' --> Maximum allowed # of nodes =',MMN
             WRITE(*,*)' --> Your file has ',NP,' nodes'
             WRITE(*,*)' --> Maximum allowed # of elements =',MME
             WRITE(*,*)' --> Your file has ',NE,' elements'
             STOP'DIM'
          ENDIF
          NP2 = NP
          NE2 = NE
C ...     Integer style character values
          READ (IHYDRO) IWRT1,(IPACKB(J),J = 1, IWRT1)
          IRECN = 2
          READ (IHYDRO) IWRT2,IWRT3,
     *                 (IREC(J),J=1,IWRT2),(FREC(J),J=1, IWRT3)
          IRECN = 3
          READ (IHYDRO) IWRT4, (IPACKT(J),J=1,IWRT4)
          IRECN = 4
          CALL CONVRT ( DMSBAN, 15, IPACKB, 80, 2)
          CALL CONVRT ( TITLER2, 1, IPACKT, 77, 2)
C-
          IF( I.EQ.1 ) THEN
              PRINT *,' --> WRITE INTEGER/CHAR BANNERS TO UNIT=50'
              WRITE(60) MFLG, ISWREC, NP, NE
              WRITE(60) IWRT1,(IPACKB(J),J = 1, IWRT1)
              WRITE(60) IWRT2,IWRT3,
     *                      (IREC(J),J=1,IWRT2),(FREC(J),J=1, IWRT3)
              WRITE(60) IWRT4, (IPACKT(J),J=1,IWRT4)
          ENDIF
      ENDIF
      WRITE(*,*) ' --> Your RMA2 has a version # of ',ISWREC
C-
C * * * * * * *
      IF( IDEBUG.NE.0 ) PRINT *,' Just read RMA-2 Banners successfully'
      GO TO 75
   50 PRINT *,' UNEXPECT E-O-F HIT during Banner read of RMA2 binary'
      PRINT *,'          LAST successful record number read=',IRECN
      STOP    
C
C ... READ DYNAMIC TIME ON RMA2, NEED HOURS BETWEEN T2START & T2STOP
C
   75 IF ( T2START.GT. -99. ) GO TO 250
      IRECN = 1
      DO 100 I = 1, NTS
         READ(IHYDRO,END=200) TR2(IRECN)
         IRECN = IRECN + 1
  100 CONTINUE
      PRINT *,' CAUTION - NO END OF FILE HIT AFTER ',NTS,' TIME STEPS'
      STOP
  200 IRECN = IRECN - 1
      PRINT 210, TR2(IRECN), IRECN
  210 FORMAT(' LAST TIME ON BINARY FILE=',F12.6,/,
     *       ' TOTAL DATA RECORD COUNT [includes all banners]=',
     *       I8,/)
      REWIND IHYDRO
      PRINT *,' Required to CLOSE/RE-OPEN file to avoid CRAY EOF flag'
      CLOSE (IHYDRO)
      OPEN (IHYDRO,FILE=IFILEM,FORM='UNFORMATTED',STATUS='OLD')
      IF (ITEST2.GT.200) THEN
          ISKB = 3
      ELSE
          ISKB = 4
      ENDIF
      DO 220 I = 1, ISKB
C ...    skip over banner/irec/title
         READ(IHYDRO) 
  220 CONTINUE
      T2STOP  = TR2(IRECN)
      T2START = TR2(IRECN-25)            
      TDELT   = TR2(IRECN) - TR2(IRECN-1)
      PRINT 230, T2START, T2STOP
  230 FORMAT(' ROUTINE self computes time to start and stop as=',2F12.8)
C ----------------------------------------------------------------------
C-
  250 RECN  = 0.
      IRECU = 0
      DO 3000 I = 1, NTS
         IF( IREC(1).GE.42 ) THEN
             READ(IHYDRO,END=4000) TIMER2,NP2,
     *                             ((VEL(J,K),J=1,3),K=1,NP2),
     *                             (NDRY(K),K=1,NP2),
     *                             NE2,(IMAT(K),K=1,NE2),
     *                             (WSELV(K),K=1,NP2)
         ELSE
             READ(IHYDRO,END=4000) TIMER2,NP2,
     *                             ((VEL(J,K),J=1,3),K=1,NP2),
     *                             (NDRY(K),K=1,NP2),
     *                             NE2,(IMAT(K),K=1,NE2)
         ENDIF
         IF ( NP2.GT.MMN .OR. NE2.GT.MME ) THEN
              PRINT *,' ========== ARRAY OVER-RUN OCCURING ============'
              PRINT *,' ==== JUST read RMA2 with=',NE,NP,' Elem-Nodes'
              PRINT *,' ==== Code dimensions are=',MMN,MMN,'  FIX IT!'
              PRINT *,' ========== Array over-Run STOP Now ============'
              STOP
         ENDIF
C 
C ...    IF NDRY = 1 NODE IS WET   IF -1  NODE IS REWETTING   IF 2=DRY
C
         IF( IDEBUG.GT.0 )
     *   PRINT 2, TIMER2, WSELV(1), WSELV(10), WSELV(2000),
     *                              VEL(3,1), VEL(3,10), VEL(3,2000)
    2    FORMAT(' * Debug WSELV  Time=',F6.2,' Nodes 1-10-2000=',3F12.6,
     *          '   Now DEPTH=',3F12.6)
         IF(TIMER2.LT.T2START .OR. TIMER2.GT.T2STOP) GO TO 2500
C
C ...    KEEP AN ELEMENT ACTIVE IF IT HAS BEEN WET ANYTIME
C ...    DURING THE PERIOD OF THE INTEGRATION
C
         DO 1000 II = 1, NE
            IF(IMAT(II).LE.0) GO TO 1000
            IMATI(II) = IMAT(II)
 1000    CONTINUE
C
         DO 1100 II = 1, NP
            IF(NDRY(II).EQ.2) GO TO 1100
            NDRYI(II) = NDRY(II)
 1100    CONTINUE
C
         RECN = RECN + 1.
         IRECU = IRECU + 1
         TR2(IRECU) = TIMER2
C
         DO 2000 J=1,NP
           IJK = J
           IF( IREC(1).LE. 41) WSELV(J) = BE(J) + VEL(3,J)
           WSMLW(J) = WSELV(J)  - DATUM
C  ***     COMPUTE MAGNITUDE
           VMAG(J) = SQRT(VEL(1,J)**2 + VEL(2,J)**2)
C ***      SUM FOR FUTURE AVERAGING
           IF( IDOFLUX.EQ.0) THEN
C ...          TAKE A STRAIGHT AVERAGE CURRENT
               VXSUM(J) = VXSUM(J) + VEL(1,J)
               VYSUM(J) = VYSUM(J) + VEL(2,J)
           ELSE
C ...          TAKE WEIGHTED BY DEPTH AVERAGE CURRENT
               VXSUM(J) = VXSUM(J) + VEL(1,J)*VEL(3,J)
               VYSUM(J) = VYSUM(J) + VEL(2,J)*VEL(3,J)
           ENDIF
           WESUM(J) = WESUM(J) + WSMLW(J)
           DSUM(J)  = DSUM(J) + VEL(3,J)
C ***      FIND MINIMUM AND MAXIMUM FOR EACH NODE OVER THE CYCLE
           IF(VEL(1,J).GT. VXMAX(J)) VXMAX(J) = VEL(1,J)
           IF(VEL(1,J).LT. VXMIN(J)) VXMIN(J) = VEL(1,J)
           IF(VEL(2,J).GT. VYMAX(J)) VYMAX(J) = VEL(2,J)
           IF(VEL(2,J).LT. VYMIN(J)) VYMIN(J) = VEL(2,J)
           IF(WSMLW(J).GT. WSMAX(J)) WSMAX(J) = WSMLW(J)
           IF(WSMLW(J).LT. WSMIN(J)) WSMIN(J) = WSMLW(J)
 2000    CONTINUE
         GO TO 3000
 2500    IF( IDEBUG.GT.0 )   WRITE(3,2510) TIMER2
 2510    FORMAT(' Skipping RMA2 Time=',F15.7)
 3000 CONTINUE
C
      IF( IRECU.LE.1 ) THEN
            PRINT 3050,   RECN, T2START, T2STOP
            WRITE(3,3050) RECN, T2START, T2STOP
 3050       FORMAT(' WARNING --- RECN=',F5.1,/,
     *      ' Cannot find RMA2 unit IHYDRO time between =',2F12.6,/)
            STOP
      ENDIF
C
C ... TAKE AVERAGES, THEN SCALE, THEN WRITE TO OUTPUT FILE
C
 4000 TCYCLE = T2STOP - T2START
      WRITE(3,4100) IDOFLUX, TCYCLE, T2START, T2STOP
 4100 FORMAT(1H1,/,2X,
     *' Switch setting for weighting average current by depth was=',I3,
     *  //,10X,'AVERAGE OF EACH NODE OVER A',F12.6,
     *' HR CYCLE, WITH START/STOP TIMES=',2F12.6,/,5X,
     *' NODE  NDRY  XVEL  X-MIN  X-MAX',4X,'YVEL  Y-MIN  Y-MAX',
     *       '   WSE  WE-MIN  WE-MAX',4X,'XCORD     YCORD  DEPTH'
     *      ,/, 18X,'AVG',20X,'AVG',17X,'AVG',20X,' AVG',/)
      IZNODES = 0
      DO 5000 J = 1, NP
         IWORRY = 0
         DSUM(J)  = DSUM(J)/RECN
         IF(ABS(DSUM(J)-.001).LE.0.01) THEN
            IWORRY = 1
            IF(ABS(VEL(1,J)-.001).LE. 0.01) IWORRY = IWORRY + 1
            IF(ABS(VEL(2,J)-.001).LE. 0.01) IWORRY = IWORRY + 1
            IF(ABS(VEL(3,J)-.001).LE. 0.01) IWORRY = IWORRY + 1
            IF(ABS(WSELV(J)-.001).LE. 0.01) IWORRY = IWORRY + 1
            IF(NDRY(J).EQ. 0)               IWORRY = IWORRY + 1
            IF (IWORRY.GT.3) THEN  
                PRINT 4110, IWORRY, J
 4110           FORMAT(' *** Warning',I3,
     *             ' ZERO Values detected for Node=',I6,' SKIP IT!')
                IZNODES = IZNODES + 1
                GO TO 4120
            ENDIF
         ENDIF
         IF( IDOFLUX.EQ.0 ) THEN
             VXAVG(J) = VXSUM(J)/RECN
             VYAVG(J) = VYSUM(J)/RECN
         ELSE
             VXAVG(J) = VXSUM(J)/RECN/DSUM(J)
             VYAVG(J) = VYSUM(J)/RECN/DSUM(J)
         ENDIF
         WEAVG(J) = WESUM(J)/RECN   
 4120    WRITE(3,4200) J,NDRYI(J),VXAVG(J),VXMIN(J),VXMAX(J),
     *                 VYAVG(J),VYMIN(J),VYMAX(J),
     *                 WEAVG(J), WSMIN(J),WSMAX(J),
     *                 CORD(J,1),CORD(J,2),DSUM(J)
 4200    FORMAT( 5X, 2I5, 9F7.2, 2F11.2,F8.2)
 5000 CONTINUE
C
C ... WRITE INTEGRATED INFORMATION TO UNIT 60
C
        PRINT *,' --> Writing integrated data to unit=60'
        IF (ITEST2.GT.200) THEN
C ...       Write character banners
            WRITE(60) (DMSBAN(I),I=1,15)
            IREC(1) = 42
            WRITE(60) (IREC(J),J=1,40),(FREC(J),J=1,40)
            WRITE(60) TITLER2
        ELSE
            WRITE(60) MFLG, ISWREC, NP2, NE2
            WRITE(60) IWRT1,(IPACKB(J),J = 1, IWRT1)
            WRITE(60) IWRT2,IWRT3,
     *                (IREC(J),J=1,IWRT2),(FREC(J),J=1, IWRT3)
            WRITE(60) IWRT4, (IPACKT(J),J=1,IWRT4)
        ENDIF
        TIME = -99.
        IF( IREC(1).GE.42 ) THEN
            WRITE(60) TIME,NP2,
     *                (VXAVG(J),VYAVG(J),DSUM(J),J=1,NP2),
     *                (NDRYI(K),K=1,NP2),
     *                NE2,(IMATI(K),K=1,NE2),
     *                (WEAVG(K),K=1,NP2)
        ELSE
            WRITE(60) TIME,NP2,
     *                (VXAVG(J),VYAVG(J),DSUM(J),J=1,NP2),
     *                (NDRYI(K),K=1,NP2),
     *                NE2,(IMATI(K),K=1,NE2)
        ENDIF
C
      TDELT    = TR2(IRECU) - TR2(IRECU-1)
      TLENGTH  = TR2(IRECU) - TR2(1)
      WRITE(3,6010) T2START, T2STOP, TLENGTH, IRECU,
     *                       TDELT, IZNODES, IDOFLUX
 6010 FORMAT(' COMPLETE-',/,' UNIT 60 CONTAINS 1 TIME STEP OF',
     *' VELOCITY AND DEPTH INTEGRATED FROM HRS',F12.6,' TO ',F12.6,/,
     *' LENGTH OF TIME OVER WHICH THE AVERAGE WAS COMPUTED=',F12.6,/,
     *' TOTAL NUMBER OF TIME STEPS USED FOR THE AVERAGE   =',I8,/,
     *' DELTA TIME STEP OF LAST 2 TIME STEPS USED WAS     =',F12.6,/,
     *' TOTAL NUMBER OF NODES WITH ZERO VALUES            =',I8,/,
     *' SWITCH FOR WEIGHTING THE AVERAGE CURRENT BY DEPTH WAS =',I3,/)
C
      RETURN
      END
C ------
      SUBROUTINE R4MERG
C
C ... MERGE INFORMATION OF UNITS 71-80 ONTO UNIT 70 (NSAL)
C
      PARAMETER(MME=25000,MMN=70000,NTS=3000)
C
      INTEGER TRMLEN
      CHARACTER*80   DMSBAN
      CHARACTER*10   FLAG
      CHARACTER*50   IFILE(10), NAMEF, IFILEm
      CHARACTER*77   TITLEGF, TITLER10, TITLER2, TITLER4
C
      COMMON /DMS /  DMSBAN(15),
     *               IREC(40),FREC(40),TITLER2,TITLER4,
     *               TITLEGF, TITLER10, FLAG, IFILEM,
     *               IPACKT(77), IPACKB(1200)
      COMMON /BLKR2/  
     *               VEL(3,MMN),NDRY(MMN),IMAT(MME),VMAG(MMN),
     *               WSEL(MMN), WIDTH(MMN),SS1(MMN),SS2(MMN),
     *               WIDS(MMN), CORD(MMN,2),ALPHA(MMN),BE(MMN),
     *               NOP(MME,20),TH(MME),NFIXH(MME),
     *               NEG,NPG,NE2,NP2
      COMMON /BLKR4/ TIMER4, NQAL, NPR4, CON(MMN,6), UL(MMN), VL(MMN)
C
      DIMENSION TSTART(10), TEND(10)
      LOGICAL HERE
C
      WRITE(3,10)
   10 FORMAT(1H1,/,'  MERGE TOGETHER - MULTIPLE RMA4 FILES')
      IUNIT =  71
      NSAL  = 70
      NREC  =  0
      TFIRST = -99.
      TLAST  = -99.
C
      PRINT *,'  PURPOSE -->  MERGE together - MULTIPLE RMA4 files'
      PRINT *,' '
   20 PRINT *,' Enter name of RMA4 MERGED file to be created' 
      READ(5,40,END=2300) IFILEM
      LASTC = TRMLEN( IFILEM )
      NSAL = 70
      INQUIRE (FILE=IFILEM, EXIST=HERE)
      IF (HERE) THEN
         PRINT *,' That file name already exists   OverWrite? '
         FLAG= 'N'
         READ(5,'(A)')  FLAG
         IF (FLAG(1:1).EQ.'N' .OR. FLAG(1:1).EQ.'n') FLAG(1:1) = 'N'
         IF( FLAG(1:1).EQ.'N') GO TO 20
         OPEN (NSAL,FILE=IFILEM,FORM='UNFORMATTED',STATUS='OLD')
      ELSE
         OPEN (NSAL,FILE=IFILEM,FORM='UNFORMATTED',STATUS='NEW')
      ENDIF
C
      DO 1000 I = 1, 10
         PRINT *,' ENTER FLAG IDENTIFIER OR (?) FOR MENU'
         READ(5,40) FLAG
         IF( FLAG(1:1).EQ.'?') THEN
             PRINT 30
   30        FORMAT(' VALID FLAG OPTIONS FOR RMA4 MERGE ARE:',/,
     *              '   To read first file enter     -->  1ST RMA4',/,
     *              '   To read second file enter    -->  2ND RMA4',/,
     *              '   To read third file enter     -->  3RD RMA4',/,
     *              '   To read fourth file enter    -->  4TH RMA4',/,
     *              '   To read fifth file enter     -->  5TH RMA4',/,
     *              '   To put above pieces together -->  MERGE EM',/,
     *              '   To leave this routine enter  -->  EXIT    ',/)
             READ(5,40) FLAG
         ENDIF
   40    FORMAT(A)
         IF (FLAG(1:3) .EQ. '1ST' .OR. FLAG(1:3) .EQ. '1st') THEN
   50         PRINT *,' Enter name of 1st RMA4 binary file'
              READ(5,40,END=2300) IFILE(1)
              PRINT *,' Enter starting and ending decimal hour'
              READ(5,*,END=2300)           TSTART(1), TEND(1)
              MERGTOT = 1
              IUNIT = 71
              LASTC = TRMLEN( IFILE(1) )
              INQUIRE (FILE=IFILE(1), EXIST=HERE)
              IF (HERE) THEN
                OPEN (IUNIT,FILE=IFILE(1),
     *                      FORM='UNFORMATTED',STATUS='OLD')
              ELSE
                PRINT *,' Sorry ... no such file name ... try again'
                GO TO 50
              ENDIF
            ELSEIF (FLAG(1:3) .EQ. '2ND' .OR. FLAG(1:3) .EQ. '2nd') THEN
   60         PRINT *,' Enter name of 2ND RMA4 binary file'
              READ(5,40,END=2300) IFILE(2)
              PRINT *,' Enter starting and ending decimal hour'
              READ(5,*,END=2300)           TSTART(2), TEND(2)
              MERGTOT = 2
              IUNIT = 72
              LASTC = TRMLEN( IFILE(2) )
              INQUIRE (FILE=IFILE(2), EXIST=HERE)
              IF (HERE) THEN
                OPEN (IUNIT,FILE=IFILE(2),
     *                      FORM='UNFORMATTED',STATUS='OLD')
              ELSE
                PRINT *,' Sorry ... no such file name ... try again'
                GO TO 60
              ENDIF
            ELSEIF (FLAG(1:3) .EQ. '3RD' .OR. FLAG(1:3) .EQ. '3rd') THEN
   70         PRINT *,' Enter name of 3rd RMA4 binary file'
              READ(5,40,END=2300) IFILE(3)
              PRINT *,' Enter starting and ending decimal hour'
              READ(5,*,END=2300)           TSTART(3), TEND(3)
              MERGTOT = 3
              IUNIT = 73
              LASTC = TRMLEN( IFILE(3) )
              INQUIRE (FILE=IFILE(3), EXIST=HERE)
              IF (HERE) THEN
                OPEN (IUNIT,FILE=IFILE(3),
     *                      FORM='UNFORMATTED',STATUS='OLD')
              ELSE
                PRINT *,' Sorry ... no such file name ... try again'
                GO TO 70
              ENDIF
            ELSEIF (FLAG(1:3) .EQ. '4TH' .OR. FLAG(1:3).EQ. '4th') THEN
   80         PRINT *,' Enter name of 4th RMA4 binary file'
              READ(5,40,END=2300) IFILE(4)
              PRINT *,' Enter starting and ending decimal hour'
              READ(5,*,END=2300)           TSTART(4), TEND(4)
              MERGTOT = 4
              IUNIT = 74
              LASTC = TRMLEN( IFILE(4) )
              INQUIRE (FILE=IFILE(4), EXIST=HERE)
              IF (HERE) THEN
                OPEN (IUNIT,FILE=IFILE(4),
     *                      FORM='UNFORMATTED',STATUS='OLD')
              ELSE
                PRINT *,' Sorry ... no such file name ... try again'
                GO TO 80
              ENDIF
            ELSEIF (FLAG(1:3) .EQ. '5TH' .OR. FLAG(1:3) .EQ. '5th') THEN
   90         PRINT *,' Enter name of 5th RMA4 binary file'
              READ(5,40,END=2300) IFILE(5)
              PRINT *,' Enter starting and ending decimal hour'
              READ(5,*,END=2300)           TSTART(5), TEND(5)
              MERGTOT = 5
              IUNIT = 75
              LASTC = TRMLEN( IFILE(5) )
              INQUIRE (FILE=IFILE(5), EXIST=HERE)
              IF (HERE) THEN
                OPEN (IUNIT,FILE=IFILE(5),
     *                      FORM='UNFORMATTED',STATUS='OLD')
              ELSE
                PRINT *,' Sorry ... no such file name ... try again'
                GO TO 90
              ENDIF
            ELSE
              GO TO 3000
         ENDIF
C
C ...    determine type of binary file and read 
         READ (IUNIT,END=2050,ERR=2050) ITEST4
         PRINT *,' -->  1st integer on RMA2 binary=',ITEST4
         REWIND (IUNIT)
         IF (ITEST4.GT.200) THEN
C ...        This must be a character banner style binary file
             READ (IUNIT, END=2050,ERR=2050) (DMSBAN(K),K=1,15)
             READ (IUNIT) (IREC(K),K=1,40), (FREC(K),K=1,40)
             IF (ISWREC.GT.425) READ (IUNIT) TITLER4
             IF( I.EQ.1 ) THEN
                 PRINT *,' --> WRITE CHARACTER BANNERS TO UNIT=70'
                 WRITE(NSAL) (DMSBAN(K),K=1,15)
                 WRITE(NSAL) (IREC(K),K=1,40), (FREC(K),K=1,40)
                 WRITE(NSAL) TITLER4
             ENDIF
         ELSE
             READ (IUNIT, END=2050,ERR=2050) MFLG, ISWREC, NPR4, NER4
             IF (NPR4 .GT. MMN .OR. NER4 .GT. MME) THEN
                WRITE(*,*)' *** Array dimension over-run problem *** '
                WRITE(*,*)' --> Maximum allowed # of nodes =',MMN
                WRITE(*,*)' --> Your file has ',NPR4,' nodes'
                WRITE(*,*)' --> Maximum allowed # of elements =',MME
                WRITE(*,*)' --> Your file has ',NER4,' elements'
                STOP'DIM'
             ENDIF
C ...        INTEGER STYLE CHARACTER VALUES
             READ (IUNIT) IWRT1,(IPACKB(J),J = 1, IWRT1)
             READ (IUNIT) IWRT2,IWRT3,
     *                    (IREC(J),J=1,IWRT2),(FREC(J),J=1, IWRT3)
             READ (IUNIT) IWRT4, (IPACKT(J),J=1,IWRT4)
             CALL CONVRT ( DMSBAN, 15, IPACKB, 80, 2)
             CALL CONVRT ( TITLER4, 1, IPACKT, 77, 2)
C-
             IF( I.EQ.1 ) THEN
                 PRINT *,' --> WRITE INTEGER/CHAR BANNERS TO UNIT=50'
                 WRITE(NSAL) MFLG, ISWREC, NPR4, NER4
                 WRITE(NSAL) IWRT1,(IPACKB(J),J = 1, IWRT1)
                 WRITE(NSAL) IWRT2,IWRT3,
     *                         (IREC(J),J=1,IWRT2),(FREC(J),J=1, IWRT3)
                 WRITE(NSAL) IWRT4, (IPACKT(J),J=1,IWRT4)
             ENDIF
         ENDIF
         WRITE(*,*) ' --> Your RMA4 has a version # of ',ISWREC
C-
C
         WRITE(3,110) IFILE(I), IUNIT,(DMSBAN(J),J=1,15)
         WRITE(3,120) IREC(1), TSTART(I), TEND(I)
  110    FORMAT(' FILE=',A50,' UNIT =',I2,' BANNERS',/,
     *     '  1)',A80,/,4(5X,A80,/),/,
     *     '  2)',A80,/,4(5X,A80,/),/,
     *     '  3)',A80,/,4(5X,A80,/),//)
C
  120    FORMAT(9X,'IREC(1)=',I3,5X,'EXTRACT HRS',F12.6,' TO',F12.6,/)
C
       INSIDE = 0
       DO 900 JJ = 1, 2000
          READ(IUNIT,END=2050)  TIMER4, NQAL, NPR4,
     *                         ((CON(J,K),J=1,NPR4),K=1,NQAL)
          IF (JJ.EQ.1) T1ST = TIMER4
          IF (JJ.EQ.2) T2ND = TIMER4
C
C
          IF( TIMER4.GE.TSTART(I) .AND. TIMER4.LE.TEND(I) ) THEN
              INSIDE = INSIDE + 1
              WRITE(NSAL)      TIMER4, NQAL, NPR4,
     *                        ((CON(J,K),J=1,NPR4),K=1,NQAL)
              IF( INSIDE.EQ.1 ) TFIRST = TIMER4
              NREC = NREC + 1
              TLAST = TIMER4
          ENDIF
  900   CONTINUE
  910   IUNIT = IUNIT + 1
        REWIND IUNIT
        TDELT = T2ND - T1ST
 1000 CONTINUE
C 
 2000 WRITE(3,2010) IFILEM,
     *              (IFILE(JJ),TSTART(JJ),TEND(JJ),JJ=1,MERGTOT)
 2010 FORMAT(/,' THE MERGED FILE IS NAMED=',A50,
     *      ' AND THESE DATA FILES AND TIMES WERE USED TO CREATE IT',/,
     *       3X,10(A50,5X,2F12.6),//)
      WRITE(3,2030) NREC, TFIRST, TLAST
 2030 FORMAT(/,I5,' TIME STEPS MERGED TO UNIT 50 ... FIRST=',F12.6,/,
     *                                           39X,'LAST=',F12.6,/)
      GO TO 3000
 2050 WRITE(3,2060) IUNIT
 2060 FORMAT(' END OF FILE WHILE READING UNIT =',I2,' SUB R4MERGE')
      GO TO 3000
 2100 WRITE(3,2200) IUNIT
 2200 FORMAT(' ERROR WHILE READING UNIT =',I2,' SUBROUTINE R4MERGE')
      GO TO 3000
 2300 WRITE(3,2310)
 2310 FORMAT(' END OF FILE UNIT  5 INPUT')
 3000 DO 3050 II = 71, IUNIT
         CLOSE (II)
 3050 CONTINUE
      PRINT *,' ========== Closing active files from R4MERGE ========='
      CLOSE(NSAL)
      PRINT 3060, IFILEM, INSIDE
 3060 FORMAT('  ==== MERGED RMA4 file named -->',A,/,
     *       '  ==== Closed with',I6,' time steps',/)
      RETURN
      END
C --------
      SUBROUTINE R4INT 
C
      PARAMETER(MME=25000,MMN=70000,NTS=3000)
C
      INTEGER TRMLEN
      CHARACTER*80   DMSBAN
      CHARACTER*10   FLAG
      CHARACTER*50   IGEO, IFILE1, IFILE2, NAMEF, IFILEM
      CHARACTER*77   TITLEGF, TITLER10, TITLER2, TITLER4
C
      COMMON /DMS /  DMSBAN(15),
     *               IREC(40),FREC(40),TITLER2,TITLER4,
     *               TITLEGF, TITLER10, FLAG, IFILEM,
     *               IPACKT(77), IPACKB(1200)
      COMMON /BLKR2/  
     *               VEL(3,MMN),NDRY(MMN),IMAT(MME),VMAG(MMN),
     *               WSEL(MMN), WIDTH(MMN),SS1(MMN),SS2(MMN),
     *               WIDS(MMN), CORD(MMN,2),ALPHA(MMN),BE(MMN),
     *               NOP(MME,20),TH(MME),NFIXH(MME),
     *               NEG,NPG,NE2,NP2
      COMMON /BLKR4/ TIMER4, NQAL, NPR4, CON(MMN,6), UL(MMN), VL(MMN)
C
      DIMENSION      CMIN(MMN,6),CMAX(MMN,6),
     *               CSUM(MMN,6),CAVE(MMN,6), TR4(NTS)
C
C ... INITIALIZE MINIMUM AND MAXIMUM COUNTERS AND AVERING VARIABLES
C
      DO 20 J = 1, MMN
         DO 10  K = 1,6
            CMAX(J,K) = -1.0E+20
            CMIN(J,K) =  1.0E+20
            CSUM(J,K) = 0.
            CAVE(J,K) = 0.
  10     CONTINUE
  20  CONTINUE
C
      PRINT *,' ************** CONCENTRATION AVERAGE ***************'
      T2START = -9999
      PRINT *,' Enter the RMA4 decimal time to start in HOURS'
      PRINT *,'                Default=last time minus 25 time steps'
      CALL DCODE (T2START,T2START)
      T2STOP  = -9999
      PRINT *,' Enter the RMA4 decimal time to stop in HOURS'
      PRINT *,'                Default=last time'
      CALL DCODE (T2STOP,T2STOP)
      ITOTCON = 1
      PRINT *,' Enter the total number of constituents to consider'
      PRINT *,'                Default= 1'
      CALL DCODE (ITOTCON,ITOTCON)
      IF (ITOTCON.GT.6) THEN
          PRINT *,' No-No Maximum allowed total is =6,  Now its 6.'
          ITOTCON = 6
      ENDIF
      IDEBUG = 0
      PRINT *,' DO you want to turn FULL prints on? (Enter 1 of yes)'
      READ(5,*,END=30) ITEST
      IDEBUG = ITEST
C
C  READ RMA4-TYPE BINARY DATA AND BANNERS
C
   30 IRECN = 0 
      NSAL = 70
      REWIND NSAL
      PRINT *,' INSIDE R4INT, rewinding nsal=70 to read rma-4 binary'
C ... determine type of binary file and read 
      READ (NSAL,END=50,ERR=50) ITEST4
      PRINT *,' -->  1st integer on RMA4 binary=',ITEST4
      REWIND (NSAL)
      IF (ITEST4.GT.200) THEN
C ...     This must be a character banner style binary file
          READ (NSAL, END=50,ERR=50) (DMSBAN(K),K=1,15)
          IRECN = 1
          READ (NSAL) (IREC(K),K=1,40), (FREC(K),K=1,40)
          IRECN = 2
          IF (ISWREC.GT.425) READ (NSAL) TITLER2
          IRECN = 3
          IF( I.EQ.1 ) THEN
              PRINT *,' --> WRITE CHARACTER BANNERS TO UNIT=50'
              WRITE(80) (DMSBAN(K),K=1,15)
              WRITE(80) (IREC(K),K=1,40), (FREC(K),K=1,40)
              WRITE(80) TITLER4
          ENDIF
      ELSE
          READ (NSAL, END=50,ERR=50) MFLG, ISWREC, NPR4, NER4
          IRECN = 1
          IF (NPR4 .GT. MMN .OR. NER4 .GT. MME) THEN
             WRITE(*,*)' *** Array dimension over-run problem *** '
             WRITE(*,*)' --> Maximum allowed # of nodes =',MMN
             WRITE(*,*)' --> Your file has ',NPR4,' nodes'
             WRITE(*,*)' --> Maximum allowed # of elements =',MME
             WRITE(*,*)' --> Your file has ',NER4,' elements'
             STOP'DIM'
          ENDIF
C ...     Integer style character values
          READ (NSAL) IWRT1,(IPACKB(J),J = 1, IWRT1)
          IRECN = 2
          READ (NSAL) IWRT2,IWRT3,
     *                 (IREC(J),J=1,IWRT2),(FREC(J),J=1, IWRT3)
          IRECN = 3
          READ (NSAL) IWRT4, (IPACKT(J),J=1,IWRT4)
          IRECN = 4
          CALL CONVRT ( DMSBAN, 15, IPACKB, 80, 2)
          CALL CONVRT ( TITLER4, 1, IPACKT, 77, 2)
C-
          IF( I.EQ.1 ) THEN
              PRINT *,' --> WRITE INTEGER/CHAR BANNERS TO UNIT=50'
              WRITE(80) MFLG, ISWREC, NPR4, NER4
              WRITE(80) IWRT1,(IPACKB(J),J = 1, IWRT1)
              WRITE(80) IWRT2,IWRT3,
     *                      (IREC(J),J=1,IWRT2),(FREC(J),J=1, IWRT3)
              WRITE(80) IWRT4, (IPACKT(J),J=1,IWRT4)
          ENDIF
      ENDIF
      WRITE(*,*) ' --> Your RMA4 has a version # of ',ISWREC
C-
C * * * * * * *
      IF( IDEBUG.NE.0 ) PRINT *,' Just read RMA-4 Banners successfully'
      GO TO 75
   50 PRINT *,' UNEXPECTED E-O-F HIT during Banner read of RMA4 binary'
      PRINT *,'            LAST successful record number read=',IRECN
      STOP    
C
C ... READ DYNAMIC TIME ON RMA4, NEED HOURS TSINT THRU TEINT
C
   75 IF ( T2START.GT. -99. ) GO TO 250
      IRECN = 1
      DO 100 I = 1, NTS
         READ(NSAL,END=200,ERR=200) TR4(IRECN), NQAL
         IRECN = IRECN + 1
  100 CONTINUE
      PRINT *,' CAUTION - NO END OF FILE HIT AFTER ',NTS,' TIME STEPS'
      STOP
  200 PRINT 210, TR4(IRECN), IRECN, NQAL
  210 FORMAT(' LAST TIME ON BINARY FILE=',F12.6,/,
     *       ' TOTAL DATA RECORD COUNT=',I8,/,
     *       ' NUMBER OF CONSTITUENTS WRITTEN TO BINARY=',I4,/) 
      IF (ITEST4.GT.200) THEN
          ISKB = 3
      ELSE
          ISKB = 4
      ENDIF
      REWIND NSAL
      DO 220 I = 1, ISKB
         READ(NSAL) 
  220 CONTINUE
      T2STOP  = TR4(IRECN)
      T2START = TR4(IRECN-25)            
      PRINT 230, T2START, T2STOP
  230 FORMAT(' PROGRAM self computes time to start and stop as=',2F12.8)
C ----------------------------------------------------------------------
C-
  250 RECN = 0.
      DO 3000 I = 1, NTS
         READ(NSAL,END=4000,ERR=4000)  TIMER4, NQAL, NPR4,
     *                        ((CON(J,K),J=1,NPR4),K=1,NQAL)
C
         IF(TIMER4.LT.T2START .OR. TIMER4.GT.T2STOP) GOTO 2500
C
         RECN = RECN + 1.
         DO 2000 J = 1,NPR4
           JJ = J
           DO 1800 K = 1, ITOTCON
C ***         SUM FOR FUTURE AVERAGING
              CSUM(J,K) = CSUM(J,K) + CON(J,K)
C ***         FIND MINIMUM AND MAXIMUM FOR EACH NODE OVER THE CYCLE
              IF(CON(J,K).GT. CMAX(J,K)) CMAX(J,K) = CON(J,K)
              IF(CON(J,K).LT. CMIN(J,K)) CMIN(J,K) = CON(J,K)
 1800      CONTINUE
 2000    CONTINUE
         GO TO 3000
 2500    WRITE(3,2510) TIMER4
 2510    FORMAT(' Skipping RMA4 Time=',F15.6)
 3000 CONTINUE
C
      IF( RECN.LE.1.0) THEN
            PRINT 3050,   RECN, T2START, T2STOP
            WRITE(3,3050) RECN, T2START, T2STOP
 3050       FORMAT(' WARNING --- RECN=',F5.1,/,
     *      ' CANNOT FIND RMA-4 UNIT NSAL TIME BETWEEN =',2F12.8,/)
            STOP
      ENDIF
C
C ... TAKE AVERAGES, THEN SCALE, THEN WRITE TO OUTPUT FILE
C
 4000 TCYCLE = T2STOP - T2START
      WRITE(3,4100) TCYCLE, T2STOP
 4100 FORMAT(1H1,/,20X,'AVERAGE OF EACH NODE OVER A',F12.6,
     *                 ' HR CYCLE, WITH LAST HR=',F12.6,/,5X,/,
     *             20X,' ONLY FIRST 3 CONSTITUENTS CAN BE PRINTED',/,
     *       '  NODE  ','  CON1-AVE-MIN-MAX  ',
     *                  '  CON2-AVE-MIN-MAX  ',
     *                  '  CON3-AVE-MIN-MAX  ',/)
      IZNODES = 0
      ICONPRT = ITOTCON
      IF (ICONPRT.GT.3) ICONPRT = 3
      DO 5000 J = 1, NPR4
         DO 4150 K = 1, ITOTCON
            CAVE(J,K) = CSUM(J,K)/RECN
 4150    CONTINUE
         WRITE(3,4200)
     *      J,(CAVE(J,K),CMIN(J,K),CMAX(J,K),K=1,ICONPRT)
 4200    FORMAT(  1X,I6,1X, 3F7.3,3F7.3,3F7.3 )
 5000 CONTINUE
C
C ... WRITE INTEGRATED INFORMATION TO UNIT 80
C
        PRINT *,' WRITING DATA TO UNIT=60'
        TIMER4 = -99.
        WRITE(80) TIMER4, ITOTCON, NPR4,
     *            ((CAVE(J,K),J=1,NPR4),K=1,ITOTCON)
C
      WRITE(3,6010) T2START, T2STOP, NPR4, ITOTCON
 6010 FORMAT(' COMPLETE-',/,' UNIT 80 CONTAINS 1 TIME STEP OF',
     *' AVERAGED CONCENTRATIONS',/,
     *' FROM HRS',F15.7,' TO ',F15.7,/,
     *' TOTAL NODES=',I7,/,
     *' TOTAL NUMBER OF CONSTITUENTS PROCESSED=',I3,/)
C
      RETURN
      END
C -------
      SUBROUTINE R10MERG
C
C ... MERGE INFORMATION OF UNITS 91-93 ONTO UNIT 90 (NRMA10)
C
      PARAMETER(MME=25000,MMN=70000,NTS=3000)
C
      INTEGER TRMLEN
      CHARACTER*80   DMSBAN
      CHARACTER*10   FLAG
      CHARACTER*50   IFILE(10), NAMEF, IFILEM, TIMES
      CHARACTER*77   TITLEGF, TITLER10, TITLER2, TITLER4
C
      COMMON /DMS /  DMSBAN(15),
     *               IREC(40),FREC(40),TITLER2,TITLER4,
     *               TITLEGF, TITLER10, FLAG, IFILEM,
     *               IPACKT(77), IPACKB(1200)
      COMMON /BLKR2/  
     *               VEL(3,MMN),NDRY(MMN),IMAT(MME),VMAG(MMN),
     *               WSEL(MMN), WIDTH(MMN),SS1(MMN),SS2(MMN),
     *               WIDS(MMN), CORD(MMN,2),ALPHA(MMN),BE(MMN),
     *               NOP(MME,20),TH(MME),NFIXH(MME),
     *               NEG,NPG,NE2,NP2
      COMMON /BLKR4/ TIMER4, NQAL, NPR4, CON(MMN,6), UL(MMN), VL(MMN)
C
C GLB-R10 change VELR10 dimension from 4 to 6
      COMMON /BLKR10/ TIMER10, NP10, NE10, NDF, VELR10(6,MMN),
     *               VERTVEL(MMN),NDEP(MMN),NREF(MMN),
     *               NOPR10(MME,20),CORDR10(MMN,3),SPEC(MMN,3),
     *               ALFA(MMN),NFIX(MMN),AO(MMN),NSURF(MMN),
     *               NCORN(MME),IMATR10(MME),
     *               THR10(MME),WIDTHR10(MMN)
C GLB-R10 add the following dimensions
     *               ,IRESAV(8),DELBED(MMN),BSHR(MMN),DFCT(MME)
C
      DIMENSION TSTART(10), TEND(10)
      LOGICAL HERE
C
      WRITE(3,10)
   10 FORMAT(1H1,/,'  MERGE TOGETHER - MULTIPLE RMA10 FILES')
      IUNIT   =  91
      NRMA10  =  95
      NREC    =   0
      IREC(1) =   0
      TFIRST  = -99.
      TLAST   = -99.
C
      PRINT *,'  PURPOSE -->  MERGE together - MULTIPLE RMA10 files'
      PRINT *,' '
  20  PRINT *,' Enter name of RMA10 MERGED file to be created' 
      READ(5,40,END=2300) IFILEM
      LASTC = TRMLEN( IFILEM )
      NRMA10 = 95
      INQUIRE (FILE=IFILEM, EXIST=HERE)
      IF (HERE) THEN
         PRINT *,' That file name already exists   OverWrite? '
         FLAG= 'N'
         READ(5,'(A)')  FLAG
         IF (FLAG(1:1).EQ.'N' .OR. FLAG(1:1).EQ.'n') FLAG(1:1) = 'N'
         IF( FLAG(1:1).EQ.'N') GO TO 20
         OPEN (NRMA10,FILE=IFILEM,FORM='UNFORMATTED',STATUS='OLD')
      ELSE
         OPEN (NRMA10,FILE=IFILEM,FORM='UNFORMATTED',STATUS='NEW')
      ENDIF
C
      DO 1000 I = 1, 10
         PRINT *,' Enter flag identifier or (?) for menu'
         READ(5,40) FLAG
         IF( FLAG(1:1).EQ.'?') THEN
             PRINT 30
   30        FORMAT(' VALID FLAG OPTIONS FOR RMA10 MERGE ARE:',/,
     *              '   To read first file enter     -->  1ST RMA10',/,
     *              '   To read second file enter    -->  2ND RMA10',/,
     *              '   To read third file enter     -->  3RD RMA10',/,
     *              '   To leave this routine enter  -->  EXIT    ',/)
             READ(5,40) FLAG
         ENDIF
   40    FORMAT(A)
         IF (FLAG(1:3) .EQ. '1ST' .OR. FLAG(1:3) .EQ. '1st') THEN
   50         PRINT *,' Enter name of 1st RMA10 primary binary file'
              READ(5,40,END=2300) IFILE(1)
              MERGTOT = 1
              IUNIT = 91
              LASTC = TRMLEN( IFILE(1) )
              INQUIRE (FILE=IFILE(1), EXIST=HERE)
              IF (HERE) THEN
                OPEN (IUNIT,FILE=IFILE(1),
     *                      FORM='UNFORMATTED',STATUS='OLD')
              ELSE
                PRINT *,' Sorry ... no such file name ... try again'
                GO TO 50
              ENDIF
              PRINT *,' Enter starting and ending decimal hour'
              READ(5,*,END=2300)           TSTART(1), TEND(1)
C
            ELSEIF (FLAG(1:3) .EQ. '2ND' .OR. FLAG(1:3) .EQ. '2nd') THEN
   60         PRINT *,' Enter name of 2ND RMA10 primary binary file'
              READ(5,40,END=2300) IFILE(2)
              MERGTOT = 2
              IUNIT = 92
              LASTC = TRMLEN( IFILE(2) )
              INQUIRE (FILE=IFILE(2), EXIST=HERE)
              IF (HERE) THEN
                OPEN (IUNIT,FILE=IFILE(2),
     *                      FORM='UNFORMATTED',STATUS='OLD')
              ELSE
                PRINT *,' Sorry ... no such file name ... try again'
                GO TO 60
              ENDIF
              PRINT *,' Enter starting and ending decimal hour'
              READ(5,*,END=2300)           TSTART(2), TEND(2)
C
            ELSEIF (FLAG(1:3) .EQ. '3RD' .OR. FLAG(1:3) .EQ. '3rd') THEN
   70         PRINT *,' Enter name of 3rd RMA10 primary binary file'
              READ(5,40,END=2300) IFILE(3)
              MERGTOT = 3
              IUNIT = 93
              LASTC = TRMLEN( IFILE(3) )
              INQUIRE (FILE=IFILE(3), EXIST=HERE)
              IF (HERE) THEN
                OPEN (IUNIT,FILE=IFILE(3),
     *                      FORM='UNFORMATTED',STATUS='OLD')
              ELSE
                PRINT *,' Sorry ... no such file name ... try again'
                GO TO 70
              ENDIF
              PRINT *,' Enter starting and ending decimal hour'
              READ(5,*,END=2300)           TSTART(3), TEND(3)
            ELSE
              GO TO 3000
         ENDIF
C
C ...    READ BANNERS
C=         READ(IUNIT,END=2050,ERR=2100) 
C=     *               (DMSBAN(J),J=1,15)
C=         READ(IUNIT) (IREC(J),J=1,40),(FREC(J),J=1,40)
C=         READ(IUNIT) TITLER10
C
C=         WRITE(3,110) IFILE(I), IUNIT,
C=     *               (DMSBAN(J),J=1,15)
         WRITE(3,120) IREC(1), TSTART(I), TEND(I)
  110    FORMAT(' FILE=',A50,' UNIT =',I2,' BANNERS',/,
     *      '  1)',A80,/,4(5X,A80,/),/,
     *      '  2)',A80,/,4(5X,A80,/),/,
     *      '  3)',A80,/,4(5X,A80,/),//)
C
  120    FORMAT(9X,'IREC(1)=',I3,5X,'EXTRACT HRS',F12.6,' TO',F12.6,/)
C=         IF( I.EQ.1 ) THEN
C=            PRINT *,' WRITING BANNERS FOR MERGED RMA10 TYPE FILE UNIT=90'
C=            WRITE(NRMA10)   (DMSBAN(J),J=1,15)
C=            WRITE(NRMA10)   (IREC(J),J=1,40),(FREC(J),J=1,40)
C=            WRITE(NRMA10)   TITLER10
C=         ENDIF
C
C GLB-R10 comment out the 3D-geo read/write info
C ...  READ 3D GEOMETRY INFORMATION ON BINARY RMA10 FILE
C       READ (IUNIT,END=2050,ERR=2100) NP10, NE10, NPM, NES,
C     *      ((CORDR10(J,K),SPEC(J,K),K=1,3),
C     *      ALFA(J),NFIX(J),AO(J),NSURF(J), J=1, NP10),
C     *      (NDEP(J),NREF(J),J=1,NPM),
C     *      ((NOPR10(J,K),K=1,20), NCORN(J), IMATR10(J),
C     *      TH(J), NFIXH(J), J=1,NE10),
C     *      (WIDTHR10(J),J=1,NP10)
C-
C      IF( NP10.GT.MMN .OR. NE10.GT.MME ) THEN
C          PRINT *,' ========= ARRAY OVER-RUN OCCURING ============='
C          PRINT *,' === Just read RMA10 with',NE10,NP10,' Elem-Nodes'
C          PRINT *,' === Code Dimensions are=',MME,MMN,'   FIX IT!'
C          PRINT *,' ========= ARRAY OVER-RUN STOP NOW ============='
C          STOP
C      ENDIF
C      PRINT *,' Just read 3D geometry info successfully UNIT=',IUNIT
C
C ...  3D GEOMETRY INFORMATION ON BINARY RMA10 FILE
C       IF (I.EQ.1 ) WRITE (NRMA10)  NP10, NE10, NPM, NES,
C     *      ((CORDR10(J,K),SPEC(J,K),K=1,3),
C     *      ALFA(J),NFIX(J),AO(J),NSURF(J), J=1, NP10),
C     *      (NDEP(J),NREF(J),J=1,NPM),
C     *      ((NOPR10(J,K),K=1,20), NCORN(J), IMATR10(J),
C     *      TH(J), NFIXH(J), J=1,NE10),
C     *      (WIDTHR10(J),J=1,NP10)
C
C       PRINT *,' Just wrote 3D geometry information on new RMA10 file'
       INSIDE = 0
       DO 900 JJ = 1, NTS
C GLB-R10 update the binary file format
          READ(IUNIT,END=910,ERR=910)
     &       TIMER10, NP10, NDF, NE10,
     &       NDFS,(IRESAV(K),K=1,NDFS),
     &       ((VELR10(K,J),J = 1, IRESAV(K)),K=1,NDF),
     &       (WSEL(J),J = 1, IRESAV(3)),
     &       (IMAT(J), J = 1, NE10), (NDRY(J), J = 1, NP10),
     &       (DELBED(J),J=1,IRESAV(7)),(BSHR(J),J=1,IRESAV(8)),
     &       (VERTVEL(J), J = 1, NP10), (DFCT(J),
     &       J = 1, NE10)
          IF (JJ.EQ.1) T1ST = TIMER10
          IF (JJ.EQ.2) T2ND = TIMER10
C
C
          IF( TIMER10.GE.TSTART(I) .AND. TIMER10.LE.TEND(I) ) THEN
             INSIDE = INSIDE + 1
             WRITE(NRMA10)     
C GLB-R10 update the binary file format
     &       TIMER10, NP10, NDF, NE10,
     &       NDFS,(IRESAV(K),K=1,NDFS),
     &       ((VELR10(K,J),J = 1, IRESAV(K)),K=1,NDF),
     &       (WSEL(J),J = 1, IRESAV(3)),
     &       (IMAT(J), J = 1, NE10), (NDRY(J), J = 1, NP10),
     &       (DELBED(J),J=1,IRESAV(7)),(BSHR(J),J=1,IRESAV(8)),
     &       (VERTVEL(J), J = 1, NP10), (DFCT(J),
     &       J = 1, NE10)
             IF( INSIDE.EQ.1 ) TFIRST = TIMER10
             NREC = NREC + 1
             TLAST = TIMER10
          ENDIF
  900   CONTINUE
  910   PRINT *,' JUST wrote ',INSIDE,' 3D time steps on file= ',IFILEM
        IUNIT = IUNIT + 1
        REWIND IUNIT
 1000 CONTINUE
C 
 2000 WRITE(3,2010) IFILEM,
     *              (IFILE(JJ),TSTART(JJ),TEND(JJ),JJ=1,MERGTOT)
 2010 FORMAT(/,' THE MERGED FILE IS NAMED=',A50,
     *      ' AND THESE DATA FILES AND TIMES WERE USED TO CREATE IT',/,
     *       3X,10(A50,5X,2F12.6),//)
      WRITE(3,2030) NREC, TFIRST, TLAST
 2030 FORMAT(/,I5,' TIME STEPS MERGED TO UNIT 90 ... FIRST=',F12.6,/,
     *                                39X,'LAST=',F12.6,/)
      GO TO 3000
 2050 WRITE(3,2060) IUNIT
 2060 FORMAT(' END OF FILE WHILE READING UNIT =',I2,' SUB R10MERGE')
      GO TO 3000
 2100 WRITE(3,2200) IUNIT
 2200 FORMAT(' ERROR WHILE READING UNIT =',I2,' SUBROUTINE R10MERGE')
      GO TO 3000
 2300 WRITE(3,2310)
 2310 FORMAT(' END OF FILE UNIT  5 INPUT')
 3000 DO 3050 II = 91, IUNIT
         CLOSE (II)
 3050 CONTINUE
      PRINT *,' ========== Closing active files from R10MERGE ========='
      CLOSE(NRMA10)
      PRINT 3060, IFILEM, INSIDE
 3060 FORMAT('  ==== MERGED RMA10 file named --> ',A,/,
     *       '  ==== Closed with',I8,' time steps',/,
     *       '  ======================================================')
      RETURN
      END
C --------
      SUBROUTINE R10INT 
C
      PARAMETER(MME=25000,MMN=70000,NTS=3000)
C
      INTEGER TRMLEN
      CHARACTER*80   DMSBAN
      CHARACTER*10   FLAG
      CHARACTER*50   IGEO, IFILE1, IFILE2, NAMEF, IFILEM
      CHARACTER*77   TITLEGF, TITLER10, TITLER2, TITLER4
C
      COMMON /DMS /  DMSBAN(15),
     *               IREC(40),FREC(40),TITLER2,TITLER4,
     *               TITLEGF, TITLER10, FLAG, IFILEM,
     *               IPACKT(77), IPACKB(1200)
      COMMON /BLKR2/  
     *               VEL(3,MMN),NDRY(MMN),IMAT(MME),VMAG(MMN),
     *               WSEL(MMN), WIDTH(MMN),SS1(MMN),SS2(MMN),
     *               WIDS(MMN), CORD(MMN,2),ALPHA(MMN),BE(MMN),
     *               NOP(MME,8),TH(MME),NFIXH(MME),
     *               NEG,NPG,NE2,NP2
      COMMON /BLKR4/ TIMER4, NQAL, NPR4, CON(MMN,6), UL(MMN), VL(MMN)
C
C GLB-R10 change VELR10 dimension from 4 to 6
      COMMON /BLKR10/ TIMER10, NP10, NE10, NDF, VELR10(6,MMN),
     *               VERTVEL(MMN),NDEP(MMN),NREF(MMN),
     *               NOPR10(MME,20),CORDR10(MMN,3),SPEC(MMN,3),
     *               ALFA(MMN),NFIX(MMN),AO(MMN),NSURF(MMN),
     *               NCORN(MME),IMATR10(MME),
     *               THR10(MME),WIDTHR10(MMN)
C GLB-R10 add the following dimensions
     *               ,IRESAV(8),DELBED(MMN),BSHR(MMN),DFCT(MME)
C
C GLB-R10 change dimension 4 to 6 in the following arrays
      DIMENSION      VMIN(6,MMN),VMAX(6,MMN),
     *               VSUM(6,MMN),VAVE(6,MMN), TR10(NTS),
     *               VERTMIN(MMN), VERTMAX(MMN), VERTSUM(MMN),
     *               VERTAVE(MMN), NODEMX(6), NODEMN(6),
     *               BIGGEST(6),   SMALLEST(6)
      DATA NODEMX/6*0/, NODEMN/6*0/
      DATA BIGGEST/6*-1.0E+10/, SMALLEST/6*1.0E+10/
      DATA WIDTHR10 /MMN*-99/
C
C ... INITIALIZE MINIMUM AND MAXIMUM COUNTERS AND AVERAGING VARIABLES
C
C GLB-R10 change NDF = 4 to NDF = 6
      NDF = 6
      DO 20 I = 1,6 
         DO 10  J = 1, MMN
            VMAX(I,J) = -1.0E+10
            VMIN(I,J) =  1.0E+10
            VSUM(I,J) =  0.
            VAVE(I,J) =  0.
  10     CONTINUE
  20  CONTINUE
      DO 30 I = 1, MMN
         VERTMIN(I) =  1.0E+10
         VERTMAX(I) = -1.0E+10
         VERTSUM(I) =  0.
         VERTAVE(I) =  0.
  30  CONTINUE
C
      PRINT *,' *********** VELOCITY/CONCENTRATION AVERAGE ************'
      T2START = -9999
      PRINT *,' Enter the RMA10 decimal time to START in HOURS'
      PRINT *,'                Default=last time minus 25 time steps'
      CALL DCODE (T2START,T2START)
      T2STOP  = -9999
      PRINT *,' Enter the RMA10 decimal time to STOP in HOURS'
      PRINT *,'                Default=last time'
      CALL DCODE (T2STOP,T2STOP)
      IDEBUG = 4
      PRINT *,' Value for DEBUG prints? (Enter 1 if ALL nodes printed)'
      READ(5,*,END=40) ITEST
      IDEBUG = ITEST
      PRINT *,'                 IDEBUG now=',IDEBUG
C
C  READ RMA10-TYPE BINARY DATA AND BANNERS
C
   40 IRECN  = 0 
      NRMA10 = 95
      REWIND NRMA10
      PRINT *,' INSIDE R10INT, REWIND NRMA10=95 TO READ RMA-10 BINARY'
C=      READ(NRMA10,END=50)   (DMSBAN(I),I=1,15)
C=      IRECN = 1
C=      READ(NRMA10,END=50)   (IREC(J),J=1,40),(FREC(J),J=1,40)
C=      IRECN = 2
C=      READ(NRMA10,END=50)   TITLER10
C=      IRECN = 3 
C=      IF( IDEBUG.NE.0 ) PRINT *,' Just read RMA10 Banners successfully'
C=      GO TO 75
C=   50 PRINT *,' UNEXPECTED E-O-F HIT during Banner read of RMA10 binary'
C=      PRINT *,'            LAST successful record number read=',IRECN
C=      STOP    
C
c    75 PRINT *, ' WRITE Banners to UNIT=90'
C=      WRITE(90) (DMSBAN(I),I=1,15)
C=      WRITE(90) (IREC(J),J=1,40),(FREC(J),J=1,40)
C=      WRITE(90) TITLER10
C GLB-R10 comment out the 3D-geo read/write info
C ...  READ 3D GEOMETRY INFORMATION ON BINARY RMA10 FILE
C
C       READ (NRMA10,END=7000,ERR=7000) NP10, NE10, NPM, NES,
C     *      ((CORDR10(J,K),SPEC(J,K),K=1,3),
C     *      ALFA(J),NFIX(J),AO(J),NSURF(J), J=1, NP10),
C     *      (NDEP(J),NREF(J),J=1,NPM),
C     *      ((NOPR10(J,K),K=1,20), NCORN(J), IMATR10(J),
C     *      TH(J), NFIXH(J), J=1,NE10),
C     *      (WIDTHR10(J),J=1,NP10)
C
C      IF( NP10.GT.MMN .OR. NE10.GT.MME ) THEN
C          PRINT *,' ========= ARRAY OVER-RUN OCCURING ============='
C          PRINT *,' === Just read RMA10 with',NE10,NP10,' Elem-Nodes'
C          PRINT *,' === Code Dimensions are=',MME,MMN,'   FIX IT!'
C          PRINT *,' ========= ARRAY OVER-RUN STOP NOW ============='
C          STOP
C      ENDIF
C      PRINT *,' Just read 3D geometry info successfully'
C- 
C      WRITE (90) NP10, NE10, NPM, NES,
C     *      ((CORDR10(J,K),SPEC(J,K),K=1,3),
C     *      ALFA(J),NFIX(J),AO(J),NSURF(J), J=1, NP10),
C     *      (NDEP(J),NREF(J),J=1,NPM),
C     *      ((NOPR10(J,K),K=1,20), NCORN(J), IMATR10(J),
C     *      TH(J), NFIXH(J), J=1,NE10),
C     *      (WIDTHR10(J),J=1,NP10)
C      PRINT *,' Just wrote 3D geometry to unit=90'
C
C ... READ DYNAMIC TIME ON RMA10, NEED HOURS TSINT THRU TEINT
C
      IF ( T2START.GT. -99. ) GO TO 250
      IRECN = 1
      DO 100 I = 1, NTS
         READ(NRMA10,END=200,ERR=198) TR10(IRECN), NP10
         IRECN = IRECN + 1
  100 CONTINUE
      PRINT *,' CAUTION - No end of file hit after ',NTS,' Time steps'
      STOP
  198 PRINT *,' ERROR ON rma10 read ******'
  200 IRECN = IRECN - 1
      PRINT 210, TR10(IRECN), IRECN, NP10
  210 FORMAT(' LAST TIME ON BINARY FILE=',F12.6,/,
     *       ' TOTAL DATA RECORD COUNT=',I8,/,
     *       ' NUMBER OF NODES ON THIS FILE=',I10,/) 
      REWIND NRMA10
      PRINT *,' Required to CLOSE/RE-OPEN file to avoid CRAY EOF flag'
      CLOSE (NRMA10)
      OPEN (NRMA10,FILE=IFILEM,FORM='UNFORMATTED',STATUS='OLD')
C=      DO 220 I = 1, 3
C=         READ(NRMA10) 
C=  220 CONTINUE
      T2STOP  = TR10(IRECN)
      T2START = TR10(IRECN-25)            
      TDELT   = TR10(IRECN) - TR10(IRECN-1)
      PRINT 230, T2START, T2STOP, TDELT
  230 FORMAT(' PROGRAM computes time to start and stop as=',2F15.10,/,
     *       ' DELTA TIME STEP BETWEEN LAST 2 TIME STEPS =',F12.10,/)
C- 
C ----------------------------------------------------------------------
C-
  250 RECN = 0.
      DO 3000 I = 1, NTS
          READ(NRMA10,END=4000,ERR=4000)
C GLB-R10 update the binary file format
     &       TIMER10, NP10, NDF, NE10,
     &       NDFS,(IRESAV(K),K=1,NDFS),
     &       ((VELR10(K,J),J = 1, IRESAV(K)),K=1,NDF),
     &       (WSEL(J),J = 1, IRESAV(3)),
     &       (IMAT(J), J = 1, NE10), (NDRY(J), J = 1, NP10),
     &       (DELBED(J),J=1,IRESAV(7)),(BSHR(J),J=1,IRESAV(8)),
     &       (VERTVEL(J), J = 1, NP10), (DFCT(J),
     &       J = 1, NE10)
C
         IF( i.lt.2 ) print *,' Barbara Debug NDG-NDF binary=',NDG,NDF
C
         IF(TIMER10.LT.T2START .OR. TIMER10.GT.T2STOP) GOTO 2500
C
         RECN = RECN + 1.
         DO 2000 J = 1, NDF
           DO 1800 K = 1, NP10
              NODE = K
C ***         SUM ALL DEGREES OF FREEDOM FOR FUTURE AVERAGING
              VSUM(J,K) = VSUM(J,K) + VELR10(J,K)
C ***         FIND MINIMUM AND MAXIMUM FOR EACH NODE OVER THE CYCLE
              IF(VELR10(J,K).GT. VMAX(J,K)) THEN
                 VMAX(J,K) = VELR10(J,K)
                 IF( VMAX(J,K).GT.BIGGEST(J) ) THEN
                     BIGGEST(J) = VMAX(J,K)
                     NODEMX(J) = NODE
                 ENDIF
              ENDIF
              IF(VELR10(J,K).LT. VMIN(J,K)) THEN
                 VMIN(J,K) = VELR10(J,K)
                 IF (VMIN(J,K).LT.SMALLEST(J) ) THEN
                     SMALLEST(J) = VMIN(J,K)
                     NODEMN(J) = NODE
                 ENDIF
              ENDIF
 1800      CONTINUE
 2000    CONTINUE
         GO TO 3000
 2500    IF( I.LE.2)  WRITE(3,2510) TIMER10
         IF( I.LE.2) PRINT *,' Be Patient - Skipping RMA10 time Steps'
 2510    FORMAT(' Skipping along time steps in RMA10 -- TIME= ',F15.8)
 3000 CONTINUE
C
 4000 IF( RECN.LE. 0.5) THEN
            PRINT 4010,   RECN, T2START, T2STOP
            WRITE(3,4010) RECN, T2START, T2STOP
 4010       FORMAT(' WARNING --- RECN=',F5.1,/,
     *      ' CANNOT FIND RMA-10 UNIT NRMA10 TIME BETWEEN =',2F15.8,/)
            STOP
      ENDIF
C
C ... TAKE AVERAGES, THEN SCALE, THEN WRITE TO OUTPUT FILE
C
      WRITE(3,4030) 
 4030 FORMAT(1H1,/, 10X, 'RESULTS FROM MERGE-AVERAGE PROGRAM',/)
C
C GLB-R10 update this output to reflect 6 DOF
      PRINT   4050,  (NODEMX(I), BIGGEST(I), I= 1,6),
     *               (NODEMN(I), SMALLEST(I), I= 1,6)
C
      WRITE(3,4050)  (NODEMX(I), BIGGEST(I), I= 1,6),
     *               (NODEMN(I), SMALLEST(I), I= 1,6)

 4050 FORMAT(/,
     *  2X,' CHECK THESE NODES FOR MAXIMUMS',/,
     *  5X,'    X-VELOCITY    MAX AT NODE=',I10,' WAS=',F15.2,' fps',/,
     *  5X,'    Y-VELOCITY    MAX AT NODE=',I10,' WAS=',F15.2,' fps'/,
     *  5X,'    DEPTH         MAX AT NODE=',I10,' WAS=',F15.2,' ft'/,
     *  5X,'    SALINITY      MAX AT NODE=',I10,' WAS=',F15.2,' ppt',/,
     *  5X,'    TEMPERATURE   MAX AT NODE=',I10,' WAS=',F15.2,' d.C',/,
     *  5X,'    SEDIMENT      MAX AT NODE=',I10,' WAS=',F15.2,' ppt',//,
     *  2X,' CHECK THESE NODE FOR MINIMUMS',/,
     *  5X,'    X-VELOCITY    MIN AT NODE=',I10,' WAS=',F15.2,' fps'/,
     *  5X,'    Y-VELOCITY    MIN AT NODE=',I10,' WAS=',F15.2,' fps'/,
     *  5X,'    DEPTH         MIN AT NODE=',I10,' WAS=',F15.2,' ft'/,
     *  5X,'    SALINITY      MAX AT NODE=',I10,' WAS=',F15.2,' ppt',/,
     *  5X,'    TEMPERATURE   MAX AT NODE=',I10,' WAS=',F15.2,' d.C',/,
     *  5X,'    SEDIMENT      MAX AT NODE=',I10,' WAS=',F15.2,' ppt',/)
      TCYCLE = T2STOP - T2START
      WRITE(3,4100) TCYCLE, T2STOP
 4100 FORMAT(//,10X,'AVERAGE OF EACH NODE OVER A',F12.6,
     *                 ' HR CYCLE, WITH LAST HR=',F12.6,/,5X,/,
     *             20X,' ALL 6 DEGREES OF FREEDOM ARE PRINTED',/,
     *       '  NODE  ','  VELX-AVE-MIN-MAX   ',
     *                  '  VELY-AVE-MIN-MAX   ',
     *                  '  DEEP-AVE-MIN-MAX   ',
     *                  '  SALT-AVE-MIN-MAX   ',
     *                  '  TEMP-AVE-MIN-MAX   ',
     *                  '   SED-AVE-MIN-MAX',/)
      DO 4150 I = 1, NDF
      DO 4150 J = 1, NP10
         VAVE(I,J)  = VSUM(I,J)/RECN
 4150 CONTINUE
C
      DO 4200 J = 1, NP10
         VERTAVE(J) = VERTSUM(J)/RECN
 4200 CONTINUE
C
      NTH = IDEBUG
      IF( IDEBUG.EQ.0 ) GO TO 5200
C
      DO 5000 J = 1, NP10, NTH
C ...    WRITE RESULTS TO PRINT FILE
         WRITE(3,4300)
     *      J,(VAVE(I,J),VMIN(I,J),VMAX(I,J), I=1,NDF)
 4300    FORMAT( 1X,I6,1X, 3(F6.2,1X),3(F6.2,1X),3(F6.2,1X),
     *                     3(F6.2,1X),3(F6.2,1X),3(F6.2,1X) )
 5000 CONTINUE
C
C ... WRITE INTEGRATED INFORMATION TO UNIT 90
C
 5200    PRINT *,' Writing FINAL data to unit=90'
C
         TIME = 9999.0 
         WRITE(90)
C GLB-R10 update the binary file format
     &       TIME, NP10, NDF, NE10,
     &       NDFS,(IRESAV(K),K=1,NDFS),
     &       ((VELR10(K,J),J = 1, IRESAV(K)),K=1,NDF),
     &       (WSEL(J),J = 1, IRESAV(3)),
     &       (DELBED(J),J=1,IRESAV(7)),(BSHR(J),J=1,IRESAV(8)),
     &       (VERTVEL(J), J = 1, NP10), (DFCT(J),
     &       J = 1, NE10)
C
      WRITE(3,6010) T2START, T2STOP, NP10, NDF
 6010 FORMAT(' COMPLETE-',/,' UNIT 90 CONTAINS 1 TIME STEP OF',
     *' AVERAGED CONCENTRATIONS FROM HRS',F12.6,' TO ',F12.6,/,
     *' TOTAL NODES=',I8,/,
     *' TOTAL NUMBER OF DEGREES OF FREEDOM PROCESSED=',I3,/)
C
      RETURN
 7000 PRINT *,' E-O-F OR ERROR ON RMA10 TRYING TO READ 3D GEOM'
      PRINT *,' Variable NP10-NE10-NPM-NES= ',NP10,NE10,NPM,NES
      PRINT *,' Variable (NOPR10(NE10,J),J=1,20) = '
      PRINT *,  (NOPR10(NE10,J),J=1,20)
      PRINT *,' Variable IMATR10(NE10)  = ',IMATR10(NE10)
      PRINT *,' Variable TH(NE10)       = ', TH(NE10)
      PRINT *,' Variable NFIXH(NE10)    = ',NFIXH(NE10)
      PRINT *,' Variable WIDTHR10(NP10) = ',WIDTHR10(NP10)
      RETURN
      END
C -------
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
C --------
      SUBROUTINE DCODE(VALUE, DEFALT)
C 
      CHARACTER STRING*20, BLANK*20
C 
      INTEGER TRMLEN
C 
      DATA BLANK/'                   '/
C 
      READ (*,'(A)') STRING
C 
      IF (STRING .EQ. BLANK) THEN
        VALUE = DEFALT
        RETURN 
      END IF 
C 
      IL = TRMLEN(STRING)
      INTFLG =  - 1
      DO 100 I = 1, IL
  100 IF (STRING(I:I) .EQ. '.') INTFLG =  + 1
C 
      IF (INTFLG .LT. 0) STRING = STRING(1:IL) // '.0'
C 
      READ (STRING,'(F20.10)') VALUE
      RETURN 
      END
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      SUBROUTINE CONVRT ( ISTRNG, LENGTH, INTVAL, ISIZE, ISWTCH )
      SAVE
C-
C ... AUTHOR: BOB EVANS/BARBARA DONNELL              01-22-1993
C ... ISWTCH:  1 CONVERT FROM CHARACTER TO ASCII INTEGER VALUE
C ...           2 CONVERT FROM INTEGER VALUE TO CHARACTER EQUIVALENT
C ...           3 CONSOLIDATE BANNERS
C ... PURPOSE:  Avoid FORTRAN to C-Language binary file read problems
C-
      PARAMETER (MM8=1200)
      CHARACTER  ISTRNG(15)*80
      DIMENSION  INTVAL(MM8)
C-
C-
      ISTOP = 0
      IF ( (ISIZE*LENGTH) .GT. MM8 ) THEN
          PRINT *,' Error in CONVRT ... LENGTH & SIZE are inconsistant'
          PRINT *,'          with dimension capabilities'
          ISTOP= 1
      ENDIF
      IF ( ISIZE .GT. MM8 ) THEN
          PRINT *,' Error in CONVRT ... size larger than 1200'
          ISTOP= 1
      ENDIF
      IF ( ISTOP .GE. 1 ) THEN
           STOP'CONVRT'
      ENDIF
C-
C-
      IF (ISWTCH .EQ. 1) THEN
C-
C ...     Convert from character to ascii integer value
C-
          II = 0
          DO 300 LL = 1, LENGTH
             DO 200 I = 1, ISIZE          
                II = II + 1
                INTVAL(II) = ICHAR( ISTRNG(LL)(I:I) )
  200        CONTINUE
  300     CONTINUE
C-
      ELSEIF (ISWTCH .EQ. 2) THEN
C-
C ...     Convert from ascii integer value to character 

          IIE = 0
          DO 600 LL = 1, LENGTH
             IIS = 1 + IIE
             IIE = IIS + (ISIZE-1)
             J = 0
             DO 500 I = IIS, IIE 
                J = J + 1
                ISTRNG(LL)(J:J) = CHAR( INTVAL(I) )
  500        CONTINUE
  600     CONTINUE
C-
      ELSE
C-
          PRINT *,' --> Value of ISWTCH invalid in CONVRT'
      ENDIF
      RETURN
      END
