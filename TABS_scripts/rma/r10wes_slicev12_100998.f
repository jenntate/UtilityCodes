      PROGRAM SLICEIT
C 
C   THIS PROGRAM WILL ACCESS THE GENERAL OUTPUT FILE OF AN RMA-10 RUN
C   AND TAKE HORIZONTAL "SLICES" BASED ON SPECIFIC DEPTHS. THE DATA WILL
C   BE WRITTEN TO SEPARATE FILES FOR EACH DEPTH.  BOB EVANS 9-1991
C   ENHANCED BY BARBARA DONNELL 12-05-1991
C 
C******************************************************************
C 
C GLB change MAXNDF to 6
      PARAMETER (MAXNE = 30000, MAXN = 60000, MAXNDF = 6,
C GLB elim. LUIN add LUIN1 and LUIN2 and MAXIM
     *   MAXDEP = 10,   LUIN1 = 55, LUIN2 = 56, MAXIM = 10)
C 
      COMMON  /BLKIN/ NP, NE, NDF, NDF2DO, ZSLICE(MAXDEP),
     *                NOP(MAXNE,20), NDEP(MAXN), NREF(MAXN), 
     *                CORD(MAXN,3),  XVEL(MAXNDF,MAXN),AO(MAXN)
C GLB-NSF add the following line
     *                ,DELBED(MAXN),BSHR(MAXN)
C
C GLB add NSIMATS,IMTFLG2
      COMMON  /BLKOUT/VEL(MAXNDF,MAXN), 
     *                VVEL(MAXN), WSEL(MAXN), ZSEL(MAXN),
     *                NDRY(MAXN), IMAT(MAXNE), NSIMATS(MAXIM),
     *                IMTFLG2(MAXN)
C 
C GLB add the following common blocks for banners
C ... needed for banners
      COMMON  /DMSCHR/ HEADER, IDMS(15), BANSLC(2),
     *                 BANGFG(5), BANRM2(5), BANRM4(5), EMPTY(5)
      COMMON  /DMSREC/ IREC(40),FREC(40), IPACKB(1200), IPACKT(77)
      CHARACTER        HEADER*77,  IDMS*80, BANSLC*80,
     *                 BANGFG*80, BANRM2*80, BANRM4*80, EMPTY*80
C
      COMMON  /REF/ IREF,REFEL
      CHARACTER *80 INFILE, OUTFILV(MAXDEP),OUTFILS(MAXDEP),
     *              PREFIX1, PREFIX1OLD,PREFIX2, PREFIX2OLD
      CHARACTER CDEPTH*6, CSLICE*2, CHECKV*1, CHECKS*1, REPLY*1
      CHARACTER *1 VECTOR(2)
C 
      INTEGER TRMLEN
      LOGICAL HERE
C GLB-NSF add this
      DIMENSION IRESAV(9)
C 
C******************************************************************
C 
C ... DIMENSIONS SPECIFIC TO RMA2 LOOK-A-LIKE BINARY FILES:
C 
C GLB comment out these banner dimension statements
C      DIMENSION IREC(40), FREC(40)
C 
C      CHARACTER HEADER*80, IDMS(15)*80, BANSLIC(2)*40,
C     *          BANGFGN(5)*40, BANRMA2(5)*40, BANRMA4(5)*40
C GLB add BANSLIC
       CHARACTER BANSLIC(2)*40
 
C ... SET IREC(1) = 425 TO SIGNAL  WETTING & DRYING & WSEL ARRAY
C                               
      DATA IREC/40*0/, FREC/40*0.0/, VECTOR/'X','Y'/
      DATA IDEBUG /0/, IEOF /0/
C-
C                      123456789-123456789-123456789-123456789-
      DATA BANSLIC(1)/'CEWES-HE PROGRAM SLICE version 1.0      '/
      DATA BANSLIC(2)/'Last modified  3-03-1992                '/
C GLB comment out the following
C
C      DATA BANGFGN(1)/'CEWES-HE DMS BANNER HEADINGS FOR GFGEN  '/
C      DATA BANGFGN(2)/'ENGLISH UNITS APPLIED IN THIS GEOMETRY  '/
C      DATA BANGFGN(3)/'GFGEN VERSION 4.25         1-D AND 2-D  '/
C      DATA BANGFGN(4)/'CAPABILITY.   LAST MOD DATE 05-02-1991  '/
C      DATA BANGFGN(5)/'        Run thru SLICE                  '/
C
C                      123456789-123456789-123456789-123456789-
C      DATA BANRMA2(1)/'CEWES-HE DMS BANNER HEADINGS FOR RMA2-V '/
C      DATA BANRMA2(2)/'ENGLISH UNITS APPLIED IN THIS HYDRO     '/
C      DATA BANRMA2(3)/'RMA2-V VERSION 4.25         1-D AND 2-D '/
C      DATA BANRMA2(4)/'CAPABILITY.  LAST MOD DATE  10-01-1991  '/
C      DATA BANRMA2(5)/'         Run through SLICE              '/
C
C                      123456789-123456789-123456789-123456789-
C      DATA BANRMA4(1)/'CEWES-HE DMS BANNER HEADINGS FOR RMA4   '/
C      DATA BANRMA4(2)/'ENGLISH UNITS APPLIED IN THESE RESULTS  '/
C      DATA BANRMA4(3)/'RMA4-V VERSION 3.00+        1-D AND 2-D '/
C      DATA BANRMA4(4)/'CAPABILITY.   LAST MOD DATE  04-01-1990 '/
C      DATA BANRMA4(5)/'        Run through SLICE               '/
C-
C-
C*******************************************************************
C 
C
C GLB-NSF add the following initializations
      DO I = 1,MAXN
        DO J = 1,MAXNDF
          XVEL(J,I) = 0.0
        END DO
        DELBED(I) = 0.0
        BSHR(I) = 0.0
      END DO
      WRITE(*,'(A,/,A,/)' ) BANSLIC(1), BANSLIC(2)
C
      IDOSAL = 0
C GLB      DO 20 I = 1, 15
C GLB         DO 10 J = 1, 80
C GLB         IDMS(I)(J:J) = '.'
C GLB   10    CONTINUE
C GLB   20 CONTINUE         
      PREFIX1(1:20)    = '                   '
      PREFIX1OLD(1:20) = '                   '
      PREFIX2(1:20)    = '                   '
      PREFIX2OLD(1:20) = '                   '
C
C GLB replace request for one output file with requests for
C GLB a 3d binary geo file and a solution file
   50 WRITE (*,*) ' Enter the rma10 output 3D geometry file name '
      READ (*,'(A)',END = 3000) INFILE
C
      INQUIRE(FILE=INFILE,EXIST=HERE)
      IF ( .NOT. HERE) THEN
        WRITE (*,*) ' You entered a non-existant file>>',INFILE
        GO TO 50
      END IF
      OPEN(LUIN1,FILE=INFILE,FORM='UNFORMATTED',STATUS='OLD')
C
   60 WRITE (*,*) ' Enter the rma10 output solution file name '
      READ (*,'(A)',END = 3000) INFILE
C
      INQUIRE(FILE=INFILE,EXIST=HERE)
      IF ( .NOT. HERE) THEN
        WRITE (*,*) ' You entered a non-existant RMA10 file>>',INFILE
        GO TO 60
      END IF
      OPEN(LUIN2,FILE=INFILE,FORM='UNFORMATTED',STATUS='OLD')
C 
      WRITE (*,*) ' Enter RMA10 decimal hour to process (DEF=LAST ONE)'
C GLB-ATS
      WRITE(*,*) ' NOTE: Enter a -100 to process ALL time steps on the'
      WRITE(*,*) '       rma10 output solution file.'
C GLB-ATS
      TIME2GET = 99999.
      CALL DCODE(TIME2GET, TIME2GET)
C 
C GLB change DEF-4 to DEF=6
      WRITE (*,*) ' Enter the number of degrees of freedom (DEF=6)'
      READ (*,'(BN,I20)') NDF2DO
C GLB change the default from 4 to 6
      IF (NDF2DO .LE. 0) NDF2DO = 6 
C 
      WRITE (*,*) ' Want to run a check on any of the deg of freedom?'
      READ (*,'(A)') REPLY
      IF (REPLY .EQ. 'n' .OR. REPLY .EQ. 'N') GO TO 70
C
      WRITE (*,*) ' Want to run a check on the velocities (DEF=N)?'
      READ (*,'(A)') CHECKV
      IF (CHECKV .EQ. 'y' .OR. CHECKV .EQ. 'Y') THEN
        IDEBUG = 1
        CHECKV = 'Y'
        WRITE (*,*) ' Enter the maximum expected velocity magnitude ',
     *              '(DEF   = 10 ft/s)'
        VLIMIT = 10.0
        CALL DCODE(VLIMIT, VLIMIT)
      END IF 
      WRITE (*,*) ' Want to run a check on salinity concentration?'
      READ (*,'(A)') CHECKS
      IF (CHECKS .EQ. 'n' .OR. CHECKS .EQ. 'N') GO TO 70 
        WRITE(*,*) ' Enter the maximum expected concentration (DEF=35).'
        IDEBUG = 1
        CHECKS = 'Y'
        SLIMIT = 35.
        CALL DCODE (SLIMIT, SLIMIT)
C
C GLB add the following line and comment out the next 4
   70 CONTINUE
C   70 WRITE (*,*) ' Enter the base elevation (DEF = 100 )'
C GLB add the following line for clarity
C      WRITE (*,*) ' *** NOTE: use the initial WS elevation from RMA10 '
C      ELEV0 = 100.0
C      CALL DCODE(ELEV0, ELEV0)
C 
      WRITE (*,*) ' Enter the prefix for new "2D" HYDRO OUTPUT files ',
     *            '(DEF="S_VEL")'
      READ (*,'(A)') PREFIX1
      IF (PREFIX1(1:3) .EQ. '   ') PREFIX1 = 'S_VEL'
C 
C 
C GLB-MCS alter the program to allow slicing for salinity
C GLB-MCS tempterature or sediment.  Changes for this purpose
C GLB-MCS are dentoed with C GLB-MCS
C GLB-MCS change the following line to 'slice for a constituent'
      WRITE (*,*) ' Want to slice for a constituent?  (DEF=NO)'
      READ (*,'(A)') REPLY
      IF (REPLY .EQ. 'y' .OR. REPLY .EQ. 'Y') THEN
        IDOSAL = 1
C GLB-MCS add the following 10 lines
        ICK = 0
        WRITE (*,*) ' Enter a 1 to slice for salinity'
        WRITE (*,*) '         2 to slice for temperature'
        WRITE (*,*) '         3 to slice for sediment'
C GLB-NSF add the following 2 lines
        WRITE (*,*) '         4 to slice for delbed (RMA10-SED only)'
        WRITE (*,*) '         5 to slice for bed shear (RMA10-SED only)'
        READ  (*,*) ICK
C GLB-NSF change GT 3 to GT 5
        IF (ICK .LT. 1 .OR. ICK .GT. 5) ICK = 1
        IF (ICK .EQ. 1) WRITE(*,*) 'Slicing for SALINITY'
        IF (ICK .EQ. 2) WRITE(*,*) 'Slicing for TEMPERATURE'
        IF (ICK .EQ. 3) WRITE(*,*) 'Slicing for SEDIMENT'
C GLB-NSF add the following 2 lines
        IF (ICK .EQ. 4) WRITE(*,*) 'Slicing for DELBED'
        IF (ICK .EQ. 5) WRITE(*,*) 'Slicing for BED SHEAR'
        WRITE (*,*)
C GLB-MCS change the following from SALINITY to CONSTITUENT
        WRITE (*,*) ' Enter the prefix for "2D" CONSTITUENT OUTPUT ',
     *              'files  (DEF="S_SAL")'
        READ (*,'(A)') PREFIX2
        IF (PREFIX2(1:3) .EQ. '   ') PREFIX2 = 'S_SAL'
      ENDIF
C
C 
   80 WRITE (*,*) ' Enter the number of slices (<0 TO DO INTERVALS',
     *            ',DEF=1)'
      READ (*,'(BN,I20)') NSLICE
      IF (NSLICE .EQ. 0) NSLICE = 1
      IF (NSLICE .GT. MAXDEP) THEN
        WRITE (*,*) ' YOU MAY NOT EXCEED ', MAXDEP, ' SLICES!'
        GO TO 80
      END IF 
  100 CONTINUE
C 
      IF (NSLICE .GT. 0) THEN
        WRITE (*,*) ' Enter the ', NSLICE, ' depths for each slice.'
        READ (*,*) (ZSLICE(I), I = 1, NSLICE)
      ELSE 
        INT = ABS(NSLICE)
        WRITE (*,*) ' Enter the INITIAL DEPTH'
        READ (*,*) Z1
        WRITE (*,*) ' Enter the FINAL DEPTH'
        READ (*,*) Z2
        IF (Z2 .LT. Z1) THEN
          ZT = Z2
          Z2 = Z1
          Z1 = ZT
        END IF 
C 
        WRITE (*,*) ' Enter the number of slices (DEF=', INT, ')'
        READ (*,'(BN,I20)') INTDEF
        IF (INTDEF .NE. 0) INT = INTDEF
        IF (INT .EQ. 1) GO TO 100
C 
        IF (INT .GT. MAXDEP) THEN
          WRITE (*,*) ' YOU MAY NOT EXCEED ', MAXDEP, ' SLICES!'
          GO TO 80
        END IF 
C 
        NSLICE = INT
        DELZ = (Z2 - Z1) / FLOAT(INT-1)
        ZSLICE(1) = Z1
        DO 120 I = 2, NSLICE
            ZSLICE(I) = ZSLICE(I-1) + DELZ
  120   CONTINUE
      END IF 
C GLB add the following for IMAT isolation option
      IMTFLG1 = 0
      WRITE(*,*) 'Do you want to isolate specific imats for slicing?'
      READ (*,'(A)') REPLY
      IF (REPLY .EQ. 'Y' .OR . REPLY .EQ. 'y' ) THEN
        IMTFLG1 = 1
 105    WRITE (*,*) ' Enter the number of imats',
     *            ' (DEF=1)'
        READ (*,'(BN,I20)') NIMATS
        IF (NIMATS .EQ. 0) NIMATS = 1
        IF (NIMATS .GT. MAXIM) THEN
          WRITE (*,*) ' YOU MAY NOT EXCEED ', MAXIM, ' IMATS!'
          GO TO 105 
        END IF
        WRITE (*,106) NIMATS
 106    FORMAT (' Enter the ',I2, ' selected imats.')
        READ (*,*) (NSIMATS(I), I = 1, NIMATS)
      END IF
       IREF = 0
      WRITE(*,*) 'DO YOU WANT TO REFERENCE SLICES TO A FIXED ELEVATION?'
      READ (*,'(A)') REPLY
      IF (REPLY .EQ. 'Y' .OR . REPLY .EQ. 'y' ) THEN
      WRITE(*,*)  '  ENTER THE REFERENCE ELEVATION BELOW WHICH TO SLICE'
       READ (*,*)  REFEL
        IREF = 1
      ENDIF
C 
C 
C  READ THE 3D GEOMETRY INFORMATION
C     NPTOT = TOTAL NUMBER OF NODES
C     NETOT = TOTAL NUMBER OF ELEMENTS
C        NP = NUMBER OF SURFACE NODES
C        NE = NUMBER OF SURFACE ELEMENTS
C 
C GLB change LUIN to LUIN1
      READ (LUIN1)   NPTOT, NETOT, NP, NE
      WRITE (*,140) NPTOT, NETOT, NP, NE
  140 FORMAT ( /, ' FOR THE INPUT RMA10 FILE:',  /,
     *            '  TOTAL NUMBER OF NODES      = ', I20,  /,
     *            '  TOTAL NUMBER OF ELEMENTS   = ', I20,  /, 
     *            '  NUMBER OF SURFACE NODES    = ', I20,  /, 
     *            '  NUMBER OF SURFACE ELEMENTS = ', I20,  // )
C 
C GLB change LUIN to LUIN1
      REWIND LUIN1
C 
      READ (LUIN1,ERR=4000) NPTOT, NETOT, NP, NE,
     *     ((CORD(J,K),DUMMY,K=1,3), 
     *     DUM, IDUM, AO(J), NDUM, J = 1, NPTOT), (NDEP(J), 
     *     NREF(J), J = 1, NP), ((NOP(J,K),K=1,20), NDUM, 
     *     IMAT(J), DUM, NDUM, J = 1, NETOT), (DUM, J = 1, NPTOT)
C 
C GLB assign flag to determine which IMATS to slice
      DO I = 1, NP
        IMTFLG2(I) = 1
      END DO
      IF (IMTFLG1 .EQ. 1) THEN
        DO I = 1, NP
          IMTFLG2(I) = 0
        END DO    
        DO I = 1, NE
         DO J = 1, NIMATS
           IF (NSIMATS(J) .EQ. IMAT(I)) THEN 
             DO K = 1, 20
               L = NOP(I,K)
               IF (L .LE. NP .AND. L .GT. 0)
     *         IMTFLG2(L) = 1 
             END DO 
           END IF
         END DO
        END DO
      END IF
C 
C 
C** OPEN EACH OUTPUT FILE & WRITE BANNERS & HEADERS IN STANDARD RMA2
C   FORMAT
C 
      DO 200 I = 1, NSLICE
        WRITE (CDEPTH,'(F6.2)') ZSLICE(I)
        HEADER = '  DATA FROM RMA10 3D FILE ' // 
     *       INFILE(1:TRMLEN(INFILE)) // '; DEPTH = ' // CDEPTH
C 
        IF (I .LT. 10) THEN
          WRITE (CSLICE,'(I1)') I
          IL = 1
        ELSE 
          WRITE (CSLICE,'(I2)') I
          IL = 2
        END IF 
C       --------------------------
C ...   HYDRO 2D LOGICAL UNIT NUMBER TO OPEN
        OUTFILV(I) = PREFIX1(1:TRMLEN(PREFIX1)) // '_' // 
     *       CSLICE(1:IL) // '.sol'
        write(*,150) i, outfilv(i)
        OPEN(I,FILE=OUTFILV(I),FORM='UNFORMATTED',STATUS='UNKNOWN')
  150   FORMAT(' ------------ Opening Unit=',i3,'  File Name=',A )
C 
C GLB replace the following 6 lines with the next 2
C        IREC(1) = 425
C        WRITE (I) (BANGFGN(J), J = 1, 4), (IDMS(J),J=1,3),
C     *            (BANRMA2(J), J = 1, 4), (IDMS(J),J=1,3),
C     *            (IDMS(J), J=1,5)
C        WRITE (I) (IREC(J), J = 1, 40), (FREC(J), J = 1, 40)
C        WRITE (I) HEADER
C-...   Process INTEGER type CHARACTER DMS BANNERS on Fake HYDRO
        CALL PUT_BANNER (NE, NP, I, 2)
C       --------------------------
        IF (IDOSAL.EQ.0) GO TO 200
        II = I + 10
C ...   SALINITY 2D LOGICAL UNIT NUMBERS TO OPEN (RMA4 LOOK=A=LIKE)
        OUTFILS(I) = PREFIX2(1:TRMLEN(PREFIX2)) // '_' // 
     *       CSLICE(1:IL) // '.sol'
        write(*,150) ii, outfils(i)
        OPEN(II,FILE=OUTFILS(I),FORM='UNFORMATTED',STATUS='UNKNOWN')
C 
C GLB replace the following 6 lines with the next 2
C        IREC(1) = 30
C        WRITE (II) (BANGFGN(J), J = 1, 4), (IDMS(J), J=1,3),
C     *             (BANRMA2(J), J = 1, 4), (IDMS(J), J=1,3),
C     *             (BANRMA4(J), J = 1, 4), (IDMS(J), J=1,3)
C        WRITE (II) (IREC(J), J = 1, 40),   (FREC(J), J = 1, 40)
C        WRITE (II) HEADER
C-...   Process INTEGER type CHARACTER DMS BANNERS on Fake RMA4
        CALL PUT_BANNER (NE, NP, II, 4)
C       --------------------------- 
  200 CONTINUE
C 
C 
C ... NOW READ EACH TIME STEP IN THE INPUT DATA FILE, SLICE OUT A
C ...      HORIZONAL SECTION, & WRITE IT TO THE OUTPUT FILE
C 
C 
C ... NOTE: THE NUMBER OF DEGREES OF FREEDOM FOR VPLOT IS 3
C 
      NDF = NDF2DO
C 
      NTIMES = 0
      DO 900 ITIME = 1, 999999
C 
        IF (IEOF.GE.1 ) GO TO 1000
C GLB change LUIN to LUIN2, NDG to NDF and add NE, (DUM, I = 1, NE)
C GLB-NSF        READ (LUIN2,END=850)   TIMERD, NP1, NDF,  NE,
C GLB-NSF     *       ((XVEL(K,J),K=1,NDF), VVEL(J), J = 1, NP1),
C GLB-NSF     *       (DUM,I = 1, NE)
C GLB-NSF add the following 8 lines
        NDF = 6
        NDFP1 = NDF + 1
C GLB change NE to NE1
        READ (LUIN2,END=850) TIMERD, NP1, NDF, NE1,
     &       NDFS,(IRESAV(K),K=1,NDFS),
     &       ((XVEL(K,J),J = 1, IRESAV(K)),K=1,NDF),
     &       (WSEL(J),J = 1, IRESAV(3)),
     &       (IMAT(J), J = 1, NE1), (NDRY(J), J = 1, NP1),
     &       (DELBED(J),J=1,IRESAV(NDFP1)),(BSHR(J),J=1,IRESAV(NDFS)),
     &       (VVEL(J), J = 1, NP1), (DUM,
     &       J = 1, NE1)
C
        IF( NDF.NE.NDF2DO)
     *      PRINT *, ' User has over-ridden NDF to be=',NDF2DO
C
        TIME = TIMERD
C GLB-ATS add the lowecase code below
  205   IF (ABS(TIME-TIME2GET) .LE. 0.01
     +      .or. time2get .eq. -100.0) THEN
          IGOTIT = 1
        ELSE 
          IF (ITIME.LT.40)  WRITE(*,220) ITIME, TIME
  220     FORMAT(' BE Patient ... SKIPPING RMA10 TS=',I6,
     *           ' HOUR=',F12.6)
          GO TO 900
        END IF 
C 
C ...   DO A MAX VELOCITY/SALINITY CHECK, IF DESIRED
C 
  240   IF (IDEBUG.GT.0 ) THEN
            IF (CHECKS.NE.'Y') GO TO 330
            SALMAX = -999.
            SALMIN =  999.
            NODMIN = -77
            NODMAX = -77
            DO 300 I = 1, NP
               IF( XVEL(4,I).LT.SALMIN) THEN
                   SALMIN = XVEL(4,I)          
                   NODMIN = I
               ENDIF
               IF( XVEL(4,I).GT.SALMAX) THEN
                   SALMAX = XVEL(4,I)
                   NODMAX = I
               ENDIF
  300       CONTINUE
            PRINT *,' ***** just read RMA-10 record *****'
            PRINT *,' Check on Min and Max SALINITY at time= ',TIME
            PRINT *,' Minimum occurs at node=',nodmin,'  was=',SALMIN
            PRINT *,' Maximum occurs at node=',nodmax,'  was=',SALMAX
C
  330       IF (CHECKV.NE.'Y') GO TO 430
            VELMAX = -999.
            VELMIN =  999.
            NODMIN = -77
            NODMAX = -77
            DO 400 I = 1, NP
               RESVEL =(XVEL(1,I)*XVEL(1,I)) + (XVEL(2,I)*XVEL(2,I))
               IF( RESVEL.LT.VELMIN) THEN
                   VELMIN = RESVEL          
                   NODMIN = I
               ENDIF
               IF( RESVEL.GT.VELMAX) THEN
                   VELMAX = RESVEL
                   NODMAX = I
               ENDIF
  400       CONTINUE
            PRINT *,' Check Min and Max VELOCITY at time= ',TIME
            PRINT *,' Minimum occurs at node=',nodmin,'  was=',VELMIN
            PRINT *,' Maximum occurs at node=',nodmax,'  was=',VELMAX
        ENDIF
C
  430   IF( IDEBUG.EQ.0 ) GO TO 530
        DO 500 J = 1, NP1
            IF (CHECKV .EQ. 'Y') THEN
                DO 450 K = 1, 2
                   IF (ABS(XVEL(K,J)) .GT. VLIMIT) 
     *                     WRITE (*,420) J, VECTOR(K), XVEL(K,J)
  420              FORMAT (10X, 'WARNING, NODE ', I6, 
     *                     1X, A1, ' VELOCITY =',F15.1,
     *                     ' EXCEEDS THE LIMIT')
  450           CONTINUE  
            ENDIF
            IF (CHECKS.EQ.'Y') THEN
                IF( ABS(XVEL(4,J)) .GT. SLIMIT)
     *                     WRITE (*,470) J, XVEL(4,J)
  470           FORMAT(10X, ' WARNING FOR NODE ',I6,
     *                 1X,' SALINITY=',F15.1,' EXCEEDS THE LIMIT')
            ENDIF
  500   CONTINUE
C 
C   FOR ALL 3-D NODES, COMPUTE THE ACTUAL DEPTHS & STORE IN XVEL(3,NODE#)       
C 
  530   WRITE (*,*) ' WORKING ON TIME STEP ', ITIME, ' TIME =', TIME
        DO 600 NSURF = 1, NP
C        WSEL(NSURF) = AO(NSURF) + XVEL(3,NSURF)
          VEL(3,NSURF) = XVEL(3,NSURF)
          IF (NREF(NSURF) .GT. 0) THEN
            NBOT = NREF(NSURF) + NDEP(NSURF) - 1
C GLB change the following line to the next line
C            ZWHOLE = ELEV0 - CORD(NBOT,3)
            ZWHOLE = CORD(NSURF,3) - CORD(NBOT,3)
            XVEL(3,NSURF) = 0.0
            DO 570 IZ = 2, NDEP(NSURF)
              NODE = NREF(NSURF) + IZ - 1
C GLB change the following line to the next line
C              ZFRACT = (ELEV0 - CORD(NODE,3)) / ZWHOLE
              ZFRACT = (CORD(NSURF,3) - CORD(NODE,3)) / ZWHOLE
              XVEL(3,NODE) = ZFRACT * XVEL(3,NODE)
  570       CONTINUE
          END IF 
  600   CONTINUE
C 
        NTIMES = NTIMES + 1
C 
        DO 700 ISLICE = 1, NSLICE
          CALL SLICE (ISLICE, IDEBUG, SLIMIT, VLIMIT)
C ...     RMA2 LOOK-A-LIKE WRITE 
          WRITE (ISLICE) TIME, NP, 
     *                  ((VEL(J,K),J=1,3), K = 1, NP), 
     *                  (NDRY(K), K = 1, NP), 
     *                  NE, (IMAT(K), K = 1, NE), 
C GLB change the following line to the next line
C     *                  (ZSEL(K),K=1, NP)
     *                  (WSEL(K),K=1, NP)
          WRITE(*,620) TIME, ISLICE, ZSLICE(ISLICE)
  620     FORMAT(' ** WRITE TIME =',F12.8,'  HYDRO I-O UNIT=  ',I3,
     *           ' DEPTH=',F7.3)
          IF (IDOSAL.EQ.0) GO TO 700
          IISLICE = ISLICE + 10
          NQAL = 1
C ...     RMA4 LOOK-A-LIKE-WRITE
C GLB-NSF replace the following 3 lines with the next 11 lines
C          WRITE (IISLICE) TIME, NQAL, NP,
C GLB-MCS change VEL(4,K) to VEL(ICK+3,K) and add NE,IMAT
C     *      (VEL(ICK+3,K),K=1,NP),NE,(IMAT(K),K=1,NE)
          IF (ICK .LE. 3) THEN
            WRITE (IISLICE) TIME, NQAL, NP,
C GLB-MCS change VEL(4,K) to VEL(ICK+3,K) and add NE
     *      (VEL(ICK+3,K),K=1,NP),NE,(IMAT(K),K=1,NE)
          ELSE IF (ICK .EQ. 4) THEN
              WRITE (IISLICE) TIME, NQAL, NP,
     *        (DELBED(K),K=1,NP),NE,(IMAT(K),K=1,NE)
          ELSE IF (ICK .EQ. 5) THEN
              WRITE (IISLICE) TIME, NQAL, NP,
     *        (BSHR(K),K=1,NP),NE,(IMAT(K),K=1,NE)
          END IF
          WRITE(*,640) TIME, IISLICE, ZSLICE(ISLICE)
  640     FORMAT(' ** WRITE TIME =',F12.8,'  SALINITY I-O UNIT=',I3,
     *           ' DEPTH=',F7.3)          
  700   CONTINUE
C
      PRINT *,' ***** WROTE SLICED RESULT TO RMA4 Look-a-like'
C-
C GLB-ATS add the following if-then clause
      if (time2get .ne. -100.0) then
        WRITE (*,*) ' ******************************************'
        WRITE (*,*) ' ***** You have the Option to do more *****'
        WRITE (*,*) ' ENTER ANOTHER RMA10 DECIMAL HOUR TO PROCESS',
     *              ' (DEFAULT=NO MORE)'
        WRITE (*,*) '         Requested time must be > time =',TIME
        TIME2GET = -9999.
        CALL DCODE(TIME2GET, TIME2GET)
        IF( TIME2GET.LT.-9000 ) GO TO 1000
        PREFIX1OLD = PREFIX1
        PREFIX2OLD = PREFIX2
        WRITE (*,*) ' ENTER THE PREFIX FOR "2D" HYDRO OUTPUT FILES '
        WRITE (*,*) '                           DEFAULT=',PREFIX1
        READ (*,'(A)') PREFIX1
        IF (PREFIX1(1:3) .EQ. '   ') PREFIX1(1:20) = PREFIX1OLD(1:20)
        IF (PREFIX1OLD(1:20).NE.PREFIX1(1:20) ) NEWHYD = 1
        IF (IDOSAL.NE.0) THEN
C GLB-MCS change SALINITY to CONSTITUENT
          WRITE (*,*) ' ENTER THE PREFIX FOR "2D" CONSTITUENT OUTPUT',
     +                ' FILES '
          WRITE (*,*) '                           DEFAULT=',PREFIX2
          READ (*,'(A)') PREFIX2
          IF (PREFIX2(1:3) .EQ. '   ') PREFIX2(1:20) = PREFIX2OLD(1:20)
          IF (PREFIX2OLD(1:20).NE.PREFIX2(1:20) ) NEWSAL = 1
        ENDIF
        IF ( NEWHYD.NE.1) THEN
             PRINT *, ' IDENTICAL HYDRO NAME AS PREVIOUS  --- SO APPEND'
             GO TO 900
        ENDIF
        IF ( NEWSAL.NE.1 ) THEN
C GLB-MCS change SALINITY to CONSTITUENT
        PRINT *, ' IDENTICAL CONSTITUENT NAME AS PREVIOUS --- SO APPEND'
             GO TO 900
        ENDIF
      end if
      if (time2get .eq. -100.0) go to 900
C GLB-ATS
C
C ... OPEN EACH OUTPUT FILE & WRITE BANNERS & HEADERS IN STANDARD RMA2
      DO 800 I = 1, NSLICE
        WRITE (CDEPTH,'(F6.2)') ZSLICE(I)
        HEADER = '  DATA FROM RMA10 3D FILE ' // 
     *       INFILE(1:TRMLEN(INFILE)) // '; DEPTH = ' // CDEPTH
C 
        IF (I .LT. 10) THEN
          WRITE (CSLICE,'(I1)') I
          IL = 1
        ELSE 
          WRITE (CSLICE,'(I2)') I
          IL = 2
        END IF 
C       --------------------------
C ...   HYDRO 2D LOGICAL UNIT NUMBER TO OPEN
        IF (NEWHYD.EQ.1 ) GO TO 750
        OUTFILV(I) = PREFIX1(1:TRMLEN(PREFIX1)) // '_' // 
     *       CSLICE(1:IL) // '.sol'
        write(*,150) i, outfilv(i)
        OPEN(I,FILE=OUTFILV(I),FORM='UNFORMATTED',STATUS='UNKNOWN')
C 
C GLB replace the following 6 lines with the next 2
C        IREC(1) = 39
C        WRITE (I) (BANGFGN(J), J = 1, 4), (IDMS(J),J=1,3),
C     *            (BANRMA2(J), J = 1, 4), (IDMS(J),J=1,3),
C     *            (IDMS(J), J=1,5)
C        WRITE (I) (IREC(J), J = 1, 40), (FREC(J), J = 1, 40)
C        WRITE (I) HEADER
C-...   Process INTEGER type CHARACTER DMS BANNERS on Fake HYDRO
        CALL PUT_BANNER (NE, NP, I, 2)
C       --------------------------
 750   IF ( NEWSAL.EQ.1 ) GO TO 800
        II = I + 10
C ...   SALINITY 2D LOGICAL UNIT NUMBERS TO OPEN
        OUTFILS(I) = PREFIX2(1:TRMLEN(PREFIX2)) // '_' // 
     *       CSLICE(1:IL) // '.sol'
        write(*,150) ii, outfils(i)
        OPEN(II,FILE=OUTFILS(I),FORM='UNFORMATTED',STATUS='UNKNOWN')
C 
C GLB replace the following 6 lines with the next 2
C        IREC(1) = 30
C        WRITE (II) (BANGFGN(J), J = 1, 4), (IDMS(J), J=1,3),
C     *             (BANRMA2(J), J = 1, 4), (IDMS(J), J=1,3),
C     *             (BANRMA4(J), J = 1, 4), (IDMS(J), J=1,3)
C        WRITE (II) (IREC(J), J = 1, 40), (FREC(J), J = 1, 40)
C        WRITE (II) HEADER
C-...   Process INTEGER type CHARACTER DMS BANNERS on Fake RMA4
        CALL PUT_BANNER (NE, NP, II, 4)
C       --------------------------- 
  800 CONTINUE
      GO TO 900
C
  850 IF (ABS(TIME2GET-99999.).LT. .01) THEN
          TIME2GET = TIME
          PRINT *,' Found the last time step ... time (hours)=',TIME
          IEOF = IEOF + 1
          GO TO 240
      ENDIF
C GLB-ATS
      IF (TIME2GET .EQ. -100) THEN
        WRITE(*,*) 'finished at time = ', time
        GO TO 1000
      END IF
C GLB-ATS
C-
  900 CONTINUE
C 
C 
C GLB replace close LUIN with clode LUIN1 and LUIN2
 1000 CLOSE (LUIN1)
      CLOSE (LUIN2)
      PRINT *,' ..... Close ALL active files .....'
      DO 2000 ISLICE = 1, NSLICE
         CLOSE(ISLICE)
 2000 CONTINUE
C 
      WRITE (*,*) 'FOR THIS INPUT FILE, ', NTIMES, ' TIMES STEPS '
     *     , 'PROCESSED'
 3000 CONTINUE
      GO TO 5000
C 
 4000 PRINT *,' ***************** Warning ************************'
      PRINT *,' UNEXPECTED E-O-F or ERROR on RMA10 Geometry Record'
      GO TO 5000 
 4010 PRINT *,' ***************** Warning ************************'
      PRINT *,' UNEXPECTED E-O-F or ERROR on RMA10 Time Step Record'
 5000 END
C 
C GLB insert subroutines PUT_BANNER and CONVRT
      SUBROUTINE PUT_BANNER (NEGEO, NPGEO, LUOUT, IKIND)
C 
C ... LUOUT = Logical unit output #
C ... IKIND = 2 for RMA2    =4 for RMA4 file
C
C ... needed for banners
      COMMON  /DMSCHR/ HEADER, IDMS(15), BANSLC(2),
     *                 BANGFG(5), BANRM2(5), BANRM4(5), EMPTY(5)
      COMMON  /DMSREC/ IREC(40),FREC(40), IPACKB(1200), IPACKT(77)
      CHARACTER        HEADER*77,  IDMS*80, BANSLC*80,
     *                 BANGFG*80, BANRM2*80, BANRM4*80, EMPTY*80
C
      IF ( IKIND.EQ.2 ) THEN
C-...      Process INTEGER type CHARACTER DMS BANNERS on Fake RMA2
           IREC(1) = 429
           CALL CONVRT ( IFAKE2, 1, IFAKE3, 1, 3 )
C-
C ...      WRITE INTEGER type CHARACTER BANNER ..  BINARY HYDRO
C-
           CALL CONVRT ( IDMS, 15, IPACKB, 80, 1 )
C-
           CALL CONVRT ( HEADER, 1, IPACKT, 77, 1 )
C-
           REWIND LUOUT
           MFLG = 120
           WRITE (LUOUT)   MFLG, IREC(1), NPGEO, NEGEO
           IWRT1 = 1200
           WRITE (LUOUT)   IWRT1, (IPACKB(ID),ID= 1,IWRT1)
           IWRT2 = 40
           IWRT3 = 40
           WRITE (LUOUT)   IWRT2, IWRT3,
     *                     (IREC(ID),ID=1, IWRT2),
     *                     (FREC(ID),ID=1,IWRT3)
           IWRT4 = 77
           WRITE (LUOUT)   IWRT4, (IPACKT(ID),ID= 1,IWRT4)
C- 
       ELSEIF ( IKIND.EQ.4 ) THEN
C-...      Process INTEGER type CHARACTER DMS BANNERS on Fake RMA4
           IREC(1) = 427
           CALL CONVRT ( IFAKE2, 1, IFAKE3, 1, 4 )
C-          
C ...      WRITE INTEGER type CHARACTER BANNER ..  BINARY HYDRO
C-
           CALL CONVRT ( IDMS, 15, IPACKB, 80, 1 )
C-
           CALL CONVRT ( HEADER, 1, IPACKT, 77, 1 )
C-
           REWIND LUOUT
           MFLG = 140
           WRITE (LUOUT) MFLG, IREC(1), NPGEO, NEGEO
           IWRT1 = 1200
           WRITE (LUOUT) IWRT1, (IPACKB(ID),ID= 1,IWRT1)
           IWRT2 = 40
           IWRT3 = 40
           WRITE (LUOUT) IWRT2, IWRT3,
     *                   (IREC(ID),ID=1, IWRT2),
     *                   (FREC(ID),ID=1,IWRT3)
           IWRT4 = 77
           WRITE (LUOUT) IWRT4, (IPACKT(ID),ID= 1,IWRT4)
       ELSE
           PRINT *,' --> PUT_BANNER does not recognize '
           PRINT *,'     file type= ', IKIND
           STOP
       ENDIF
       WRITE (*,*) ' --> Integer Banners written on LU=',LUOUT
       WRITE (*,*) '     MFLG=',MFLG,' NE =',NEGEO, ' NP =',NPGEO
       RETURN
       END
C********************************************************************
C 
C ----------------
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
C ... needed for banners
      COMMON  /DMSCHR/ HEADER, IDMS(15), BANSLC(2),
     *                 BANGFG(5), BANRM2(5), BANRM4(5), EMPTY(5)
      COMMON  /DMSREC/ IREC(40),FREC(40), IPACKB(1200), IPACKT(77)
      CHARACTER        HEADER*77,  IDMS*80, BANSLC*80,
     *                 BANGFG*80, BANRM2*80, BANRM4*80, EMPTY*80
C
      CHARACTER  ISTRNG(15)*80
      DIMENSION  INTVAL(MM8)
C-
      IF( ITRACE.GE.1 ) PRINT *,' =+= CALLED CONVRT (ISIZE=',ISIZE,
     *                                '...ISWTCH=',ISWTCH,')'
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
      ELSEIF (ISWTCH .EQ. 3) THEN
C-
C ...   consolidate BANNER section for a RMA2 look alike
C-
        IDMS(1)(1:40)  = BANGFG(1)(1:40)
        IDMS(1)(41:80) = BANGFG(2)(1:40)
        IDMS(2)(1:40)  = BANGFG(3)(1:40)
        IDMS(2)(41:80) = BANGFG(4)(1:40)
        IDMS(3)(1:40)  = BANGFG(5)(1:40)
        IDMS(3)(41:80) = EMPTY(1)(1:40)
        IDMS(4)(1:40)  = EMPTY(1)(1:40)
        IDMS(4)(41:80) = EMPTY(1)(1:40)
        IDMS(5)(1:40)  = EMPTY(1)(1:40)
        IDMS(5)(41:80) = EMPTY(1)(1:40)
C
        IDMS(6)(1:40)  = BANRM2(1)(1:40)
        IDMS(6)(41:80) = BANRM2(2)(1:40)
        IDMS(7)(1:40)  = BANRM2(3)(1:40)
        IDMS(7)(41:80) = BANRM2(4)(1:40)
        IDMS(8)(1:40)  = BANRM2(5)(1:40)
        IDMS(8)(41:80) = EMPTY(1)(1:40)
        IDMS(9)(1:40)  = EMPTY(1)(1:40)
        IDMS(9)(41:80) = EMPTY(1)(1:40)
        IDMS(10)(1:40) = EMPTY(1)(1:40)
        IDMS(10)(41:80)= EMPTY(1)(1:40)
C
        IDMS(11)(1:40) = EMPTY(1)(1:40)
        IDMS(11)(41:80)= EMPTY(2)(1:40)
        IDMS(12)(1:40) = EMPTY(3)(1:40)
        IDMS(12)(41:80)= EMPTY(4)(1:40)
        IDMS(13)(1:40) = EMPTY(5)(1:40)
        IDMS(13)(41:80)= EMPTY(1)(1:40)
        IDMS(14)(1:40) = EMPTY(1)(1:40)
        IDMS(14)(41:80)= EMPTY(1)(1:40)
        IDMS(15)(1:40) = EMPTY(1)(1:40)
        IDMS(15)(41:80)= EMPTY(1)(1:40)
C-
      ELSEIF (ISWTCH .EQ. 4) THEN
C-
C ...   consolidate BANNER section for a RMA4 look alike
C-
        IDMS(1)(1:40)  = BANGFG(1)(1:40)
        IDMS(1)(41:80) = BANGFG(2)(1:40)
        IDMS(2)(1:40)  = BANGFG(3)(1:40)
        IDMS(2)(41:80) = BANGFG(4)(1:40)
        IDMS(3)(1:40)  = BANGFG(5)(1:40)
        IDMS(3)(41:80) = EMPTY(1)(1:40)
        IDMS(4)(1:40)  = EMPTY(1)(1:40)
        IDMS(4)(41:80) = EMPTY(1)(1:40)
        IDMS(5)(1:40)  = EMPTY(1)(1:40)
        IDMS(5)(41:80) = EMPTY(1)(1:40)
C
        IDMS(6)(1:40)  = BANRM2(1)(1:40)
        IDMS(6)(41:80) = BANRM2(2)(1:40)
        IDMS(7)(1:40)  = BANRM2(3)(1:40)
        IDMS(7)(41:80) = BANRM2(4)(1:40)
        IDMS(8)(1:40)  = BANRM2(5)(1:40)
        IDMS(8)(41:80) = EMPTY(1)(1:40)
        IDMS(9)(1:40)  = EMPTY(1)(1:40)
        IDMS(9)(41:80) = EMPTY(1)(1:40)
        IDMS(10)(1:40) = EMPTY(1)(1:40)
        IDMS(10)(41:80)= EMPTY(1)(1:40)
C
        IDMS(11)(1:40) = BANRM4(1)(1:40)
        IDMS(11)(41:80)= BANRM4(2)(1:40)
        IDMS(12)(1:40) = BANRM4(3)(1:40)
        IDMS(12)(41:80)= BANRM4(4)(1:40)
        IDMS(13)(1:40) = BANRM4(5)(1:40)
        IDMS(13)(41:80)= EMPTY(1)(1:40)
        IDMS(14)(1:40) = EMPTY(1)(1:40)
        IDMS(14)(41:80)= EMPTY(1)(1:40)
        IDMS(15)(1:40) = EMPTY(1)(1:40)
        IDMS(15)(41:80)= EMPTY(1)(1:40)
C-
      ELSE
C-
C-
          PRINT *,' --> Value of ISWTCH invalid in CONVRT'
      ENDIF
      RETURN
      END
C***************************************************************
      SUBROUTINE SLICE  (ISLICE, IDEBUG, SLIMIT, VLIMIT)
      SAVE
C 
C  THIS ROUTINE WILL TAKE THE DATA FROM AN RMA10 RUN & RETURN
C  A HORIZONTAL SLICE (AT AN DEPTH = ZSLICE)
C 
C    INPUTS:
C 
C     ZSLICE(ISLICE) = THE DEPTH OF THE HORIZONAL SLICE
C        NPS = THE NUMBER OF SURFACE NODES
C         NE = THE NUMBER OF SURFACE ELEMENTS
C        NOP = THE 2 DIMENSIONAL ARRAY CONTAINING THE ELEMENT CONNECTIONS       
C       NDEP = THE ARRAY CONTAINING THE NUMBER OF NODES ALONG A VERTICAL
C              LINE STARTING AT A SURFACE NODE & INCLUDING THE SURFACE
C              NODE
C       NREF = THE ARRAY CONTAINING THE VALUE OF THE FIRST NODE BELOW
C              A SURFACE NODE (=0 IF THIS IS NOT A 3-D NODE)
C       CORD = THE ARRAY CONTAINING THE X, Y, & Z COORDINATES FOR EACH
C              NODE
C        NDF = THE NUMBER OF DEGREES OF FREEDOM, I.E., THE TYPES OF
C              DATA TO BE RETURNED.
C       XVEL = ARRAY CONTAINING VALUES:
C               XVEL(1,N) = X VELOCITY AT NODE N
C               XVEL(2,N) = Y VELOCITY AT NODE N
C               XVEL(3,N) = DEPTH AT NODE N 
C               XVEL(4,N) = CONCENTRATION AT NODE N
C 
C   OUTPUT:
C        VEL = A 2-D ARRAY CONTAINING THE DATA FOR EACH DEGREE OF
C              FREEDOM AT A DEPTH OF "ZSLICE", INDEXED ACCORDING TO SURFACE NODE
C       NDRY = AN ARRAY SPECIFING WHETHER A NODE IS "WET" (=1),
C              OR "DRY" (=2), OR ABOUT TO BECOME "RE-WET" (=-1)??
C       ZSEL = WATER SURFACE ELEVATION AFTER A SLICE
C       WSEL = WATER SURFACE ELEVATION AT SURFACE NODE 
C 
C****************************************************************
C 
C 
C GLB change MAXNDF to 6
      PARAMETER (MAXNE = 30000, MAXN = 60000, MAXNDF = 6,
C GLB elim. LUIN add LUIN1 and LUIN2 and MAXIM
     *   MAXDEP = 10,   LUIN1 = 55, LUIN2 = 56, MAXIM = 10)
C
      COMMON  /BLKIN/ NP, NE, NDF, NDF2DO, ZSLICE(MAXDEP),
     *                NOP(MAXNE,20), NDEP(MAXN), NREF(MAXN), 
     *                CORD(MAXN,3),  XVEL(MAXNDF,MAXN),AO(MAXN)
C GLB-NSF add the following line
     *                ,DELBED(MAXN),BSHR(MAXN)
C GLB add NSIMATS,IMTFLG2
      COMMON  /BLKOUT/VEL(MAXNDF,MAXN),
     *                VVEL(MAXN), WSEL(MAXN), ZSEL(MAXN),
     *                NDRY(MAXN), IMAT(MAXNE), NSIMATS(MAXIM),
     *                IMTFLG2(MAXN)
      COMMON  /REF/ IREF,REFEL
      DIMENSION NOPSUR(8)
C 
C*****************************************************************
C 
C   FOR EACH SURFACE NODE, DETERMINE THE X & Y VELOCITY AT A DEPTH
C   OF ZSLICE.
C 
      ZZZSLICE = ZSLICE(ISLICE)
      ZZZSAVE = ZZZSLICE

      NPS      = NP
      NOFIND   = 0
C 
      DO 260 I = 1, NPS
C         VEL(3,I) = WSEL(I) - AO(I)
C 
      ZZZSLICE = ZZZSAVE + (WSEL(I) - REFEL)*FLOAT(IREF)
        ZSEL(I) = WSEL(I) - ZZZSLICE
        IF( ZZZSLICE.GT.XVEL(3,I) )  ZSEL(I) = 0.
C
C ...   SET A FLAG IF THIS IS A 1-D OR 2-D (NFREF(I) = 0) OR NOT
C 
        IF (NREF(I) .EQ. 0) THEN
          ISTART = 1
          ISTOP = 8
          ISTEP = 1
        ELSE 
          ISTART = 20
          ISTOP = 1
          ISTEP =  - 1
        END IF 
C 
C   CHECK THROUGH THE ELEMENTS UNTIL ONE IS FOUND WITH NODE I
C 
        DO 110 J = 1, NE
          DO 100 K = ISTART, ISTOP, ISTEP
            KK = K
            IF (NOP(J,K) .EQ. I) GO TO 120
  100     CONTINUE
  110   CONTINUE
C 
C        WRITE (*,*) ' NO ELEMENT TO NODE MATCH FOR NODE #', I
C        WRITE(*,*)'   SEARCHED THROUGH ',NE,' ELEMENTS'
        NOFIND = NOFIND + 1
        NDRY(I) =  - 20
        IF (NOFIND .LT. 100) GO TO 260
C 
        STOP 
C 
C  A MATCH HAS BEEN FOUND...
C 
  120   CONTINUE
C 
C  FIND THE FIRST SURFACE NODE FOR THIS ELEMENT
C 
        KSTART = 1
        DO 130 K = 19, 1,  - 1
          IF (NOP(J,K) .GT. NPS) THEN
            KSTART = K + 1
            GO TO 140
          END IF 
  130   CONTINUE
  140   CONTINUE
C 
C  SET UP A TEMPORARY NODE CONNECTION ARRAY
C 
        KOUNT = 0
        KK = 0
        DO 150 K = KSTART, 20
          IF (NOP(J,K) .NE. 0) THEN
            KOUNT = KOUNT + 1
            NOPSUR(KOUNT) = NOP(J,K)
            IF (NOP(J,K) .EQ. I) KK = KOUNT
          ELSE 
            GO TO 160
          END IF 
  150   CONTINUE
  160   CONTINUE
        IF (KOUNT .LT. 8) THEN
          DO 170 K = KOUNT + 1, 8
  170     NOPSUR(K) = 0
        END IF 
C 
        NODES = I
C 
C  DO CHECKS ON THE MAXIMUM DEPTH
C 
        IF (NREF(NODES) .EQ. 0) THEN
C 
C    FIRST FOR 2-D OR 1-D NODES
C 
C          PRINT *,'     ... THIS IS A 1-D OR 2-D(HORIZ) NODE'
          DO 180 NDFF = 1, NDF
C GLB add .AND. IMTFLG2(NODES) .EQ. 1
            IF (ZZZSLICE .LT. XVEL(3,NODES) .AND.
     *          IMTFLG2(NODES) .EQ. 1) THEN
              VEL(NDFF,I) = XVEL(NDFF,NODES)
c             IF (NDFF .EQ. 3) VEL(3,I) = ZZZSLICE
              NDRY(I) = 1
            ELSE 
              VEL(NDFF,I) = 0.0
              NDRY(I) = 2
C              IF(NDFF.EQ.1) PRINT 119, I, CORD(I,3)
C 119           FORMAT(' DRY (1D/2D) NODE =',I5,', DEPTH =',F10.3)
            END IF 
  180     CONTINUE
          GO TO 260
        ELSE 
C 
C  NEXT, 3-D DEPTHS
C 
          NDEEP = NREF(NODES) + NDEP(NODES) - 1
C          PRINT *,' THIS IS A 3-D NODE'
C          PRINT *,'     DEEPEST DEPTH = ',CORD(NDEEP,3)
C          PRINT *,'     NUMBER OF NODES IN DEPTH =', NDEP(NODES)
C 
C GLB add .OR. IMTFLG2(NODES) .EQ. 0
          IF (ZZZSLICE .GE. XVEL(3,NDEEP) .OR.
     *        IMTFLG2(NODES) .EQ. 0) THEN
C 
C  SLICE TOO DEEP, ZERO THE VELOCITIES
C 
            NDRY(I) = 2
            DO 190 NDFF = 1, NDF
              VEL(NDFF,I) = 0.0
  190       CONTINUE
            GO TO 260
          ELSE 
            NDRY(I) = 1
          END IF 
C 
        END IF 
C 
C  NOW THE HARD PART...3-D NODES ...
C 
        ITYPE = MOD(KK,2)
C 
C  DETERMINE IF THIS IS A CORNER NODE (KK ODD) OR A MID-SIDE
C  NODE (KK EVEN)
C 
C 
        IF (ITYPE .EQ. 0) THEN
C 
C  THIS IS A MID-SIDE NODE...DETERMINE THE LAYER IT IS IN
C 
          DO 200 L = NDEP(NODES), 2,  - 1
            IDEP = L
            NBELOW = NREF(NODES) + L - 1
            NABOVE = NBELOW - 1
C 
            IF (IDEP .EQ. 2) NABOVE = NODES
C 
            IF (ZZZSLICE .LT. XVEL(3,NBELOW) .AND. 
     *          ZZZSLICE .GE. XVEL(3,NABOVE)) THEN
C 
              NLAYER = IDEP - 1
              GO TO 210
            END IF 
  200     CONTINUE
C 
C   NOW CHECK IF THE ADJACENT CORNER NODES HAVE THE SAME OR GREATER
C   NUMBERS OF LAYERS (I.E., THIS SHAPE IS A QUADRILATERAL)
C 
  210     CONTINUE
          NODEL = NOPSUR(KK-1)
C 
C  THE NODE TO THE "RIGHT" OF THE MID-SIDE COULD BE EITHER THE KK+1 OR
C  THE 1ST VALUE IN NOP(J,*)...
C  IT MUST HAVE A NODE VALUES .LE. NPS (MAXIMUM NUMBER OF SURFACE NODES.
C 
          IF (KK .EQ. 8) THEN
            KRIGHT = 1
          ELSE IF (KK .LT. 8) THEN
            IF (NOPSUR(KK+1) .EQ. 0) THEN
              KRIGHT = 1
            ELSE 
              KRIGHT = KK + 1
            END IF 
          END IF 
C 
          NODER = NOPSUR(KRIGHT)
          NLAYL = (NDEP(NODEL) + 1) / 2 - 1
          NLAYR = (NDEP(NODER) + 1) / 2 - 1
          IF (NLAYER .EQ. 0) THEN
            PRINT *, ' XVEL(3,I) = ', XVEL(3,I)
            PRINT *, ' XVEL(3,NREF(I)+1) = ', XVEL(3,NREF(I)+1)
            STOP 
          END IF 
C 
          IF (NLAYL .GE. NLAYER .AND. NLAYR .GE. NLAYER) THEN
C 
C  THIS IS A QUADRILATERAL ELEMENT
C 
C 
C   DETERMINE THE 8 NODES SURROUNDING THIS LAYER, IF QUADRILATERAL
C        FIRST THE NODES DOWN THE LEFT SIDE...(N7, N8, N1)
C 
            NBOT = 2 * NLAYER + 1
            N7 = NREF(NODEL) + NBOT - 3
            N8 = N7 + 1
            N1 = N7 + 2
C 
C   NEXT, THE NODES DOWN THE RIGHT SIDE...(N5, N4, N3)
C 
            N5 = NREF(NODER) + NBOT - 3
            N4 = N5 + 1
            N3 = N5 + 2
C 
C   FINALLY, THE NODES ABOVE & BELOW...(N6, N2)
C 
            N6 = NABOVE
            N2 = NBELOW
C 
            IF (NLAYER .EQ. 1) THEN
              N7 = NODEL
              N6 = NODES
              N5 = NODER
            END IF 
C 
C   DETERMINE THE "LOCAL" COORDINATE, S
C 
            S = 1. - 2. * ((ZZZSLICE-XVEL(3,N6)) /
     *                       (XVEL(3,N2) - XVEL(3,N6)))
            S2 = S * S
C 
C  COMPUTE THE SHAPE FUNCTIONS
C 
            PHI1 = 0.25 * (S2 - 1.)
            PHI2 = 0.50 * (1. - S)
            PHI3 = PHI1
            PHI4 = 0.50 * (1. - S2)
            PHI5 =  - 0.50 * PHI4
            PHI6 = 0.50 * (1. + S)
            PHI7 = PHI5
            PHI8 = PHI4
C 
C 
          ELSE IF (NLAYL .GE. NLAYER .AND. NLAYR .LT. NLAYER) THEN
C 
C  THIS IS A TRIANGLE POINTING TO THE RIGHT
C 
C 
C  DETERMINE THE 6 BOUNDARY NODES HERE...
C 
C  FIRST, THE NODES DOWN THE LEFT SIDE (N1, N2, N3)
C 
            NBOTL = 2 * NLAYER + 1
            N1 = NREF(NODEL) + NBOTL - 3
            N2 = N1 + 1
            N3 = N1 + 2
C 
C  NEXT THE SINGLE NODE ON THE "POINT" (RIGHT SIDE) (N5)
C 
            NBOTR = 2 * NLAYR + 1
            N5 = NREF(NODER) + NBOTR - 1
C 
C   FINALLY, THE NODES ABOVE & BELOW (N6, N4)
C 
            N6 = NABOVE
            N4 = NBELOW
C 
            IF (NLAYER .EQ. 1) THEN
              N1 = NODEL
              N5 = NODER
              N6 = NODES
            END IF 
C 
            N7 = 0
            N8 = 0
C 
C   COMPUTE THE LOCAL COORDINATE, S
C 
            S = 1.0 - ((ZZZSLICE - XVEL(3,N6)) /
     *                    (XVEL(3,N4) - XVEL(3,N6)))
C 
C  COMPUTE THE SHAPE FUNCTIONS
C 
            PHI1 = 0.0
            PHI2 = S * (1 - S)
            PHI3 = S * (S - 1) * 0.5
            PHI4 = 1.0 - S
            PHI5 = 0.0
            PHI6 = S
C 
            PHI7 = 0.0
            PHI8 = 0.0
          ELSE 
C 
C  MUST BE A TRIANGLE POINTING TO THE LEFT
C 
C 
C  DETERMINE THE 6 BOUNDARY NODES HERE...
C 
C  FIRST, THE NODES DOWN THE RIGHT SIDE (N5, N4, N6)
C 
            NBOTR = 2 * NLAYER + 1
            N5 = NREF(NODER) + NBOTR - 3
            N4 = N5 + 1
            N3 = N5 + 2
C 
C  NEXT THE SINGLE NODE ON THE "POINT" (LEFT SIDE) (N1)
C 
            NBOTL = 2 * NLAYL + 1
            N1 = NREF(NODEL) + NBOTL - 1
C 
C   FINALLY, THE NODES ABOVE & BELOW (N6, N2)
C 
            N2 = NBELOW
            N6 = NABOVE
C 
            IF (NLAYER .EQ. 1) THEN
              N1 = NODEL
              N5 = NODER
              N6 = NODES
            END IF 
C 
            N7 = 0
            N8 = 0
C 
C   COMPUTE THE LOCAL COORDINATE, S
C 
            S = 1.0 - ((ZZZSLICE-XVEL(3,N6)) / 
     *                    (XVEL(3,N2) - XVEL(3,N6)))
C 
C  COMPUTE THE SHAPE FUNCTIONS
C 
            PHI1 = 0.0
            PHI2 = 1.0 - S
            PHI3 = S * (S - 1) * 0.5
            PHI4 = S * (1 - S)
            PHI5 = 0.0
            PHI6 = S
C 
            PHI7 = 0.0
            PHI8 = 0.0
          END IF 
C 
C 
        ELSE 
C 
C   FIND THE VALUES ALONG A CORNER...EASIER THAN THE MID-SIDE
C 
C        PRINT *,' THIS IS A CORNER NODE'
          DO 220 L = NDEP(NODES), 3,  - 2
            IDEP = L
            NBELOW = NREF(NODES) + IDEP - 1
            NABOVE = NBELOW - 2
            IF (NABOVE .EQ. NREF(NODES)) NABOVE = NODES
C 
            IF (ZZZSLICE .LT. XVEL(3,NBELOW) .AND. 
     *          ZZZSLICE .GE. XVEL(3,NABOVE)) THEN
              NLAYER = (IDEP + 1) * 0.5 - 1
              GO TO 230
            END IF 
  220     CONTINUE
C 
C  SET THE NODES ALONG THE CORNER (N1, N2, N3)
C 
  230     CONTINUE
          N1 = NABOVE
          N2 = NBELOW - 1
          N3 = NBELOW
C 
          IF (NLAYER .EQ. 1) N1 = NODES
C 
          N4 = 0
          N5 = 0
          N6 = 0
          N7 = 0
          N8 = 0
C 
C   COMPUTE THE "LOCAL" COORDINATE (S) FOR ZSLICE
C 
          S = 1.0 - 2. * ((ZZZSLICE-XVEL(3,N1)) / 
     *                    (XVEL(3,N3) - XVEL(3,N1)))
C 
C  NOW COMPUTE THE 3 SHAPE FUNCTIONS (SET THE OTHERS TO ZERO)
C 
          PHI4 = 0.0
          PHI5 = 0.0
          PHI6 = 0.0
          PHI7 = 0.0
          PHI8 = 0.0
C 
          PHI1 = S * (S + 1.0) * 0.5
          PHI2 = 1.0 - S * S
          PHI3 = S * (S - 1.0) * 0.5
C 
        END IF 
C 
C  INSURE THAT NODE NUMBERS ARE NON-ZERO
C 
        N1 = NOZERO(N1,MAXN)
        N2 = NOZERO(N2,MAXN)
        N3 = NOZERO(N3,MAXN)
        N4 = NOZERO(N4,MAXN)
        N5 = NOZERO(N5,MAXN)
        N6 = NOZERO(N6,MAXN)
        N7 = NOZERO(N7,MAXN)
        N8 = NOZERO(N8,MAXN)
C 
C      PRINT *,' SURROUNDING NODES (N1-8) = ',N1,N2,N3,N4,N5,N6,N7,N8
C      PRINT *,' AND RESPECTIVE DEPTHS:'
C      PRINT 179, CORD(N1,3), CORD(N2,3), CORD(N3,3), CORD(N4,3),
C     +           CORD(N5,3), CORD(N6,3), CORD(N7,3), CORD(N8,3)
C 179   FORMAT(4X,4(F10.2))
C 
C 
        DO 250 NDFF = 1, NDF
          IF (NDFF .NE. 3) THEN
            VEL(NDFF,I) = XVEL(NDFF,N1) * PHI1 + XVEL(NDFF,N2) * 
     *           PHI2 + XVEL(NDFF,N3) * PHI3 + XVEL(NDFF,N4) * 
     *           PHI4 + XVEL(NDFF,N5) * PHI5 + XVEL(NDFF,N6) * 
     *           PHI6 + XVEL(NDFF,N7) * PHI7 + XVEL(NDFF,N8) * 
     *           PHI8
          END IF 
  250   CONTINUE
C 
C  DO THE NEXT NODE POINT
C 
  260 CONTINUE
C 
C   COUNT THE NUMBER OF "DRY" & "WET" NODES
C 
      NUMWET = 0
      NUMDRY = 0
      DO 1000 I = 1, NPS
        IF (NDRY(I) .EQ. 1) THEN
          NUMWET = NUMWET + 1
        ELSE IF (NDRY(I) .EQ. 2) THEN
          NUMDRY = NUMDRY + 1
        END IF 
 1000 CONTINUE
C 
      WRITE (*,2000) NPS, ZZZSLICE, NUMWET, NUMDRY, NOFIND
 2000 FORMAT ( /, 5X, 'FOR ', I5, ' SURFACE NODES AND A DEPTH OF '
     *    , F8.2, ' FEET',  /, 5X, 'THERE ARE ', I5, ' "WET" NODES, '
     *    , I5, ' "DRY" NODES, AND ', I5, ' UNUSED NODES')
C 
      RETURN 
      END
C 
C***************************************************************
C 
      INTEGER FUNCTION TRMLEN(STRING)
      CHARACTER * (*)STRING
C 
      ILAST = LEN(STRING)
      DO 100 I = ILAST, 1,  - 1
        IVAL = ICHAR(STRING(I:I))
        IF (IVAL .GE. 33 .AND. IVAL .LE. 126) GO TO 110
  100 CONTINUE
      TRMLEN = ILAST
      RETURN 
  110 TRMLEN = I
      RETURN 
      END
C 
C***************************************************************
C 
      INTEGER FUNCTION NOZERO(INVAL, IDEFLT)
C 
      IF (INVAL .EQ. 0) THEN
        NOZERO = IDEFLT
      ELSE 
        NOZERO = INVAL
      END IF 
      RETURN 
      END
C 
C********************************************************************
C 
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
