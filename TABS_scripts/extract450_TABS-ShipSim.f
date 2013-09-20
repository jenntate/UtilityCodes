      PROGRAM EXTRACT
C 
C0 ... Author:  Barbara Donnell  CEWES-HE-S    12-16-1993
C 
      PARAMETER (MAXN = 30000, MAXE = 9000)
C 
      CHARACTER *30 GFGFILE, RMAFILE, EXTOUT, IBUFF, TTLAB
      CHARACTER *1 REPLY
C
C-    DATA MANAGEMENT REQUIREMENTS
      COMMON /DMSCHR/BANGFG(15),VERRM2(4),STAMP,DESC(2),
     *               BANRM2(15)
      COMMON /DMSREC/IREC(40),FREC(40),
     *               IPACKB(1200), IPACKT(77), IPACKH(1200)
      CHARACTER      BANGFG*80, BANRM2*80, TGFGEN*77, TRMA2*77
C 
      DIMENSION ALFA(MAXN), CORD(MAXN,2), NOP(MAXE,8),  
     *     TH(MAXE), VEL(3,MAXN), AO(MAXN), WSEL(MAXN),
     *     IFLGEW(MAXE), IFLGNW(MAXN), IMAT(MAXE),
     *     NDRY(MAXN), NFIXH(MAXE)

C ... DIMENSION WIDTH(MAXN),SS1(MAXN),SS2(MAXN),WIDS(MAXN),
C    *          SSS(MAXN), WSCRIT(MAXN)
       
      DATA IFLGEW/MAXE*-1/
      DATA IFLGNW/MAXN*-1/
      DATA IWINDOW/1/
      DATA XMINUS, YMINUS /0,0/
C 
      LOGICAL AROUND
C**********************************************************************
C 
C 
      PRINT *,' *** Program EXTRACT                          ***'
	PRINT *,' *** Version 4.50 Last Modified 2-27-2002     ***'
	PRINT *,' ***'
	PRINT *,' *** Reads GFGEN and RMA2 binary results, converts to ascii'
	PRINT *,' *** Used to pass solutions to ship simulator ***'
      
      PRINT *,' '
   20 PRINT *,' Want to subtract constant from coordinates? (y/n)'
      READ(*,100,END=20) REPLY
      IF (REPLY.EQ.'Y' .OR. REPLY.EQ.'y') THEN
          PRINT *,' Enter  X- and Y-constant to subtract'
          READ(*,*) XMINUS, YMINUS
      ENDIF
   30 PRINT *,' Want to extract the entire mesh domain? (y/n)'
      READ(*,100,END=30) REPLY
      IF (REPLY.EQ.'Y' .OR. REPLY.EQ.'y') THEN
          IWINDOW = 0
          DO 40 I = 1, MAXN
             IFLGNW(I) = 1
   40     CONTINUE
          DO 50 I = 1, MAXE
             IFLGEW(I) = 1
   50     CONTINUE
      ELSE
          PRINT *,' Enter coordinates for the window'
		PRINT *,'     (before subtraction)'
   60     PRINT *,' Enter  X-min and X-max MESH coordinates of window'
          READ(*,*,END=60) XMIN, XMAX
   70     PRINT *,' ENTER  Y-min and Y-max MESH coordinates of window'
          READ(*,*,END=70) YMIN, YMAX
      ENDIF
   80 PRINT *,' Do you want to extract only 1 time step? (y/n)'
      READ(*,100,END=80) REPLY
      IF (REPLY.EQ.'N' .OR. REPLY.EQ.'n') THEN
   90     PRINT *,' Enter time to start and stop extracting (in hours)'
          READ(*,*,END=90) TSTART, TSTOP
          HR2GET = TSTART
          TSTART = TSTART - .00001
          TSTOP  = TSTOP  + .00001
      ELSE
   95     PRINT *, ' Enter the RMA2 HOUR to EXTRACT'
          READ(*,*,END=95) HR2GET
          TSTART = HR2GET - .00001
          TSTOP  = HR2GET + .00001
      ENDIF
C ---------
C
  100 FORMAT (A)
  120 PRINT *, ' ENTER THE GFGEN GEOMETRY BINARY FILENAME (1)'
      READ(*,100) GFGFILE
      INQUIRE (FILE=GFGFILE,EXIST=AROUND)
      IF (.NOT.AROUND) THEN
           PRINT *,' That file not found ... try again'
           GO TO 120
      ENDIF
      IGEON = 1
      OPEN (IGEON,FILE=GFGFILE,STATUS='OLD',FORM='UNFORMATTED')
C-
  130 PRINT *, ' ENTER THE RMA2 BINARY SOLUTION FILENAME (2)'
      READ(*,100) RMAFILE
      INQUIRE (FILE=RMAFILE,EXIST=AROUND)
      IF (.NOT.AROUND) THEN
           PRINT *,' That file not found ... try again'
           GO TO 130
      ENDIF
      IHYDN = 2
      OPEN (IHYDN,FILE=RMAFILE,STATUS='OLD',FORM='UNFORMATTED')
C-
C 
  140 PRINT *,' ENTER THE NAME OF THE EXTRACT OUTPUT FILE (20)'
      READ(*,100) EXTOUT
      INQUIRE (FILE=EXTOUT,EXIST=AROUND)
      IF (.NOT.AROUND) THEN
           PRINT *,' A new file will be created'
      ELSE
           PRINT *,' File name already exists ... will over write'
      ENDIF
      OPEN (20, FILE=EXTOUT, STATUS='UNKNOWN',FORM='FORMATTED')
C
C 
C*************************************************
C  READ GFGEN OUTPUT FILE
C 
          REWIND IGEON
          READ (IGEON)  ITEST
          REWIND IGEON
          PRINT *,'  EXTRACT reading gfgen binary ... itest=',ITEST
C-
          IF (ITEST.GT.200) THEN
C ...        This is a true character variable type read
             READ (IGEON) (BANGFG(I), I = 1, 15)
             READ (IGEON) (IREC(I), I = 1, 40), (FREC(I), I = 1, 40)
             IF( IREC(1).GE.425 ) READ (IGEON) TGFGEN
          ELSE
C ...        This must be an integer style character-- so convert
             READ (IGEON)   MFLG1, MFLG2, MFLG3, MFLG4
             READ (IGEON)  IWRT1, (IPACKB(I), I=1,IWRT1)
             READ (IGEON)  IWRT2, IWRT3,
     *                     (IREC(I), I =1,IWRT2), (FREC(I), I =1,IWRT3)
             READ (IGEON)  IWRT4, (IPACKT(I),I =1,IWRT4)
             CALL CONVRT ( BANGFG, 15, IPACKB, 80, 2)
             CALL CONVRT ( TGFGEN,  1, IPACKT, 77, 2)
             NPGEO = MFLG3
             NEGEO = MFLG4
             IF (NPGEO.GT.MAXN .OR. NEGEO.GT.MAXE) THEN
                 PRINT *,' ********************************************'
                 PRINT *,' --> GFGEN GEOMETRY EXCEEDS PROGRAM DIMENSION'
                 PRINT *,' --> BINARY GEOM CONTAINS    ',MFLG3,MFLG4
                 PRINT *,' --> EXTRACT DIMENSIONED TO  ',MAXN, MAXE
                 PRINT *,' ********************************************'
                 STOP'Dim'
             ENDIF
          ENDIF
C-
          READ (IGEON) N, M,
     *                ((CORD(J,K),K=1,2), ALFA(J), AO(J), J=1,N),
     *                ((NOP(J,K),K=1,8), IMAT(J), TH(J), NFIXH(J),J=1,M)
     
     
C /// this binary geometry read is not needed for this utility
C     IF (IRECG(1) .GE. 450) THEN
C ...     GFGEN has the 1D/2D off-channel storage information    
C         READ (IGEON) (WIDTH(J),SS1(J),SS2(J),WIDS(J),
C    *                  WSCRIT(J), SSS(J), J=1,N)
C ...     GFGEN has marsh porosity parameters (capability starts 2-8-2002)
C         READ (IGEON) IDNOPT      
C         IF(IDNOPT.NE.0) READ(IGEON) N,   
C    *                    (IDMN(J),  WDMC1(J), WDMC2(J), 
C    *                               WDMC3(J), WDMC4(J), J = 1, N)                       
C     ELSE IF ( IRECG(1).GE.425 .AND. IRECG(1).LT. 449) THEN
C         READ (IGEON) (WIDTH(J),SS1(J),SS2(J),WIDS(J), J=1, N)
C     ENDIF
   
     
           NEGEO = M
           NPGEO = N
C-
      PRINT *, ' JUST read GFGEN unit(1)'
      PRINT 150, TGFGEN
  150 FORMAT(' --> GFGENs TITLE= ',A,/)
C ... --------------------------------------- 
C
C ... FLAG ELEMENTS AS POSITIVE IF 100% IN THE WINDOW
      IF ( IWINDOW.EQ.0 ) GO TO 200
      DO 180 I = 1, NEGEO
      DO 170 J = 1, 8
         NODE = NOP(I,J)
         IF ( CORD(NODE,1).GT.XMIN .AND. CORD(NODE,1).LT.XMAX ) THEN
C ...         THE X-CORD IS IN THE WINDOW FOR THIS ELEMENT
              IF ( CORD(NODE,2).GT.YMIN .AND. CORD(NODE,2).LT.YMAX )
     *        IFLGEW(I) = 1
              IFLGNW(NODE) = 1
         ENDIF
  170 CONTINUE
  180 CONTINUE
C             
C ... ---------------------------------------
  200 PRINT *, '  Begin to read Banners from RMA2 binary file' 
C 
      REWIND IHYDN
      READ (IHYDN) ITEST
      REWIND IHYDN
      PRINT *,'  EXTRACT reading rma2 binary ... itest=',ITEST
C
      IF (ITEST.GT.200) THEN
C ...     This is a true character variable type read
          READ (IHYDN) (BANRM2(I), I = 1,15)
          READ (IHYDN) (IREC(I),I=1,40), (FREC(I),I=1,40)
          READ (IHYDN) TRMA2
      ELSE
C ...     This is an integer type variable read
          READ (IHYDN) MFLG1, MFLG2, MFLG3, MFLG4
          READ (IHYDN) IWRT1, (IPACKB(I), I=1, IWRT1)
          READ (IHYDN) IWRT2, IWRT3,
     *                 (IREC(I),I=1,IWRT2), (FREC(I),I=1,IWRT3)
          READ (IHYDN) IWRT4, (IPACKT(I), I=1, IWRT4)
          CALL CONVRT (BANRM2, 15, IPACKB, 80, 2)
          CALL CONVRT (TRMA2,   1, IPACKT, 77, 2)
          NPR2 = MFLG3
          NER2 = MFLG4
          IF (NPR2.GT.MAXN .OR. NER2.GT.MAXE) THEN
              PRINT *,' ******************************************'
              PRINT *,' --> RMA2 HYDRO EXCEEDS PROGRAM DIMENISON  '
              PRINT *,' --> BINARY DATA SET =', MFLG3, MFLG4
              PRINT *,' --> EXTRACT DIMENSIONED TO ', MAXN, MAXE
              PRINT *,' ******************************************'
              STOP'Dim'
          ENDIF
      ENDIF
C
        WRITE (20,280)    TGFGEN, TRMA2, (BANRM2(I), I = 1, 15),  
     *                    IREC(1), IREC(2), IREC(3), TSTART, TSTOP
  280   FORMAT ( /,' --> GFGEN TITLE= ',A,/,
     *             ' --> RMA2  TITLE= ',A,//,
     *          10X, '  RMA2 BANNER HEADINGS',  //,
     *               '  1) ', A80,  /, 4(5X,A80,/),  /,
     *               '  2) ', A80,  /, 4(5X,A80,/), /,
     *               '  3) ', A80,  /, 4(5X,A80,/), /,
     *               ' IREC CONTROL VALUES = ',3I8, /,
     *               ' EXTRACTING RMA2 Hours between=', 2F10.4,/)
C       
        IELEM = 0
        WRITE(20,285)
  285   FORMAT('CO ... Element Connection Table contained within window',
     *  /, 'CO  Elem# <--  The eight nodal connection table  -->',
     *  7x,'IMAT',' Theta')
        DO 300 I = 1, NEGEO
           IELEM = IELEM + 1
           IF (IFLGEW(I).EQ.1) 
     *        WRITE (20,290) I, (NOP(I,K),K=1,8), IMAT(I), TH(I)
  290      FORMAT('GE ', 10I6,F4.1)
  300   CONTINUE
        PRINT *, ' JUST wrote GE CARDS for the domain' 
C 
C  READ RMA2 OUTPUT FILE - Dynamic Section
C 
  320 PRINT *,' **** Begin Dynamic Loop '
      PRINT *,' **** Looking for hours between =', TSTART, TSTOP
      DO 800 IJK = 1, 2000
C ...    READ VERSION 4.0 OR GREATER (STORES WS ELEV)
         ILAST = IJK
         READ (2,END = 900)
     *           TT, NP1,   ((VEL(J,K),J=1,3), K = 1, NP1),
     *                      (NDRY(K), K = 1, NP1), 
     *               NE1,   (IMAT(K), K = 1, NE1),
     *                      (WSEL(K), K = 1, NP1)
         PRINT 420, TT, NE1, NP1
  420    FORMAT(' --> Read RMA2 time='F10.5,'  Elem/nodes=',2I10)
         IF( TT .LT. TSTART ) GO TO 800
         IF( TT .GT. TSTOP )  GO TO 850
         HR2GET = TT
         WRITE(IBUFF,'(F15.6)') TT
         READ(IBUFF,'(A15)') TTLAB
         DO 430 M = 1,15
  425       IF( TTLAB(1:1).EQ.' ') THEN
                TTLAB(1:14) = TTLAB(2:15)
                TTLAB(15:15) = ' '
                GO TO 425
            ENDIF
            IF( TTLAB(M:M).EQ.'.') TTLAB(M:M) = '_'
  430    CONTINUE
  435    WRITE(20,440)  TTLAB, TT
  440    FORMAT('TIME LABEL ', A,/,
     *          'TIME DECIMAL HRS',2X,F15.5)
        WRITE(20,445)
  445   FORMAT('CO ... Nodal Solution contained within requested window',
     *       /,'CO   Node#    X-Cord    Y-Cord Bot-Elev', 
     *         ' Ruff  X-Vel  Y-Vel  WDepth  WS-Elev')  
	   IWROTE = 0
         DO 500 III = 1, NP1
            IF ( IFLGNW(III).NE.1 ) GO TO 500
            IF( NDRY(III).NE.1 ) GO TO 500
            XVEL = VEL(1,III)
            YVEL = VEL(2,III)
            DEP  = VEL(3,III)
            BOTE = AO(III)
            ELEV = WSEL(III)
            VMAG = SQRT( (XVEL*XVEL) + (YVEL*YVEL) )
            XCORD= CORD(III,1) - XMINUS
            YCORD= CORD(III,2) - YMINUS
            FMAN = -99.
	      IWROTE = IWROTE + 1
            WRITE(20,450) III, XCORD, YCORD, BOTE, FMAN,
     *                    XVEL, YVEL, DEP, ELEV
  450       FORMAT('GNW ',I6,2F10.1, F8.2,F6.1,2F7.3, 2F8.2)
  500   CONTINUE
  800 CONTINUE
  850 PRINT 855, IWROTE, TT
  855 FORMAT('  WROTE ',I8,' GNW TYPE CARDS for RMA2 hour=',F12.5)
      STOP 
  900 PRINT *,' END-OF-FILE SEARCHING FOR TIME=',HR2GET
      PRINT *,' Last RMA2 Dynamic Record Attempted=', ILAST
      STOP
      END
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
      PARAMETER (MAXN = 18000, MAXE = 6000)
C
      CHARACTER *30 GFGFILE, RMAFILE, EXTOUT
C
C-    DATA MANAGEMENT REQUIREMENTS
      COMMON /DMSCHR/BANGFG(15),VERRM2(4),STAMP,DESC(2),
     *               BANRM2(15)
      COMMON /DMSREC/IREC(40),FREC(40),
     *               IPACKB(1200), IPACKT(77), IPACKH(1200)
      CHARACTER      BANGFG*80, BANRM2*80, TGFGEN*77, TRMA2*77
C
      DIMENSION ALFA(MAXN), CORD(MAXN,2), NOP(MAXE,8),  
     *     TH(MAXE), VEL(3,MAXN), AO(MAXN), WSEL(MAXN),
     *     IFLGEW(MAXE), IFLGNW(MAXN), IMAT(MAXE),
     *     NDRY(MAXN), NFIXH(MAXE)

C ... DIMENSION WIDTH(MAXN),SS1(MAXN),SS2(MAXN),WIDS(MAXN),
C    *          SSS(MAXN), WSCRIT(MAXN)
C
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
C-
      ELSE
C-
          PRINT *,' --> Value of ISWTCH invalid in CONVRT'
      ENDIF
      RETURN
      END
