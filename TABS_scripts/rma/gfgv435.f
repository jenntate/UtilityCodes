       PROGRAM GFGEN
C 
C     ******************************************************************
C 
C     GFGEN (ALIAS RMA-1)    VERSION 4.35   FORTRAN-77   SEP 1995
C 
C     GENERATES FINITE ELEMENT GRIDS FOR USE BY PROGRAMS
C     RMA-2,  STUDH,  & RMA-4.     1-D AND/OR 2-D ELEMENTS
C     BASIC PROGRAM BY RESOURCE MANAGEMENT ASSOCIATES, LAFAYETTE, CA.
C     MODIFICATIONS BY WATERWAYS EXPERIMENT STATION WES-HL
C         5-1-1991  Incorporated optional widths on the GNN card
C         11-27-91  Collapse Unit=98 now HEC style & works for 1D/2D
C         11-12-92  IBATCH, multi-binary output, DR-Card option
C         12-30-92  Revised Binary Write for general purpose 
C         06-30-93  DISSPLA CGM binary plot (individually labeled)
C         09-08-95  FLAG 1D and 2D control structures
C         03-22-96  Flag open statements to modify for PC Microsoft FORTRAN
C                   C ECGL marks the location
C         06-20-96  Created SUB AUTO_FILE for the super file concept
C         03-06-97  Allow for larger coordinate format for scratch file=18
C         04-18-97  IKing - 'ipk' add pseudo integer*8 and max front width
C                   for 1D/2D reorder option #2
C         06-18-97  Sub MOVFNT, Reorder Option # 2 problem if IMAT=0
C         08-21-97  IKing - 'ipk' add pseudo integer*8 and max front width
C                   for reorder option #1  
C         09-20-97  GWT-Card capability added (St. Johns project)
C         08-25-98  1D Width zero caution lable
C         05-04-00  Auto_File, allow key word 'GEOMETRY_FRL' 
C         09-18-00  Auto_File, allow switch from auto to 'interactive'
C         04-09-01  Sub Auto_file controlled by IBATCH, read super file 
C         05-04-01  Sub File will be controlled by IBATCH  
C
C     ******************************************************************
C     ******************************************************************
      IMPLICIT REAL (A-H, O-Z)
C
      INCLUDE'gfgv435.inc'
C 
      COMMON /PNTS/ XLO, YLO, SHITE
C-
      COMMON /DMSREC/ IREC(40),FREC(40)
C-
      DIMENSION RF(50), NODC(NBN), NODM(NBN), JELE(MAXE), 
     *     NODA(MM2), LISTR(NL,4), NODES(MM2), NODEN(MM2), 
     *     NODEL(MM2), NODEF(NBN), SLOPE(NBN)
C-
      COMMON /DMSCHR/ VERGFG(4), STAMP, DESC(2),
     *                BANGFG(5), BANRM2(5), BANCON(5), BANNER(15),
     *                CHKDMS, RECRD
C
      CHARACTER CHKDMS*6, RECRD*6, IBLANK*1
      CHARACTER BANGFG*80, BANRM2*80, BANCON*80, BANNER*80
      CHARACTER VERGFG*40, STAMP*80, DESC*80
      CHARACTER ICG*1, IDT*1, ISI*1
C-
      LOGICAL ONCE
C-
C                  123456789-123456789-123456789-123456789-
      VERGFG(1) = 'CEWES-CHL DMS BANNER HEADINGS FOR GFGEN '
      VERGFG(2) = 'ENGLISH UNITS APPLIED IN THIS GEOMETRY  '
      VERGFG(3) = 'GFGEN VERSION 4.35 1D AND 2D CAPABILITY '
      VERGFG(4) = 'LAST MODIFICATION DATE:  05-29-2001     '
C-                                             901234567890
      CHKDMS = ' DMS ='
C-
      STAMP(1:40) = ' DMS =                          GRADE = '
      STAMP(41:80)= '    PERSON =                     DESC = '
C-
      DESC(1)(1:40)    = '........................................'
      DESC(1)(41:80)   = '........................................'
      DESC(2)(1:40)    = '........................................'
      DESC(2)(41:80)   = '........................................'
      BANRM2(1)(1:40)  = '........................................'
      BANRM2(1)(41:80) = '........................................'
      BANCON(1)(1:40)  = '........................................'
      BANCON(1)(41:80) = '........................................'
      BANRM2(2)(1:40)  = '........................................'
      BANRM2(2)(41:80) = '........................................'
      BANCON(2)(1:40)  = '........................................'
      BANCON(2)(41:80) = '........................................'
      BANRM2(3)(1:40)  = '........................................'
      BANRM2(3)(41:80) = '........................................'
      BANCON(3)(1:40)  = '........................................'
      BANCON(3)(41:80) = '........................................'
      BANRM2(4)(1:40)  = '........................................'
      BANRM2(4)(41:80) = '........................................'
      BANCON(4)(1:40)  = '........................................'
      BANCON(4)(41:80) = '........................................'
      BANRM2(5)(1:40)  = '........................................'
      BANRM2(5)(41:80) = '........................................'
      BANCON(5)(1:40)  = '........................................'
      BANCON(5)(41:80) = '........................................'
C-
C ... INITIALIZE VARIABLES
C-
      NBNPRT = NBN
      ONCE   = .FALSE.
      HORIZ  = 0
      IBLANK = ' '
      ICOLL  = 0
      IDEBUG = 0
      IECHO  = 1
      IEND   = 99999
      IER    = 0
      IFXSP  = 0
      IFM    = 0
      IGIN   = 0
      IGNBLK = 0
      IHEC   = 1
      IMETRIC= 0
      IN     = 8
      IPO    = 0
C-
      DO 10   I = 1, 40
         IREC(I) = 0
         FREC(I) = 0.0
  10  CONTINUE
C-
      IREC(1)= 435 
      IRGRID = 0
      IRO    = 0
      IRFN   = 0
      IPRT   = 1
      IS     = 0
      ISDB   = 0
      ISLP   = 0
      ITRACE = -1
      IZERO  = 0
      LUOLD  = 0
      KN1    = 0
      KN2    = 0
      KN3    = 0
      KN4    = 1
      KN5    = 1
      NUMNE  = 0
      KN7    = 0
      KN8    = 0
      KN9    = 0
      KN10   = 0
      KN11   = 0
      LP     = 7
      NPRT   = 0
      NSTOP  = 0
      NX     = 0
      NY     = 0
      VERT   = 0.0
      XL     = 0.0
      XR     = 0.0
      XSCALE = 0.0
      YL     = 0.0
      YR     = 0.0
      YSCALE = 0.0
C ...     
      MAXPP = MAXP
      MAXEP = MAXE
      NLP   = NL
      NBNP  = NBN  
      
      DO NE = 1, MAXE
        DO NEI = 1, 8
          NOP(NE,NEI) = 0
        END DO
      END DO
      
      NUMGWT = 0
      NUMGWN = 0      
C ...           English units with  SCALE=1  are the default
      XFACT = 1.
      YFACT = 1.
C-
      WRITE (*,30)  MAXPP, MAXEP, VERGFG(4)(25:40)
   30 FORMAT ( / ,
     *    '     PROGRAM GFGEN version 4.35   ',/,
     *    ' >>>>> Plotting de-activated <<<<< ', /,
     *    ' This executable is dimensioned for', /,
     *    I8, ' Nodes and ', I8, ' Elements ',/,
     *    ' Last modification date: ', A )
C-
C-... Variable IBATCH determines methodology that program prompts/runs 
C ... IBATCH = 0    ! IBATCH =0 indicates interactive file prompt
C ... IBATCH = 1    ! IBATCH =1 indicates true batch, an out-of-date method
C ... IBATCH = 2    ! set for reading set of keyword/filename 
C ... IBATCH = 3    ! set for reading "name" of auto_simulation run file (SMS) 

      IBATCH = 2     
C-
   40 IF (IBATCH .GE. 2) CALL AUTO_FILE  
C-
      DO 50 I = 1, MAXP
        IRDGNN(I) = -1
        WIDTH(I) = 0.
        SS1(I)   = 0.
        SS2(I)   = 0.
        WIDS(I)  = 0.
   50 CONTINUE
      DO 60 I = 1, MAXE
        TH(I)      = 0.0
        NCNODES(I) = 0
        MLIST(I)   = 0
        IMAT(I)    = 0
        IEM(I)     = 0
   60 CONTINUE
      DO 70 I = 1, MM2
        NODA(MM2)  = 0
        NODES(MM2) = 0
        NODEN(MM2) = 0
   70 CONTINUE

      MINMAT =  999999
      MAXMAT = -999999
C 
      IF (LP.GT.0) WRITE (LP,920) 
C 
      READ (IN,100,END=1110) RECRD
      REWIND IN
  100 FORMAT (A6)
C 
C ... CHECK THE FIRST LINE OF INPUT DATA FOR KEY WORD OF BANNERS
      IF (RECRD .EQ. CHKDMS) THEN
        READ (IN,120) STAMP
        READ (IN,120) DESC(1)
        READ (IN,120) DESC(2)
  120   FORMAT (A80)
        IF (LP.GT.0) WRITE (LP,130)  STAMP, DESC(1), DESC(2)
  130   FORMAT ('1', 20X, 'GFGEN BANNER INPUT ', /,3(5X,A80,/))
        READ (IN,140) ICG
  140   FORMAT (A)
        BACKSPACE IN
      ELSE 
        DESC(1)(1:35) = 'NO BANNERS WERE SUPPLIED AS INPUT  '
        PRINT *, DESC(1)(1:35)
      ENDIF
C-
C ... CHECK FOR 'T' IN COLUME #1 TO DETERMINE IF HEC STYLE INPUT
C-
      READ (IN,140,END=1110) ICG
      BACKSPACE IN
      IF (ICG .NE. 'T') THEN
          IHEC = 0
          WRITE (*,150) IN, RECRD, CHKDMS, ICG
          IF (LP .GT. 0) WRITE (LP,150) IN, RECRD, CHKDMS, ICG
  150     FORMAT ( /, ' READING FROM LOGICAL UNIT   --->', I6, /,
     *                ' FIRST RECORD ON INPUT FILE  --->', A6, /,
     *                ' PROGRAM HAS THIS CHECK WORD --->', A6, /,
     *                ' Instead of [T], 1st Letter was =', A1, //,
     *                ' Old- Ian King style format is now expected')
          CALL RMA1
          CALL BEEP (2)
          IF (IBATCH.EQ.0) THEN
            IF (ITRACE.LT.0) THEN
              PRINT *,' PAUSE has been invoked.'
              PRINT *,' Hit ENTER-KEY to clear the screen'
              PAUSE
            ENDIF      
          ENDIF
          PRINT *,' Finished'
          IF (LP .GT. 0) WRITE (LP,'('' Finished'')')
          STOP 
      END IF 
C-
C ... READ TITLE TYPE CARD                                     (T3-CARD)
C-
  160 READ (IN,180,END=1110)         ICG, IDT, TITLE
      IF (IECHO.GT.0 .AND. LP.GT.0) WRITE (LP,190) ICG, IDT, TITLE
  180 FORMAT (2A1, 1X, A)
  190 FORMAT (1X, 2A1, 1X, A)
      IF (ICG .NE. 'T' .OR. IDT .NE. '3') GO TO 160
C 
C ... READ FORMAT CONTROL CARD   -$                            ($F-CARD)
C 
      READ (IN,910) ICG, IDT
      IF (ICG .NE. '$' .OR. IDT .NE. 'F') THEN
        BACKSPACE IN
      ELSE 
        IF (IECHO.GT.0 .AND. LP.GT.0) WRITE (LP,930)  ICG, IDT
        IFM = 1
      END IF 
C ... //////////////// MAIN TABS-MD DATA CARD READ /////////////
C 
  200 CALL READFF (ICG, IDT, ISI, RF, NF, IER, IFM)
      IF (IER.LE.-1) THEN
C ...     End of file has been hit
          IEOF = 1
          GO TO 750
      ENDIF
      IF (IECHO.GT.0 .AND. LP.GT.0)
     *                WRITE (LP,220) ICG, IDT, ISI, (RF(KK),KK=1,NF)
  220 FORMAT (1X, 3A1, 7F10.3, '...', ( /, 4X, 10X, 6F10.3))
C
C ... //////////////////////////////////////////////////////////
C
C ... READ INPUT/OUTPUT UNIT NUMBERS                           ($L-CARD)
C 
  230 IF (ICG .EQ. '$' .AND. IDT .EQ. 'L') THEN
  240     FORMAT (3A1, '   3  0  7')
          IF (RF(1) .LT. 0) THEN
C ...         binary results geometry file
              LUNIT = ABS(RF(1))
C ...         INTENTIONALLY TURNED OFF    LUOLD = ABS(RF(1))
          ELSE IF (RF(1) .GT. 0) THEN
              LUNIT = 3
              LUOLD = 0
C ...         LUOLD WAS INTENTIONALLY TURNED OFF (BPD 12-30-92)
          END IF 
C 
          IF (RF(2) .LT. 0) THEN
C ...         prior binary geometry from another run
              IGIN = ABS(RF(2))
          ELSE IF (RF(2) .GT. 0) THEN
              IGIN = 4
          END IF 
C 
          IF (RF(3) .LT. 0) THEN
C ...         full printout and  gfgen diagnostics
              LP = ABS(RF(1))
          ELSE IF (RF(3) .GT. 0) THEN
              LP = 7
          END IF 
C 
C ....    for interactive run ... ask the user for any $L in/output
          IF (IBATCH .LE. 0) CALL FILE(2)
          ONCE = .FALSE.
C
C ... COMMENT CARD                                            (CO-CARD)
C
      ELSE IF (ICG .EQ. 'C' .AND. IDT .EQ. 'O') THEN
          GO TO 200
C 
C ... READ DREDGE/GLOBAL BOTTOM ELEVATION CHANGE              (DR-CARD)
C 
  280 ELSE IF (ICG .EQ. 'D' .AND. IDT .EQ. 'R') THEN
          IF (ISI .EQ. ' ') THEN
C ...         DREDGE DR(BLANK) ALL BOTTOM ELEVATIONS
              NODE1ST = RF(1)
              DRDALL  = RF(2)
              DO 290 I = NODE1ST, MAXP
                 IF (WD(I) .GT. DRDALL) WD(I) = DRDALL
  290         CONTINUE
          ELSE IF (ISI .EQ. 'T') THEN
C ...         DREDGE BOTTOM ELEVATIONS TOUCHING THIS IMAT
              IMATDR = RF(1)
              DRDALL = RF(2)
              DO 310 J = 1, MAXE
                 IF (IMAT(J) .NE. IMATDR) GO TO 310
                DO 300 K = 1, 8, 2
                   NN = NOP(J,K)
                   IF (NN .EQ. 0) GO TO 300
                   IF (WD(NN) .GT. DRDALL) WD(NN) = DRDALL
  300           CONTINUE
  310         CONTINUE
          ELSE IF (ISI .EQ. 'E') THEN
C ...         DREDGE BOTTOM ELEVATIONS TOUCHING THIS ELEMENT
              IE = RF(1)
              DRDALL = RF(2)
              DO 330 K = 1, 8, 2
                 NN = NOP(IE,K)
                 IF (NN .EQ. 0) GO TO 330
                 IF (WD(NN) .GT. DRDALL) WD(NN) = DRDALL
  330         CONTINUE
          ELSE IF (ISI .EQ. 'N') THEN
C ...         DREDGE BOTTOM ELEVATION FOR THIS NODE
              NN = RF(1)
              DRDALL = RF(2)
             IF (WD(NN) .GT. DRDALL) WD(NN) = DRDALL
          ELSE 
             IF (LP.GT.0) WRITE (LP,340) ICG, IDT, ISI
             WRITE (*,340) ICG, IDT, ISI
  340        FORMAT (' ***  NOT A VALID CARD INPUT ... CARD=', 3A1)
             NSTOP = 1
          END IF 
          ONCE = .FALSE.
C 
C ... READ NODES DESCRIBING BOUNDARIES FOR AUOTMATIC CURVE FIT (GB CARD)
C 
  360 ELSE IF (ICG .EQ. 'G' .AND. IDT .EQ. 'B') THEN
           ISLP = 1
           DO 370 I = 1, NF, 3
              KN8 = KN8 + 1
              IF (KN8 .GT. MM2) THEN
                  NSTOP = 1
                  IF (IECHO.LE.0 .AND. LP.GT.0) THEN
                    WRITE (LP,220) ICG, IDT, ISI, (RF(KK),KK=1,NF)
                    WRITE (LP,1080) 
                  ENDIF
                  GO TO 200
              ELSE 
                  NODES(KN8) =  - RF(I)
                  NODEN(KN8) = RF(I+1)
                  NODEL(KN8) = RF(I+2)
              END IF 
  370      CONTINUE
           ONCE = .FALSE.
C 
C ....READ SLOPES AT BOUNDARY CORNER NODES                   (GC CARD)
C 
  400 ELSE IF (ICG .EQ.'G' .AND. IDT .EQ. 'C') THEN
           ISLP = 1
           IF (ISI .EQ. ' ') THEN
               J = RF(1)
               DO 410 I = J, MAXP
                  IRDGNN(I) = 111
                  ALPHA(I) = RF(2)
  410          CONTINUE
               GO TO 200
           END IF 
           DO 420 I = 1, NF, 2
              IF (RF(I) .LT. 1.0) GO TO 200
              KN7 = KN7 + 1
              IF (KN7 .GT. NBN) THEN
                  IF (IECHO.LE.0 .AND. LP.GT.0) THEN
                    WRITE (LP,220) ICG, IDT, ISI, (RF(KK),KK=1,NF)
                    WRITE (LP,960) 
                  ENDIF
                  NSTOP = 1
              ELSE 
                  J = RF(I)
                  NODC(KN7) = J
                  ALPHA(J) = RF(I+1)
              END IF 
  420     CONTINUE
          ONCE = .FALSE.
C 
C ... READ BOUNDARY MIDSIDE NODES                        (GM CARD)
C 
  440 ELSE IF (ICG .EQ. 'G' .AND. IDT .EQ. 'M') THEN
          DO 450 I = 1, NF
             IF (RF(I) .LT. 1.0) GO TO 200
             KN1 = KN1 + 1
             IF (KN1 .GT. NBN - NF) THEN
                 IF (IECHO.LE.0 .AND. LP.GT.0) THEN
                   WRITE (LP,220) ICG, IDT, ISI, (RF(KK),KK=1,NF)
                   WRITE (LP,960) 
                 ENDIF
                 NSTOP = 1
             ELSE 
                 NODM(KN1) = RF(I)
             END IF 
  450     CONTINUE
          ONCE = .FALSE.
C 
C ... READ GRID DEBUG CONTROLS                         (GF CARD)
C 
  480 ELSE IF (ICG .EQ. 'G' .AND. IDT .EQ. 'F') THEN
  490     FORMAT (3A1, '  1')
          IF (KN2 .LT. 1) THEN
              IDEBUG = RF(1)
              IFXSP  = RF(2)
              IF( NF .GE. 3)  ISDB   = 3
          END IF 
          IF (IDEBUG.GE.4) THEN
C ...         Collapsed geometry files requested (get the filename)
              ICOLL = 98
              IF (IBATCH .LE. 0) CALL FILE (3)
              IF (ICOLL.GT.0) THEN
                WRITE (ICOLL,495)
  495           FORMAT ('T1  GFGEN Collapsed Data Set ')
                WRITE (ICOLL,'(A1,A2,A77)') ICG, IDT, TITLE
                WRITE (ICOLL,496)
  496           FORMAT ('SI  0',/,'$L  3   0  7',/,'GF  1')
              ENDIF      
          ELSE
              CLOSE(ICOLL,STATUS='DELETE')
              ICOLL = 0
          ENDIF      
C ...     Auto adjustment of slope-rule-error mid-side node list
          IF ( NF .LT. ISDB ) GO TO 510
          DO 500 I = ISDB, NF
             IF (RF(I) .LT. 1) GO TO 200
             KN2 = KN2 + 1
             IF (KN2 .GT. NBN) THEN
                 IF (IECHO.LE.0 .AND. LP.GT.0) THEN
                   WRITE (LP,220) ICG, IDT, ISI, (RF(KK),KK=1,NF)
                   WRITE (LP,960) 
                 ENDIF
                 NSTOP = 1
             ELSE 
                 NODA(KN2) = RF(I)
             END IF 
  500     CONTINUE
          ISDB = 1
  510     ONCE = .FALSE.
          GO TO 200
C 
C ... READ AUTOMATIC GRID GENERATOR CONTROLS           (GG CARD)
C 
  520 ELSE IF (ICG .EQ. 'G' .AND. IDT .EQ. 'G') THEN
          IRGRID = 1
          NX = RF(1)
          NY = RF(2)
          XL = RF(3)
          XY = RF(4)
          XR = RF(5)
          YR = RF(6)
          ONCE = .FALSE.
C 
C ... READ GRID REORDERING INSTRUCTIONS                  (GO CARD)
C 
  550 ELSE IF (ICG .EQ. 'G' .AND. IDT .EQ. 'O') THEN
         KKK = 1
         IF (IRO .EQ. 0) THEN
             KKK = 2
             IRO = RF(1)
             IF (IRO .NE.1 .AND. IRO .NE. 2 .AND. IRO .NE. 3) THEN
                 PRINT *,'****************************************'
                 PRINT *,'*** REODER Option on GO-Card unknown ***'
                 PRINT *,'*** User has a value of ',IRO,'  ***'
                 PRINT *,'*********** Keep going *****************'
                 CALL BEEP (2)
             ENDIF
         ENDIF
C 
C ...    CHECK TO VERIFY IF REORDERING DESIRED (IRO=3 IS FOR
C ...    BACKWARD COMPATABILITY FOR OLD GFGEN INPUTS
C 
         IF (IRO .EQ. 1 .OR. IRO .EQ. 2 .OR. IRO .EQ. 3) THEN
            DO 560 I = KKK, NF
               IF (RF(I) .LE. 0.) THEN
                   IF (KN4 .GT. NL) THEN
                       NSTOP = 1
                       IF (IECHO.LE.0 .AND. LP.GT.0) THEN
                         WRITE (LP,220) ICG, IDT, ISI, (RF(KK),KK=1,NF)
                         WRITE (LP,1070) 
                       ENDIF
                   ELSE 
                       LISTO(KN4) = KN10
                       LISTN(KN4,KN10+1) = 0
                       KN4 = KN4 + 1
                       KN10 = 0
                   END IF 
                   GO TO 200
               ELSE 
                   KN10 = KN10 + 1
                   LISTN(KN4,KN10) = RF(I)
                   IF (KN10 .GT. MM2) THEN
                       NSTOP = 1
                       IF (IECHO.LE.0 .AND. LP.GT.0) THEN
                         WRITE (LP,220) ICG, IDT, ISI, (RF(KK),KK=1,NF)
                         WRITE (LP,1080) 
                       ENDIF
                   END IF 
               END IF 
  560       CONTINUE
         ENDIF
         ONCE = .FALSE.
C 
C ... READ GRID REFINING INSTRUCTIONS                  (GR CARD)
C 
  580 ELSE IF (ICG .EQ. 'G' .AND. IDT .EQ. 'R') THEN
          DO 590 I = 1, NF
             IF (RF(I) .LE. 0) THEN
                 KN5 = KN5 + 1
                 IF (KN5 .GT. NL) THEN
                     NSTOP = 1
                     IF (IECHO.LE.0 .AND. LP.GT.0) THEN
                       WRITE (LP,220) ICG, IDT, ISI, (RF(KK),KK=1,NF)
                       WRITE (LP,1070) 
                     ENDIF
                     GO TO 200
                 ELSE 
                     IRFN = 1
                 END IF 
                 GO TO 200
              ELSE 
                 LISTR(KN5,IRFN) = RF(I)
                 IRFN = IRFN + 1
              END IF 
  590     CONTINUE
          ONCE = .FALSE.
C 
C ... READ BOUNDARY SLOPES TO OVERRIDE AUTOMATIC CURVE FITTING (GS CARD)
C 
  610 ELSE IF (ICG .EQ. 'G' .AND. IDT .EQ. 'S') THEN
          DO 620 I = 1, NF, 2
             KN9 = KN9 + 1
             IF (KN9 .GT. MM2) THEN
                 NSTOP = 1
                 IF (IECHO.LE.0 .AND. LP.GT.0) THEN
                   WRITE (LP,220) ICG, IDT, ISI, (RF(KK),KK=1,NF)
                   WRITE (LP,1080) 
                 ENDIF
                 GO TO 200
             ELSE 
                 NODEF(KN9) = RF(I)
                 SLOPE(KN9) = RF(I+1)
             END IF 
  620    CONTINUE
         ONCE = .FALSE.
C 
C ... READ GRID SCALES -                               (GX CARD)
C 
  640 ELSE IF (ICG .EQ. 'G' .AND. IDT .EQ. 'X') THEN
         IF (RF(1) .GT. 0.) XFACT = RF(1)
         IF (RF(2) .GT. 0.) YFACT = RF(2)
         ONCE = .FALSE.
C 
C ... READ  PLOT OPTIONS                               (PO CARD)
C 
  650 ELSE IF (ICG .EQ. 'P' .AND. IDT .EQ. 'O') THEN
         IPO    = 1
         IPNN   = RF(1)
         IPEN   = RF(2)
         HORIZ  = RF(3)
         VERT   = RF(4)
         XSCALE = RF(5)
         YSCALE = RF(6)
         AR     = RF(7)
         HITEL  = 0.2
         HITNN  = 0.2
         IF (RF(8) .GT. 0.) HITEL = RF(8)
         IF (RF(9) .GT. 0.) HITNN = RF(9)
         ONCE = .FALSE.
C 
C ... READ PARTIAL --WINDOW-- PLOT OPTIONS                 (PP CARD)
C 
  670 ELSE IF (ICG .EQ. 'P' .AND. IDT .EQ. 'P') THEN
         IPP = 1
         NXPMIN = RF(1)
         NXPMAX = RF(2)
         NYPMIN = RF(3)
         NYPMAX = RF(4)
         ONCE = .FALSE.
C
C ... METRIC UNITS    //SYSTEM INTERNATIONAL UNITS//       (SI CARD)
C
  680 ELSE IF (ICG .EQ. 'S' .AND. IDT .EQ. 'I') THEN
          IF (RF(1) .GT. 0.5) THEN
C ...         THE USER HAS SPECIFIED  METRIC UNITS FOR INPUT AND OUTPUT
              IMETRIC = 1
              VERGFG(2)(1:7) = 'METRIC '
          ENDIF
          ONCE = .FALSE.
C
C ... TRACE DEBUG    //TRACE DEBUG//                       (TR CARD)
C
  685 ELSE IF (ICG .EQ. 'T' .AND. IDT .EQ. 'R') THEN
          IF (RF(1) .GT. 0.5) THEN
C ...         THE USER HAS SPECIFIED A SUBROUTINE TRACE
              ITRACE = 1        ! TR-Card activated
          ENDIF
C 
C ... READ ELEMENT ARRAY                                   (GE CARD)
C 
  690 ELSE IF (ICG .EQ. 'G' .AND. IDT .EQ. 'E') THEN
         NUMNE = NUMNE + 1
         IF (NUMNE .GT. MAXE) THEN
             NSTOP = 1
             IF (IECHO.LE.0 .AND. LP.GT.0) THEN
               WRITE (LP,220) ICG, IDT, ISI, (RF(KK),KK=1,NF)
               WRITE (LP,1050) 
             ENDIF
          ELSE 
             J = RF(1)
             JELE(NUMNE) = J
             IMAT(J) = RF(10)
             IF (RF(11) .GT. 0.) THETA = RF(11)
             TH(J) = THETA
             DO 700 I = 2, 9
                I1 = I - 1
                NOP(J,I1) = RF(I)
  700        CONTINUE
             IF (IMAT(J).GT.MAXMAT) MAXMAT = IMAT(J)
             IF (IMAT(J).LT.MINMAT) MINMAT = IMAT(J)
          END IF 
          ONCE = .FALSE.
C ...     Turn off the echo for all but the first GE-CARD
          IECHO = -1
          IF (NUMNE.LE.1 .AND. LP.GT.0) WRITE (LP,710)
  710     FORMAT(' -----> card echo has been turned off <----- ')
C 
C ... READ  NODE LOCATIONS & BED ELEVATIONS  -GN TYPE     (GNN CARD)
C 
  720 ELSE IF (ICG .EQ. 'G' .AND. IDT .EQ. 'N') THEN
          IF (ISI .EQ. ' ') THEN
              WDALL = RF(1)
              IGNBLK = 1
              GO TO 200
          END IF 
C-
C ...     OVERIDE GN(BLANK) IF WD FIELD IS SPECIFIED ON GNN CARD
          IF (ISI .NE. 'N') THEN
              PRINT *,' --> THE CARD ', ICG,IDT,ISI, ' is NOT valid'
              GO TO 200
          ENDIF
          IF (RF(1) .LT. 1) GO TO 200
          IF (KN3 .EQ. MAXP) KN3 = RF(1)
          IF (KN3 .LT. MAXP) KN3 = KN3 + 1
          IF (KN3 .GT. MAXP) THEN
              IF (IECHO.LE.0 .AND. LP.GT.0) THEN
                WRITE (LP,220) ICG, IDT, ISI, (RF(KK),KK=1,NF)
                WRITE (LP,1060) 
              ENDIF
              NSTOP = 1
          ELSE 
C ...         VALID GNN CARD
              KN11 = KN11 + 1
              J         = RF(1)
              IRDGNN(J) = 111
              CORD(J,1) = RF(2)
              CORD(J,2) = RF(3)
              WD(J)     = RF(4)
              IF (NF .GT. 4) THEN
                  WIDTH(J) = RF(5)
                  SS1(J)   = RF(6)
                  SS2(J)   = RF(7)
                  WIDS(J)  = RF(8)
              END IF 
              IF (IGNBLK .EQ. 1) WD(J) = WDALL
          END IF 
          ONCE = .FALSE.
C 
C ... READ 1D NODE WIDTH/SIDE SLOPES/ AND STORAGE WIDTH ON -GWN CARDS
C 
  730 ELSE IF (ICG .EQ. 'G' .AND. IDT .EQ. 'W' .AND. ISI .EQ. 'N') THEN
C ...      GWN CARD PROCESSING
           J = RF(1)
           WIDTH(J) = RF(2)
           SS1(J)   = RF(3)
           SS2(J)   = RF(4)
           WIDS(J)  = RF(5)
           NUMGWN   = NUMGWN + 1
           ONCE = .FALSE.    
           
  740 ELSE IF (ICG .EQ. 'G' .AND. IDT .EQ. 'W' .AND. ISI .EQ. 'T') THEN 
C ...      GWT CARD PROCESSING ADDED TO LIBRARY CODE 9-1997
C ...          This section added via request of St. Johns Project
           IMATREAD = RF(1)
           DO I = 1, NUMNE
              IF (IMATREAD .EQ. IMAT(I)) THEN 
C ...             imat matches, it better be for off-channel storage elements              
                  DO NEI = 1, 8
                     NODE = NOP(I,NEI)
                     IF (NODE.GT.0) THEN 
                         WIDTH(NODE) = RF(2)
                         SS1(NODE)   = RF(3)
                         SS2(NODE)   = RF(4)
                         WIDS(NODE)  = RF(5)
                         NUMGWT   = NUMGWT + 1               
                         ONCE = .FALSE. 
                     ENDIF   
                  END DO
              ENDIF
           END DO         
C-
C ... MARK THE END OF THE DECK WITH AN 'END-CARD'
C
      ELSE IF (ICG .EQ. 'E' .AND. IDT .EQ. 'N' .AND. ISI .EQ. 'D') THEN
          GO TO 750
C-
C ... NO SUCH CARD IS DEFINED (IGNORE IT)
C
      ELSE
           PRINT *,' --> THE CARD ',  ICG, IDT, ISI, ' is not supported'
           GO TO 200
      ENDIF
C-
C ... GO GET ANOTHER CARD ... KEEP GOING UNTIL E-O-F IS HIT
      GO TO 200
C-
C ... //////////////////////////////////////////////////////////////
C-
C 
C ... RUN WILL TERMINATE IF THERE ARE ERRORS IN INPUT CARDS ...
C 
  750 WRITE (*,1140)  IN,NUMNE,MINMAT,MAXMAT,KN11,NUMGWN,NUMGWT
      IF (LP.GT.0) 
     *WRITE (LP,1140) IN,NUMNE,MINMAT,MAXMAT,KN11,NUMGWN,NUMGWT
      IF (NSTOP .GT. 0) THEN
          WRITE (*,755)  NSTOP
          IF (LP.GT.0) WRITE (LP,755) NSTOP
  755     FORMAT (/, '*** RUN TERMINATED BECAUSE ',
     *            I6,' ERRORS WERE FOUND IN CARD INPUT ***')
          IF (IBATCH.EQ.0) THEN
            IF (ITRACE.LT.0) THEN
              PRINT *,' PAUSE invoked, unrecoverable error detected.'
              PRINT *,' Hit ENTER-KEY to clear the screen'
              PAUSE
            ENDIF          
          ENDIF
          STOP'Error'
      END IF 
C- 
C ... Try to catch those bugs - 
          
      do nel = 1, maxe
        if(imat(nel).gt.0 .and. nop(nel,6).eq.0)then
         
          do nc1d = 1, 5
             node1d = nop(nel,nc1d)
             if(node1d.gt.0.and. width(node1d).le.0.)
     &        write(*,756) node1d
  756         format(' --> 1D node=',I8,' caution: width not defined')
          end do
        end if

C ...   Make sure junctions and control structures have no-side slope

        if(imat(nel).ge.900) then
          do nc1d = 1, 6
            node1d = nop(nel,nc1d)
            if(node1d.gt.0) then
              ss1(node1d) = 0.           
              ss2(node1d) = 0.
            end if
          end do
        end if
      end do


      IF (IRFN .GT. 0) IRFN = 1
C ... BpD what is this for      IF (KN2 .GT. 0)  IFXSP = 1
C 
C ........ REWRITE INPUT FILE FOR SUBSEQUENT READS IN RMA1 ..........
C 
      INRMA1 = 18
      IN = INRMA1
      IF (ITRACE.LE.0)  THEN
C ...     opening as scratch will automatically make unit 18 disappear 
          OPEN(UNIT=18,STATUS='SCRATCH')
      ELSE IF (ITRACE.GE.1) THEN
          PRINT *,' Scratch file unit=18 will be saved for debug'
          OPEN(UNIT=18,STATUS='UNKNOWN')
      ENDIF
      IF (RECRD .EQ. CHKDMS) THEN
          WRITE (INRMA1,760) STAMP, DESC(1), DESC(2)
  760     FORMAT (A80,  /, A80,  /, A80)
      END IF 
      WRITE (INRMA1,980) TITLE
      WRITE (INRMA1,990) ISLP, IPRT, IPNN, IPEN, IPO, IRO, IPP, 
     *             IRFN, IGIN, LUOLD, NPRT, IRGRID, IDEBUG, IFXSP
      WRITE (INRMA1,1000) HORIZ, VERT, XSCALE, YSCALE, AR, XFACT, 
     *                    YFACT, HITEL, HITNN
      IF (IRGRID .GT. 0) WRITE (INRMA1,1010) NX, NY, XL, XY, XR, YR
C 
      IF (IPP .GT. 0) WRITE (INRMA1,990) NXPMIN, NXPMAX, NYPMIN, 
     *                                   NYPMAX
      IF (ISLP .GT. 0) THEN
        IF (KN7 .LT. 1) GO TO 780
        DO 770 I = 1, KN7
          J = NODC(I)
          WRITE (INRMA1,1030) J, ALPHA(J)
  770   CONTINUE
  780   IF (KN8 .GT. 0) THEN
          DO 790 I = 1, KN8
  790     WRITE (INRMA1,1010) NODES(I), NODEN(I), NODEL(I)
        END IF 
        IF (KN9 .GT. 0) THEN
          DO 800 I = 1, KN9
            WRITE (INRMA1,1090) IZERO, NODEF(I), SLOPE(I)
  800     CONTINUE
        END IF 
        WRITE (INRMA1,990) IEND
        IF (KN1 .LT. 1) GO TO 820
        DO 810 I = 1, KN1
          WRITE (INRMA1,1010) NODM(I)
  810   CONTINUE
  820   WRITE (INRMA1,990) IEND
      END IF 
C 
      DO 830 I = 1, NUMNE
        J = JELE(I)
        WRITE (INRMA1,1020) JELE(I), (NOP(J,K), K = 1, 8), 
     *                                IMAT(J), TH(J)
  830 CONTINUE
      WRITE (INRMA1,990) IEND
C 
      DO 840 I = 1, MAXP
        IF (IRDGNN(I) .GT. 1) THEN
          J = I
          WRITE (INRMA1,850) J, CORD(J,1), CORD(J,2), WD(J), 
     *                       WIDTH(J), SS1(J), SS2(J), WIDS(J)
        END IF 
  840 CONTINUE
C ... Modify format statement to handle large coordinates 3-6-97
  850 FORMAT (I10, 2F15.3, 2F10.2, 2F10.6, F10.2)
      WRITE (INRMA1,990) IEND
C 
      IF (IFXSP .GT. 0) THEN
        DO 860 I = 1, KN2
          if(noda(i).gt.99999)
     *    print *,' IFXSP NODA exceeds 5 dig',NODA(I)
          WRITE (INRMA1,990) NODA(I)
  860   CONTINUE
        WRITE (INRMA1,990) IEND
      END IF 
C 
      IF (IRFN .GT. 0) THEN
        DO 870 I = 1, IRFN
          IF (LISTR(I,1) .GT. 0) WRITE (INRMA1,990) (LISTR(I,J),J=1,4)
  870   CONTINUE
        WRITE (INRMA1,990) IEND
      END IF 
C 
      IF (IRO .EQ. 1 .OR. IRO .EQ. 2 .OR. IRO .EQ. 3) THEN
        KN4 = KN4 - 1
c       print *,' ===> debug ... total reorder lists KN4=', KN4
        DO 880 I = 1, KN4
          WRITE (INRMA1,990) (LISTN(I,J), J = 1, LISTO(I))
          IF (MOD(LISTO(I),16) .EQ. 0) WRITE (INRMA1,910) IBLANK
  880   CONTINUE
        WRITE (INRMA1,990) IEND
      END IF 
C 
      REWIND INRMA1
C 
      CALL RMA1
C-
      WRITE(*,*) '--> GFGEN has Finished'
      IF (LP .GT. 0) WRITE (LP,'('' --> GFGEN has Finished'')')

      IF (IBATCH.EQ.0) THEN
          CALL BEEP (1)
          IF (ITRACE.LT.0) THEN
              PRINT *,' PAUSE has been invoked. GFGEN Finished'
              PRINT *,' Hit ENTER-KEY to clear the screen'
              PAUSE
          ENDIF
      ENDIF
      STOP
      
C-... Variable IBATCH determines methodology that program prompts/runs 
C ... IBATCH = 0    ! IBATCH =0 indicates interactive file prompt
C ... IBATCH = 1    ! IBATCH =1 indicates true batch, an out-of-date method
C ... IBATCH = 2    ! set for reading set of keyword/filename 
C ... IBATCH = 3    ! set for reading "name" of auto_simulation run file (SMS) 
C 
  910 FORMAT (2A1, 1X, A)
  920 FORMAT ('1',  // 10X, 'PROGRAM GFGEN VERSION 4.35 JUNE 1996'// )
  930 FORMAT (1X, 2A1, 2X, A)
  940 FORMAT (1X, 3A1, 8G11.5, ( /, 4X, 8G11.5))
  960 FORMAT (1X, '**** ABOVE CARD CAUSES ARRAY TO OVERRUN, INCREASE ',
     *            'VALUE OF NBN ****')
  980 FORMAT (A)
  990 FORMAT (16I5)
 1000 FORMAT (7G10.4, 2F5.2)
 1010 FORMAT (8I10)
 1020 FORMAT (10I5, F10.4)
 1030 FORMAT (I10, 3G10.4)
 1040 FORMAT ( /, 1X, '**** ABOVE CARD IS NOT RECOGNIZED AS A VALID '
     *     , 'INPUT CODE')
 1050 FORMAT (1X, '**** ABOVE CARD CAUSES ARRAY TO OVERRUN, INCREASE '
     *     , 'VALUE OF MAXE ****')
 1060 FORMAT (1X, '**** ABOVE CARD CAUSES ARRAY TO OVERRUN, INCREASE '
     *     , 'VALUE OF MAXP ****')
 1070 FORMAT (1X, '**** ABOVE CARD CAUSES ARRAY TO OVERRUN, INCREASE '
     *     , 'VALUE OF NL ****')
 1080 FORMAT (1X, '**** ABOVE CARD CAUSES ARRAY TO OVERRUN, INCREASE '
     *     , 'VALUE OF MM2 ****')
 1090 FORMAT (2I10, E10.4)
 1100 FORMAT (1X, 3A1, 11G11.5)
 1110 IF( LP.GT.0 .AND. LP.NE.6) WRITE(LP,1120) IN
      WRITE(*,1120)  IN
 1120 FORMAT(' ---> UNEXPECTED END-OF-FILE ON INPUT LU=',I3,' <---',/)
C-
      IF (IBATCH.EQ.0) THEN
         CALL BEEP (2)
         IF (ITRACE.LT.0) THEN
              PRINT *,' PAUSE invoked, end-of-file detected.'         
              PRINT *,' Hit ENTER-KEY to clear the screen. EOF'
              PAUSE
          ENDIF
      ENDIF
      STOP'EOF'
 1140 FORMAT(/,
     *  ' FINISHED READING GFGEN CONTROL CARDS FROM LU=',I3,/,
     *  ' --> Total number of  GE-Cards read  =',I7,/,
     *  '         Smallest IMAT # in the mesh =',I7,/,
     *  '         Largest IMAT # in the mesh  =',I7,/,
     *  ' --> Total number of nodes affected by GNN-Cards =',I7,/,
     *  ' --> Total number of nodes affected by GWN-Cards =',I7,/,
     *  ' --> Total number of nodes effected by GWT-Cards =',I7,//)
      END

cipk jan97
      block data
      IMPLICIT REAL (A-H, O-Z)
      INCLUDE'gfgv435.inc'      
      DATA LISTO /NL*0/
      end
cipk jan97
C
      SUBROUTINE RMA1
C   ********************************************************************** 
C   VERSION 4.30 2D control structure diagnostic  B.P.Donnell  09/1995     
C   VERSION 4.27 BATCH OR INTERATIVE CAPABILITY
C        CRAY-YMP/VAX8800/MACINTOSH-PC        B.P.DONNELL      12/1992
C   VERSION 4.2  MODIFIED TO ACCOMODATE 1-D WIDTH AND TO INCORPORATE
C        THE 1-D INFORMATION DIRECTLY TO THE BINARY FILE
C        CRAY-YMP                             B.P.DONNELL      05/1991
C        1-D AND 2-D COLLAPSE FIXED           B.P.DONNELL      11/1991
C   VERSION 4.0  FOR 1-D AND OR 2-D ELEMENT GEOMETRY GENERATION
C        TACOM CRAY2, WES CRAY-YMP
C        T.S.DOZIER, D.JOHNSON, G. LYNCH, AND B.P.DONNELL      07/1988
C   **********************************************************************      
C   MODIFIED FOR USE ON BCS CRAY COMPUTERS. TWO NEW ROUTINES HAVE BEEN
C        ADDED---- /RGRID/ AND /GRBUGS/. THESE ROUTINES PROVIDE FOR
C        RECTANGULAR GRID GENERATION AND GRID CHECKING (DEBUGGING). ALSO,       
C        USER INPUT OF NODE AND ELEMENT NUMBER SIZES IS NOW AVAILABLE,
C        IF THESE INPUT FIELDS ARE LEFT BLANK,THE PROGRAM WILL GENERATE
C        REASONABLE VALUES FOR THESE VARIABLES.    S.A. ADAMEC, JR  08/82       
C   **********************************************************************      
C   VERSION 3.01  CREATES UP TO 5 PLOTS.     BARBARA DONNELL
C        1 -----GRID PLOT WITH NO NUMBERS
C        2 -----GRID PLOT WITH ELEMENT NUMBERS ONLY
C        3 -----GRID PLOT WITH CORNER AND MIDSIDE NODE NUMBERS ONLY
C        4 -----GRID PLOT WITH ELEMENT TYPE (IMAT) NUMBERS ONLY
C        5 -----GRID PLOT WITH CORNER DEPTHS ONLY
C   ********************************************************************
C   DATA MANAGEMENT READ-WRITE BY DONNELL--11/81
C   ********************************************************************
C   SLOPE RULE DIAGNOSTICS AND ADJUSTMENTS ARE PERFORMED BY ROUTINES
C         'FIXSLP', 'FNDANG', 'CKNODE', AND 'MIDPNT'   DON BACH 10/81
C   ********************************************************************
C   AUTOMATIC SLOPE GENERATION FEATURE ADDED TO RMA1    03/83
C          ALGORITHM ORIGINALLY DEVELOPED BY CHARLIE BERGER, WESHE2
C          WAS CONVERTED TO SUBR -SLOPES- WITH ADDITIONAL UTILITY SUBR
C         -BOUNDS- AND IMPLEMENTED  BY JAMES D. ETHRIDGE JR. WES-HE3
C   ********************************************************************
C 
      IMPLICIT REAL (A-H, O-Z)
      INCLUDE'gfgv435.inc'
C
C
      COMMON /DMSCHR/ VERGFG(4), STAMP, DESC(2),
     *                BANGFG(5), BANRM2(5), BANCON(5), BANNER(15),
     *                CHKDMS, RECRD
      COMMON /DMSREC/ IREC(40),FREC(40)
C
      CHARACTER CHKDMS*6, RECRD*6 
      CHARACTER BANGFG*80, BANRM2*80, BANCON*80, BANNER*80
      CHARACTER VERGFG*40, STAMP*80, DESC*80
C
      DIMENSION INX(8), CX(2), ISPFX(100)
      DIMENSION NOPL(MAXP,3), NODES(25,3)
      DIMENSION IPACKB(1200), IPACKT(77)
      DATA NSLP, NSLPMX/0, 25/
      DATA IPACKB /1200*0/
      DATA IPACKT /77*0/
C-
      IF( ITRACE.GE.2 ) PRINT *,' =+= CALLED RMA1'
C-
C ... CHECK THE FIRST LINE OF INPUT DATA FOR KEY WORD OF BANNERS
      READ (IN,100) RECRD
  100 FORMAT (A6)
      IF (RECRD .EQ. CHKDMS) THEN
        REWIND IN
        READ (IN,110) STAMP
        READ (IN,110) DESC(1)
        READ (IN,110) DESC(2)
  110   FORMAT (A80)
      ELSE 
        REWIND IN
        DESC(1)(1:35) = 'NO BANNERS WERE SUPPLIED AS INPUT  '
      END IF 
C 
      IF (LP.GT.0) WRITE (LP,130)
     *              (VERGFG(I), I = 1, 4), STAMP, DESC(1), DESC(2),
     *              (BANRM2(I), I = 1, 5), (BANCON(I), I = 1,5),
     *              (IREC(I), I = 1, 10), (FREC(I), I = 1, 10)
C-
      READ (IN,120) TITLE
C-
C ... LOGICAL UNIT 98 IS WHERE THE COLLAPSED NEW GFGEN INPUT WILL GO
      IF (IHEC.EQ.0) THEN
          REWIND ICOLL
          IF (ICOLL.GT.0) WRITE (ICOLL,120) TITLE
      ENDIF
C 
  120 FORMAT (A)
  130 FORMAT ('1', 40X, 'BANNER HEADINGS',  //, 
     *        '  1) ', 2A40, /, (5X, 2A40,  / ), 3(5X,A80,/),  /,
     *        '  2) ', A80,  / , 4(5X,A80,/),
     *        '  3) ', A80,  /, 4(5X,A80,/),  /, 
     *        10I8,/, 10F8.1,/ )
      MAXPP = MAXP
      MAXEP = MAXE
      NLP = NL
      NBNP = NBN
      IF (LP.GT.0) WRITE (LP,140) MAXPP, MAXEP, NLP, NBNP
  140 FORMAT ( // 10X, 'THIS COPY OF THE PROGRAM IS DIMENSIONED FOR'
     *    ,  / 10X, I7, ' NODES  ', I7, ' ELEMENTS ', I7, ' LISTS '
     *    , I7, ' BOUNDARY LISTS')
      IF (LP.GT.0) WRITE (LP,145) TITLE
  145 FORMAT ('1',  /, 10X, A)
      READ (IN,150,ERR=155) ISLP, IPRT, IPNN, IPEN, IPO, IRO, IPP, 
     *     IRFN, IGIN, LUOLD, NPRT, IRGRID, IDEBUG, IFXSP
  150 FORMAT (16I5)
      GO TO 160
  155 PRINT *,' ERROR while reading in RMA1 Sub ... Logical Unit= ',IN
      IF (IBATCH.EQ.0) THEN
          CALL BEEP (2)
          IF (ITRACE.LT.1) THEN
              PRINT *,' Hit ENTER-KEY to clear the screen. ERROR'
              PAUSE
          ENDIF          
      ENDIF
      STOP'ERR'
  160 IPWD = 0
      IPIM = 0
      IF (IPNN .GE. 2) IPWD = 1
      IF (IPEN .GE. 2) IPIM = 1
      IF (IPNN .EQ. 2) IPNN = 0
      IF (IPEN .EQ. 2) IPEN = 0
C 
      IDUMMY = 1
      ICRAP = 0
      IF( IHEC.EQ.0 .AND. ICOLL.GT.0 )
     *                 WRITE (ICOLL,150) ISLP, IPRT, IPNN, IPEN, 
     *                IPO, IRO, ICRAP, IRFN, IGIN, LUOLD, 
     *                NPRT, IRGRID, IDUMMY, IFXSP
C 
      IF (LP.GT.0) 
     *WRITE (LP,170) ISLP, IPRT, IPNN, IPEN, IPO, IRO, IPP, IRFN, 
     *               IGIN, LUOLD, NPRT, IRGRID, IDEBUG, IFXSP
  170 FORMAT ( // ' PRIMARY SWITCHES',/,
     *            5X, 'INPUT SLOPES               ', I5 / 
     *            5X, 'PRINT DATA                 ', I5 / 
     *            5X, 'PLOT NODES                 ', I5 / 
     *            5X, 'PLOT ELEMENTS              ', I5 /
     *            5X, 'PLOT NETWORK               ', I5 / 
     *            5X, 'REORDER OPTION             ', I5 /
     *            5X, 'PARTIAL PLOTS              ', I5 /
     *            5X, 'NETWORK REFINE             ', I5 / 
     *            5X, 'GEOMETRY INPUT             ', I5 / 
     *            5X, 'GEOMETRY OUTPUT            ', I5 /
     *            5X, 'VELOCITY FLD UNIT          ', I5 / 
     *            5X, 'RECTANGULAR GRID GENERATOR ', I5 / 
     *            5X, 'GRID CHECK                 ', I5 /
     *            5X, 'FIX SLOPES                 ', I5 )
C-
      READ (IN,180) HORIZ, VERT, XSCALE, YSCALE, AR, XFACT, 
     *     YFACT, HITEL, HITNN
  180 FORMAT (7F10.0, 2F5.0)
C 
      IF( IHEC.EQ.0 .AND. ICOLL.GT.0)
     *                WRITE (ICOLL,190) HORIZ, VERT, XSCALE, YSCALE, 
     *                               AR, XFACT, YFACT, HITEL, HITNN
  190 FORMAT (7F10.4, 2F5.2)
C 
      IF (LP.GT.0) 
     *WRITE (LP,200) HORIZ, VERT, XSCALE, YSCALE, AR, HITEL, HITNN,
     *               XFACT, YFACT, IMETRIC
  200 FORMAT ( // ' PLOTTING INFORMATION',/
     *            5X, 'HORIZONTAL SIZE     ', F12.3 / 
     *            5X, 'VERTICAL SIZE       ', F12.3 /
     *            5X, 'X SCALE             ', F12.5 / 
     *            5X, 'Y SCALE             ', F12.5 / 
     *            5X, 'PLOT ROTATION       ', F12.3 / 
     *            5X, 'ELEMENT NO. HEIGHT  ', F12.3,/ 
     *            5X, 'NODE NO. HEIGHT     ', F12.3,
     *         // ' OUTPUT COORDINATE SCALE FACTORS',/,
     *            5X, 'X CORD SCALE FACTOR  ', F12.5 / 
     *            5X, 'Y CORD SCALE FACTOR  ', F12.5,/
     *            5X, 'SYSTEM INTERNATIONAL ', I12 )
      IF (IPP .EQ. 0) GO TO 220
      READ (IN,150)  NXPMIN, NXPMAX, NYPMIN, NYPMAX
      IF (LP.GT.0)   WRITE (LP,210) NXPMIN, NXPMAX, NYPMIN, NYPMAX
  210 FORMAT ( // ' PARTIAL PLOT WINDOW (defined by node#)',/, 
     *           5X, 'X MIN', I10 / 5X, 'X MAX', I10 /
     *           5X, 'Y MIN', I10 / 5X, 'Y MAX', I10)
  220 CONTINUE
      IF (ABS(XFACT) .LT. 0.0001) XFACT = 1.0
      IF (ABS(YFACT) .LT. 0.0001) YFACT = 1.0
C-
C-.....INITIALIZE ARRAYS.....
C-
      IEOR = 99999
      AR = AR / 57.3
      NCN = 8
      VOID =  - 1.0E36
      VDX =  - 1.0E35
      DO 230 J = 1, MAXE
        IMAT(J) = 0
        DO 230 K = 1, NCN
          NOP(J,K) = 0
  230 CONTINUE
      DO 240 J = 1, MAXP
        INUM(J) = 0
        ALPHA(J) = VOID
        CORD(J,1) = VOID
        CORD(J,2) = VOID
        WD(J) = VOID
  240 CONTINUE
C 
C ... GENERATE RECTANGULAR GRID...
C 
      IF (IRGRID .GT. 0) THEN
          CALL RGRID
          GO TO 750
      ENDIF
C-
C ... READ A PREVIOUSLY GENERATED GFGEN BINARY FILE AS INPUT .....
C-
      IF (IGIN .EQ. 0) GO TO 270
      IIGIN = IABS(IGIN)
      REWIND IIGIN
      READ (IIGIN)  ITEST
      REWIND IIGIN
      IF (ITEST.GT.200) THEN
C ...     This is indeed a true character variable
          READ (IIGIN) (BANNER(I), I = 1, 15)
          READ (IIGIN) (IREC(I), I = 1, 40), (FREC(I), I = 1, 40)
          IF (IREC(1) .GE. 425)   READ (IGIN) TITLE
      ELSE 
C ...     This must be an integer style character-- so convert
          READ (IIGIN) MFLG1, MFLG2, MFLG3, MFLG4
          READ (IIGIN) IWRT1, (IPACKB(I), I =1, IWRT1)
          READ (IIGIN) IWRT2, IWRT3,
     *                 (IREC(I), I = 1, IWRT2), (FREC(I), I = 1, IWRT3)
          READ (IIGIN) IWRT4, (IPACKT(I), I=1, IWRT4)
C----     IWRT1=1200   IWRT2 AND IWRT3 =40  IWRT4=77
          CALL CONVRT ( BANNER, 15, IPACKB, 80, 2)
          CALL CONVRT ( TITLE,   1, IPACKT, 77, 2)
  250     CONTINUE 
C-
      ENDIF
      WRITE (*,260) TITLE, (BANNER(I), I = 1,15),
     *              (IREC(I), I = 1, 10), (FREC(I), I = 1, 10)
  260 FORMAT ('1', 40X, 'BANNER HEADINGS FROM PRIOR GFGEN ',  //, 
     *        ' TITLE=',A77,/,
     *        '  1) ', A80, /, 4(5X,A80,/),  /,
     *        '  2) ', A80,  /, 4(5X,A80,/), 
     *        '  3) ', A80,  /, 4(5X,A80,/),  /,
     *        10I8,/ , 10F8.2,/ )
C-
      READ (IIGIN) NP, NE,
     *           ((CORD(J,K),K=1,2), ALPHA(J), WD(J), J = 1, NP), 
     *           ((NOP(J,K),K=1,8), IMAT(J), TH(J), IEM(J), J = 1, NE)
C-
      IF (IREC(1) .GE. 425) READ (IIGIN)
     *           (WIDTH(J), J = 1, NP), (SS1(J), J = 1, NP),
     *           (SS2(J), J = 1, NP), (WIDS(J),  J = 1, NP)
C-    ----------------------------------------------------------
C ... INPUT SIDE SLOPES.....
C-
  270 IF (ISLP .LE. 0) GO TO 350
      NMSN = 0
      CALL BOUNDS(NOPL)
  280 READ (IN,550) J, (CX(I), I = 1, 2)
      IF (J .GE. IEOR) GO TO 330
      IF (J .LT. 0) GO TO 310
      IF (J .EQ. 0) GO TO 300
  290 ALPHA(J) = CX(1)
      GO TO 280
  300 J = CX(1)
      NOPL(J,3) = 1
      ALPHA(J) = CX(2)
      GO TO 280
  310 NSLP = NSLP + 1
      IF (NSLP .LE. NSLPMX) GO TO 320
      NSLP = NSLP - 1
      IERR = 7
      IF (LP.GT.0) WRITE (LP,460) IERR, (NODES(NSLP,I), I = 1, 3)
      IF (IBATCH.EQ.0) THEN
          IF (ITRACE.LT.1) THEN
              PRINT *,' Hit ENTER-KEY to clear the screen. ERROR.'
              PAUSE
          ENDIF      
      ENDIF
      STOP 
  320 NODES(NSLP,1) =  - J
      NODES(NSLP,2) = CX(1) + .5
      NODES(NSLP,3) = CX(2) + .5
      GO TO 280
C-
C ... INPUT MID-SIDE NODES.....
C-
  330 NMSN = NMSN + 1
      READ (IN,550) MSN(NMSN)
      IF (MSN(NMSN) .GE. IEOR) GO TO 340
      J = MSN(NMSN)
      IF (J .LE. 0) GO TO 330
      ALPHA(J) = VOID
      GO TO 330
  340 NMSN = NMSN - 1
C-
C ... INPUT ELEMENT CONNECTION - IMAT - THETA .....
C-
  350 CONTINUE
      READ (IN,360,END = 390) J, (INX(III), III = 1, 8), IMX, THX
  360 FORMAT (10I5, F10.0)
  370 IF (J .GE. IEOR) GO TO 390
      DO 380 K = 1, 8
        NOP(J,K) = INX(K)
  380 CONTINUE
      IMAT(J) = IMX
      TH(J) = THX
      GO TO 350
  390 CALL FILL
C-
C ... INPUT NODAL LOCATIONS.....
C-
  400 CONTINUE
      READ (IN,405,END = 420) J, (CX(III), III = 1, 2), WXX, 
     *     WIDE, S1, S2, WDS
C ... enlarged coordinate format 3-6-97
  405 FORMAT (I10, 2F15.3, 2F10.2, 2F10.6, F10.2)
  410 IF (J .GE. IEOR) GO TO 420
      CORD(J,1) = CX(1)
      CORD(J,2) = CX(2)
      WD(J) = WXX
      WIDTH(J) = WIDE
      SS1(J) = S1
      SS2(J) = S2
      WIDS(J) = WDS
      GO TO 400
  420 NE = 0
      NP = 0
      DO 430 J = 1, MAXE
        IF (IMAT(J) .GT. 0) NE = J
        DO 430 K = 1, NCN
          IF (NOP(J,K) .GT. NP) NP = NOP(J,K)
  430 CONTINUE
      IF (NSLP .EQ. 0) GO TO 470
      CALL BOUNDS(NOPL)
      DO 450 NNO = 1, NSLP
        CALL SLOPES(NNO, NODES, NOPL, NSLPMX, VOID, IERR)
        IF (IERR .LE. 0) GO TO 450
  440   IF (LP.GT.0) WRITE (LP,460) IERR, (NODES(NNO,I), I = 1, 3)
        IF (IBATCH.EQ.0) THEN
            CALL BEEP (2)
            IF (ITRACE.LT.0) THEN
              PRINT *,' Hit ENTER-KEY to clear the screen. ERROR'
              PAUSE
            ENDIF            
        ENDIF
        STOP 
  450 CONTINUE
  460 FORMAT ('1',  ////, 1X, 43('*'),  /, 
     *    ' ***** ABORT - BOUNDARY NODE GEOMETRY CONFLICT ****',/,
     *    1X, 43('*'),  //, 
     *    ' ERROR NO. ', I3, ' - BOUNDARY SEGMENT REQUEST ...', 
     *    I6, ',', I6, ', THRU', I6,  //, 
     *    ' ERROR LIST -', /, 12X,
     *    ' 1 - STARTING NODE(S) NOT ON BOUNDARY OR NOT ACTIVE', /,12X,
     *    ' 2 - STARTING NODE(S) NOT A CORNER', /, 12X,
     *    ' 3 - STARTING NODES NOT ADJACENT BOUNDARY CORNERS', /, 12X,
     *    ' 4 - TERMINATING NODE NOT A CORNER ON THIS BOUNDARY', /,12X,
     *    ' 5 - GEOMETRY OK, SUBR -SLOPES-',
     *                            'FAILED AT INDICATED NODE', /,12X,
     *    ' 6 - SAME AS ERROR 5',/,12X,
     *    ' 7 - GEOMETRY OK, MAXIMUM NO. OF BOUNDARY LISTS EXCEEDED',/,
     *    17X, 'ALTER -NSLPMX- AND ARRAY SIZE OF -NODES-', // )
  470 CONTINUE
      IF (IFXSP .LT. 1) GO TO 540
C ... READ NODES FOR SLOPE ADJUSTMENTS
      DO 490 IDOSLP = 1, 100
        READ (IN,480) ISPFX(IDOSLP)
  480   FORMAT (I5)
        IF (ISPFX(IDOSLP) .GE. IEOR) GO TO 510
        ISLPMX = IDOSLP
  490 CONTINUE
      IF (LP.GT.0) WRITE (LP,500) 
  500 FORMAT (' **ERROR** TOO MANY BOUNDARIES TO HAVE SLOPES CORRECTED',
     *        ', CURRENT LIMIT IS 100')
      IF (IBATCH.EQ.0) THEN
          CALL BEEP (2)
          IF (ITRACE.LT.0) THEN
              PRINT *,' Hit ENTER-KEY to clear the screen. ERROR'
              PAUSE
          ENDIF         
      ENDIF
      STOP 'ERROR'
  510 CONTINUE
      IF (LP.GT.0) WRITE (LP,520) 
  520 FORMAT ( //, ' THE FOLLOWING MIDSIDES DEFINE BOUNDARIES TO',
     *             ' HAVE SLOPES ADJUSTED ')
      IF (LP.GT.0) WRITE (LP,530) (ISPFX(IDOSLP), IDOSLP = 1, ISLPMX)
  530 FORMAT (' ', 26I5)
  540 CONTINUE
  550 FORMAT (I10, 7E10.0)
C-
C ... COMPUTE MID SIDE LOCATIONS.....
C-
      IF (IRFN .GT. 0) CALL REFINE
      IF (ISLP .LE. 0) GO TO 750
C-
C ... OUTPUT SLOPE INPUTS.....
C-
      IF (LP.GT.0) THEN
          WRITE (LP,560) 
  560     FORMAT ('1',  // 15X, 
     *      'BOUNDARY SLOPE SPECIFICATIONS SECTION',//,10x
     *      '*** FOLLOWING NODE SLOPE SPECIFICATIONS WERE ',
     *      'INPUT BUT MAY BE OVERRIDDEN BY AUTOMATIC SLOPE ',
     *      'COMPUTATION',  // 10X,
     *      '*** CORNER NODE SLOPES SPECIFIED',  / )
          N = 0
          DO 580 J = 1, NP
             IF (ALPHA(J) .LT. VDX .OR. ALPHA(J) .EQ. 0.0) GO TO 580
             N = N + 1
             WRITE (LP,570) N, J, ALPHA(J)
  570        FORMAT (2I10, F20.6)
  580     CONTINUE
          WRITE (LP,590) (J, MSN(J), J = 1, NMSN)
  590     FORMAT ( /// 10X, '*** MID SIDE NODES ***' / (2I10))
          WRITE(LP,600) ((NODES(J,K),K=1,3), J = 1, NSLP)
  600     FORMAT ( // 10X,
     *        '*** BOUNDARIES FOR AUTOMATIC SLOPE CALCULATION',/,
     *        ' FROM NODE  THRU NODE  TO NODE',  / (3I10))
          WRITE (LP,620) 
          DO 610 J = 1, NP
             IF (NOPL(J,3) .EQ. 1) WRITE (LP,630) J, ALPHA(J)
  610     CONTINUE
  620     FORMAT ( //, 10X, '*** NODES ON AUTOMATIC CALCULATION LIST '
     *       , 'WITH CONSTRAINED SLOPE',  / )
  630     FORMAT (I10, F20.6)
      ENDIF
C-
      DO 740 J = 1, NE
        IF (IMAT(J) .LT. 0) GO TO 740
        NCN = 6
        IF (NOP(J,7) .GT. 0) NCN = 8
        DO 730 K = 1, NCN, 2
          N1 = NOP(J,K)
          N2 = NOP(J,K+1)
          N3 = MOD(K+2,NCN)
          N3 = NOP(J,N3)
C-
C ... CHECK FOR VALID NODE.....
C-
          DO 640 L = 1, NMSN
            IF (MSN(L) .EQ. N2) GO TO 650
  640     CONTINUE
          GO TO 730
  650     CONTINUE
C-
C ... CHECK FOR ALL VOIDS.....
C-
          S1 = ALPHA(N1)
          S2 = ALPHA(N2)
          S3 = ALPHA(N3)
          IF (S1 .LT. VDX .AND. S2 .LT. VDX .AND. S3 .LT. VDX)
     *          GO TO 730
C-
C ... CHECK FOR ALL SPECIFIED.....
C-
          IF (S1 .GT. VOID .AND. S2 .GT. VOID .AND. S3 .GT. VOID)
     *          GO TO 730
C-
C ... SOME SPECIFIED..SOME NOT...
C-
          DX = CORD(N3,1) - CORD(N1,1)
          DY = CORD(N3,2) - CORD(N1,2)
          IF (ABS(DX) .LT. 1.0E - 05) DX = SIGN(1.0E-05,DX)
          SA = DY / DX
          IF (S1 .GT. VOID .AND. S3 .GT. VOID) GO TO 670
C-
C ... MID POINT TRANSFER CASE.....
C-
          IF (S1 .GT. VOID) GO TO 660
          S1 = S2
          GO TO 670
  660     S3 = S2
C-
C ... CHECK FOR SPECIAL CASES.....
C-
  670     IF (ABS(S1-S3) .GT. 1.0E - 3) GO TO 690
          IF (ABS(DX) .LT. 1.0E - 4) GO TO 730
          IF (ABS(SA-S1) .GT. 1.0E - 3) THEN
              IF (LP.GT.0 .AND. LP.NE.6) WRITE (LP,680) J, N1, N2, N3
              WRITE (*,680) J, N1, N2, N3
          ENDIF
  680     FORMAT ( // 10X, '*** SLOPE ERROR AT ELT', I5, ' NODES', 3I5)
          ALPHA(N2) = SA
          GO TO 730
C-
C ... COMPUTE CORDS.....
C-
  690     IF (ABS(S1) .GT. 9000.) GO TO 710
          IF (ABS(S3) .GT. 9000.) GO TO 720
          CORD(N2,1) = CORD(N1,1) + (DX * (S1 - 3. * S3) + 2. * 
     *         DY) / (4. * (S1 - S3))
          CORD(N2,2) = CORD(N1,2) + (DY * (3.0 * S1 - S3) - 2.0 * 
     *         DX * S1 * S3) / (4.0 * (S1 - S3))
  700     CONTINUE
          ALPHA(N2) = SA
          WD(N2) = 0.5 * (WD(N1) + WD(N3))
          GO TO 730
  710     CORD(N2,1) = CORD(N1,1) + DX / 4.
          CORD(N2,2) = CORD(N1,2) + (3. * DY - 2. * DX * S3) / 4.
          GO TO 700
  720     CORD(N2,1) = CORD(N1,1) + DX * 3. / 4.
          CORD(N2,2) = CORD(N1,2) + (DY + 2. * DX * S1) / 4.
          GO TO 700
  730   CONTINUE
  740 CONTINUE
C-
C ... COMPUTE BOUNDARY SLOPES.....
C-
  750 DO 800 I = 1, NP
        NN = 0
        DO 770 J = 1, NE
          IF (IMAT(J) .LE. 0) GO TO 770
          NCN = 6
          IF (NOP(J,7) .GT. 0) NCN = 8
          DO 760 K = 1, NCN
            IF (NOP(J,K) .NE. I) GO TO 760
            NN = NN + 1
            JE = J
            IF (NN .GE. 2) GO TO 800
  760     CONTINUE
  770   CONTINUE
        NCN = 6
        IF (NOP(JE,7) .GT. 0) NCN = 8
        IF (NN .NE. 1) GO TO 800
        DO 790 K = 2, NCN, 2
          IF (NOP(JE,K) .NE. I) GO TO 790
          N2 = I
          IF (ALPHA(N2) .GT. VDX) GO TO 790
          N1 = NOP(JE,K-1)
          N3 = MOD(K+1,NCN)
          N3 = NOP(JE,N3)
          DY = CORD(N3,2) - CORD(N1,2)
          DX = CORD(N3,1) - CORD(N1,1)
          IF (DX .LT. 0.) GO TO 780
          IF (DX .LE. 0.) GO TO 800
  780     ALPHA(N2) = DY / DX
          IF (ALPHA(N1) .LT. VDX) ALPHA(N1) = ALPHA(N2)
          IF (ALPHA(N3) .LT. VDX) ALPHA(N3) = ALPHA(N2)
          GO TO 800
  790   CONTINUE
  800 CONTINUE
C-
C ... OUTPUT NODAL VALUES
C-
      IF (IPRT .LT. 1 .OR. LP .EQ. 0) GO TO 880
      WRITE (LP,145) TITLE
      WRITE (LP,820) 
  820 FORMAT ( / 5X, 'ORDER      NODE       X LOC       Y LOC     ',
     *   ' B-ELEV', 9X, 'SLOPE    1D-WIDTH   1D-L.SLOPE   1D-R.SLOPE ',
     *   '  1D-STORE.W')
      N = 0
      DO 830 J = 1, NP
C ...   For printing purposes SLOPRT set 99999.99 for infinity
        IF (CORD(J,1) .LT.  -1.0E6) GO TO 830
        N = N + 1
        SLOPRT = ALPHA(J)
        IF (SLOPRT.GE. 1.0E6) SLOPRT =  99999.99
        IF (SLOPRT.LE.-1.0E6) SLOPRT = -99999.99
        WRITE (LP,840) N, J, CORD(J,1), CORD(J,2), WD(J), 
     *                 SLOPRT, WIDTH(J), SS1(J), SS2(J), WIDS(J)
  830 CONTINUE
  840 FORMAT (2I10, 3F12.2, F14.2, 4F12.2)
      WRITE (LP,145) TITLE
      WRITE (LP,850) 
  850 FORMAT ( // 5X, 'ORDER     NUMBER     NODES', 43X, 'ANGLE')
      NCN = 8
      N = 0
      DO 870 J = 1, NE
        IF (IMAT(J) .LE. 0) GO TO 870
        N = N + 1
        WRITE (LP,860) N, J, (NOP(J,K), K = 1, NCN), IMAT(J), TH(J)
  860   FORMAT (3I10, 8I5, F10.2)
  870 CONTINUE
  880 CONTINUE
C 
C ... SKIP NEXT SECTION OF CODE IF RECT. GRID GENERATOR
C ... IS SPECIFIED...
C 
      IF (IRGRID .GT. 0) GO TO 930
C-
C ... COMPUTE MID SIDE VALUES.....
C-
      DO 920 J = 1, NE
        IF (IMAT(J) .LE. 0) GO TO 920
        IF (IMAT(J) .GT. 900) GO TO 920
        NCN = NCNODES(J)
        DO 910 N = 2, NCN, 2
          N1 = NOP(J,N-1)
          N2 = NOP(J,N)
          N3 = MOD(N+1,NCN)
          IF (N3 .EQ. 0) N3 = NCN
          N3 = NOP(J,N3)
          IF (CORD(N2,1) .GT.  - 1.0E35) GO TO 890
          CORD(N2,1) = (CORD(N1,1) + CORD(N3,1)) / 2.0
          CORD(N2,2) = (CORD(N1,2) + CORD(N3,2)) / 2.0
  890     WD(N2) = 0.5 * (WD(N1) + WD(N3))
          IF (NCN .EQ. 3) THEN
C ...     MID-SIDE NODE ASSIGNMENTS FOR 1D ELEMENTS
            IERR = 0
            IF (WIDTH(N1) .LE. 0.00001) IERR = N1
            IF (WIDTH(N3) .LE. 0.00001) IERR = N3
            IF (IERR .GT. 0) THEN
              IF (LP.NE.0 .AND. LP.NE.6) WRITE (LP,900) IERR
              WRITE (*,900) IERR
  900         FORMAT (' *** 1D-CORNER-NODE=', I8,' HAS ZERO WIDTH ****')
            ELSE 
              WIDTH(N2) = (WIDTH(N1) + WIDTH(N3)) / 2.
              SS1(N2) = (SS1(N1) + SS1(N3)) / 2.
              SS2(N2) = (SS2(N1) + SS2(N3)) / 2.
              WIDS(N2) = (WIDS(N1) + WIDS(N3)) / 2.
            END IF 
          END IF 
  910   CONTINUE
  920 CONTINUE
C 
  930 SF = 1.0
      IF (XFACT * YFACT .LT. 0.) SF =  - 1.
      DO 940 J = 1, NP
        IF (CORD(J,1) .GT. VOID) CORD(J,1) = XFACT * CORD(J,1)
        IF (CORD(J,2) .GT. VOID) CORD(J,2) = YFACT * CORD(J,2)
        IF (CORD(J,1) .LT. VDX) CORD(J,1) = 0.0
        IF (CORD(J,2) .LT. VDX) CORD(J,2) = 0.0
        IF (WD(J) .LT. VDX) WD(J) = 0.0
        IF (ALPHA(J) .LT. VDX) ALPHA(J) = 0.0
        ALPHA(J) = ALPHA(J) * SF
  940 CONTINUE
      IF (IGIN .NE. 0) GO TO 960
      DO 950 N = 1, NE
         IEM(N) = N
  950 CONTINUE
  960 CONTINUE
C 
C ... REORDER GRID...
C 
      IF (NMSN .GE. 1) THEN
        DO 970 J = 1, NMSN
          MSNKP(J) = MSN(J)
  970   CONTINUE
      END IF 
      WRITE(*,975) IRO
  975 FORMAT(/,' --> PROCESSING REORDERING OPTION =',I5,/)
      IF (IRO .EQ. 1) CALL REORD
      IF (IRO .EQ. 2) CALL REORD1
      IF (IABS(IRO) .GE. 3) THEN
          IF (LP.NE.0 .AND. LP.NE.6) WRITE (LP,980) IRO
          WRITE (*,980)  IRO
      ENDIF
  980 FORMAT (' *** REORDERING OPTION=', I5, ' NOT AVAILABLE ***',/,
     *        ' OPTIONS INCLUDE   (1) FOR FULLY 2D MESH  ',/,
     *        '                   (2) FOR 1D AND 2D MESH',/,
     *        ' ---> no reordering will be performed ... keep going',/)
C-
C ... CHECK MID SIDE LOCATIONS.....
C-
      DO 990 J = 1, NP
        MSN(J) = 0
  990 CONTINUE
      DO 1140 J = 1, NE
        IF (IMAT(J) .EQ. 0)   GO TO 1140
        IF (IMAT(J) .GT. 900) GO TO 1140
        NCN = NCORN(J)
        IF (NCN .EQ. 5) NCN = 3
        DO 1130 K = 2, NCN, 2
          ITRY = 0
          JCHECK = 0
          KCHECK = 0
          N2 = NOP(J,K)
          IF (NCN .EQ. 5 .AND. N2 .EQ. 4) GO TO 1130
          IF (MSN(N2) .NE. 0) GO TO 1130
          MSN(N2) = 1
          N1 = NOP(J,K-1)
          N3 = MOD(K+1,NCN)
          IF (N3 .EQ. 0) N3 = NCN
          N3 = NOP(J,N3)
          GO TO 1010
 1000     CONTINUE
          NCN1 = 6
          IF (NOP(JCHECK,7) .NE. 0) NCN1 = 8
          N1 = NOP(JCHECK,KCHECK)
          N3 = MOD(KCHECK+2,NCN1)
          N3 = NOP(JCHECK,N3)
          N2 = NOP(JCHECK,KCHECK+1)
          JCHECK = 0
          KCHECK = 0
 1010     CONTINUE
          DY1 = CORD(N3,2) - CORD(N1,2)
          DX1 = CORD(N3,1) - CORD(N1,1)
          DY2 = CORD(N2,2) - CORD(N1,2)
          DX2 = CORD(N2,1) - CORD(N1,1)
          IF (DY1 .EQ. 0.0 .AND. DX1 .EQ. 0.0) THEN
              IF (LP.GT.0 .AND. LP.NE.6) WRITE (LP,1020) J, N1, N2, N3
              WRITE (*,1020) J, N1, N2, N3
          ENDIF
          IF (DY2 .EQ. 0.0 .AND. DX2 .EQ. 0.0) THEN
              IF (LP.GT.0 .AND. LP.NE.6) WRITE (LP,1020) J, N1, N2, N3
              WRITE (*,1020) J, N1, N2, N3
          ENDIF
 1020     FORMAT ( // 5X, 'TAN ERROR AT ELT ', I5, '  NODES = ', 3I6)
          IF (ABS(DX1) .LT. 0.00001 .OR. ABS(DX2) .LT. 0.00001)
     *          GO TO 1120
          AN1 = ATAN(DY1/DX1)
          AN2 = ATAN(DY2/DX2)
          AN3 = AN2 - AN1
          H1 = SQRT(DY1**2+DX1**2)
          H2 = SQRT(DY2**2+DX2**2)
          XMSN = ABS(H2*COS(AN3))
          XCK = H1 / 3.
          IF (XMSN .GE. XCK .AND. XMSN .LE. 2. * XCK) GO TO 1120
C ... MIDDLE THIRD RULE IS VIOLATED
          IF (LP.GT.0 .AND. LP.NE.6) WRITE (LP,1030) N2
          WRITE (*,1030) N2
 1030     FORMAT ( // 10X, ' *** NODE ', I6, 
     *                     '  VIOLATES MIDDLE THIRD RULE ***')
          FRAC = XMSN / H1
          IF (LP.GT.0 .AND. LP.NE.6) WRITE (LP,1040) FRAC, N1, N3
          WRITE (*,1040) FRAC, N1, N3
 1040     FORMAT (' THE NODE LIES ', F7.5, ' OF THE WAY FROM NODE '
     *        , I6, ' TO NODE ', I6)
          ANCK1 = ATAN(ALPHA(N1))
          CALL FNDANG(AN1, ANCK1, DELAN1)
          DEL1 = DELAN1
          ANCK2 = ATAN(ALPHA(N3))
          CALL FNDANG(AN1, ANCK2, DELAN2)
          DEL2 = DELAN2
          IF (ABS(DEL1-SIGN(DEL1,DEL2)) .GT. 0.00001) GO TO 1110
          IF (LP.GT.0 .AND. LP.NE.6) WRITE (LP,1050) 
          WRITE (*,1050)
 1050     FORMAT (' *** SLOPE RULE HAS ALSO BEEN VIOLATED ***')
          IF (N1 .NE. NOP(J,K)) GO TO 1110
          IF (IFXSP .LT. 1) GO TO 1110
          DO 1060 IDOSLP = 1, ISLPMX
            IF (N2 .EQ. ISPFX(IDOSLP)) GO TO 1080
 1060     CONTINUE
          IF (LP.GT.0 .AND. LP.NE.6) WRITE (LP,1070) 
 1070     FORMAT ('+', 34X, ' THIS SIDE IS NOT IN LIST TO BE FIXED')
          GO TO 1110
 1080     CONTINUE
          CALL FIXSLP(J, K, JCHECK, KCHECK, NCN, AN1, DEL1, DEL2)
          ITRY = ITRY + 1
          IF (ITRY .LT. 10) GO TO 1100
          IF (LP.GT.0 .AND. LP.NE.6) WRITE (LP,1090) 
 1090     FORMAT (' I GIVE UP ...')
          GO TO 1130
 1100     CONTINUE
          IF (JCHECK .GT.  - 1) GO TO 1010
 1110     CONTINUE
 1120     CONTINUE
          IF (JCHECK .GT. 0) GO TO 1000
 1130   CONTINUE
 1140 CONTINUE
C 
C ... CHECK GRID FOR ERRORS....
C 
      IF (IDEBUG .GT. 0) CALL GRBUGS (IDEBUG)
C-
C ... consolidate GFGEN BANNER section all into (BANNER(i),i=1,15)
C-
      CALL CONVRT (BANNER, 15, IPACKB, 80, 3)
C-
      IF (LUOLD .GE. 1) THEN
C ...      WRITE FULL BINARY OUTPUT GEOMETRY FILE...
           PRINT *,' --> write the old character banner style geometry'
           REWIND LUOLD
C
           WRITE (LUOLD) (BANNER(I), I = 1, 15)
C 
           WRITE (LUOLD) (IREC(I), I = 1, 40), (FREC(I), I = 1, 40)
C 
           WRITE (LUOLD) TITLE
C
           WRITE (LUOLD) NP, NE, 
     *             ((CORD(J,K),K=1,2), ALPHA(J), WD(J), J = 1, NP),
     *             ((NOP(J,K),K=1,8), IMAT(J), TH(J), IEM(J), J = 1, NE)
C 
           WRITE (LUOLD) (WIDTH(J), SS1(J), SS2(J), WIDS(J), J = 1, NP)
      ENDIF
C-
      IF (LUNIT .GE. 1) THEN
C-
C ...      WRITE INTEGER type CHARACTER BANNER ..  BINARY GEOMETRY FILE
C-
           CALL CONVRT ( BANNER, 15, IPACKB, 80, 1 )
C-
           CALL CONVRT ( TITLE, 1, IPACKT, 77, 1)
C-         -----------------
           REWIND LUNIT
           IREC(1) = 435
           MFLG1 = 100
           WRITE (LUNIT) MFLG1, IREC(1), NP, NE
           IWRT1  = 1200
           WRITE (LUNIT) IWRT1, (IPACKB(I),I =1,IWRT1)
           IWRT2  = 40
           IWRT3  = 40
           WRITE (LUNIT) IWRT2, IWRT3, 
     *                    (IREC(I), I =1,IWRT2), (FREC(I), I =1,IWRT3)
           IWRT4 = 77
           WRITE (LUNIT) IWRT4, (IPACKT(I),I =1,IWRT4)
C-
           WRITE (LUNIT) NP, NE, 
     *             ((CORD(J,K),K=1,2), ALPHA(J), WD(J), J = 1, NP),
     *             ((NOP(J,K),K=1,8), IMAT(J), TH(J), IEM(J), J = 1, NE)
C-
           WRITE (LUNIT) (WIDTH(J), SS1(J), SS2(J), WIDS(J), J = 1, NP)
           WRITE (*,*) '--> Binary geometry file has been written'
      ENDIF
C-
C ... Create the DISSPLA PLOT 
C-
C ..BpD turn off plotting..    IF (IPO.GE. 1)  CALL PLTMSH
C-
      RETURN 
      END
C
      SUBROUTINE AUTO_FILE 
      SAVE
C-                         AUTO_FILE Created       6-20-1996 
C-                         AUTO_FILE LAST MODIFIED 4-10-2001
C ... Open files as requested by input
C-
      INCLUDE'gfgv435.inc'
C-
      COMMON /DMSCHR/ VERGFG(4), STAMP, DESC(2),
     *                BANGFG(5), BANRM2(5), BANCON(5), BANNER(15),
     *                CHKDMS, RECRD
      CHARACTER CHKDMS*6, RECRD*6
      CHARACTER BANGFG*80, BANRM2*80, BANCON*80, BANNER*80
      CHARACTER VERGFG*40, STAMP*80, DESC*80
C-
      CHARACTER *67 SIMRUN, FNAME, FNAME1, FNAME2, PATHWAY
      CHARACTER *12 IDENTIFY
      CHARACTER *134 FILE_PATH
      INTEGER TRMLEN
      LOGICAL AROUND
C-      
      PRINT *,' =+= CALLED GFGEN AUTO_FILE.  IBATCH=', IBATCH
C
C ... Open all input and output files
C-
        PRINT *,' '
        PRINT *,' ==== '       ,VERGFG(3),            ' ===='
        PRINT *,' ==== '       ,VERGFG(4),            ' ===='
        PRINT *,' ====           No Plotting            ===='
        PRINT *,' ==== TABS-MD GEOMETRY FILE GENERATION ===='
        PRINT *,' /// Original Author: Dr. Ian P. King of RMA ///'
        PRINT *,' /// Modified and Maintained by USAE WES-CHL ///'
        PRINT *,' '

        PRINT *,' SMS simulation super file concept used unless, ...'
        PRINT *,' "interactive" is entered for file name prompting.' 
        PRINT *,' '       
C
        IN = 0
        LP = 0
        LUNIT = 0
        LUOLD = 0
        IGIN = 0
        ICOLL = 0

      IF (IBATCH.EQ.3) THEN
C ...   IBATCH = 3    ! set for reading "name" of auto_simulation run file (SMS) 
        ITRY = 0
   10   ITRY = ITRY + 1
        WRITE (*,20)
   20   FORMAT (' Enter GFGEN simulation.run file name.',/,
     *          ' or Enter ? for key-word help.',/)
   30   READ (*,'(A)') SIMRUN
        IF (SIMRUN(1:1).EQ.'?') THEN
            WRITE (*,40)
   40       FORMAT (/,' Simulation file Requested',/,
     *      ' Searching for a file that contains keywords:',/)            
            WRITE (*,50)
   50       FORMAT (/,
     *      ' Key-Words must start in column #1',/,
     *      ' Corresponding Files names must start in column #14',//,
     *      ' Valid Key-Words are:',/,
     *           '    FILE_PATHWAY  Sets the pwd for I/O files',/,
     *           '    GEOMETRY_INP  GFGEN run control file (ascii)',/,
     *           '    GEOMETRY_FRL  GFGEN full results listing',/,
     *           '    GEOMETRY_BIN  GFGEN binary geometry',/,
     *           '    quit          stops the program immediately'/)           
            GO TO 10
        ENDIF
        IF (SIMRUN(1:11) .EQ. 'INTERACTIVE' .OR. 
     *         SIMRUN(1:11) .EQ. 'interactive') THEN
               PRINT *,'Changing the IBATCH to equal 0 ... interactive'
               IBATCH = 0
               CALL FILE (1) 
               RETURN
        ENDIF
        IF (SIMRUN(1:4) .EQ. 'QUIT' .OR. 
     *     SIMRUN(1:4)  .EQ. 'quit') THEN
           PRINT *,'User has issued a quit.   GFGEN stops'
           STOP
        ENDIF
        INQUIRE (FILE=SIMRUN,EXIST=AROUND)
        IF (.NOT.AROUND) THEN
             PRINT *,' --> File not found ... try again'
             IF (ITRY.GE.3) THEN
                WRITE (*,50)
                STOP
             ENDIF
             GO TO 10
        ENDIF
        
        WRITE (*,60) SIMRUN
   60   FORMAT (/,' GFGEN simulation run file name = ', A,/)
        INSIM  = 88
        OPEN (INSIM,FILE=SIMRUN,STATUS='OLD')
        WRITE(*,*) ' '
      ENDIF
C ... --------------------- simulation super file on unit=INSIM=88
C ... ---------------------

      IF (IBATCH.EQ.2) THEN
          WRITE(*,64) 
   64     FORMAT( 
     *     ' Enter keyword and file name information',/,
     *     ' Enter a question mark in column 1 for HELP...',/)
      ENDIF 
        
      WRITE(*,65) 
   65 FORMAT(' FILE NAME INFORMATION:'/,' ----------------------')

      PATHWAY(1:4) = 'null'
      
        DO 5000 III = 1, 20
           IF (IBATCH.EQ.3) THEN
C ...          The IBATCH=3 is the setup for simulation super file name
               READ (INSIM,'(A,1X,A)',END=6000) IDENTIFY, FNAME
           ELSE       
               READ (*,'(A,1X,A)',END=6000) IDENTIFY, FNAME
           ENDIF

           LASTF = TRMLEN( FNAME )   ! find last non-blank character

           IF (IDENTIFY .EQ. 'FILE_PATHWAY' .OR. 
     *         IDENTIFY .EQ. 'file_pathway') THEN
               PATHWAY = FNAME
               LASTP = LASTF   ! length of pathway
               WRITE (*,70) FNAME
  70           FORMAT (' Default pathway for file names= ', A)
               GO TO 5000
           ENDIF

           CALL REPLY(FNAME, ISWT)

           IF (ISWT .EQ. 0) THEN
              IF (PATHWAY(1:4).NE. 'null') THEN
C ...             I have an active path and an active file name
                  FILE_PATH(1:LASTP)   = PATHWAY(1:LASTP)
                  FILE_PATH(LASTP+1:LASTP+LASTF) = FNAME(1:LASTF)
                  LASTC = LASTP + LASTF
              ELSE
C ...             the path is already incorporated into the file name
                  FILE_PATH(1:67) = FNAME(1:67)
                  LASTC = LASTF
              ENDIF

           ELSE IF (ISWT .EQ. 1) THEN
C ...         the file string said 'null'
              WRITE(*,80) IDENTIFY
   80         FORMAT(' Turned off the file identified as:',A)
           ELSEIF (ISWT  .EQ. 2) THEN
              PRINT *,' Stop/Quit was requested'
              STOP'GFGEN_AUTO_FILE' 
           ELSEIF (ISWT .EQ. 3) THEN
              IF (FNAME(1:4) .EQ. 'quit') STOP        
           ENDIF

           IF (IDENTIFY .EQ. 'GEOMETRY_INP' .OR. 
     *         IDENTIFY .EQ. 'geometry_inp') THEN
               WRITE (*,100) FNAME
  100          FORMAT (' Gfgen run control input file name= ', A)
               IF (FNAME(1:4).EQ.'null') THEN
                   WRITE(*,*) ' Cannot nullify the manditory input'
                   IN = 0
                   STOP'error'
               ELSE
                   FNAME1 = FNAME
                   INQUIRE (FILE=FILE_PATH,EXIST=AROUND)
                   IF (.NOT.AROUND) THEN
                        WRITE(*,*) ' --> Cannot find GFGEN input file.'
                        GO TO 7000
                   ENDIF
                   IN  = 8
                   OPEN (IN,FILE=FILE_PATH,STATUS='OLD')
               ENDIF

           ELSE IF (IDENTIFY .EQ. 'GEOMETRY_FPL' .OR. 
     *         IDENTIFY .EQ. 'geometry_fpl' .OR.
     *         IDENTIFY .EQ. 'GEOMETRY_FRL' .OR.
     *         IDENTIFY .EQ. 'geometry_frl' ) THEN
               WRITE (*,200) FNAME
  200          FORMAT (' Gfgen full print listing file name= ', A)
               IF (FNAME(1:4).EQ.'null') THEN
                   PRINT *,' Full print turned off.'
                   LP = 0
                   STOP'error'
               ELSE
                   FNAME2 = FILE_PATH
                   LP = 7
                   OPEN (LP,FILE=FILE_PATH,STATUS='UNKNOWN')
                   WRITE(LP,230) FNAME1
  230              FORMAT (' GFGEN RUN CONTROL INPUT [ascii]= ', A)
                   WRITE(LP,240) FNAME2
  240              FORMAT (' FULL PRINT LISTING FILE [ascii]= ', A)
               ENDIF

           ELSE IF (IDENTIFY .EQ. 'GEOMETRY_BIN' .OR. 
     *         IDENTIFY .EQ. 'geometry_bin') THEN
               WRITE (*,300) FNAME
  300          FORMAT (' Gfgen binary geometry output file name= ', A)
               IF (FNAME(1:4).EQ.'null') THEN
                   PRINT *,' Binary geometry output turned off.'
                   LUNIT = 0
               ELSE
                   LUNIT = 3
                   OPEN(LUNIT,FILE=FILE_PATH,STATUS='UNKNOWN',
     *                                         FORM='UNFORMATTED')
C ECGL ADD BEG     REPLACE UNFORMATED with BINARY FOR PC Microsoft FORTRAN 
C ECGL             OPEN(LUNIT,FILE=FILE_PATH,STATUS='UNKNOWN',
C ECGL                                         FORM='BINARY')
C ECGL ADD END
                   WRITE(LP,330) FNAME
  330              FORMAT (' GFGEN OUTPUT GEOMETRY [binary]= ', A)
               ENDIF

           ELSE IF (IDENTIFY .EQ. 'GEOMETRY_OLD' .OR. 
     *         IDENTIFY .EQ. 'geometry_old') THEN
               WRITE (*,400) FNAME
  400          FORMAT (' Gfgen PRIOR [binary] geometry file name= ', A)
               IF (FNAME(1:4).EQ.'null') THEN
                   PRINT *,' Prior Binary geometry input turned off.'
                   IGIN = 0
               ELSE
                   IGIN = 3
                   INQUIRE (FILE=FILE_PATH,EXIST=AROUND)
                   IF (.NOT.AROUND) THEN
                        PRINT *,' --> Cannot find GFGEN input file.'
                        GO TO 7000
                   ENDIF
                   OPEN(IGIN,FILE=FILE_PATH,STATUS='OLD',
     *                                   FORM='UNFORMATTED')
C ECGL ADD BEG     REPLACE UNFORMATED with BINARY FOR PC Microsoft FORTRAN 
C ECGL             OPEN(IGIN,FILE=FILE_PATH,STATUS='OLD',FORM='BINARY')
C ECGL ADD END
                   WRITE(LP,430) FNAME
  430              FORMAT (' GFGEN PRIOR GEOMETRY input [binary] ', A)
               ENDIF

           ELSE IF (IDENTIFY .EQ. 'GEOMETRY_COL' .OR. 
     *         IDENTIFY .EQ. 'geometry_col') THEN
               WRITE (*,500) FNAME
  500          FORMAT (' Gfgen collapsed [binary] geometry file name= ',
     *                                                                A)
               IF (FNAME(1:4).EQ.'null') THEN
                   PRINT *,'Collapsed new geometry turned off.'
                   ICOLL = 0
               ELSE
                   ICOLL = 98
                   OPEN(ICOLL,FILE=FILE_PATH,STATUS='UNKNOWN',
     *                                         FORM='UNFORMATTED')
C ECGL ADD BEG     REPLACE UNFORMATED with BINARY FOR PC Microsoft FORTRAN 
C ECGL             OPEN(ICOLL,FILE=FILE_PATH,STATUS='OLD',
C ECGL                                         FORM='BINARY')
C ECGL ADD END
                   WRITE(LP,530) FNAME
  530              FORMAT (' GFGEN PRIOR GEOMETRY input [binary]= ', A)
               ENDIF

           ELSE IF (IDENTIFY .EQ. 'GEOMETRY_PLT' .OR. 
     *         IDENTIFY .EQ. 'geometry_plt') THEN
               PRINT *,'Cannot re-name the plot file ... popfil'
               WRITE(LP,630) 
  630          FORMAT (' GFGEN PLOT FILE = popfil')
C
C ....         moved the plot file interactive questions & open 
C ...          inside of sub pltmsh
C
           ELSE IF (IDENTIFY(1:1) .EQ. '?') THEN
               WRITE (*,50)

           ELSE IF (IDENTIFY(1:4) .EQ. 'STOP' .OR. 
     *         IDENTIFY .EQ. 'stop')  THEN
               STOP 
               
           ELSE IF (IDENTIFY .EQ. 'INTERACTIVE' .OR. 
     *         IDENTIFY .EQ. 'interactive') THEN
               PRINT *,'Changing the IBATCH to equal 0 ... interactive'
               IBATCH = 0
               CALL FILE (1) 
               RETURN
               
           ELSE IF (IDENTIFY(1:4).EQ.'QUIT' .OR. 
     *              IDENTIFY(1:4).EQ.'quit') THEN
              PRINT *,'User has issued a quit.   GFGEN stops'
              STOP

           ELSE 
               WRITE(*,2000) IDENTIFY
 2000          FORMAT(/,' ---> Ignore Unknown KEY word =', A,/)
      ENDIF
 5000 CONTINUE
C-
 6000 IF (IN . LE. 0) THEN
          WRITE(*,6010) 
 6010     FORMAT(' --> ERROR',/,
     *           '     The string "GEOMETRY_INP" was not found.',/,
     *           '     Cannot find the gfgen manditory geometry input',/,
     *           '     GFGEN,  SUBROUTINE AUTO_FILE STOPS.',/)
      ENDIF
      RETURN 
C-
 7000 WRITE(*,7010) IDENTIFY, PATHWAY, FNAME, FILE_PATH
      IF (LP.GT.0)  WRITE(LP,7010) IDENTIFY, PATHWAY, 
     *                             FNAME, FILE_PATH
 7010 FORMAT(/,' --> GFGEN cannot find this input file.',/,
     *         ' -->       IDENTIFY flag     =', A,/,
     *         ' -->       path way to file  =', A,/,
     *         ' -->       file name         =', A,/,
     *         ' -->       Inquire failed on =', A,/,
     *         ' --> GFGEN SUBROUTINE AUTO_FILE STOPS.',/)
      STOP
      END

      SUBROUTINE BEEP ( IHONK )
      IMPLICIT REAL (A-H, O-Z)
C ... Will cause the terminal to BEEP, IHONK times
C-
      N = IHONK
      IF ( N.GT.40 ) N = 40
      WRITE (6,*) ( CHAR(7), I = 1, N)
C-
      RETURN 
      END
C
      SUBROUTINE ADJPT(II, M)
      IMPLICIT REAL (A-H, O-Z)
      INCLUDE'gfgv435.inc'
C-
      IF( ITRACE.GE.2 ) PRINT *,' =+= CALLED ADJPT (II=',II,'M=',M,')'
C      
      MAXC = 60
      NAD = 0
      DO 100 I = 1, MAXC
        JJ = ICON(II,I)
        IF (JJ .LE. 0) GO TO 100
        IF (JJ .EQ. MIST) GO TO 100
        NAD = NAD + 1
  100 CONTINUE
      IQWIFZ = NAD - NADM(NR)
      IF (IQWIFZ .GT. 0) GO TO 150
      IF (IQWIFZ .EQ. 0) GO TO 120
  110 NADM(NR) = NAD
      ICOL(NR,1) = M
      NC = 1
      GO TO 150
  120 NC = NC + 1
      IF (NC .GE. MM2) THEN
          IF (LP.GT.0 .AND. LP.NE.6) WRITE (LP,130) II,M
          PRINT 130, II, M
  130     FORMAT (' NC is greater than MM2 in subroutine ADJPT',/,
     *            ' Probable array over-run for array ICOL()',/,
     *            ' CALLED ADJPT (II=',I6,'M=',I6,')',/)
          IF (IBATCH.EQ.0) THEN
              CALL BEEP (2)
              IF (ITRACE.LT.0) THEN
                 PRINT *,' Hit ENTER-KEY to clear the screen. ERROR'
                 PAUSE
              ENDIF          
          ENDIF
          STOP 'ERROR'
      ENDIF
  140 CONTINUE
CCCC  ICOL(NR,NC)=M   ON RECOMMENDATION OF IAN KING 12-6-82
  150 CONTINUE
      RETURN 
      END
C-
      SUBROUTINE BOUNDS(NOPL)
C 
C   -BOUNDS- EXAMINES THE ELEMENT CONNECTIVITY
C   ARRAY (NOP) AND GENERATES LINKED LISTS OF BOUNDARY NODES IN
C   ARRAY (NOPL) WITH INACTIVE NODES FLAGGED (0), NON-BOUNDARY
C   NODES FLAGGED (-1) AND MIDSIDE REFERENCES NEGATED.  THE NUMBER OF
C   ELEMENTS (NE), NUMBER OF NODES (NP) AND ELEMENT STATUS (IMAT)
C   NEED TO BE AVAILABLE.  THE LINKED LISTS PERMIT TRAVERSING
C   EXTERNAL BOUNDARIES IN A CW OR CCW FASHION AND TRAVERSING
C   INTERNAL BOUNDARIES IN A CCW OR CW FASHION DEPENDING ON
C   CHOICE OF RESPECTIVE COLUMN OF (NOPL). THE THIRD COLUMN OF (NOPL)
C   IS RESERVED AS A FLAG LIST.  (NOPL) IS INITIALIZED ON THE
C   FIRST CALL TO BOUNDS.
C 
C   (NOPL(3,N)) IS USED TO FLAG NODES WITH FORCED SLOPES.
C 
C   PROGRAMMER- JAMES D. ETHRIDGE JR., WESHE3, 601/634-3818
C 
C 
      IMPLICIT REAL (A-H, O-Z)
      INCLUDE'gfgv435.inc'
      DIMENSION NOPL(MAXP,3), NOPA(9)
      DATA NOPA/1, 2, 3, 4, 5, 6, 7, 8, 1/
      DATA IFRS/1/
C
      IF( ITRACE.GE.2 ) PRINT *,' =+= CALLED BOUNDS (NOPL=',NOPL,')'
C
      GO TO (100, 120), IFRS
  100 DO 110 I1 = 1, NP
        DO 110 I2 = 1, 3
  110 NOPL(I1,I2) = 0
      IFRS = 2
      RETURN 
C 
C   EXAMINE THE MIDSIDE NODES REFERENCED BY (NOP) AND SET THE
C   NODE POSITIONS IN (NOPL) TO -
C 
C    -1   -1  NOT A BOUNDARY, REFERENCED BY MORE THAN ONE ELEMENT
C     0    0  NOT A BOUNDARY, NOT REFERENCED BY ANY ELEMENT
C   NCW NCCW  BOUNDARY, SAVE ADJACENT CW AND CCW CORNER NODE NUMBERS
C 
  120 DO 160 I1 = 1, NE
        IF (IMAT(I1) .EQ. 0) GO TO 160
        DO 150 I2 = 2, 8, 2
          IDX1 = NOPA(I2)
          IDX2 = NOP(I1,IDX1)
          IQWIFZ = NOPL(IDX2,1)
          IF (IQWIFZ .GT. 0) GO TO 140
          IF (IQWIFZ .LT. 0) GO TO 150
  130     IDX1 = NOPA(I2-1)
          NOPL(IDX2,1) = NOP(I1,IDX1)
          IDX1 = NOPA(I2+1)
          NOPL(IDX2,2) = NOP(I1,IDX1)
          GO TO 150
  140     NOPL(IDX2,1) =  - 1
          NOPL(IDX2,2) =  - 1
  150   CONTINUE
  160 CONTINUE
C 
C   SET THE BOUNDARY CORNER NODES IN (NOPL) TO POINT TO THE
C   BOUNDARY MIDSIDE NODES REFERENCING THEM. NEGATE THEM TO
C   INDICATE THEIR MIDSIDE NODE STATUS.
C 
      DO 190 I1 = 1, NP
        DO 180 I2 = 1, 2
          IDX1 = NOPL(I1,I2)
          IF (IDX1 .LE. 0) GO TO 190
  170     IDX2 = MOD(I2,2) + 1
  180   NOPL(IDX1,IDX2) =  - I1
  190 CONTINUE
      RETURN 
      END

      SUBROUTINE CHKOUT
C 
C ... CHECK FINAL TOTAL.  SAVE ORDER IF BETTER ...
C 
      INCLUDE 'gfgv435.inc'
C 
      DATA ITIME/0/
C
      IF( ITRACE.GE.2 ) PRINT *,' =+= CALLED CHKOUT '
C      
      IF (ITIME .EQ. 0) THEN
C ...   ipk 08-21-97 add MTSUM1 for original element order case
        IF (LP.GT.0 .AND. LP.NE.6) WRITE (LP,100) MTSUM1,MTSUM,NFWSAV
C ...   ipk 04-18-97 change to add pseudo integer*8
        WRITE(*,105) MTSUM1,MTSUM,NFWSAV
  100   FORMAT ('1',/,
     *           ' --> ORDERING SUM FOR ORIGINAL ELEMENT ORDER = ',3I12)
  105   FORMAT(/,' --> ORDERING SUM FOR ORIGINAL ELEMENT ORDER = ',3I12)
      ELSEIF (MTSUM1 .gt. mrsum1) then
        IF (LP.GT.0 .AND. LP.NE.6) WRITE (LP,110) MTSUM1,MTSUM,NFWSAV
        WRITE(*,110) MTSUM1,MTSUM,NFWSAV
        RETURN
      ELSEIF (MTSUM1 .eq. mrsum1) then
        IF (MTSUM .LT. 0 .OR. MTSUM .GE. MRSUM) THEN
          IF (LP.GT.0 .AND. LP.NE.6) WRITE (LP,110) MTSUM1,MTSUM,NFWSAV
          WRITE(*,110) MTSUM1,MTSUM,NFWSAV
  110     FORMAT (/, ' --> ORDERING SUM FOR THIS LIST = ', 3I12)
          RETURN 
        ELSE
          IF (LP.GT.0 .AND. LP.NE.6) WRITE (LP,110) MTSUM1,MTSUM,NFWSAV
          WRITE(*,110) MTSUM1,MTSUM,NFWSAV
        ENDIF
      ELSE 
        IF (LP.GT.0 .AND. LP.NE.6) WRITE (LP,110) MTSUM1,MTSUM,NFWSAV
        WRITE(*,110) MTSUM1,MTSUM,NFWSAV
      END IF 
      mrsum1=MTSUM1
c ... ipk 04-18-97 end changes
      MRSUM = MTSUM
      ITIME = 1
C 
C ... COPY ORDER
C 
      DO 120 N = 1, NAE
        IEM(N) = MLIST(N)
  120 CONTINUE
C 
C ... FILL IEM ARRAY
C 
      NAEP = NAE + 1
      DO 130 N = 1, NE
        IF (IMAT(N) .EQ. 0) THEN
          IEM(NAEP) = N
          NAEP = NAEP + 1
        END IF 
  130 CONTINUE
      RETURN 
      END
C
      SUBROUTINE CKNODE(N, JCHECK, KCHECK, AN1, DELAN, NCURR)
      IMPLICIT REAL (A-H, O-Z)
      INCLUDE'gfgv435.inc'
C-
C ... WILL FIND IF SLOPE CORRECTION OF SELECTED NODE CAN BE SAFELY MADE,
C     SELECT CONNECTING BOUNDARY, AND UPDATE SLOPES IF SAFE
      DATA VDX/-1.0E36/
C
      IF( ITRACE.GE.2 ) PRINT *,' =+= CALLED CKNODE (N=',N,'...)'
C
C ... FIND CONNECTING BOUNDARY SIDE
      DO 140 J = 1, NE
        IF (IMAT(J) .EQ. 0) GO TO 140
        NCN = 6
        IF (NOP(J,7) .NE. 0) NCN = 8
        DO 130 K = 1, NCN, 2
          IF (NOP(J,K) .NE. N) GO TO 130
          ICHECK = K - 1
          IF (ICHECK .LT. 1) ICHECK = NCN
          N2 = NOP(J,ICHECK)
          IF (N2 .EQ. NCURR) GO TO 110
          JNEW = J
          KCHECK = ICHECK - 1
          IF (KCHECK .LT. 1) KCHECK = NCN
          DO 100 I = 1, NMSN
            IF (N2 .EQ. MSNKP(I)) GO TO 150
  100     CONTINUE
  110     ICHECK = K + 1
          IF (ICHECK .GT. NCN) ICHECK = 1
          N2 = NOP(J,ICHECK)
          IF (N2 .EQ. NCURR) GO TO 140
          JNEW = J
          KCHECK = K
          DO 120 I = 1, NMSN
            IF (N2 .EQ. MSNKP(I)) GO TO 150
  120     CONTINUE
          GO TO 140
  130   CONTINUE
  140 CONTINUE
      GO TO 190
  150 CONTINUE
      JCHECK = JNEW
      N1 = NOP(JCHECK,KCHECK)
      N3 = MOD(KCHECK+2,NCN)
      N3 = NOP(JCHECK,N3)
      DY = CORD(N3,2) - CORD(N1,2)
      DX = CORD(N3,1) - CORD(N1,1)
      IF (DELAN .GT. 0.) GO TO 160
      ALCK = AN1 + 0.05
      GO TO 170
  160 ALCK = AN1 - 0.05
  170 AN2 = ATAN(DY/DX)
      CALL FNDANG(AN2, ALCK, DELAN1)
      CALL FNDANG(AN2, ALPHA(N), DELAN2)
      DEL1 = DELAN1
      DEL2 = DELAN2
      IF (ABS(DEL1-SIGN(DEL1,DEL2)) .GT. 0.0001) GO TO 190
      ALPHA(N) = TAN(ALCK)
      IF (LP.GT.0 .AND. LP.NE.6) WRITE (LP,180) N, ALPHA(N)
  180 FORMAT ('+', 34X, ' NODE NO.', I5, ' ADJUSTED, NEW SLOPE = '
     *    , 1PE20.6)
      RETURN 
  190 CONTINUE
      JCHECK = 0
      RETURN 
      END
C 
      SUBROUTINE CONVRT ( ISTRING, LENGTH, INTVAL, ISIZE, ISWITCH )
      SAVE
C-
C ... AUTHOR: BOB EVANS/BARBARA DONNELL              12-16-1992
C ... ISWITCH:  1 CONVERT FROM CHARACTER TO ASCII INTEGER VALUE
C ...           2 CONVERT FROM INTEGER VALUE TO CHARACTER EQUIVALENT
C ...           3 CONSOLIDATE FROM BANNER PIECES TO A WHOLE
C ... PURPOSE:  Avoid FORTRAN to C-Language binary file read problems
C-
      INCLUDE'gfgv435.inc'
      PARAMETER (MM8=1200)
      COMMON /DMSCHR/ VERGFG(4), STAMP, DESC(2),
     *                BANGFG(5), BANRM2(5), BANCON(5), BANNER(15),
     *                CHKDMS, RECRD
C
      CHARACTER CHKDMS*6, RECRD*6
      CHARACTER BANGFG*80, BANRM2*80, BANCON*80, BANNER*80
      CHARACTER VERGFG*40, STAMP*80, DESC*80
C-
      CHARACTER  ISTRING(15)*80
      DIMENSION  INTVAL(MM8)
C-
      IF( ITRACE.GE.2 ) PRINT *,' =+= CALLED CONVRT (ISIZE=',ISIZE,
     *                                '...ISWITCH=',ISWITCH,')'
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
      IF (ISWITCH .EQ. 1) THEN
C-
C ...     Convert from character to ascii integer value
C-
          II = 0
          DO 300 LL = 1, LENGTH
             DO 200 I = 1, ISIZE          
                II = II + 1
                INTVAL(II) = ICHAR( ISTRING(LL)(I:I) )
  200        CONTINUE
  300     CONTINUE
C-
      ELSEIF (ISWITCH .EQ. 2) THEN
C-
C ...     Convert from ascii integer value to character 

          IIE = 0
          DO 600 LL = 1, LENGTH
             IIS = 1 + IIE
             IIE = IIS + (ISIZE-1)
             J = 0
             DO 500 I = IIS, IIE 
                J = J + 1
                ISTRING(LL)(J:J) = CHAR( INTVAL(I) )
  500        CONTINUE
  600     CONTINUE
C-
      ELSEIF (ISWITCH .EQ. 3) THEN
C-
C ...   consolidate BANNER section
C-
        BANNER(1)(1:40)  = VERGFG(1)(1:40)
        BANNER(1)(41:80) = VERGFG(2)(1:40)
        BANNER(2)(1:40)  = VERGFG(3)(1:40)
        BANNER(2)(41:80) = VERGFG(4)(1:40)
        BANNER(3)(1:80)  = STAMP(1:80)
        BANNER(4)(1:80)  = DESC(1)(1:80)
        BANNER(5)(1:80)  = DESC(2)(1:80)
C
        BANNER(6)(1:80)  = BANRM2(1)(1:80)
        BANNER(7)(1:80)  = BANRM2(2)(1:80)
        BANNER(8)(1:80)  = BANRM2(3)(1:80)
        BANNER(9)(1:80)  = BANRM2(4)(1:80)
        BANNER(10)(1:80) = BANRM2(5)(1:80)
C
        BANNER(11)(1:80) = BANCON(1)(1:80)
        BANNER(12)(1:80) = BANCON(2)(1:80)
        BANNER(13)(1:80) = BANCON(3)(1:80)
        BANNER(14)(1:80) = BANCON(4)(1:80)
        BANNER(15)(1:80) = BANCON(5)(1:80)
C-
      ELSE
C-
          PRINT *,' Value of ISWITCH invalid in CONVRT'
          STOP'CONVRT'
      ENDIF
      RETURN
      END
C-
      SUBROUTINE CRACK(I1, NWD, REA, INT, CHA, TYPE, IERR)
C ... ORIGINAL AUTHOR = DON BACH
C ... CRACKS DATA CARDS SIMULATING LIST DIRECTED READS
C ... THIS SUBROUTINE CONFORMS TO ANSI X3.9-1978 (FORTRAN 77)
C ... CARD IS PASSED THROUGH COMMON BLOCK
C-
      IMPLICIT REAL (A-H, O-Z)
C-
      COMMON /CARD/ JREC
C-
      DIMENSION REA(50), INT(50)
      CHARACTER *1 IBLANK, ICMT, ICMD, ICOMMA
      CHARACTER JREC(80)*1, CHA(50)*20, TYPE*9, IBUF*80, 
     *     IFOR(1)*10
      DATA IBLANK/' '/
      DATA ICMT/'*'/
      DATA ICMD/'$'/
      DATA ICOMMA/','/
C
C ... LOOP FOR EACH WORD TO BE READ
      IF (NWD .GT. 50) THEN
        PRINT *, ' OVER-RUN ARRAYS DIMENSIONED TO 50 IN SUB CRACK'
        STOP 'CRACK'
      END IF 
      IF (I1 .GT. 77) RETURN
      NWD1 = NWD
      DO 190 I = 1, NWD1
C ... FIND START OF DATA (FIRST NON-BLANK CHARACTER)
  100   CONTINUE
        IF (JREC(I1) .NE. IBLANK .AND. JREC(I1) .NE. ICOMMA) GO TO 110
        I1 = I1 + 1
        IF (JREC(I1) .EQ. ICOMMA) THEN
          I1 = I1 + 1
          GO TO 190
        END IF 
        IF (I1 .GT. 77) THEN
          NWD = I - 1
          RETURN 
        END IF 
        GO TO 100
C 
  110   CONTINUE
C  FIND END OF DATA
        I2 = I1 + 1
  120   CONTINUE
        IF (JREC(I1) .EQ. '''') THEN
          IF (JREC(I2) .EQ. '''') GO TO 130
        ELSE 
          IF (JREC(I2) .EQ. IBLANK .OR. JREC(I2) .EQ. ICOMMA)
     *          GO TO 130
        END IF 
        I2 = I2 + 1
        IF (I2 .LT. 78) GO TO 120
        I2 = 77
  130   CONTINUE
        IF (JREC(I1) .EQ. '''') I1 = I1 + 1
        LENGTH = I2 - I1
        IF (LENGTH .LT. 1) LENGTH = 1
        I2 = I2 - 1
C ... PACK DATA INTO BUFFER
        WRITE (IBUF,140) (JREC(J), J = I1, I2)
  140   FORMAT (77A1)
C ... SET UP CORRECT FORMAT AND READ DATA
        IF (TYPE(1:4) .EQ. 'REAL') THEN
          WRITE (IFOR,150) LENGTH
  150     FORMAT ('(F', I2, '.0)')
          READ (IBUF,IFOR) REA(I)
        ELSE IF (TYPE(1:7) .EQ. 'INTEGER') THEN
          WRITE (IFOR,160) LENGTH
  160     FORMAT ('(I', I2, ')')
          READ (IBUF,IFOR) INT(I)
        ELSE IF (TYPE(1:9) .EQ. 'CHARACTER') THEN
          WRITE (IFOR,170) LENGTH
  170     FORMAT ('(A', I2, ')')
          READ (IBUF,IFOR) CHA(I)
        ELSE 
          PRINT 180, TYPE
  180     FORMAT (' *** TYPE ', A10, ' is illegal in call to crack ***')
          IERR = IERR + 1
          RETURN 
        END IF 
        IF (JREC(I2+1) .EQ. '''') I2 = I2 + 1
        I1 = I2 + 1
  190 CONTINUE
      RETURN 
      END
      SUBROUTINE FILL
C-
      INCLUDE'gfgv435.inc'
C-
      IF( ITRACE.GE.2 ) PRINT *,' =+= CALLED FILL'
C
C-....FIND MAXIMUM NODE AND ELT NUMBERS.....
C-
      NP = 0
      DO 110 J = 1, MAXE
        IF (IMAT(J) .EQ. 0) GO TO 110
        NE = J
        DO 100 K = 1, 8
          IF (NOP(J,K) .GT. NP) NP = NOP(J,K)
          IF (NOP(J,K) .GT. 0) NCORN(J) = K
  100   CONTINUE
        IF (NCORN(J) .EQ. 7) NCORN(J) = 8
        IF (NCORN(J) .EQ. 5 .AND. NOP(J,4) .EQ. 0) NCORN(J) = 6
  110 CONTINUE
CC-
CC-.....PUT INPUTS INTO PROPER LOCATIONS.....
CC-
C      DO 140 J = 1, NE
C      IF( IMAT(J) .EQ. 0 ) GO TO 140
C      IF( NOP(J,5) .GT. 0 ) GO TO 140
C      DO 130 K = 1, 4
C      IT(K) = NOP(J,K)
C      NOP(J,K) = 0
C  130 CONTINUE
C      KKK = 0
C      DO 135 K = 1, 8, 2
C      KKK = KK + 1
C      NOP(J,K) = IT(KKK)
C  135 CONTINUE
C  140 CONTINUE
C-
C-.....INSERT NEW NUMBERS.....
C-
      DO 150 J = 1, NE
        IF (IMAT(J) .EQ. 0) GO TO 150
        IF (IMAT(J) .GT. 900) GO TO 150
        NCN = NCORN(J)
        JN = J + 1
        DO 140 K = 2, NCN, 2
          IF (NOP(J,K) .GT. 0) GO TO 140
          NP = NP + 1
          NOP(J,K) = NP
          NA = K - 1
          NB = MOD(K+1,NCN)
          IF (NB .EQ. 0) NB = NCN
          NA = NOP(J,NA)
          NB = NOP(J,NB)
C-
C-.....SEARCH FOR OTHER ELEMENT.....
C-
          DO 130 JJ = JN, NE
            IF (IMAT(JJ) .EQ. 0) GO TO 130
            IF (IMAT(JJ) .GT. 900) GO TO 130
            NNCN = NCORN(JJ)
            DO 120 KKK = 2, NNCN, 2
              IF (NOP(JJ,KKK-1) .NE. NB) GO TO 120
              KN = MOD(KKK+1,NNCN)
              IF (KN .EQ. 0) KN = NNCN
              IF (NOP(JJ,KN) .NE. NA) GO TO 120
              NOP(JJ,KKK) = NP
              GO TO 140
  120       CONTINUE
  130     CONTINUE
  140   CONTINUE
  150 CONTINUE
      DO 160 JK = 1, NE
        NCNODES(JK) = NCORN(JK)
  160 CONTINUE
      RETURN 
      END
C
      SUBROUTINE FNDANG(ANG, ANCK, DELAN)
C ... FINDS WHETHER TO USE AN ANGLE IN THE RANGE PI/2
C ... OR PI   ANG    0 FOR SLOPE RULE CHECK
      IMPLICIT REAL (A-H, O-Z)
      DATA PI/3.141592654/

C     no include files so ITRACE is not defined
C     IF( ITRACE.GE.2 ) PRINT *,' =+= CALLED FNDANG (ANG=',ANG,')'
C
      DANCK1 = ANCK - ANG
      IF (ANCK .GT. 0.) GO TO 110
      ANCK2 = (PI / 2.) - ANCK
      DANCK2 = ANCK2 - ANG
      IF (ABS(DANCK1) .LT. ABS(DANCK2)) GO TO 120
  100 CONTINUE
      DELAN = DANCK2
      RETURN 
  110 IF (ANG .GT. 0.) GO TO 120
      ANCK2 =  - (PI - ANCK)
      DANCK2 = ANCK2 - ANG
      IF (ABS(DANCK1) .GT. ABS(DANCK2)) GO TO 100
  120 DELAN = DANCK1
      RETURN 
      END
C
      SUBROUTINE FILE (NT)
      SAVE
C-                          FILE LAST MODIFIED 4-9-2001
C ... Open files as requested by input
C-
      INCLUDE'gfgv435.inc'
C-
      COMMON /DMSCHR/ VERGFG(4), STAMP, DESC(2),
     *                BANGFG(5), BANRM2(5), BANCON(5), BANNER(15),
     *                CHKDMS, RECRD
      CHARACTER CHKDMS*6, RECRD*6
      CHARACTER BANGFG*80, BANRM2*80, BANCON*80, BANNER*80
      CHARACTER VERGFG*40, STAMP*80, DESC*80
C-
      CHARACTER *50 FNAME, FNAME1, FNAME2
      LOGICAL AROUND
C-      
C     PRINT *,' =+= CALLED FILE with IBATCH=',IBATCH,'  NT=',NT
C
C-... NT = 1        Manditory files need to be opened
C-... NT = 2        Optional files opened as requested on $L-card
C-... NT = 3        Collapsed geometry has been requested GF-card
C-... NT = 4        DISSPLA meta plot has been requested  PO-card
C-
      IF (NT .EQ. 1) THEN
C-
C ...   Open manditory input and output files
C-
        PRINT *,' '
        PRINT *,' ==== '       ,VERGFG(3),            ' ===='
        PRINT *,' ==== '       ,VERGFG(4),            ' ===='
        PRINT *,' ====            No Plotting           ===='
        PRINT *,' ==== TABS-MD GEOMETRY FILE GENERATION ===='
        PRINT *,' /// Original Author: Dr. Ian P. King of RMA ///'
        PRINT *,' /// Modified and Maintained by USAE WES-CHL ///'
        PRINT *,' '
        PRINT *,' Enter a --> ?  to receive a response menu'
        PRINT *,' '
C
   50   WRITE (*,60)
   60   FORMAT (' Enter gfgen run control input file name',
     *                                          '  (*.geo)')
   80   READ (*,100) FNAME1
        CALL REPLY(FNAME1, ISWT)
        IF (ISWT .EQ. 2) GO TO 1000    ! quit or stop
        IF (ISWT .EQ. 3) GO TO 50      ! user asked for info
  100   FORMAT (A)
        INQUIRE (FILE=FNAME1,EXIST=AROUND)
        IF (.NOT.AROUND) THEN
             PRINT *,' --> File not found ... try again'
             GO TO 50
        ENDIF
        IN  = 8
        OPEN (IN,FILE=FNAME1,STATUS='OLD')
        INQUIRE (IN,NAME=FNAME1)
C-
        IF( LP.EQ.0) WRITE (*,105) 
  105   FORMAT (' --> $L CARD has full print OFF ...',
     *          ' last chance to over-ride that choice ...')
  110   WRITE (*,120) 
  120   FORMAT (' Enter full print output file name')
        READ (*,100) FNAME2
        CALL REPLY(FNAME2, ISWT)
        IF (ISWT .EQ. 0) THEN
            LP = 7
            OPEN(LP,FILE=FNAME2,STATUS='UNKNOWN')
            INQUIRE (LP,NAME=FNAME2)
            WRITE(LP,130) FNAME1
  130       FORMAT (' GFGEN RUN CONTROL INPUT FILE NAME [ascii]  ', A50)
            WRITE(LP,140) FNAME2
  140       FORMAT (' FULL PRINT OUTPUT FILE NAME [ascii]        ', A50)
          ELSEIF (ISWT .EQ. 1) THEN
C ...       user 'nulled' away the file
            CLOSE(LP,STATUS='DELETE')
            LP = 0
            PRINT *, ' You just turned off the full printout!'
          ELSEIF (ISWT .EQ. 2) THEN
C ...       user want to quit the program          
            GO TO 1000
          ELSEIF (ISWT .EQ. 3) THEN
C ...       user needed information and a menu          
            GO TO 110
        END IF
C-      --------------------------------------------- Optional Files
      ELSE IF (NT.EQ.2) THEN
C-
  150   IF ( LUOLD.LE.0 )  GO TO 200
        WRITE (*,160) 
  160   FORMAT (' Enter binary output geometry file name')
        READ (*,100) FNAME
        CALL REPLY(FNAME, ISWT)
        IF (ISWT .EQ. 0) THEN
C ...     I intentionally set LUOLD = 0 to turn it off 12-30-92
          LUOLD = 2
          LUOLD = 0
CCCcccc   OPEN(LUOLD,FILE=FNAME,STATUS='UNKNOWN',FORM='UNFORMATTED')
CCCcccc   IF (LP.GT.0 .AND. LP.NE.6) WRITE (LP,170) FNAME
  170     FORMAT (' PRIOR 1993 GEOMETRY OUTPUT [bin] FILE NAME ', A50)
        ELSE IF (ISWT.EQ.1) THEN
C ...     user 'nulled' away the file
          LUOLD = 0
        ELSE IF (ISWT .EQ. 2) THEN
C ...     user want to quit the program 
          GO TO 1000
        ELSE IF (ISWT .EQ. 3) THEN
C ...     user needed information and a menu
          GO TO 150
        END IF 
C
  200   IF ( LUNIT.LE.0 )  GO TO 330
        WRITE (*,210) 
  210   FORMAT (' Enter the binary output geometry file name')
        READ (*,100) FNAME
        CALL REPLY(FNAME, ISWT)
        IF (ISWT .EQ. 0) THEN
          LUNIT = 3
          OPEN(LUNIT,FILE=FNAME,STATUS='UNKNOWN',FORM='UNFORMATTED')
C ECGL ADD BEG  REPLACE UNFORMATED with BINARY FOR PC Microsoft FORTRAN 
C ECGL    OPEN(LUNIT,FILE=FNAME,STATUS='UNKNOWN',FORM='BINARY')
C ECGL ADD END
          IF (LP.GT.0 .AND. LP.NE.6) WRITE (LP,220) FNAME
  220     FORMAT (' GEOMETRIC OUTPUT FILE NAME [binary]        ', A50)
        ELSE IF (ISWT.EQ.1) THEN
C ...     user 'nulled' away the file
          LUNIT = 0
          PRINT *, ' You just turned off the binary geometry results!'
        ELSE IF (ISWT .EQ. 2) THEN
C ...     user want to quit the program 
          GO TO 1000
        ELSE IF (ISWT .EQ. 3) THEN
C ...     user needed information and a menu
          GO TO 200
        END IF 
C
  330   IF( IGIN.LE.0 ) GO TO 500
        WRITE (*,340) 
  340   FORMAT (' Enter PRIOR [binary] geometry file name')
        READ (*,100) FNAME
        CALL REPLY(FNAME, ISWT)
        IF (ISWT .EQ. 0) THEN
          IGIN = 4
          OPEN(IGIN,FILE=FNAME,STATUS='OLD',FORM='UNFORMATTED')
C ECGL ADD BEG  REPLACE UNFORMATED with BINARY FOR PC Microsoft FORTRAN 
C ECGL    OPEN(IGIN,FILE=FNAME,STATUS='OLD',FORM='BINARY')
C ECGL ADD END
          IF (LP.GT.0 .AND. LP.NE.6) WRITE (LP,350) FNAME
  350     FORMAT (' PREVIOUS GEOMETRY INPUT FILE NAME [binary] ', A50)
        ELSEIF (ISWT.EQ.1) THEN
C ...     user 'nulled' away the file
          IGIN = 0
          PRINT *, ' You just turned off the prior geometry !'
        ELSEIF (ISWT .EQ. 2) THEN
          GO TO 1000
        ELSEIF (ISWT .EQ. 3) THEN
          GO TO 330
        END IF 
C-        
      ELSE IF (NT.EQ.3) THEN
C
  380   WRITE (*,390) 
  390   FORMAT (' Enter name of collapsed [ascii] geometry file name',
     *          '       to be created by this run')
        READ (*,100) FNAME
        CALL REPLY(FNAME, ISWT)
        IF (ISWT .EQ. 0) THEN
          ICOLL = 98
          OPEN(ICOLL,FILE=FNAME,STATUS='UNKNOWN',FORM='UNFORMATTED')
C ECGL ADD BEG  REPLACE UNFORMATED with BINARY FOR PC Microsoft FORTRAN 
C ECGL    OPEN(ICOLL,FILE=FNAME,STATUS='UNKNOWN',FORM='BINARY')
C ECGL ADD END
          IF (LP.GT.0 .AND. LP.NE.6) WRITE (LP,400) FNAME
  400     FORMAT (' COLLAPSED [ascii] GEOMETRY INPUT FILE NAME ', A50)
        ELSEIF (ISWT.EQ.1) THEN
          CLOSE(ICOLL,STATUS='DELETE')
          ICOLL = 0
          PRINT *, ' You just turned off the collapsed gfgen output!'
        ELSEIF (ISWT .EQ. 2) THEN
          GO TO 1000
        ELSEIF (ISWT .EQ. 3) THEN
          GO TO 380
        END IF 
C
      ELSE IF (NT.EQ.4) THEN
C
C ....  moved the plot file interactive questions & open inside
C ...   of sub pltmsh
C
      ENDIF
C-
  500 CONTINUE
C-
      RETURN 
C-
 1000 IF (IBATCH.EQ.0) THEN
          PRINT *,' Stop was requested'
          STOP'FILE'
      ENDIF
      END
C
C
      SUBROUTINE FIXSLP(J, K, JCHECK, KCHECK, NCN, AN1, DELAN1, DELAN2)
      IMPLICIT REAL (A-H, O-Z)
      INCLUDE'gfgv435.inc'
C-
C ... WILL ADJUST CORNER SLOPES TO SATISFY SLOPE RULE, IF THE ADJUSTMENT
C ... CAN BE MADE 'SAFELY'
C-
      IF( ITRACE.GE.2 ) 
     *    PRINT *,' =+= CALLED FIXSLP(J=',J,' K=',K,'..)'
C 
      N1 = NOP(J,K)
      N3 = MOD(K+2,NCN)
      N3 = NOP(J,N3)
      N2 = NOP(J,K+1)
C ... DECIDE WHERE THE CORRECTION WILL DO THE LEAST DAMAGE
      INODE = 0
      DEL1 = DELAN1
      DEL2 = DELAN2
      IF (ABS(DELAN1) .LT. ABS(DELAN2)) GO TO 110
  100 IF (INODE .GT. 1) GO TO 130
      CALL CKNODE(N3, JNEW, KNEW, AN1, DEL2, N2)
      IF (JNEW .GT. 0) GO TO 120
      INODE = INODE + 1
  110 IF (INODE .GT. 1) GO TO 130
      CALL CKNODE(N1, JNEW, KNEW, AN1, DEL1, N2)
      IF (JNEW .GT. 0) GO TO 120
      INODE = INODE + 1
      GO TO 100
  120 CONTINUE
      JCHECK = JNEW
      KCHECK = KNEW
C ... NODE HAS BEEN FOUND AND CHECKED, COMPUTE NEW MIDSIDE NODES
      CALL MIDPNT(J, K)
      CALL MIDPNT(JCHECK, KCHECK)
      IF (MSN(NOP(JCHECK,KCHECK+1)) .LT. 1) JCHECK = 0
      RETURN 
  130 IF (LP.GT.0) WRITE (LP,140) 
  140 FORMAT ('+', 34X, ' UNABLE TO SAFELY ADJUST SLOPES FOR THIS '
     *     , 'BOUNDARY')
      JCHECK =  - 1
      RETURN 
      END
C
      FUNCTION SIND(X)
      SIND = SIN(X/57.29578)
      RETURN 
      END
C
      FUNCTION COSD(X)
      COSD = COS(X/57.29578)
      RETURN 
      END
C
      SUBROUTINE GRBUGS(IDEBUG)
C******************************************************************
C 
C   THIS ROUTINE SEARCHES THE FINITE ELEMENT GRID FOR MISSING(UNUSED)
C   NODES AND ELEMENTS. EACH ELEMENT IS CHECKED FOR PROPER
C   DEFINITION AND A NODE CROSS-REFERENCE MAP IS PRODUCED.
C   THE MAP SHOWS THE ELEMENTS WHICH REFERENCE EACH NODE AND
C   THE NODE USAGE FOR EACH (CORNER, MIDSIDE, TRANSITION, OR
C    JUNCTION). A USAGE CONFLICT(SINGLE NODE USED AS BOTH MIDSIDE
C    AND CORNER) WILL BE FLAGGED WITH AN ASTERISK BY THE NODE NUMBER.
C   OPTIONALLY, ALL UNUSED NODES CAN BE REMOVED.
C 
C   IT IS ASSUMED THAT NO MORE THAN 15(FIFTEEN) ELEMENTS REFERENCE
C   ANY ONE NODE. IF SO,THE DIMENSION OF -IEFLAG- SHOULD BE
C   INCREASED ACCORDINGLY AND FORMAT 6045 SHOULD BE CHANGED.
C 
C                       **OPTIONS**
C 
C       IDEBUG                        ACTION
C       ------                        ------
C         1            -LIST HIGH AND LOW ELTS AND NODES
C                      -LIST ILL-DEFINED ELTS
C                      -LIST UNUSED ELTS AND NODES
C                      -LIST 1D NORMAL ELEMENTS
C                      -LIST 1D TRANSITIONAL ELEMENTS, JUNCTION, CONTROL STRUCTURES
C                      -LIST 2D CONTROL STRUCTURES
C                      -LIST BAD SLOPE SPECS
C                      -LIST ALL BOUNDARY NODES
C 
C         2            -PERFORM ALL OF OPTION 1 PLUS
C                       GENERATE NODAL CROSS-REFERENCE
C                       LIST
C 
C         3            -PERFORM ALL OF OPTION 2 PLUS
C                       ELIMINATE ALL UNUSED NODES
C 
C         4            -PERFORM ALL OF OPTION 3 PLUS
C                       ELIMINATE UNDEFINED ELEMENTS AND
C                       WRITE A NEW RMA1 INPUT FILE ON
C                       LOGICAL UNIT 98
C 
C            **STEPHEN A. ADAMEC,JR.**
C 
C*******************************************************************
      IMPLICIT REAL (A-H, O-Z)
      INCLUDE'gfgv435.inc'
C-
C-
      DIMENSION ITP(MAXP), ITE(MAXE), IBN(MAXP), IDISP(MAXE), 
     *     ICN(MAXP), ISCR(MAXP), ICS(MAXP), I1D(MAXE), 
     *     ITRD(MAXE), IJEL(MAXE), I1GWN(MAXP), I1DBN(MAXP),
     *     IFC1D(MAXFC), IFC2D(MAXFC), 
     *     IDFC1(MAXFC), IDFC2(MAXFC), I2D(MAXE)
C 
      CHARACTER INFLAG*1, IEFLAG(15)*1
      DATA IBN/MAXP*0/
      DATA ICN/MAXP*0/
      DATA ISCR/MAXP*0/
C 
C   INITIALIZE VARIABLES...
C 
      IF( ITRACE.GE.2 ) PRINT *,' =+= CALLED GRBUGS(IDEBUG=',IDEBUG,')'
C
      IHELE = 0
      IHNOD = 0
      ILELE = 100000
      ILNOD = 100000
      DO 100 I = 1, MAXP
        ITP(I) = 0
        ICS(I) = 0
  100 CONTINUE
      DO 110 I = 1, MAXE
        ITE(I) = 0
  110 CONTINUE
C 
C**********************************************************
C   FIND HIGH AND LOW ELTS AND NODES AND FLAG
C   THOSE ELEMENTS AND NODES WHICH ARE MISSING FROM NETWORK..
C**********************************************************
C 
      DO 140 J = 1, MAXE
        NNODES = 0
        DO 130 K = 1, 8
          NODE = NOP(J,K)
          IF (NODE .LT. 1) GO TO 130
          NNODES = NNODES + 1
          IF (NODE .GT. IHNOD) IHNOD = NODE
          IF (NODE .LT. ILNOD) ILNOD = NODE
C ...     ITP IS FLAG OF WHETHER NODE IS USED
          ITP(NODE) = 1
  130   CONTINUE
        IF (NNODES .LT. 1) GO TO 140
C ...   ITE IS FLAG OF WHETHER ELEMENT IS USED
        ITE(J) = 1
        IF (J .GT. IHELE) IHELE = J
        IF (J .LT. ILELE) ILELE = J
  140 CONTINUE
      IF (LP.GT.0) WRITE (LP,150) IHNOD, IHELE, ILNOD, ILELE
  150 FORMAT ('1',  / 60X, 12('*'),  / 60X, '*GRID CHECK*',  / 
     *        60X, 12('*'), 
     *      // 10X, 'HIGH NODE=', I5, 5X, 'HIGH ELEMENT=', I5,
     *       / 10X, ' LOW NODE=', I5, 5X, ' LOW ELEMENT=', I5)
C 
C*********************************************************
C   LIST UNUSED NODES AND UNDEFINED ELEMENTS...
C*********************************************************
C 
         IF (LP.GT.0 .AND. LP.NE.6) WRITE (LP,160) 
  160    FORMAT ('0', 130('*'),  / 10X, 'UNUSED NODES ',  / )
         NNODES = 0
         DO 170 I = ILNOD, IHNOD
            IF (ITP(I) .GT. 0) GO TO 170
            NNODES = NNODES + 1
            ISCR(NNODES) = I
  170    CONTINUE
         IF (NNODES .GT. 0) THEN
             IF (LP.GT.0 .AND. LP.NE.6) 
     *                    WRITE (LP,180) (ISCR(II),II=1, NNODES)
             WRITE (*,185) (ISCR(II), II=1, NNODES)
  180        FORMAT (5X, 20I5)
  185        FORMAT (' --> BEWARE ... UNUSED NODES =', /, (1X,12I6))
         ELSE 
             IF (LP.GT.0 .AND. LP.NE.6) WRITE (LP,190) 
  190        FORMAT ( / 13X, '*NONE*')
         ENDIF
         IF (LP.GT.0 .AND. LP.NE.6) WRITE (LP,200) 
  200    FORMAT ('0', 130('*'),  / 10X, 'UNDEFINED ELEMENTS ',  / )
         NNODES = 0
         DO 210 J = ILELE, IHELE
            IF (ITE(J) .GT. 0) GO TO 210
            NNODES = NNODES + 1
            ISCR(NNODES) = J
  210    CONTINUE
         IF (NNODES .GT. 0) THEN
            IF (LP.GT.0 .AND .LP.NE.6)
     *          WRITE (LP,180) (ISCR(II),II=1,NNODES)
            WRITE (*,215) (ISCR(II),II=1,NNODES)
  215       FORMAT (' --> BEWARE ... UNUSED ELEMENTS =', /, (12I6))
         ELSE
            IF (LP.GT.0)  WRITE (LP,190) 
         ENDIF
C 
C ***********************************************************
C   LIST ONE-DIMENSIONAL ELEMENTS, TRANSITIONAL ELEMENTS,
C    AND JUNCTION ELEMENTS
C **************************************************************
C 
      N2D   = 0
      N1D   = 0
      NTRD  = 0
      NJEL  = 0
      NFC   = 0
      NFC1D = 0
      NFC2D = 0
      DO 220 I = 1, MAXE
        I2D(I)  = 0
        I1D(I)  = 0
        ITRD(I) = 0
        IJEL(I) = 0
  220 CONTINUE
      DO 225 I = 1 , MAXFC
        IFC2D(I) = 0
        IFC1D(I) = 0
        IDFC1(I) = 0
        IDFC2(I) = 0
  225 CONTINUE
C     
C ... DIAGNOSTICS OF 1-D ELEMENTS
      DO 230 I = ILELE, IHELE
C ...   see if the element is used      
        IF (ITE(I) .EQ. 0) GO TO 230
        IF (IMAT(I).LE.900) THEN
            IF (NOP(I,6).GT.0) THEN
C ...          2D element of the vanilla kind
               N2D = N2D + 1
               I2D(N2D) = I 
            ELSEIF (NOP(I,5).GT.0) THEN                      
C ...          1D transition
               NTRD = NTRD + 1
               ITRD(NTRD) = I 
            ELSE
C ...          1D element of the vanilla kind 
               N1D = N1D + 1
               I1D(N1D) = I
            ENDIF           
        ELSEIF (IMAT(I).GE.904) THEN
C ...       flow control structure
            NFC = NFC + 1
            IF (NOP(I,3).EQ.0) THEN
                NFC1D = NFC1D + 1
                IF (NFC1D .GT. MAXFC) STOP'maxfc'
                IFC1D(NFC1D) = I
                IDFC1(NFC1D) = IMAT(I)
            ELSE
                NFC2D = NFC2D + 1
                IF (NFC2D .GT. MAXFC) STOP'maxfc'
                IFC2D(NFC2D) = I
                IDFC2(NFC2D) = IMAT(I)
            ENDIF                
        ELSE
C ...       1D junction
            NJEL = NJEL + 1
            IJEL(NJEL) = I 
        ENDIF           
  230 CONTINUE       
C-
      IF (LP.GT.0 .AND .LP.NE.6) THEN
         WRITE (LP,240) 
  240    FORMAT ('0', 130('*'),  / 10X, 'ONE-DIMENSIONAL ELEMENTS',/)
         IF (N1D .GT. 0) WRITE (LP,180) (I1D(K), K = 1, N1D)
         IF (N1D .LT. 1) WRITE (LP,190) 
         WRITE (LP,250) 
  250    FORMAT ('0', 130('*'),  / 10X, 'TRANSITIONAL ELEMENTS',/)
         IF (NTRD .GT. 0) WRITE (LP,180) (ITRD(K), K = 1, NTRD)
         IF (NTRD .LT. 1) WRITE (LP,190) 
         WRITE (LP,260) 
  260    FORMAT ('0', 130('*'),  / 10X, 'JUNCTION ELEMENTS',/)
         IF (NJEL .GT. 0) WRITE (LP,180) (IJEL(K), K = 1,NJEL)
         IF (NJEL .LT. 1) WRITE (LP,190) 
         WRITE (LP,270) 
  270    FORMAT ('0', 130('*'), 
     *               / 10X, '1D CONTROL STRUCTURE ELEMENTS',/)
         IF (NFC1D .GT. 0) WRITE (LP,180) (IFC1D(K), K=1,NFC1D)
         IF (NFC1D .LT. 1) WRITE (LP,190)
         WRITE (LP,280) 
  280    FORMAT ('0', 130('*'), 
     *               / 10X, '2D CONTROL STRUCTURE ELEMENTS',/)
         IF (NFC2D .GT. 0) WRITE (LP,180) (IFC2D(K), K=1,NFC2D)
         IF (NFC2D .LT. 1) WRITE (LP,190)                   
         WRITE (LP,290) 
      ENDIF
      IF (N2D .GT. 0) THEN
          WRITE (*,*) '--> Total num of regular 2D ELEM  =',N2D
          WRITE (*,295) I2D(1), I2D(N2D) 
      ENDIF     
      IF (N1D .GT. 0) THEN
          WRITE (*,*) '--> Total num of regular 1D ELEM  =',N1D
          WRITE (*,295) I1D(1), I1D(N1D)
      ENDIF
      IF (NTRD.GT. 0) THEN
          WRITE (*,*) '--> Total num of 1D TRANSITIONS   =',NTRD
          WRITE (*,285) (ITRD(K), K = 1, NTRD)
      ENDIF
      IF (NJEL.GT. 0) THEN
          WRITE (*,*) '--> Total num of 1D JUNCTION ELEM =',NJEL
          WRITE (*,285) (IJEL(K), K = 1, NJEL)
      ENDIF
      IF (NFC1D.GT. 0) THEN
          WRITE (*,*) '--> Total num of 1D STRUCTURES    =',NFC1D
          WRITE (*,*) '    the Elem# and IMAT pairs follow'
          WRITE (*,300) (IFC1D(K), IDFC1(K), K = 1, NFC1D)
      ENDIF    
      IF (NFC2D.GT. 0) THEN
          WRITE (*,*) '--> Total num of 2D STRUCTURES    =',NFC2D
          WRITE (*,*) '    the Elem# and IMAT pairs follow'
          WRITE (*,300) (IFC2D(K), IDFC2(K), K = 1, NFC2D) 

      ENDIF         
  285 FORMAT(5X,10I6)
  290 FORMAT ('0', 130('*'))
  295 FORMAT(5X,'First=',I6,'  Last=',I6)
  300 FORMAT(5X,2I6,6X,2I6,6X,2I6)
C 
C*********************************************************
C   LOOK FOR ILL-DEFINED ELEMENTS...
C*********************************************************
C 
      DO 350 I = ILELE, IHELE
        IF (IMAT(I) .GT. 900) GO TO 350
        IF (ITE(I) .EQ. 0) GO TO 350
        ILL = 0
        NNODES = 0
        JNODES = 0
        DO 305 J = 1, 3
          NODE = NOP(I,J)
          IF (NODE .GE. 1) GO TO 305
          ILL = 1
  305   CONTINUE
        DO 310 J = 4, 5
          NODE = NOP(I,J)
          IF (NODE .GE. 1) GO TO 310
          JNODES = JNODES + 1
  310   CONTINUE
        IF (JNODES .EQ. 1) GO TO 330
        IF (ILL .EQ. 1) GO TO 330
        DO 320 J = 7, 8
          NODE = NOP(I,J)
          IF (NODE .GE. 1) GO TO 320
          NNODES = NNODES + 1
  320   CONTINUE
        IF (NNODES .NE. 1) GO TO 350
  330   IF (LP.GT.0 .AND .LP.NE.6) WRITE (LP,340) I
        WRITE (*,345) I
  340   FORMAT('0', 10X,'***WARNING--ELEMENT ',I5,' IS ILL-DEFINED***')
  345   FORMAT(' **** WARNING... ILL-DEFINED ELEMENT =', I6)
  350 CONTINUE
      IF (LP.GT.0) WRITE (LP,290) 
C 
C******************************************************
C   LOOK FOR BAD SLOPE SPECIFICATIONS ON
C   2D BOUNDARY NODES...
C******************************************************
C 
      DO 400 L = 1, NP
        KNT = 0
        DO 370 J = 1, NE
          IF (IMAT(J) .EQ. 0) GO TO 370
          IF (IMAT(J) .GT. 900) GO TO 370
          NCN = 8
          IF (NOP(J,7) .LT. 1) NCN = 6
          IF (NOP(J,6) .LT. 1) NCN = 5
          IF (NOP(J,4) .LT. 1) NCN = 3
          IF (NCN .LE. 5) GO TO 370
          DO 360 K = 2, NCN, 2
            IF (NOP(J,K) .NE. L) GO TO 360
            KNT = KNT + 1
            N1 = NOP(J,K-1)
            N2 = NOP(J,K)
            N3 = MOD(K+1,NCN)
            N3 = NOP(J,N3)
  360     CONTINUE
  370   CONTINUE
        IF (KNT .NE. 1) GO TO 400
C 
C  CHECK IF A TRANSITION SIDE
        DO 380 KCHCK = 1, NE
          IF (N2 .EQ. NOP(KCHCK,3)) GO TO 400
  380   CONTINUE
C 
C ...   FOUND A BOUNDARY SIDE--CHECK THE SLOPES...
C 
        IBN(N1) = 1
        IBN(N2) =  - 1
        IBN(N3) = 1
        IF (ALPHA(N1) .EQ. 0.0 .OR. ALPHA(N2) .EQ. 0.0 .OR. 
     *      ALPHA(N3) .EQ. 0.0) THEN 
            IF (LP.GT.0) WRITE (LP,390) N1, N2, N3
  390       FORMAT (5X, '***WARNING*** A LACK OF(OR IMPROPER) SLOPE ',
     *      'SPECS FOR BOUNDARY NODES', 3I6, ' MAY HAVE OCCURRED***')
        ENDIF
  400 CONTINUE
      IF (LP.GT.0) WRITE (LP,290) 
C 
      IF (IDEBUG .LT. 3) GO TO 660
C 
C*****************************************************
C   LOOK FOR UNUSED NODES AND GET RID OF THEM...
C*****************************************************
C 
C ... REMOVE NON-EXISTENT NODES FROM MIDSIDE LIST
      DO 420 I = 1, NP
        IF (ITP(I) .GT. 0) GO TO 420
        DO 410 J = 1, NMSN
          IF (MSNKP(J) .EQ. I) MSNKP(J) = 0
  410   CONTINUE
  420 CONTINUE
      ITOP = IHNOD
      NCOLL = 0
      DO 470 I = 1, ITOP
        IF (I .GE. IHNOD) GO TO 480
        IF (ITP(I) .EQ. 1) GO TO 470
C 
C ...   FOUND AN UNUSED NODE--GET RID OF IT...
C 
        NCOLL = NCOLL + 1
        CORD(I,1) = CORD(IHNOD,1)
        CORD(I,2) = CORD(IHNOD,2)
        ALPHA(I)  = ALPHA(IHNOD)
        WD(I)     = WD(IHNOD)
        WIDTH(I)  = WIDTH(IHNOD)
        SS1(I)    = SS1(IHNOD)
        SS2(I)    = SS2(IHNOD)
        WIDS(I)   = WIDS(IHNOD)
        IRDGNN(I) = IRDGNN(IHNOD)
C
        DO 430 ICE = 1, NMSN
          IF (MSNKP(ICE) .EQ. IHNOD) MSNKP(ICE) = I
  430   CONTINUE
C 
        DO 450 J = ILELE, IHELE
          IF (ITE(J) .EQ. 0) GO TO 450
          DO 440 K = 1, 8
            IF (NOP(J,K) .EQ. IHNOD) NOP(J,K) = I
  440     CONTINUE
  450   CONTINUE
        ITP(IHNOD) = 0
        ITP(I) = 1
        IF (IBN(IHNOD) .EQ. 0) GO TO 460
        IBN(I) = IBN(IHNOD)
C 
C   FIND THE NEXT HIGHEST NODE FOR THE NEXT ELIMINATION...
C 
  460   IHNOD = IHNOD - 1
        NP = IHNOD
        IF (ITP(IHNOD) .NE. 1) GO TO 460
C 
  470 CONTINUE
  480 IF (NCOLL .LE. 0) GO TO 500
      IF (LP.GT.0 .AND. LP.NE.6) WRITE (LP,490) NCOLL, NP
      WRITE (*,490) NCOLL, NP
  490 FORMAT ( / 11X, I4, ' UNUSED NODES HAVE BEEN ELIMINATED--HIGH '
     *     , 'NODE IS NOW', I5)
      IF (LP.GT.0) WRITE (LP,290) 
C 
  500 IF (IDEBUG .LT. 4) GO TO 660
C 
C**************************************************
C   GET RID OF UNDEFINED ELEMENTS AND WRITE A NEW
C   GFGEN/RMA1 INPUT FILE ON LOGICAL UNIT 98...
C**************************************************
C 
C   WRITE CORNER NODE SLOPES...
C 
      DO 530 I = 1, MAXE
        IF (ITE(I) .EQ. 0)    GO TO 530
        IF (IMAT(I) .GT. 900) GO TO 530
        NCN = 8
        IF (NOP(J,7) .LT. 1) NCN = 6
        IF (NOP(J,6) .LT. 1) NCN = 5
        IF (NOP(J,4) .LT. 1) NCN = 3
        DO 520 J = 1, NCN, 2
          NODE = NOP(I,J)
          ICN(NODE) = 1
C ...     CHECK FOR ADJOINING MIDSIDE NODES IN CURVED SIDE LIST
C ...     SET ICS=1 IF SLOPE IS TO BE WRITTEN
          J1 = J + 1
          J2 = J - 1
          IF (J2 .LT. 1) J2 = NCN
          DO 510 K = 1, NMSN
            IF (MSNKP(K) .NE. NOP(I,J1) .AND. MSNKP(K) .NE. 
     *           NOP(I,J2)) GO TO 510
            ICS(NODE) = 1
            GO TO 520
  510     CONTINUE
  520   CONTINUE
  530 CONTINUE
C 
      IF (ICOLL.GT.0) THEN
        DO 540 I = 1, NP
          IF (ICN(I) .LT. 1) GO TO 540
          IF (ICS(I) .GT. 0) THEN
            IF( IHEC.EQ.1 .AND. ICOLL.GT.0) WRITE(ICOLL,550) I,ALPHA(I)
            IF( IHEC.EQ.0 .AND. ICOLL.GT.0) WRITE(ICOLL,551) I,ALPHA(I)
          ENDIF
  540   CONTINUE
  550   FORMAT ('GCN', I10, F20.5)
  551   FORMAT (I10,F10.4)  
        IF( IHEC.EQ.0 .AND. ICOLL.GT.0) WRITE (ICOLL,552) 
  552   FORMAT ('9999999999')
C 
C ...   WRITE MIDSIDE NODES ON CURVED BOUNDARY...
C 
        IF( NMSN.EQ.0 ) GO TO 580
        IF (IHEC.EQ.1) THEN
            WRITE (ICOLL,560) (MSNKP(II),II=1,NMSN)
  560       FORMAT('GMN',10I7)
        ELSE
           DO 570 I = 1, NMSN
              WRITE (ICOLL,571) MSNKP(I)
  570      CONTINUE
  571      FORMAT (I10)
           WRITE (ICOLL,552) 
        ENDIF
C 
C ...   WRITE ELEMENT CONNECTIONS...
C 
  580   NUMBER = 0
        DO 600 I = 1, MAXE
          IF (ITE(I) .EQ. 0) GO TO 600
          NUMBER = NUMBER + 1
          IF( IHEC.EQ.1) THEN
              WRITE (ICOLL,590) NUMBER, (NOP(I,J), J = 1, 8), 
     *                          IMAT(I), TH(I)
  590         FORMAT('GE',10I6,F10.4)
          ELSE
              WRITE (ICOLL,591) NUMBER, (NOP(I,J), J = 1, 8), 
     *                          IMAT(I), TH(I)
  591         FORMAT (10I5, F5.1)
          ENDIF
  600   CONTINUE
        IF( IHEC.EQ.0) WRITE (ICOLL,552) 
C 
C ...   WRITE CORNER NODE COORDINATES...
C 
        DO 610 I = 1, NP
          IF (ICN(I) .LE. 0  .AND. IRDGNN(I).LE.0 ) GO TO 610
          IF( IHEC.EQ.1) THEN
               IF( WIDTH(I).GT.0 ) THEN
                   WRITE (ICOLL,615) I, (CORD(I,J),J = 1, 2), WD(I),
     *                            WIDTH(I), SS1(I), SS2(I), WIDS(I)
               ELSE
                   WRITE (ICOLL,615) I, (CORD(I,J),J = 1, 2), WD(I)
               ENDIF
          ENDIF
          IF( IHEC.EQ.0) THEN
               IF( WIDTH(I).GT.0 ) THEN
                   WRITE (ICOLL,616) I, (CORD(I,J),J = 1, 2), WD(I),
     *                            WIDTH(I), SS1(I), SS2(I), WIDS(I)
               ELSE
                   WRITE (ICOLL,616) I, (CORD(I,J),J = 1, 2), WD(I)
               ENDIF
          ENDIF
  610   CONTINUE
  615   FORMAT('GNN',I6,2F14.3,F8.2,1X,F9.1,1X,2F6.2,1X,F9.1)
  616   FORMAT(I10,7F10.2)
        IF( IHEC.EQ.0 ) WRITE (ICOLL,552) 
C 
C ...   WRITE REORDERING LIST
C 
        IF (IRO .EQ. 1 .OR. IRO .EQ. 2 .OR. IRO .EQ. 3) THEN
            DO 630 I = 1, KN4
            IF(IHEC.EQ.1) WRITE (ICOLL,640) (LISTN(I,J),J =1, LISTO(I))
            IF(IHEC.EQ.0) WRITE (ICOLL,641) (LISTN(I,J),J =1, LISTO(I))
  630       CONTINUE
  640       FORMAT('GO',12I6)
  641       FORMAT (16I5)
            IF( IHEC.EQ.0) WRITE (ICOLL,552) 
        END IF 
C 
        IF (LP.GT.0) THEN
            WRITE (LP,290) 
            WRITE (LP,650) NUMBER
            WRITE (LP,290) 
        ENDIF
      ENDIF
  650 FORMAT (5X, 'A NEW GFGEN/RMA1 INPUT FILE HAS BEEN WRITTEN TO '
     *     , 'UNIT 98--',  / 5X, 'THE HIGH ELEMENT NUMBER IS NOW', I5)
C 
C 
C*******************************************************
C   GENERATE NODAL CROSS-REFERENCE MAP...
C*******************************************************
C 
  660 IF (IDEBUG .LT. 2) GO TO 770
      IF (LP .LE. 0 ) GO TO 790
      WRITE (LP,670) 
  670 FORMAT ('0', 52X, 'NODE CROSS-REFERENCE LISTING',  
     *        // 5X, 'NODE', 10X, 'ELEMENTS',  / )
      DO 760 I = ILNOD, IHNOD
        NELTS = 0
        INFLAG = ' '
        IF (ITP(I) .GT. 0) GO TO 690
        WRITE (LP,680) I
  680   FORMAT (4X, I5, 10X, '*UNUSED*')
        GO TO 760
  690   DO 710 J = ILELE, IHELE
          NCN = 8
          IF (NOP(J,7) .LT. 1) NCN = 6
          IF (NOP(J,6) .LT. 1) NCN = 5
          IF (NOP(J,4) .LT. 1) NCN = 3
          DO 700 K = 1, 8
            NODE = NOP(J,K)
            IF (NODE .NE. I) GO TO 700
            NELTS = NELTS + 1
            ITE(NELTS) = J
            IEFLAG(NELTS) = 'C'
            IF (MOD(K,2) .EQ. 0)           IEFLAG(NELTS) = 'M'
            IF (NCN .EQ. 5 .AND. K .GE. 3) IEFLAG(NELTS) = 'T'
            IF (IMAT(J) .GT. 900)          IEFLAG(NELTS) = 'J'
  700     CONTINUE
  710   CONTINUE
        IF (NELTS .LT. 2) GO TO 740
        NMID = 0
        DO 720 K = 1, NELTS
C ...    DOES A MID-SIDE NODE OCCUR MORE THAN TWICE?
          IF (IEFLAG(K) .NE. 'M') GO TO 720
          NMID = NMID + 1
  720   CONTINUE
        IF (NMID .GT. 2) INFLAG = '*'
        DO 730 K = 2, NELTS
C ...     DOES A NODE FLAGE AS BOTH A CORNER AND A MID-SIDE?
          IF (IEFLAG(K) .EQ. IEFLAG(1)) GO TO 730
          IF (IEFLAG(K) .EQ. 'J' .OR. IEFLAG(1) .EQ. 'J') GO TO 730
          IF (IEFLAG(K) .EQ. 'T' .OR. IEFLAG(1) .EQ. 'T') GO TO 730
          INFLAG = '*'
          GO TO 740
  730   CONTINUE
  740   WRITE (LP,750) I, INFLAG, (ITE(K), IEFLAG(K), K = 1, NELTS)
  750   FORMAT (1X, I8, A1, 9X, 15(I6,A1))
  760 CONTINUE
C 
      IF (LP.EQ.0) GO TO 790
      WRITE (LP,290) 
C 
C***************************************************
C   LIST THE 2D BOUNDARY NODES WHICH REQUIRE BOUNDARY
C   SPECIFICATIONS IN THE F.E. MODEL...
C***************************************************
C 
  770 KNT = 0
      DO 780 I = 1, NP
        IF (IBN(I) .EQ. 0) GO TO 780
        KNT = KNT + 1
        IDISP(KNT) = I
  780 CONTINUE
      WRITE (LP,785) KNT, (IDISP(J), J = 1, KNT)
  785 FORMAT ( // 10X, 'THE FOLLOWING', I5, ' 2D BOUNDARY NODES MAY '
     *     , 'NEED B.C. SPECS APPLIED IN THE F.E. MODEL ',  /, 
     *     (1X, 20I6))
      WRITE (LP,290) 
C 
C *******************************************************************
C   LIST THE 1D BOUNDARY NODES WHICH REQUIRE SPECS IN THE F.E. MODEL
C *******************************************************************
C 
  790 DO 800 I = 1, NP
        I1DBN(I) = 0
  800 CONTINUE
      N1DBN = 0
      DO 840 I = 1, NP
        KNT = 0
        DO 830 J = 1, NE
          NCN = 8
          IF (NOP(J,7) .LT. 1)  NCN = 7
          IF (NOP(J,7) .LT. 1)  NCN = 6
          IF (NOP(J,6) .LT. 1)  NCN = 5
          IF (NOP(J,5) .LT. 1)  NCN = 4
          IF (NOP(J,4) .LT. 1)  NCN = 3
          IF (IMAT(J) .GE. 904) THEN
C ...         Is this a 1D or a 2D control structure
              IF (NOP(J,3).EQ.0) NCN = 2
              NCN = 8
          ENDIF       
          IF (NCN .GT. 5) GO TO 830
          DO  K = 1, NCN
            IF (I .EQ. NOP(J,K)) KNT = KNT + 1
          END DO
  830   CONTINUE
        IF (KNT .NE. 1) GO TO 840
C 
C ...   found a 1D boundary node
C 
        N1DBN = N1DBN + 1
        I1DBN(N1DBN) = I
  840 CONTINUE
      IF (LP.GT.0 .AND. LP .NE. 6) 
     *    WRITE (LP,850) N1DBN, (I1DBN(J), J = 1, N1DBN)
      WRITE (*,855) N1DBN, (I1DBN(J), J = 1, N1DBN)
  850 FORMAT ( // 10X, 'THE FOLLOWING', I5, ' 1D BOUNDARY NODES '
     *     , 'SHOULD HAVE B.C. SPECS APPLIED IN THE F.E. MODEL ', 
     *      /, (1X, 20I6))
  855 FORMAT ( ' --> Total num of 1D boundary nodes ' 
     *         'requiring a  B.C. spec in RMA2 =',I6,
     *         /, (5X, 10I6))
      IF (N1DBN .LT. 1 .AND. LP .GT. 0) WRITE (LP,190) 
      IF (LP.GT.0) WRITE (LP,290) 
C 
C  LIST NODES REQUIRING GWN CARDS
C 
      DO 860 I = 1, NP
        I1GWN(I) = 0
  860 CONTINUE
      N1GWN = 0
      DO 880 I = 1, NP
        DO 870 J = 1, NE
          NCN = 8
          IF (NOP(J,7) .LT. 1)  NCN = 7
          IF (NOP(J,7) .LT. 1)  NCN = 6
          IF (NOP(J,6) .LT. 1)  NCN = 5
          IF (NOP(J,5) .LT. 1)  NCN = 4
          IF (NOP(J,4) .LT. 1)  NCN = 3
          IF (IMAT(J) .GE. 904) THEN
C ...         This a either a 1D or a 2D control structure
              IF (NOP(J,3).EQ.0) THEN
                  NCN = 2
                  IF (NOP(J,1) .EQ. I .OR. NOP(J,2) .EQ. I) THEN
                     N1GWN = N1GWN + 1
                     I1GWN(N1GWN) = I
                     GO TO 880
                  ENDIF
              ELSE
                  NCN = 8
              ENDIF
          ENDIF          
          IF (NCN .GT. 5) GO TO 870       
C
          IF (NOP(J,1) .EQ. I .OR. NOP(J,3) .EQ. I) THEN
            N1GWN = N1GWN + 1
            I1GWN(N1GWN) = I
            GO TO 880
          END IF 
  870   CONTINUE
  880 CONTINUE
      IF (LP .GT. 0 .AND. LP .NE. 6)
     *WRITE (LP,890) N1GWN, (I1GWN(K), K = 1, N1GWN)
      WRITE (*,895)  N1GWN, (I1GWN(K), K = 1, N1GWN)
      IF (N1GWN .LT. 1 .AND. LP .GT. 0) WRITE (LP,190) 
      IF (LP .GT. 0) WRITE (LP,290) 
  890 FORMAT ( // 10X, 'THE FOLLOWING', I5, ' 1D NODES REQUIRE A '
     *     , 'WIDTH TO BE SPECIFIED IN THE F.E. MODEL',  /, (1X,20I6))
  895 FORMAT ( ' --> Total num of 1D nodes that require',
     *               ' a width assignment =', I6,
     *         /, (5X,10I6))
      RETURN 
      END
C
      SUBROUTINE KCON
C 
C ... ESTABLISH ELEMENT CONNECTED TO ELEMENT TABLE ...
C 
      INCLUDE'gfgv435.inc'
C 
      IF( ITRACE.GE.2 ) PRINT *,' =+= CALLED KCON '
C      
C     INITIALIZE
C 
C
      DO 100 J = 1, NCM
        DO 100 N = 1, NP
          NECON(N,J) = 0
  100 CONTINUE
      DO 110 J = 1, NCMI
        DO 110 M = 1, NE
          ICON(M,J) = 0
  110 CONTINUE
      DO 120 N = 1, NP
        NDELM(N) = 0
  120 CONTINUE
C 
C ... FORM TABLE OF ELEMENTS CONNECTED TO EACH NODE
C 
      DO 140 M = 1, NE
        DO 130 K = 1, 8
          N = NOP(M,K)
          IF (N .EQ. 0) GO TO 140
          NDELM(N) = NDELM(N) + 1
          J = NDELM(N)
          NECON(N,J) = M
  130   CONTINUE
  140 CONTINUE
C 
C ... CONVERT TABLE TO ELEMENT TO ELEMENT CONNECTION
C 
      DO 210 N = 1, NP
C 
C ...   PLACE PAIRS OF ENTRIES FOR EACH NODE INTO APPROPRIATE ROWS
C 
        INL = NDELM(N) - 1
C 
C ...   SKIP OUT WHEN ONE ELEMENT OR LESS NODE
C 
        IF (INL .LE. 0) GO TO 210
        DO 200 J = 1, INL
          M = NECON(N,J)
C 
C ...     PROCESS SECOND ELEMENT IN A GIVEN ROW
C 
          DO 190 K = J + 1, INL + 1
            MR = NECON(N,K)
            MS = M
C 
C ...       PROCESS EACH DIRECTION OF CONNECTION
C 
            DO 180 MX = 1, 2
C 
C ...         SEARCH IN CASE CONNECTION ALREADY FOUND
C 
              DO 160 L = 1, NCMI
                IF (ICON(MS,L) .NE. 0) GO TO 150
                ICON(MS,L) = MR
                GO TO 170
  150           IF (ICON(MS,L) .EQ. MR) GO TO 170
  160         CONTINUE
C 
C ...         REVERSE MR-MS FOR SECOND PASS
C 
  170         CONTINUE
              MS = MR
              MR = M
  180       CONTINUE
C 
C ...       END LOOP ON SECOND ELEMENT
C 
  190     CONTINUE
C 
C ...     END LOOP ON FIRST ELEMENT
C 
  200   CONTINUE
C 
C ...   END LOOP FOR THIS NODE
C 
  210 CONTINUE
C 
C ... PROCESS TO FIND NUMBER OF ACTIVE ELEMENTS
C 
      NAE = 0
      NTE = NE + 1
      DO 230 M = 1, NE
        IF (IMAT(M) .LT. 1) GO TO 220
        NAE = NAE + 1
        MLIST(NAE) = M
        GO TO 230
  220   NTE = NTE - 1
        MLIST(NTE) = M
  230 CONTINUE
      RETURN 
      END
C
      SUBROUTINE MIDPNT(J, K)
      IMPLICIT REAL (A-H, O-Z)
      INCLUDE'gfgv435.inc'
C-
C  WILL FIND THE COORDINATES OF A BOUNDARY MIDSIDE NODE
C-
      DATA VOID/-1.0E36/
C
      IF( ITRACE.GE.2 ) PRINT *,' =+= CALLED MIDPNT (J=',J,'K=',K,')'
C
      NCN = 6
      IF (NOP(J,7) .NE. 0) NCN = 8
      N1 = NOP(J,K)
      N2 = NOP(J,K+1)
      N3 = MOD(K+2,NCN)
      N3 = NOP(J,N3)
      S1 = ALPHA(N1)
      S2 = ALPHA(N2)
      S3 = ALPHA(N3)
      DX = CORD(N3,1) - CORD(N1,1)
      DY = CORD(N3,2) - CORD(N1,2)
      SA = DY / DX
C ... SOME SPECIFIED..SOME NOT...
C-
      DX = CORD(N3,1) - CORD(N1,1)
      DY = CORD(N3,2) - CORD(N1,2)
      SA = DY / DX
      IF (ABS(DX) .LT. 1.0D - 5) SA = 0.0
      IF (S1 .GT. VOID .AND. S3 .GT. VOID) GO TO 110
C-
C-.....MID POINT TRANSFER CASE.....
C-
      IF (S1 .GT. VOID) GO TO 100
      S1 = S2
      GO TO 110
  100 S3 = S2
C-
C-.....CHECK FOR SPECIAL CASES.....
C-
  110 IF (ABS(S1-S3) .GT. 1.0D - 3) GO TO 130
      IF (ABS(DX) .LT. 1.0D - 4) GO TO 170
      IF (ABS(SA-S1) .GT. 1.0D - 3) THEN
          IF (LP.GT.0 .AND .LP .NE. 6) WRITE (LP,120) J, N1, N2, N3
          WRITE (*,120) J, N1, N2, N3
      ENDIF
  120 FORMAT ( / 10X, '*** SLOPE ERROR AT ELT', I5, ' NODES', 3I5, /)
      ALPHA(N2) = SA
      GO TO 170
C-
C-.....COMPUTE CORDS.....
C-
  130 IF (ABS(S1) .GT. 9000.) GO TO 150
      IF (ABS(S3) .GT. 9000.) GO TO 160
      CORD(N2,1) = CORD(N1,1) + (DX * (S1 - 3. * S3) + 2. * DY) / 
     *     (4. * (S1 - S3))
      CORD(N2,2) = CORD(N1,2) + (DY * (3.0 * S1 - S3) - 2.0 * 
     *     DX * S1 * S3) / (4.0 * (S1 - S3))
  140 CONTINUE
      ALPHA(N2) = SA
      GO TO 170
  150 CORD(N2,1) = CORD(N1,1) + DX / 4.
      CORD(N2,2) = CORD(N1,2) + (3. * DY - 2. * DX * S3) / 4.
      GO TO 140
  160 CORD(N2,1) = CORD(N1,1) + DX * 3. / 4.
      CORD(N2,2) = CORD(N1,2) + (DY + 2. * DX * S1) / 4.
      GO TO 140
  170 CONTINUE
  180 CONTINUE
      IF (LP .GT. 0) WRITE (LP,190) N2, CORD(N2,1), CORD(N2,2)
  190 FORMAT (' NEW COORDINATES FOR NODE', I5, ' = X  ', F12.2, ' Y  '
     *    , F12.2)
      RETURN 
      END
C
      SUBROUTINE MOVFNT (KREC)
C 
C ... GET ELEMENT THAT INCREASES FRONT WIDTH LEAST ...
C 
      INCLUDE'gfgv435.inc'
C 
C ... INITIALIZE
C 
      IF( ITRACE.GE.2 ) PRINT *,' =+= CALLED MOVFNT (KREC=',KREC,')'
C
      MSAV = 9999999
      NSN  = 99999
C 
C ... SKIP IF KREC ALREADY DEFINED
C 
      IF (KREC .GT. 0) GO TO 120
C 
C ... SEARCH ADJACENT ELEMENTS
C 
      DO 110 K = 1, NAD
        NEL = IENXT(K)        
        
C ...   G.Brown fix Reorder #2 and imat=0 combo, 6-18-1997
 
        IF (IMAT(NEL).EQ.0) GO TO 110  
C 
C ...   GET SUMS FOR NEL
C 
        CALL SUMIT(NEL)
C 
C ...   MSA IS THE AVERAGE PER NODE ADDED
C 
        MSA = MSUM

        IF (NDP .GT. 1) MSA = (MSUM + NDP / 2) / NDP
C 
C ...   CHECK IF IT IS LESS
C 
        IF (MSA .GT. MSAV) GO TO 110
        IF (MSA .LT. MSAV) GO TO 100
C 
C ...   IF EQUAL TAKE CASE WITH LEAST NODES ADDED
C 
        IF (NDP .GE. NSN) GO TO 110
  100   KREC = NEL
        NSN = NDP
        MSAV = MSA
  110 CONTINUE
  120 CONTINUE
C 
C ... GET INFORMATION AGAIN FOR SELECTED ELEMENT
C 
      CALL SUMIT(KREC)
C 
C ...                          ipk 04-18-97 change to store max front width
      if (NFWS .gt. NFWSAV)  NFWSAV = NFWS
      IF (MSUM .EQ. 9999999) MSUM = 0
      MTSUM = MTSUM + MSUM
C ...                          ipk 04-18-97 change to pseudo integer*8
 125  continue
      if(MTSUM .gt. 100000000) then
        MTSUM1=MTSUM1+1
        MTSUM=MTSUM-100000000
        go to 125
      endif
C ...                          ipk 04-18-97 end change
C 
C ... UPDATE LIST OF NODES IN FRONT
C 
      MPN = MP
      IF (MP .EQ. 0) GO TO 160
      IF (NDP .EQ. 0) GO TO 160
C 
C ... REMOVE THE DROPPED NODES
C 
      DO 140 N = 1, NDP
C 
C ...   FIND THE NODE TO BE DROPPED IN LIST
C 
        DO 130 M = 1, MP
          IF (LIST(M) .NE. NDROP(N)) GO TO 130
          LIST(M) =  - LIST(M)
          GO TO 140
  130   CONTINUE
  140 CONTINUE
C 
C ... NOW DROP THEM
C 
      MPN = 0
      DO 150 M = 1, MP
        IF (LIST(M) .LT. 0) GO TO 150
        MPN = MPN + 1
        LIST(MPN) = LIST(M)
  150 CONTINUE
C 
C ... NOW ADD NEWLY GENERATED NODES
C 
      IF (NNEW .EQ. 0) GO TO 200
  160 DO 190 M = 1, NNEW
C 
C ...   FIRST SEE IF LNEW IS IN DROP LIST
C 
        IF (NDP .EQ. 0) GO TO 180
        DO 170 N = 1, NDP
          IF (LNEW(M) .EQ. NDROP(N)) GO TO 190
  170   CONTINUE
  180   CONTINUE
        MPN = MPN + 1
        LIST(MPN) = LNEW(M)
        K = LNEW(M)
        NINC(K) = 1
  190 CONTINUE
C 
C ... REDUCE COUNT OF ELEMENTS ACQUIRED AT THE NODES OF THE ELEMENT
C 
  200 CONTINUE
      MP = MPN
      DO 210 K = 1, 8
        N = NOP(KREC,K)
        IF (N .EQ. 0) GO TO 210
        NDELM(N) = NDELM(N) - 1
  210 CONTINUE
      RETURN 
      END
C
      SUBROUTINE ORDER (N)
      IMPLICIT REAL (A-H, O-Z)
      INCLUDE'gfgv435.inc'
C-
      DIMENSION NLIST(160)
C
      IF( ITRACE.GE.2 ) PRINT *,' =+= CALLED ORDER (N=',N,')'
C
      ITR_OLD = ITRACE
C      
      NLO =  - 15
  100 NLO = NLO + 16
      NHI = NLO + 15
      READ (IN,150) (NLIST(N), N = NLO, NHI)
      DO 110 N = NLO, NHI
        IF (NLIST(N) .EQ. 0) GO TO 120
  110 CONTINUE
      GO TO 100
  120 CONTINUE
      NHI = N - 1
      NELT = 2
      N = NLIST(1)
      IF (N .GT. MAXP) RETURN
C-
      MAXC = 60
      MPQ_KEEP       = MPQ
      MPQ_CLICK_KEEP = MPQ_CLICK

C ... BpD 08-21-97 initialize overflow variable and max front width
      MPQ_CLICK = 0
      NFWSAV    = 0
      MPQ       = 0
      ISUM      = 0

C-
      IF (LP .GT. 0 .AND. LP.NE.6) WRITE (LP,130) NLIST(1), NLIST(NHI)
      WRITE (*,135) NLIST(1), NLIST(NHI)
  130 FORMAT (//  10X, 'REORDER LIST WITH  STARTING NODE =', I6, /,
     *            10X, '                   LAST NODE     =', I6)
  135 FORMAT (/ ' --> FOR THE REORDER LIST WITH',
     *          ' STARTING NODE =', I6,' ... LAST NODE =', I6)

      IF (NPRT .GT. 0 .AND. LP. GT. 0) WRITE (LP,140) 
  140 FORMAT ( // 10X, 'INTERMEDIATE STEPS PRINTED BELOW' / 10X, 
     *     'NODE, BANDWIDTH, FRONTWIDTH, AND CONNECTIONS')
  150 FORMAT (16I5)

      NEL = 1
      IEL(1) = N
      MP = 0
      NOD = 1
      DO 180 I = 1, MAXC
        JJ = ICON(N,I)
        IF (JJ .EQ. 0) GO TO 180
        DO 160 M = 1, MAXC
          IF (ICON(JJ,M) .EQ. N) GO TO 170
  160   CONTINUE
        GO TO 180
  170   ICON(JJ,M) =  - ICON(JJ,M)
  180 CONTINUE
      IF (NEL .EQ. 1) GO TO 290
C     SET VALUES FOR EACH NEW POINT ELIMINATED
C 
  190 DO 200 I = 1, 41
        DO 200 J = 1, 40
           ICOL(I,J) = 0
  200 CONTINUE
      NOD = NOD + 1
      MIST = 0
      NR = 1
      NADM(NR) = 100
      DO 210 M = 1, MP
        II = LIST(M)
        CALL ADJPT (II, M)
  210 CONTINUE
  220 M = ICOL(1,1)
  230 N = LIST(M)
      IF (IHOLD(1) .LT. 199) GO TO 240
      M = 1
      N = LIST(1)
  240 CONTINUE
      IA = IHOLD(M)
      NEL = NEL + 1
      IF (NLIST(NELT) .EQ. 0) GO TO 260
      NELT = NELT + 1
      N = NLIST(NEL)
      DO 250 MM = 1, MP
        M = MM
        IF (LIST(M) .EQ. N) GO TO 260
  250 CONTINUE
      MP = MP + 1
      LIST(MP) = N
      M = MP
  260 CONTINUE
      IEL(NEL) = N
      DO 270 I = 1, M
         IHOLD(I) = IHOLD(I) + 1
  270 CONTINUE
      MP = MP - 1
      IF (M .GT. MP) GO TO 290
      DO 280 I = M, MP
        IHOLD(I) = IHOLD(I+1) + 1
        LIST(I) = LIST(I+1)
  280 CONTINUE
  290 CONTINUE
C     ADD TO COLUMN ADJACENT POINT OF ELIMINATED POINT
C 
      DO 300 J = 1, MAXC
        II = ICON(N,J)
        IF (II .LE. 0) GO TO 300
        MP = MP + 1
        IHOLD(MP) = 1
        LIST(MP) = II
  300 CONTINUE
      MPQ = MP * MP + MPQ
      ISUM = ISUM + IA

C ...                    BpD 08-21-97 add pseudo interger *8
  305 continue
      IF (MPQ .GT. 100000000) then
        MPQ_CLICK = MPQ_CLICK + 1
        MPQ  = MPQ - 100000000
        GO TO 305
      ENDIF


      IF (NPRT .GT. 0 .AND. LP .GT. 0) WRITE (LP,310) N, IA, MP
  310 FORMAT (3I5)
      IF (NPRT .GT. 1 .AND. LP .GT. 0) 
     *                         WRITE (LP,320) (LIST(J), J = 1, MP)
  320 FORMAT (20X, 25I4)
      DO 360 I = 1, MAXC
        JJ = ICON(N,I)
        IF (JJ .LE. 0) GO TO 360
        DO 350 M = 1, MAXC
          K = IABS(ICON(JJ,M))
          IF (K .EQ. 0) GO TO 350
          DO 330 MM = 1, MAXC
            IF (ICON(K,MM) .EQ. JJ) GO TO 340
  330     CONTINUE
          GO TO 350
  340     ICON(K,MM) =  - ICON(K,MM)
  350   CONTINUE
  360 CONTINUE
      IF (NOD .LT. NPM) GO TO 190
C ... ----------------------------------

C ...                    BpD 08-21-97 add change to store max front width
C ...                    unfortunately I don't see how to compute NFWS
      IF (NFWS .GT. NFWSAV) NFWSAV = NFWS 


      IF (LP .GT. 0 .AND. LP .NE. 6)  WRITE (LP,370) 
     *                                MPQ_CLICK, MPQ, ISUM, NFWSAV, 
     *                                (IEL(N), N = 1, NPM)
      WRITE (*,375) MPQ_CLICK, MPQ, ISUM, NFWSAV
  370 FORMAT ( // 10X, 'REORDERING SUM =',I4,' e08 + ',I10, 
     *             2X, 'BAND SUM =',I10,'  FRONT WIDTH=?',I6,
     *         // 10X, 'REORDERING NODE SEQUENCE LISTED BELOW' / 
     *         (5X, 10I7))
  375 FORMAT (' --> REORDERING SUM =', I4,' e08 + ',I10,
     *              2X,'BAND SUM  =', I10,'  FRONT WIDTH=?',I6)

      IF (MPQ_CLICK .GT. MPQ_CLICK_KEEP) THEN
C ...     the old stuff is 100% better than the present
          MPQ = MPQ_KEEP
          MPQ_CLICK = MPQ_CLICK_KEEP         !  8-21-97 
          GO TO 500
      ENDIF

      IF (MPQ_CLICK .LT. MPQ_CLICK_KEEP) THEN
C ...     no question about it ... 
C ...     the present MPQ and MPQ_CLICK are best so far
          DO N = 1, NPM
             MSN(N) = IEL(N)
          END DO
          GO TO 500
      ENDIF

      IF (MPQ_CLICK .EQ. MPQ_CLICK_KEEP) THEN
C ...     clicks are the same, so now check MPQ values

          IF (MPQ .EQ. MPQ_KEEP) GO TO 500

          IF (MPQ .GT. MPQ_KEEP) THEN
C ...         So far old reorder list is the best ... 
              MPQ = MPQ_KEEP
              MPQ_CLICK = MPQ_CLICK_KEEP         !  8-21-97 
              GO TO 500
          ENDIF

C ...     The present MPQ and MPQ_CLICK reflect the best list so far
C ...     keeps the list of elements

          DO N = 1, NPM
             MSN(N) = IEL(N)
          END DO
      ENDIF

  500 RETURN 
      END
C
      SUBROUTINE ORDER1
C 
C ... FIND ORDER AND FRONT SUM FOR A GIVEN ELEMENT No. START POINT ...
C 
      INCLUDE'gfgv435.inc'
C 
      IF( ITRACE.GE.2 ) PRINT *,' =+= CALLED ORDER1'
      ITR_OLD = ITRACE
C
C ... SET LIST OF INCORPORATED NODES
C 
      DO 100 N = 1, NP
         NINC(N) = 0
  100 CONTINUE
C 
C ... SET COUNTER ON ELEMENTS
C 
      KNT = 0
      MTSUM = 0
c ... ipk 04-18-97 initialize overflow variable and max front width
      MTSUM1 = 0
      NFWSAV = 0
C 
C ... PROCESS THROUGH ELEMENTS
C 
  110 CONTINUE
C 
C ... SET MLIST FROM INPUT IF NON-ZERO WE MUST FIND KREC
C 
      KREC = MLIST(KNT+1)
C 
C ... GET NEXT ELEMENT TO ADDED
C 
      IF (ITRACE.GT.0 .AND. KNT .GT. 5) THEN
          ITRACE = 0    !  Sub ORDER1 temporarily turns off trace
          PRINT *,' =+= ORDER1 temporarily turns off subroutine trace'
      ENDIF
          
      CALL MOVFNT(KREC)
C 
C ... SAVE SELECTED VALUE
C 
      MLIST(KNT+1) = KREC
      KNT = KNT + 1
C 
C ... UPDATE FRONT AND CONNECTION TABLES
C 
      CALL UPFNT(KREC)
C 
C ... TEST FOR FULL SET OF ELEMENTS
C 
      IF (KNT .LT. NAE) GO TO 110
C 
C ... FOR COMPLETE ORDER CHECK IF IT IS IMPROVEMENT
C
      ITRACE = ITR_OLD
      IF (ITRACE.GT.0) PRINT *,' =+= ORDER1 last looped on KREC =',KREC
C        
      CALL CHKOUT
C 
C ... FINISHED
C 
C
      RETURN 
      END
C
      SUBROUTINE READFF(IC1, IC2, IC3, RVALUE, NVALUE, IERR, IF)
C*******************************************************
C  FREE-FORMAT INPUT FOR THE CYBER 205 (ALMOST FORTRAN-77)
C 
C   -READFF- READS AN 80-CHARACTER RECRD FROM LOGICAL
C   UNIT 8 AND RETURNS THE FIRST 3 CHARACTERS IN
C   -IC1-,-IC2- AND -IC3-(IN A1 FORMAT). THE REMAINING
C   77 CHARACTERS ARE SCANNED AND CONVERTED TO FLOATING
C   FREE-FORMAT FIELDS. EACH OF THE FIELDS MUST BE
C   TERMINATED BY ONE COMMA OR BLANK; HOWEVER, LEADING
C   BLANKS IN A FIELD ARE IGNORED.
C   THE NUMBER OF FLOATING VALUES FOUND IS STORED
C   IN -NVALUE- AND THE VALUES THEMSELVES IN -RVALUE-;
C   THE USER SHOULD FIRST CHECK THE VALUE OF -IERR-
C   FOR THE FOLLOWING
C 
C   IERR = 0     (NORMAL READ--NO ERRORS)
C   IERR =-1     (END OF FILE ON READ)
C   IERR =+N     (ILLEGAL CHARACTER IN COLUMN N)
C 
C   ORIGINAL AUTHOR =  STEPHEN A. ADAMEC,JR.
C*******************************************************
      IMPLICIT REAL (A-H, O-Z)
C
      INCLUDE'gfgv435.inc'
C-
      COMMON /CARD/ JREC
C
      CHARACTER JREC(80)*1
      CHARACTER *1 IC1, IC2, IC3
      DIMENSION RVALUE(50)
C ECGL ADD BEG  Required for PC Microsoft FORTRAN 3/22/96
      CHARACTER CHA(50)*20
      DIMENSION INT(50)
C ECGL ADD END
C-
      DO 100 I = 1, 50
        RVALUE(I) = 0.
  100 CONTINUE
      IC1 = 'n'
      IC2 = 'n'
      IC3 = 'n'
C 
C ----------------------------------------- Fixed field read 
      IF (IF .GT. 0) THEN
        IER = 0
        NVALUE = 8
        READ (IN,120,ERR= 130,END = 180) IC1, IC2, IC3, 
     *                                   (RVALUE(I), I = 1, 8)
        IF (IC1 .EQ. 'G' .AND. IC2 .EQ. 'E') THEN
          BACKSPACE IN
          READ (IN,110,ERR= 130,END = 180) IC1, IC2, 
     *                                   (RVALUE(I), I = 1, 11)
        END IF 
  110   FORMAT (2A1, 6X, F8.0, 8F4.0, 2F8.0)
  120   FORMAT (3A1, F5.0, 9F8.0)
        RETURN 
  130     IERR = 100
C ...     Illegal character found 
          WRITE (*,140) IC1, IC2, IC3, (RVALUE(I),I=1,11)
          WRITE (*,150) 
          IF (LP.GT.0 .AND. LP.NE.6) THEN
              WRITE (LP,140) IC1, IC2, IC3, (RVALUE(I),I=1,11)
              WRITE (LP,150) 
          ENDIF
          NSTOP = NSTOP + 1
          RETURN 
  140     FORMAT( 1X, 3A1, 8G11.5, (/, 4X, 8G11.5) )
  150     FORMAT(' ---> In the above card there was an',/,
     *           '      ILLEGAL CHARACTER was found',/,
     *           '      during a fixed-field type read')
      END IF 
C ------------------------------------------ free field read
      READ (IN,160, END=180, ERR=200)
     *                        IC1, IC2, IC3, (JREC(I), I = 1, 77)
  160 FORMAT (3A1,77A1)
      IF (IC1 .EQ. 'C' .AND. IC2 .EQ. 'O') RETURN
C-
      IERR1 = 0
      IERR  = 0
      II1   = 1
C 
      NWD = 38
C-
      CALL CRACK(II1, NWD, RVALUE, INT, CHA, 'REAL     ', IERR1)
C-
      NVALUE = NWD
      IF (IERR1 .GT. 0) THEN
          IERR = II1
C ...     Illegal character found in column 'II1' by CRACK
          WRITE (*,140) IC1, IC2, IC3, (RVALUE(I),I=1,NVALUE)
          WRITE (*,170) II1
          IF (LP.GT.0 .AND. LP.NE.6) THEN
              WRITE (LP,140) IC1,IC2,IC3, (RVALUE(I),I=1,NVALUE)
              WRITE (LP,170) II1
          ENDIF
  170     FORMAT(' ---> In the above card there was an',/,
     *           '      ILLEGAL CHARACTER found in column=',I5,
     *           '      during a free field read via SUB-CRACK')
          NSTOP = NSTOP + 1
      ENDIF
      RETURN 
C 
  180 IERR =  - 1
C ... End of file was hit
      RETURN 
C
  200 PRINT 210,     IN, IC1, IC2, IC3
      IF (LP.GT.0 .AND. LP.NE.6) WRITE(LP,210)  IN, IC1, IC2, IC3
  210 FORMAT(' *** ERROR read from LU=',I3,' CARD identity=',3A1)
      IERR = 200
      RETURN
      END
C
      SUBROUTINE REFINE
      IMPLICIT REAL (A-H, O-Z)
      INCLUDE'gfgv435.inc'
C-
      DIMENSION IPE(MAXE), NNN(MAXE), NEW(4)
      DIMENSION NOPX(MAXE,8), IMATX(MAXE), IK(MAXE)
      DIMENSION AO(MAXP)
      EQUIVALENCE (AO(1), WD(1))
      EQUIVALENCE (ICON(1,1), IPE(1)), (ICON(1,2), NNN(1)), 
     *     (ICON(1,3), IMATX(1)), (ICON(1,4), IK(1)), (ICON(1,5), 
     *     NOPX(1,1))
C-
      IF( ITRACE.GE.2 ) PRINT *,' =+= CALLED REFINE'
C
C ... ARRAY INITALIZATION.....
C-
      VOID =  - 1.0E36
      IEOR = 99999
      DO 100 J = 1, MAXE
        IK(J) =  - 1
  100 CONTINUE
C-
C ... COMPUTE MID-SIDE CORDS.....
C-
  110 DO 140 J = 1, NE
        IF (NOP(J,1) .EQ. 0) GO TO 140
        IK(J) = 1
        NCN = 6
        IF (NOP(J,7) .GT. 0) NCN = 8
        DO 130 K = 2, NCN, 2
          N2 = NOP(J,K)
          IF (CORD(N2,1) .GT. VOID) GO TO 120
          N1 = NOP(J,K-1)
          N3 = MOD(K+1,NCN)
          N3 = NOP(J,N3)
          CORD(N2,1) = 0.5 * (CORD(N1,1) + CORD(N3,1))
          CORD(N2,2) = 0.5 * (CORD(N1,2) + CORD(N3,2))
  120     AO(N2) = 0.5 * (AO(N1) + AO(N3))
  130   CONTINUE
  140 CONTINUE
C-
C ... CREATE LIST OF ELEMENT SIDES.....
C-
      DO 150 J = 1, MAXP
        DO 150 K = 1, 3
          IES(J,K) = 0
  150 CONTINUE
      NES = 1
      DO 180 J = 1, NE
        IF (NOP(J,1) .EQ. 0) GO TO 180
        NCN = 6
        IF (NOP(J,7) .GT. 0) NCN = 8
        DO 170 K = 1, NCN, 2
          N1 = NOP(J,K)
          N2 = NOP(J,K+1)
          N3 = MOD(K+2,NCN)
          N3 = NOP(J,N3)
          DO 160 L = 1, NES
            IF (N1 .EQ. IES(L,1) .AND. N3 .EQ. IES(L,3)) GO TO 170
            IF (N1 .EQ. IES(L,3) .AND. N3 .EQ. IES(L,1)) GO TO 170
  160     CONTINUE
          IES(NES,1) = N1
          IES(NES,2) = N2
          IES(NES,3) = N3
          NES = NES + 1
  170   CONTINUE
  180 CONTINUE
C-
C ... BEGIN NEW ELEMENT CONSTRUCTION .....
C-
      NUE = 0
      NUC = 0
      IERR = 0
      NX = NE
      IF (LP.GT.0 ) WRITE (LP,190) 
  190 FORMAT ( // 10X, 'SPECS FOR REFINED ELEMENTS',  //, 3X, 
     *     'ORG ELT NODES')
  200 CONTINUE
      READ (IN,210,END = 320) NEW
  210 FORMAT (4I5)
  220 IF (NEW(1) .GE. IEOR) GO TO 320
      NC = 3
      IF (NEW(4) .GT. 0) NC = 4
      DO 250 J = 1, NE
        IF (NOP(J,1) .EQ. 0) GO TO 250
        NF = 0
        JE = J
        NCN = 6
        IF (NOP(J,7) .GT. 0) NCN = 8
        DO 240 K = 1, NC
          DO 230 L = 1, NCN
            IF (NOP(J,L) .EQ. NEW(K)) NF = NF + 1
  230     CONTINUE
  240   CONTINUE
        IF (NF .EQ. NC) GO TO 270
  250 CONTINUE
      IERR = 1
      WRITE (*,260)  NEW
      IF (LP.GT.0 .AND. LP.NE.6) WRITE (LP,260) NEW
  260 FORMAT ( / 10X, '**** ERROR, NO EXISTING ELEMENT FOR NODES',4I5)
  270 NUE = NUE + 1
      NX = NUE + NE
      IK(JE) =  - 1
      IK(NX) = 1
      IMAT(NX) = IMAT(JE)
      IF (LP.GT.0) WRITE (LP,280) JE, NEW
  280 FORMAT (I10, 5X, 4I10)
C-
C ... CREATE NEW NODAL VALUES .....
C-
      NCN = 2 * NC
      L = 0
      DO 310 K = 1, NCN, 2
        L = L + 1
        N1 = NEW(L)
        NOP(NX,K) = N1
        KL = L + 1
        IF (KL .GT. NC) KL = 1
        N3 = NEW(KL)
C-
C ... CREATE NEW ELEMENTS .....
C-
        DO 290 M = 1, NES
          N2 = IES(M,2)
          IF (N1 .EQ. IES(M,1) .AND. N3 .EQ. IES(M,3)) GO TO 300
          IF (N1 .EQ. IES(M,3) .AND. N3 .EQ. IES(M,1)) GO TO 300
  290   CONTINUE
        NUC = NUC + 1
        N2 = NP + NUC
        IES(NES,1) = N1
        IES(NES,2) = N2
        IES(NES,3) = N3
        NES = NES + 1
        CORD(N2,1) = 0.5 * (CORD(N1,1) + CORD(N3,1))
        CORD(N2,2) = 0.5 * (CORD(N1,2) + CORD(N3,2))
        AO(N2) = 0.5 * (AO(N1) + AO(N3))
        NNN(NUC) = N2
        IPE(NUC) = JE
  300   NOP(NX,K+1) = N2
  310 CONTINUE
      GO TO 200
C-
C ... OUTPUT NEW ELEMENT CONNECTIONS .....
C-
  320 IF (LP.GT.0) WRITE (LP,330) NUE, NUC
  330 FORMAT ( // 10X, 'NUMBER OF NEW ELEMENTS IS', I6 / 10X, 
     *     'NUMBER OF NEW NODES  PTS IS', I6)
      IF (IERR .GT. 0) STOP
C-
C ... WRITE INITIAL VEL FILE.....
C-
      IF (NPRT .GT. 0) WRITE (NPRT) NE, ((NOP(J,K),K=1,8), J =1, NE)
C-
C ... LOAD ARRAY OF NEW ELEMENTS .....
C-
      N = 0
      DO 350 J = 1, NX
        IF (IK(J) .LT. 1) GO TO 350
        N = N + 1
        DO 340 K = 1, 8
          NOPX(N,K) = NOP(J,K)
  340   CONTINUE
        IMATX(N) = IMAT(J)
  350 CONTINUE
      NE = N
      DO 360 J = 1, NE
        IMAT(J) = IMATX(J)
        DO 360 K = 1, 8
          NOP(J,K) = NOPX(J,K)
  360 CONTINUE
      NP = NP + NUC
C-
C ... WRITE FINAL VEL INFO FILE.....
C-
      IF (NPRT .GT. 0) WRITE (NPRT) NE, NP, NUC, 
     *     ((CORD(J,K),K=1,2), J = 1, NP), (NNN(J), J = 1, NUC), 
     *     (IPE(J), J = 1, NUC)
      RETURN 
      END
C
      SUBROUTINE REORD
      IMPLICIT REAL (A-H, O-Z)
      PARAMETER (MAXINT = 2147483647)
      INCLUDE'gfgv435.inc'
C-
      IF( ITRACE.GE.2 ) PRINT *,' =+= CALLED REORD'
      WRITE(*,*) '--> Reorder by node was selected'
C
      KOUNT = 0
C ... BpD  Recall MAXINT= 2,147,483,647 is highest integer for I*4

      MPQ   =  9999999
      NSUMP =  9999999

      MAXC = 60
      NCN = 8
      DO 100 N = 1, NP
        DO 100 M = 1, MAXC
          ICON(N,M) = 0
  100 CONTINUE
      DO 150 N = 1, NE
        DO 140 M = 1, NCN
          I = NOP(N,M)
          IF (I .EQ. 0) GO TO 150
          DO 130 K = 1, NCN
            L = NOP(N,K)
            IF (L .EQ. I) GO TO 130
            DO 110 J = 1, MAXC
              JJ = J
              IF (ICON(I,J) .EQ. 0) GO TO 120
              IF (ICON(I,J) .EQ. L) GO TO 130
  110       CONTINUE
  120       ICON(I,JJ) = L
  130     CONTINUE
  140   CONTINUE
  150 CONTINUE
      NPM = 0
      DO 160 N = 1, NP
        IF (ICON(N,1) .GT. 0) NPM = NPM + 1
  160 CONTINUE
      MPQ = 0
      LIST(1) = 1
      MP = 1
      DO 240 N = 1, NP
        IF (ICON(N,1) .EQ. 0) GO TO 240
        IS = 0
        DO 170 M = 1, MP
          IF (N .EQ. LIST(M)) IS = 1
          IF (IS .EQ. 1) LIST(M) = LIST(M+1)
  170   CONTINUE
        IF (IS .EQ. 1) MP = MP - 1
        DO 210 J = 1, MAXC
          I = ICON(N,J)
          IF (I .EQ. 0) GO TO 220
          IF (I .LE. N) GO TO 210
          DO 180 M = 1, MP
            IF (I .EQ. LIST(M)) GO TO 200
  180     CONTINUE
          IF (MP .EQ. MAXP) KOUNT = KOUNT + 1
  190     FORMAT ('0**REORD--OVER ', I6, ' TERMS IN LIST ', I5, 
     *         ' TIME(S)**')
          IF (MP .EQ. MAXP) MP = MP - 1
          MP = MP + 1
          LIST(MP) = I
  200     CONTINUE
  210   CONTINUE
        MAXPP = MAXP
        IF (KOUNT .GT. 0 .AND. LP .GT. 0) WRITE (LP,190) MAXPP, KOUNT
  220   CONTINUE
C .-.   causes huge printouts   WRITE(LP,100) (LIST(M),M=1,MP)
  230   FORMAT (15I6)

        MPQ = MPQ + MP * MP

  235   continue                  ! 8-21-97 pseudo interger*8
        IF (MPQ .GT. 100000000) then
            MPQ_CLICK = MPQ_CLICK + 1
            MPQ  = MPQ - 100000000
            GO TO 235
        ENDIF

  240 CONTINUE
      IF (LP.GT.0 .AND. LP.NE.6) WRITE (LP,250) MPQ_CLICK, MPQ
      WRITE (*,255) MPQ_CLICK, MPQ
  250 FORMAT (// 10X,'INITIAL/ORIGINAL ORDER, REORDERING SUM =',
     *                                            I4,' e08 + ',I10)
  255 FORMAT (/,' --> REORDERING SUM FOR ORIGINAL NODAL ORDER =',
     *                                            I4,' e08 + ',I10)


  260 CONTINUE
      DO 270 I = 1, NP
        DO 270 J = 1, MAXC
        ICON(I,J) = IABS(ICON(I,J))
  270 CONTINUE
C-
      CALL ORDER (IDXX)
C-
      IF (IDXX .LT. MAXP) GO TO 260

cvv    this seems stupid                 IF (MPQ_INITIAL .LE. MPQ) STOP

      DO 280 N = 1, NPM
        IEL(N) = MSN(N)
  280 CONTINUE
  290 KBR = 1
C
cvv      IF (NSUMP .LT. MPQ) THEN
cvvC ...   Reorder sum void/default value is better than what's been calculated
cvv        MPQ = NSUMP
cvv        DO 300 N = 1, NPM
cvv          IEL(N) = MSN(N)
cvv  300   CONTINUE
cvv      END IF 
C-
C ... ZERO ARRARYS
C-
      DO 310 N = 1, NE
        IEM(N) = 0
        LIST(N) = 0
  310 CONTINUE
      DO 320 N = 1, NP
        DO 320 M = 1, MAXC
        ICON(N,M) = 0
  320 CONTINUE
C-
C ... FORM NODES CONNECTED TO ELEMENTS ARRAY
C-
      DO 350 N = 1, NE
        DO 340 M = 1, NCN
          I = NOP(N,M)
          IF (I .EQ. 0) GO TO 350
          DO 330 J = 1, MAXC
            IF (ICON(I,J) .NE. 0) GO TO 330
            ICON(I,J) = N
            GO TO 340
  330     CONTINUE
  340   CONTINUE
  350 CONTINUE
      DO 360 N = 1, NP
  360 CONTINUE
C-
C ... GET LIST OF ELEMENTS TO BE FORMED
C-
      K = 0
      DO 380 N = 1, NPM
        I = IEL(N)
        IF (I .EQ. 0) GO TO 390
        DO 370 J = 1, MAXC
          M = ICON(I,J)
          IF (M .EQ. 0) GO TO 380
          IF (LIST(M) .GT. 0) GO TO 370
          K = K + 1
          IEM(K) = M
          LIST(M) = K
  370   CONTINUE
  380 CONTINUE
  390 CONTINUE
      DO 400 N = 1, NE
        IF (LIST(N) .NE. 0) GO TO 400
        K = K + 1
        IEM(K) = N
  400 CONTINUE

      IF (LP.GT.0 .AND. LP.NE.6) 
     *       WRITE (LP,410) MPQ_CLICK, MPQ, (IEM(K), K = 1, NE)
      WRITE (*,415)  MPQ_CLICK, MPQ
  410 FORMAT( // 10X, 'SELECTED ELEMENT ORDER IS LISTED BELOW', 
     *         / 10X, 'REORDERING SUM = ', I4, ' e08 + ', I10, 
     *        // (5X, 15I7))
  415 FORMAT(/,' --> SELECTED ELEMENT ORDER HAS',
     *         ' A REORDERING SUM =', I4, ' e08 + ', I10,/)
      RETURN 
      END
C-----------------------------------
      SUBROUTINE REORD1
C 
C ... DRIVING  ROUTINE  TO  REORDER  ELEMENTS, RMA - IAN KING 6-1988
C 
      INCLUDE'gfgv435.inc'
C 
      IF( ITRACE.GE.2 ) PRINT *,' =+= CALLED REORD1'
      WRITE(*,*) '--> Reorder by element was selected'
C
C     INITIALIZE
C 
      NCM  = 20
      NCMI = 20
      NAD  = 0
      MP   = 0
C 
C ... GET TABLE OF ELEMENT CONNECTIONS
C 
      CALL KCON
C 
C ... PROCESS INITIAL ORDER
C 
  100 CALL ORDER1
C 
C ... RESET MLIST
C 
      DO 110 N = 1, NAE
        MLIST(N) = 0
  110 CONTINUE
C 
C ... READ STARTING SEQUENCE
C 
      N1 = 1
      N2 = 16
  120 READ (IN,140,END = 200) (MLIST(K), K = N1, N2)
C 
C  CHECK FOR LAST SET
C 
      IF (MLIST(N1) .GT. 9000) GO TO 200
      IF (LP.GT.0) WRITE (LP,130) (MLIST(K), K = N1, N2)
  130 FORMAT ( // 16I4)
  140 FORMAT (16I5)
C 
C ... CHECK FOR END OF LIST
C 
      IF (MLIST(N2) .EQ. 0) GO TO 150
      N1 = N1 + 16
      N2 = N1 + 15
C 
C ... GO TO READ MORE LIST
C 
      GO TO 120
  150 MP = 0
      NAD = 0
C 
C ... RESET NODE TO ELEMENT LIST
C 
      DO 180 N = 1, NP
        DO 160 M = 1, NCM
          IF (NECON(N,M) .EQ. 0) GO TO 170
  160   CONTINUE
  170   NDELM(N) = M - 1
  180 CONTINUE
C 
C ... RESET ELEMENT CONNECTIONS
C 
      DO 190 N = 1, NE
        DO 190 M = 1, NCMI
          ICON(N,M) = IABS(ICON(N,M))
  190 CONTINUE
C 
C ... GO TO PROCESS THIS SEQUENCE
C 
      GO TO 100
C 
C ... PRINT FINAL ORDER
C 
  200 IF (LP.GT.0) WRITE (LP,210) (IEM(K), K = 1, NE)
C ... ipk 08-21-97  add to list and format for printing
      WRITE(*,215)  MRSUM1, MRSUM, NFWSAV
  210 FORMAT ( // ' SELECTED ELEMENT ORDER' / (15I7))
  215 FORMAT ( / ' --> THE SELECTED ELEMENT ORDER HAS A ',/,
     *         5X,'REORDERING SUM=',I4,I10,'  FRONT WIDTH=',I6,/)
C 
      RETURN 
      END
C
      SUBROUTINE REPLY(ANAM, ISWT)
      CHARACTER *32 ANAM
      ISWT = 0
   50 IF (ANAM .EQ. '?')  GO TO 200
      IF (ANAM .EQ. ' ?')  GO TO 200
C
      IF (ANAM .EQ. 'N')    GO TO 100
      IF (ANAM .EQ. 'n')    GO TO 100
      IF (ANAM .EQ. 'NO')   GO TO 100
      IF (ANAM .EQ. 'no')   GO TO 100
      IF (ANAM .EQ. 'NONE') GO TO 100
      IF (ANAM .EQ. 'none') GO TO 100
      IF (ANAM .EQ. 'null') GO TO 100
C-
      IF (ANAM .EQ. 'NOMORE') GO TO 110
      IF (ANAM .EQ. 'nomore') GO TO 110
      IF (ANAM .EQ. 'quit')   GO TO 110
      IF (ANAM .EQ. 'QUIT')   GO TO 110
      IF (ANAM .EQ. 'stop')   GO TO 110
      IF (ANAM .EQ. 'STOP')   GO TO 110
      RETURN 
C-
C ... user does not want this file saved
  100 ISWT = 1
      RETURN 
C ... user want to quit the program
  110 ISWT = 2
      RETURN 
C ... Needed a menu
  200 ISWT = 3
      WRITE (*,210)
  210 FORMAT(/, ' How to Respond',/,
     *          ' Enter  -->filename  to save as requested',/,
     *          ' Enter  -->null      if not saving that file',/,
     *          ' Enter  -->quit      to stop the program',/)
      END
C
      SUBROUTINE RGRID
C*********************************************************************
C   THIS ROUTINE IS AN AUTOMATIC RECTANGULAR GRID GENERATOR.
C**********************************************************************
      IMPLICIT REAL (A-H, O-Z)
      INCLUDE'gfgv435.inc'
C-
      IF( ITRACE.GE.2 ) PRINT *,' =+= CALLED RGRID'
C-
      READ (IN,100) NX, NY, XL, XY, XR, YR
  100 FORMAT (2I10, 4E10.0)
      IF (LP.GT.0) WRITE (LP,110) NX, NY, XL, XY, XR, YR
  110 FORMAT ('1', 20X, '**RECTANGULAR GRID GENERATOR**',  // 
     *     5X, 'NO. OF X-PANELS', I10,  / 5X, 'NO. OF Y-PANELS', 
     *     I10,  / 5X, 'X LENGTH', F17.2,  / 5X, 'Y LENGTH', 
     *     F17.2,  / 5X, 'X SPACING RATIO', F10.2,  / 5X, 'Y SPACING '
     *     , 'RATIO', F10.2)
C 
      X = 0.
      NX1 = NX + 1
      NY1 = NY + 1
      NY2 = 2 * NY + 1
      NE = NX * NY
      NP = NX1 * NY2 + NX * NY1
      IF (NP .LE. MAXP .AND. NE .LE. MAXE) GO TO 130
      MAXEP = MAXE
      MAXPP = MAXP
      WRITE (*,120) NE, MAXEP, NP, MAXPP
      IF (LP.GT.0) WRITE (LP,120) NE, MAXEP, NP, MAXPP
  120 FORMAT ('0', 10X, '***ERROR--DIMENSION LIMITS EXCEEDED***', 
     *      / 5X, 'NE=', I6, 5X, 'MAXE=', I6,  / 5X, 'NP=', I6, 
     *     5X, 'MAXP=', I6)
      IF (IBATCH.EQ.0) THEN
          CALL BEEP (2)
          IF (ITRACE.LT.0) THEN
             PRINT *,' Hit ENTER-KEY to clear the screen. DIM LIMIT'
             PAUSE
          ENDIF       
      ENDIF
      STOP'Dim'
C 
C ... PUT IN DUMMY VALUES OF NODE DEPTH AND ELEMENT TYPE...
C 
  130 DO 140 I = 1, NP
        WD(I) = 100.0
  140 CONTINUE
      DO 150 J = 1, NE
        IMAT(J) = 1
        TH(J) = 0.0
  150 CONTINUE
C 
      NPX = NX1 * NY1
      DY = 0.
      IF (YR .EQ. 1.) DY = XY / FLOAT(NY)
      DDY = DY
      IF (YR .EQ. 1.) GO TO 160
      DY = XY * (YR - 1) / (YR ** NY - 1)
      DDY = DY
  160 IF (XR .EQ. 1.) DX = XL / FLOAT(NX)
      IF (XR .EQ. 1.) GO TO 170
      DX = XL * (XR - 1) / (XR ** NX - 1)
  170 CONTINUE
      DO 190 I = 1, NX1
        DY = DDY
        Y = 0.
        DO 180 J = 1, NY2, 2
          K = (NY2 + NY1) * (I - 1) + J
          CORD(K,1) = X
          CORD(K,2) = Y
          Y = Y + DY
  180   DY = DY * YR
        X = X + DX
  190 DX = DX * XR
      DO 200 I = 1, NX
        DO 200 J = 1, NY
          NEL = (I - 1) * NY + J
          NOP(NEL,1) = 2 * J - 1 + (I - 1) * (NY2 + NY1)
          NOP(NEL,2) = NOP(NEL,1) + NY2 - (J - 1)
          NOP(NEL,3) = (NY2 + NY1) * I + (J - 1) * 2 + 1
          NOP(NEL,4) = NOP(NEL,3) + 1
          NOP(NEL,5) = NOP(NEL,4) + 1
          NOP(NEL,6) = NOP(NEL,2) + 1
          NOP(NEL,7) = NOP(NEL,1) + 2
  200 NOP(NEL,8) = NOP(NEL,1) + 1
C 
C ... COMPUTE MID-SIDE CORDS
      DO 210 J = 1, NE
        DO 210 K = 2, 8, 2
          N1 = NOP(J,K-1)
          N2 = NOP(J,K)
          N3 = MOD(K+1,8)
          N3 = NOP(J,N3)
          CORD(N2,1) = 0.5 * (CORD(N1,1) + CORD(N3,1))
          CORD(N2,2) = 0.5 * (CORD(N1,2) + CORD(N3,2))
  210 CONTINUE
      RETURN 
      END
C
      SUBROUTINE SLOPES(NNO, NODES, NOPL, NSLPMX, VOID, IERR)
      IMPLICIT REAL (A-H, O-Z)
      INCLUDE'gfgv435.inc'
      DIMENSION X(MAXE*2+2), Y(MAXE*2+2), VAL(MAXE*2+2), 
     *     XI(MAXE*2+2), YI(MAXE*2+2), R(MAXE*2+2), Z(MAXE*2+2), 
     *     W(MAXE*2+2), A(MAXE*2+2), B(MAXE*2+2), C(MAXE*2+2), 
     *     D(MAXE*2+2), CP(MAXE*2+2), DP(MAXE*2+2), S(MAXE*2+2), 
     *     NOPL(MAXP,3), NODEL(MAXP), ANG(MAXE*2+2), 
     *     NODES(NSLPMX,3)
C 
      IF( ITRACE.GE.2 ) PRINT *,' =+= CALLED SLOPES (NNO=',NNO,'...)'
C
C ... INITIALIZATION
C 
      NMSN1 = NMSN
      PI = 355.0 / 113.0
      PI2 = PI / 2.
      IERR = 0
      NNODES = 0
      ILL = 1
      AW = 1.
      AVAL = .15D0
      NODB = NODES(NNO,1)
      NODN = NODES(NNO,2)
      NODL = NODES(NNO,3)
      DO 100 I1 = 1, MAXP
  100 NODEL(I1) = 0
C 
C ... DO THE REQUESTED NODES BEGIN WITH ADJACENT BOUNDARY CORNER NODES
C 
      DO 170 NCCW = 1, 2
        NODNT = NOPL(NODB,NCCW)
        GO TO (110, 160), NCCW
  110   IF (NODNT .GT. 0) GO TO 150
        IF (NODNT .EQ. 0) GO TO 130
  120   IF (NOPL(NODB,1) .NE. NOPL(NODB,2)) GO TO 160
C ... NOT REFERENCED OR NOT A BOUNDARY
  130   IERR = 1
  140   RETURN 
C ... NOT A CORNER
  150   IERR = 2
        RETURN 
  160   NODNT = IABS(NODNT)
        NODNT = NOPL(NODNT,NCCW)
        IF (NODNT .EQ. NODN) GO TO 180
  170 CONTINUE
C ... NOT ADJACENT BOUNDARY CORNERS
      IERR = 3
      RETURN 
C 
C ... SET UP X,Y ARRAYS, VOID REFERENCED MIDSIDE NODES IN ALPHA ARRAY
C ... AND SAVE REFERENCED MIDSIDE NODE NUMBERS IN MSN ARRAY.
C 
  180 CONTINUE
  190 NNODES = NNODES + 1
      X(NNODES) = CORD(NODB,1)
      Y(NNODES) = CORD(NODB,2)
      IF (NOPL(NODB,3) .NE. 0) NODEL(NNODES) = NODB
      NODN = IABS(NOPL(NODB,NCCW))
      IF (NMSN1 .LE. 0) GO TO 210
      DO 200 I1 = 1, NMSN1
        IF (NODN .EQ. MSN(I1)) GO TO 220
  200 CONTINUE
  210 NMSN1 = NMSN1 + 1
      MSN(NMSN1) = NODN
  220 ALPHA(NODN) = VOID
      NODB = NOPL(NODN,NCCW)
      IF (NODB .EQ. NODL) GO TO 230
      IF (NODB .NE. NODES(NNO,1)) GO TO 190
C ... THE TERMINATING NODE COULD NOT BE FOUND
      IERR = 4
      RETURN 
  230 NNODES = NNODES + 1
      X(NNODES) = CORD(NODB,1)
      Y(NNODES) = CORD(NODB,2)
      IF (NOPL(NODB,3) .NE. 0) NODEL(NNODES) = NODB
      NSEG = NNODES - 1
      DO 240 I = 1, NSEG
  240 W(I) = AW
      DO 250 I = 1, NNODES
  250 VAL(I) = AVAL
      XI(1) = X(2) - X(1)
      YI(1) = Y(2) - Y(1)
      R(1) = XI(1) * XI(1) + YI(1) * YI(1)
      R(1) = SQRT(R(1))
      CCOS = XI(1) / R(1)
      CSIN = YI(1) / R(1)
      PHI = ASIN(CSIN)
      IF (CCOS .GT. 0.) GO TO 270
      IF (CSIN .LT. 0.) GO TO 260
      PHI = PI - PHI
      GO TO 270
  260 PHI =  - (PI + PHI)
  270 Z(1) = PHI
C 
C ... CALCULATE THE ANGLE OF EACH SEGMENT
C 
      DO 300 I = 2, NSEG
        XI(I) = X(I+1) - X(I)
        YI(I) = Y(I+1) - Y(I)
        R(I) = XI(I) * XI(I) + YI(I) * YI(I)
        R(I) = SQRT(R(I))
        AMAG = R(I) * R(I-1)
        XPROD = XI(I-1) * YI(I) - XI(I) * YI(I-1)
        CSIN = XPROD / AMAG
        DPROD = XI(I-1) * XI(I) + YI(I-1) * YI(I)
        CCOS = DPROD / AMAG
        PHI = ASIN(CSIN)
        IF (CCOS .GT. 0.) GO TO 290
        IF (CSIN .LT. 0.) GO TO 280
        PHI = PI - PHI
        GO TO 290
  280   PHI =  - (PI + PHI)
  290   Z(I) = PHI
        Z(I) = Z(I) + Z(I-1)
  300 CONTINUE
      DO 360 I = 1, NSEG
        NODN = NODEL(I)
        IF (NODN .EQ. 0) GO TO 360
        TB = ATAN(ALPHA(NODN))
        TA = Z(I)
        TAUP = TA + PI2
        TADP = TA - PI2
  310   IG = 1
        ZQWIFZ = TA - TB
        IF (ZQWIFZ .GT. 0.) GO TO 330
        IF (ZQWIFZ .GE. 0.) GO TO 350
  320   IG =  - 1
  330   IF ((TB .GE. TADP) .AND. (TB .LE. TAUP)) GO TO 350
        IF (ABS(TB) .GT. 700) GO TO 340
        TB = TB + PI * IG
        GO TO 310
  340   IERR = 5
        NODES(NNO,1) = NODN
        NODES(NNO,2) = NODN
        NODES(NNO,3) = NODN
        RETURN 
  350   A(I) = 0.
        B(I) = 1.
        C(I) = 0.
        D(I) = TB
  360 CONTINUE
      NODN = NODEL(NNODES)
      IF (NODN .EQ. 0) GO TO 420
      TB = ATAN(ALPHA(NODN))
      TA = Z(NSEG)
      TAUP = TA + PI2
      TADP = TA - PI2
  370 IG = 1
      ZQWIFZ = TA - TB
      IF (ZQWIFZ .GT. 0.) GO TO 390
      IF (ZQWIFZ .GE. 0.) GO TO 410
  380 IG =  - 1
  390 IF ((TB .GE. TADP) .AND. (TB .LE. TAUP)) GO TO 410
      IF (ABS(TB) .GT. 700) GO TO 400
      TB = TB + PI * IG
      GO TO 370
  400 IERR = 6
      NODES(NNO,1) = NODN
      NODES(NNO,2) = NODN
      NODES(NNO,3) = NODN
      RETURN 
  410 A(NNODES) = 0.
      B(NNODES) = 1.
      C(NNODES) = 0.
      D(NNODES) = TB
  420 CONTINUE
C 
C ... CALCULATE ARRAY VALUES
C 
  430 CONTINUE
      IF (NODEL(1) .NE. 0) GO TO 440
      B(1) = 1. + VAL(1)
      C(1) = 1.
      D(1) = (2. + VAL(1)) * Z(1)
  440 N = NNODES - 1
      DO 450 I = 2, N
        IF (NODEL(I) .NE. 0) GO TO 450
        J = I - 1
        A(I) = W(J)
        B(I) = W(J) + W(I) + 2. * VAL(I)
        C(I) = W(I)
        D(I) = (2. * W(J) + VAL(I)) * Z(J) + (2. * W(I) + 
     *       VAL(I)) * Z(I)
  450 CONTINUE
      IF (NODEL(NNODES) .NE. 0) GO TO 460
      A(NNODES) = 1.
      B(NNODES) = 1. + VAL(NNODES)
      D(NNODES) = (2. + VAL(NNODES)) * Z(N)
  460 CONTINUE
C ... CALCULATE ANGLES THEN SLOPES AT EACH POINT
C ... THE METHOD USED FOR CALCULATING THESE ANGLES IS CALLED THE
C ... THOMAS ALGORITHM WHICH IS USED TO SOLVE THE TRIDIAGONAL MATRIX
      N = NNODES - 1
      CP(1) = C(1) / B(1)
      DP(1) = D(1) / B(1)
      DO 470 I = 1, N
        Q = 1. / (B(I+1) - A(I+1) * CP(I))
        CP(I+1) = C(I+1) * Q
        DP(I+1) = (D(I+1) - A(I+1) * DP(I)) * Q
  470 CONTINUE
      ANG(NNODES) = DP(NNODES)
      I = NNODES
  480 I = I - 1
      ANG(I) = DP(I) - CP(I) * ANG(I+1)
      IF (I .GT. 1) GO TO 480
      DO 490 I = 1, NNODES
         S(I) = TAN(ANG(I))
  490 CONTINUE
      N = NSEG - 1
      IREP = 0
      DO 500 I = 2, N
        IUP = 0
        I1 = I + 1
        J = I - 1
        IF ((NODEL(I) .NE. 0) .AND. (NODEL(I1) .NE. 0)) GO TO 500
        TA1 = TAN(ANG(I)-Z(I))
        TA2 = TAN(ANG(I1)-Z(I))
        TA = TA1 / TA2
        TA = (ANG(I) - Z(I)) / (ANG(I1) - Z(I))
        IF ((TA .LT. -2.0) .OR. (TA .GT. -0.5)) IUP = 1
        IF (IUP .GT. 0) W(I) = W(I) + 0.5
        IF (IUP .GT. 0) IREP = 1
  500 CONTINUE
      IF (ILL .GT. 40) IREP = 0
      ILL = ILL + 1
      IF (IREP .GT. 0) GO TO 430
C 
C ... MOVE SLOPES INTO THE REFERENCED BOUNDARY CORNER NODE
C ... POSITIONS IN THE ALPHA ARRAY
C 
      NMSN = NMSN1
      NODB = NODES(NNO,1)
      DO 510 I = 1, NNODES
        ALPHA(NODB) = S(I)
        NODB = IABS(NOPL(NODB,NCCW))
  510 NODB = NOPL(NODB,NCCW)
      RETURN 
      END
C
      SUBROUTINE STRND (LN, NE, LE, NC, NFRNT, ND, NDW, NST, MSUM, 
     *                  NTRY, NUMNP, NUMEL, NSTART)
C 
C ... ROUTINE FINDS THE STARTING NODES...
C 
      PARAMETER (MAXINT = 2147483647)
      IMPLICIT REAL (A-H, O-Z)
      DIMENSION LN(NUMNP), NE(NUMNP), LE(NUMEL), NC(NUMNP), 
     *     NFRNT(NUMNP), ND(NUMNP), NDW(NUMNP), MSUM(NUMEL), 
     *     NST(3)
C 
      COMMON /IO/ LUNIT, ICOLL, IGIN, IN, LP, LUOLD, NPRT
C
      PRINT *,' =+= CALLED STRND (LN=',LN,'...)' 
C     
C ... INITIALIZE PARAMETERS---FORM TREE FROM NSTART...
C 
      NPTS = 1
      ND(1) = NSTART
      NST(1) = NSTART
      NTRY = 1
      IF (NUMEL .EQ. 1) RETURN
      LEVEL = 1
      NDW(NSTART) =  - NDW(NSTART)
C 
  100 NUMLV = 0
C 
C ... LOOP ON NODES IN CURRENT LEVEL...
C 
      DO 130 I = 1, LEVEL
        NDS = ND(I)
        LN2 = LN(NDS)
        NDSM = NDS - 1
C     *************************************************************
        LN1 = 1
        IF (NDSM .GT. 0) LN1 = LN(NDSM) + 1
C     *************************************************************
C 
C ... LOOP ON ALL ELEMENTS CONNECTED TO NODE I...
C 
        DO 120 J = LN1, LN2
          LELM = NE(J)
          LE2 = LE(LELM)
          IF (LE2 .LE. 0) GO TO 120
          LE(LELM) =  - LE2
          LELMM = LELM - 1
C     *************************************************************
          LE1 = 1
          IF (LELMM .GT. 0) LE1 = IABS(LE(LELMM)) + 1
C     *************************************************************
C 
C ... LOOP ON ALL NODES IN ELEMENT LELM...
C 
          DO 110 K = LE1, LE2
            LVND = NC(K)
            LVNDW = NDW(LVND)
            IF (LVNDW .LE. 0) GO TO 110
            NUMLV = NUMLV + 1
            NFRNT(NUMLV) = LVND
            NDW(LVND) =  - NDW(LVND)
            NPTS = NPTS + 1
  110     CONTINUE
  120   CONTINUE
  130 CONTINUE
C 
C ... SKIP TO NEXT SECTION IF NUMLV = 0 ...
C 
      IF (NUMLV .EQ. 0) GO TO 150
C 
C ... PUT CURRENT LEVEL IN ND ARRAY...
C 
      DO 140 L = 1, NUMLV
  140 ND(L) = NFRNT(L)
C 
C ... GO TO NEXT SECTION IF ALL POINTS ARE DONE...
C 
      IF (NPTS .EQ. NUMNP) GO TO 150
C 
C ... RESET PARAMETERS...
C 
      LEVEL = NUMLV
      GO TO 100
C 
C ... NEXT SECTION...
C 
  150 CONTINUE
      IF (NUMLV .EQ. 0) NUMLV = LEVEL
C 
C ... CHANGE BACK SIGN OF KEYS...
C 
      DO 160 I = 1, NUMNP
  160 NDW(I) = IABS(NDW(I))
      DO 170 I = 1, NUMEL
  170 LE(I) = IABS(LE(I))
C 
C ... USE LAST LEVEL TO FIND TWO MOST DISTANT POINTS
C ... IN THAT LEVEL...
C 
C ... FIND MIN NODE IN LAST LEVEL...
C 
      MIN = MAXINT
      MINSUM = MAXINT
      DO 190 I = 1, NUMLV
        NDM = ND(I)
        NDWM = NDW(NDM)
C 
C ... SET KEY FOR NODES IN LAST LEVEL...
C 
        LN(NDM) =  - LN(NDM)
        IF (NDWM .GT. MIN) GO TO 190
        LAD = LN(NDM)
        LELL = NE(LAD)
        MSM = MSUM(LELL)
        IF (NDWM .LT. MIN) GO TO 180
        IF (MSM .GE. MINSUM) GO TO 190
  180   MIN = NDWM
        NSTR = NDM
        MINSUM = MSM
  190 CONTINUE
C 
C ... FORM TREE FROM NSTR--FIND MOST DISTANT NODE...
C 
C ... SET PARAMETERS ...
C 
      NST(2) = NSTR
      NTRY = 2
      ND(1) =  - NSTR
      NDW(NSTR) =  - NDW(NSTR)
      LEVEL = 1
      NPTS = NUMLV - 1
      LN(NSTR) =  - LN(NSTR)
C 
  200 NUMLV = 0
C 
C ... LOOP ON ALL NODES IN CURRENT LEVEL...
C 
      DO 240 I = 1, LEVEL
        NDS = IABS(ND(I))
        LN2 = IABS(LN(NDS))
        NDSM = NDS - 1
C     *****************************************************
        LN1 = 1
        IF (NDSM .GT. 0) LN1 = IABS(LN(NDSM)) + 1
C     *****************************************************
C 
C ... LOOP ON ALL ELEMENTS CONNECTED TO NODE I...
C 
        DO 230 J = LN1, LN2
          LELM = NE(J)
          LE2 = LE(LELM)
          IF (LE2 .LE. 0) GO TO 230
          LE(LELM) =  - LE2
          LELMM = LELM - 1
C     ****************************************************
          LE1 = 1
          IF (LELMM .GT. 0) LE1 = IABS(LE(LELMM)) + 1
C     ***************************************************
C 
C ... LOOP ON ALL NODES OF ELEMENT LELM...
C 
          DO 220 K = LE1, LE2
            LVND = NC(K)
            LVNDW = NDW(LVND)
            IF (LVNDW .LE. 0) GO TO 220
            NUMLV = NUMLV + 1
C 
C ...       CHECK IF NODE IS FROM LAST LEVEL...
C 
            LNT = LN(LVND)
            IF (LNT .GT. 0) GO TO 210
            LN(LVND) =  - LNT
            NFRNT(NUMLV) =  - LVND
            NPTS = NPTS - 1
            NDW(LVND) =  - NDW(LVND)
            GO TO 220
  210       NFRNT(NUMLV) = LVND
            NDW(LVND) =  - NDW(LVND)
  220     CONTINUE
  230   CONTINUE
        IF (NPTS .EQ. 0) GO TO 260
  240 CONTINUE
C 
C ... PUT NEW LEVEL IN ND ARRAY...
C 
      DO 250 L = 1, NUMLV
         ND(L) = NFRNT(L)
  250 CONTINUE
C 
C ... RESET PARAMETERS...
C 
      LEVEL = NUMLV
      GO TO 200
C 
  260 CONTINUE
C 
C ... CHANGE BACK SIGN OF KEYS--ZERO ND ARRAY...
C 
      DO 270 I = 1, NUMNP
        ND(I) = 0
        NDW(I) = IABS(NDW(I))
  270 CONTINUE
      DO 280 I = 1, NUMEL
        LE(I) = IABS(LE(I))
  280 CONTINUE
C 
C ... FIND MIN NODE IN LEVEL CONTAINING FINAL NODES OF PREVIOUS LAST LEVEL      
C 
      MIN = MAXINT
      MINSUM = MAXINT
      DO 300 I = 1, NUMLV
        NDM = NFRNT(I)
        IF (NDM .GE. 0) GO TO 300
        NDM =  - NDM
        NDWM = NDW(NDM)
        IF (NDWM .GT. MIN) GO TO 300
        LAD = LN(NDM)
        LELL = NE(LAD)
        MSM = MSUM(LELL)
        IF (NDWM .LT. MIN) GO TO 290
        IF (MSM .GE. MINSUM) GO TO 300
  290   MIN = NDWM
        NSTR = NDM
        MINSUM = MSM
  300 CONTINUE
C 
C ... PUT THIRD NODE IN STARTING ARRAY...
C 
      IF (NST(2) .EQ. NSTR) RETURN
      NST(3) = NSTR
      NTRY = 3
C 
      RETURN 
      END
C
      SUBROUTINE SUMIT(NEL)
C 
C     DEVELOP SUMS FOR MAKING ELIMINATION CHOICE
C 
      INCLUDE'gfgv435.inc'
C 
      IF( ITRACE.GE.2 ) PRINT *,' =+= CALLED SUMIT(NEL=',NEL,')'
C
C     LOCATE NEW NODES
C 
      NDP = 0
      NNEW = 0
      DO 110 K = 1, 8
        N = NOP(NEL,K)
        IF (N .EQ. 0) GO TO 110
C 
C       TEST WHETHER THIS NODE ALREADY INCORPORATED
C 
        IF (NINC(N) .EQ. 1) GO TO 100
        NNEW = NNEW + 1
        LNEW(NNEW) = N
C 
C       NOW TEST IF THE NODE IS COMPLETELY FORMED
C 
  100   IF (NDELM(N) .GT. 1) GO TO 110
        NDP = NDP + 1
        NDROP(NDP) = N
  110 CONTINUE
C 
C     IMMEDIATELY ON ADDING NEW FRONT SIZE IS
C 
      NFW = MP + NNEW
C ... ipk 08-21-97   ADD A LINE
      NFWS=NFW
C 
C     NOW TAKE OUT ALL WE CAN
C 
      MSUM = 9999999

      IF (NDP .EQ. 0) RETURN
      MSUM = 0
      DO 120 K = 1, NDP
        MSUM = MSUM + NFW ** 2
        NFW = NFW - 1
  120 CONTINUE
      RETURN 
      END
C
      SUBROUTINE UPFNT(KREC)
C 
C     DEFINE NEW INFO ON FRONT
C 
      INCLUDE'gfgv435.inc'
C 
      IF( ITRACE.GE.2 ) PRINT *,' =+= CALLED UPFNT (KREC=',KREC,')'
C
C     SET ICON ENTRIES NEGATIVE TO SAY THIS ELEMENT ALREADY ADDED
C 
      DO 110 M = 1, NCMI
        K = ICON(KREC,M)
        IF (K .EQ. 0) GO TO 120
        IF (K .LT. 0) GO TO 110
        DO 100 J = 1, NCMI
          IF (ICON(K,J) .NE. KREC) GO TO 100
          ICON(K,J) =  - ICON(K,J)
          GO TO 110
  100   CONTINUE
  110 CONTINUE
C 
C     UPDATE LIST OF ELEMENTS STILL IN FRONT
C 
  120 MNAD = 0
C 
C     FIRST ELIMINATE KREC
C 
      IF (NAD .EQ. 0) GO TO 140
      DO 130 K = 1, NAD
        IF (IENXT(K) .EQ. KREC) GO TO 130
        MNAD = MNAD + 1
        IENXT(MNAD) = IENXT(K)
  130 CONTINUE
  140 CONTINUE
      NAD = MNAD
C 
C     NOW ADD NEW ELEMENTS
C 
      DO 160 J = 1, NCMI
        K = ICON(KREC,J)
        IF (K .LE. 0) GO TO 160
C 
C       CHECK OF  -K- ALREADY IN LIST
C 
        DO 150 M = 1, NAD
          IF (K .EQ. IENXT(M)) GO TO 160
  150   CONTINUE
        MNAD = MNAD + 1
        IENXT(MNAD) = K
  160 CONTINUE
      NAD = MNAD
      RETURN 
      END
C-
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
