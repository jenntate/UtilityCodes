C
      PROGRAM R10_2_GC
      SAVE
C
C ...	This program is based on r2_2_GC, which was coded by  
C ...   Joe Letter and Barbara Donnell.  R10_2_GC was coded
C ...   By Gary Brown in March, 2000.
C
C     -  - - - - - - - - - - - -
      PARAMETER(MNN=200000,  MEN=90000,
     *          MAXL=50,            ! cosmetic
     *          MAXT=20000,          ! max time steps
     *          MCC=50,MCCN=1500)    ! max GC lines and nodes per line
      COMMON /GEOM/  NE,NP,CORD(MNN,3),AO(MNN),NOP(MEN,20),
     *               IMAT(MEN), AT(MEN),DEP(MNN),AVDEP(MEN),
     *               NETOT,NPTOT,NREF(MNN),NDEP(MNN),
     *               ALFA(MNN),NCORN(MNN)
      COMMON /MESH1/ WIDTH(MNN),SS1(MNN),SS2(MNN),WIDS(MNN)
      COMMON /LINES/ LMT(MCC),LINE(MCC,MCCN),NCL
      COMMON /HYDRO/ TET, VEL(6,MNN), WSEL(MNN), NDRY(MNN)
      COMMON /FLUX/  TIME(MAXT), WFLUX(MAXL,MAXT,3)
      COMMON /CNTRL/ ICYC, IHAVE, NCALL, TETSTART, 
     *               TETSTOP, ONCER2

C-    DATA MANAGEMENT REQUIREMENTS
      COMMON /DMSCHR/BANGFG(15),BANRM2(15),BANCON(15)
      COMMON /DMSREC/IREC(40),FREC(40),
     *               IPACKB(1200), IPACKT(77), IPACKH(1200)
      CHARACTER      BANGFG*80, BANRM2*80, BANCON*80
      CHARACTER*77 TITLE, TGFGEN
C-
      COMMON /LAB/ TITLE, TGFGEN
C     -  - - - - - - - - - - - -

      CHARACTER FNAME*80
      LOGICAL AROUND
      IHAVE = 0
      IMETRIC = 0
      NCL   = 0
      ICK = 1
c
      PRINT *,' '
      PRINT *,'  PROGRAM r10_2_gc    Coded: 03-00' 
      PRINT *,'  Computes the total flow Across user defined'
      PRINT *,'  Continuity Check "GC" lines of TABSMD mesh'
      PRINT *,'  Author:  Joe Letter / Barbara Donnell / Gary Brown'
      PRINT *,' '
C
      PRINT *,'Provide a filename for output summary:'
      READ(*,'(A)')  FNAME
      OPEN(UNIT=44,FILE=FNAME,FORM='FORMATTED',STATUS='unknown')
      WRITE(44,'('' Print: '',A)') FNAME

      PRINT *,'Provide a filename for spreadsheet output file:'
      READ(*,'(A)')  FNAME
      OPEN(UNIT=55,FILE=FNAME,FORM='FORMATTED',STATUS='unknown')
C
      WRITE(*,*) 'Enter a 1 if you wish to compute SALT flux'
      WRITE(*,*) 'Enter a 2 if you wish to compute TEMPERATURE flux'
      WRITE(*,*) 'Enter a 3 if you wish to compute SEDIMENT flux'
      READ(*,*) ICK
      WRITE(*,*)
      IF (ICK .EQ. 2) THEN
        WRITE(*,*) 'You chose TEMPERATURE flux'
      ELSE IF (ICK .EQ. 3) THEN
        WRITE(*,*) 'You chose SEDIMENT flux'
      ELSE
        WRITE(*,*) 'You chose SALT flux'
        ICK = 1
      END IF
      WRITE(*,*)
      ICK = ICK + 3
C
  50  PRINT *,'Enter the 3-D binary geometry file'
      READ(*,'(A)')  FNAME
         INQUIRE (FILE=FNAME, EXIST=AROUND)
         IF (.NOT.AROUND) THEN
             PRINT *,' --> File not found ... try again'
             GO TO 50
         ENDIF
      WRITE(44,'('' 3dgeo: '',A)') FNAME
      OPEN(UNIT=33,FILE=FNAME,FORM='UNFORMATTED',STATUS='OLD')
C
  60  PRINT *,'Enter the RMA10 binary solution file to analyze:'
      READ(*,'(A)')  FNAME
         INQUIRE (FILE=FNAME, EXIST=AROUND)
         IF (.NOT.AROUND) THEN
             PRINT *,' --> File not found ... try again'
             GO TO 60
         ENDIF
      WRITE(44,'('' RMA10 : '',A)') FNAME
      OPEN(UNIT=1,FILE=FNAME,FORM='UNFORMATTED',STATUS='OLD')
C

      PRINT *,'Enter minimum and maximum time window in hours.'
      READ (*,*)   TETSTART, TETSTOP
      WRITE(44,70) TETSTART, TETSTOP
  70  FORMAT(' Time Window to process, min and max =', 2F15.6)
C
  90  PRINT *,'Enter a file that includes the both GC-type cards'
      PRINT *,'for computing total flow across continuity lines'
      PRINT *,'and any GW cards necessary to define the widths'
      PRINT *,'of 1-D nodes.'  
      PRINT *,'GC cards must be in RMA10 format.'
      PRINT *,'[GC-cards have corner nodes only]'
      READ(*,'(A)')   FNAME
         INQUIRE (FILE=FNAME, EXIST=AROUND)
         IF (.NOT.AROUND) THEN
             PRINT *,' --> File not found ... try again'
             GO TO 90
         ENDIF
      WRITE(44,'('' GC   : '',A)') FNAME
      OPEN(UNIT=4,FILE=FNAME,FORM='FORMATTED',STATUS='OLD')
C
 95   PRINT *,'If the solution is in English Units, enter a 0'
      PRINT *,'If the solution is in Metric  Units, enter a 1'
      READ(*,*) IMETRIC
      IF (IMETRIC .LT. 0 .OR. IMETRIC .GT. 1) THEN
        PRINT *,'Invalid choice.  Please try again.'
        GO TO 95
      END IF
C
      CALL GEOREAD
C
      CALL PRESET
c
      PRINT *,'--> Number of continuity lines read = ',NCL
C
      NCALL = 0
      CALL CHECK(ICK)
C
      DO 100 ICYC = 1, MAXT

C ...    Loop through the RMA2 binary solution

         CALL R10READ
         IF (TET .GT. TETSTOP) GO TO 110

         CALL CHECK(ICK)

  100 CONTINUE
  110 CONTINUE
C
      PRINT *,'Read ',IHAVE,' timesteps of data.'
c
      DO 3000 I=1,NCL
         WRITE(44,220) I
  220    FORMAT(//,' FLUX ESTIMATES FOR LINE NUMBER=',I3,/)
         IF (IMETRIC.EQ.1) THEN
             WRITE(44,230)
  230        FORMAT(/,
     *       '   TIME      WATER FLUX    SALT FLUX       ',/
     *       '   (HR)      (M3/SEC)      (C*M3/SEC)      ',/,
     *       ' --------    ---------------------------   ',/)
         ELSE
             WRITE(44,240)
  240        FORMAT(/,
     *       '   TIME      WATER FLUX    SALT FLUX       ',/
     *       '   (HR)      (FT3/SEC)     (C*FT3/SEC)     ',/,
     *       ' --------    ---------------------------   ',/)
         ENDIF
C
         DO 270 J=1,IHAVE
            WRITE(44,250) TIME(J), WFLUX(I,J,1), WFLUX(I,J,2) 
  250       FORMAT(F7.2,3X,2F14.2)
  270    CONTINUE
         WRITE(*,*)

 3000 CONTINUE
C
         DO J = 1, IHAVE
           WRITE(55,271) TIME(J),(WFLUX(I,J,1), WFLUX(I,J,2),I=1,NCL)
 271       FORMAT(F7.2,50(3X,F14.2,1X,F14.2,1X))
         END DO
C
         WRITE(*,*)
         WRITE(*,*) 'Note: Spreadsheet format is as follows:'
         WRITE(*,*) 'Time, flow(1), mass flux(1),...,',
     +              'flow(ncl), mass flux(ncl)' 
         WRITE(*,*)
C
C GLB add average flux computation 
      write(*,*) '--------------------------------------------'
      write(*,*)
      write(*,*) '       SUMMARY OF AVERAGE VALUES'
      write(*,*)
      IF (IMETRIC.EQ.1) THEN
         WRITE(*,3010)
 3010    FORMAT(/,
     *     '    CL    AVG WATER FLUX  AVG SALT FLUX    AVG CONC  ',/
     *     '             (M3/SEC)      (C*M3/SEC)        (PPT)   ',/,
     *     ' --------    --------------------------------------  ',/)
      ELSE
         WRITE(*,3020)
 3020      FORMAT(/,
     *     '    CL    AVG WATER FLUX  AVG SALT FLUX    AVG CONC  ',/
     *     '             (FT3/SEC)     (C*FT3/SEC)       (PPT)   ',/,
     *     ' --------    --------------------------------------  ',/)
      ENDIF
      write(44,*) '-------------------------------------------'
      write(44,*)
      write(44,*) '       SUMMARY OF AVERAGE VALUES'
      write(44,*)
      IF (IMETRIC.EQ.1) THEN
         WRITE(44,3010)
      ELSE
         WRITE(44,3020)
      ENDIF
      do i=1,ncl 
        rintcl = 0.0
        rintcls= 0.0
        rintclc = 0.0
        do j=2,ihave 
          rintcl=rintcl +
     +    (wflux(i,j,1) + wflux(i,j-1,1))/2.*(time(j)-time(j-1))
          rintcls=rintcls +
     +    (wflux(i,j,2) + wflux(i,j-1,2))/2.*(time(j)-time(j-1))
          if (abs(rintcl) .gt. 0.0001)
     +    rintclc = rintclc + abs(rintcls/rintcl)*(time(j)-time(j-1))
        end do
        rintcl = rintcl / (time(ihave) - time(1))
        rintcls = rintcls / (time(ihave) - time(1))
        rintclc = rintclc / (time(ihave) - time(1))
        write(*,4000) i, rintcl, rintcls, rintclc 
        write(44,4000) i, rintcl, rintcls, rintclc
 4000   format(I8, 2f15.2, f12.5)
      end do
C GLB end addition 
C
      STOP
      END
C
C  ------------------------------------------------------------------------ 
      SUBROUTINE GEOREAD
      SAVE
C
C     -  - - - - - - - - - - - -
      PARAMETER(MNN=200000,  MEN=90000,
     *          MAXL=50,            ! cosmetic
     *          MAXT=20000,          ! max time steps
     *          MCC=50,MCCN=1500)    ! max GC lines and nodes per line
      COMMON /GEOM/  NE,NP,CORD(MNN,3),AO(MNN),NOP(MEN,20),
     *               IMAT(MEN), AT(MEN),DEP(MNN),AVDEP(MEN),
     *               NETOT,NPTOT,NREF(MNN),NDEP(MNN),
     *               ALFA(MNN),NCORN(MNN)
      COMMON /MESH1/ WIDTH(MNN),SS1(MNN),SS2(MNN),WIDS(MNN)
      COMMON /LINES/ LMT(MCC),LINE(MCC,MCCN),NCL
      COMMON /HYDRO/ TET, VEL(6,MNN), WSEL(MNN), NDRY(MNN)
      COMMON /FLUX/  TIME(MAXT), WFLUX(MAXL,MAXT,3)
      COMMON /CNTRL/ ICYC, IHAVE, NCALL, TETSTART,
     *               TETSTOP, ONCER2
C-    DATA MANAGEMENT REQUIREMENTS
      COMMON /DMSCHR/BANGFG(15),BANRM2(15),BANCON(15)
      COMMON /DMSREC/IREC(40),FREC(40),
     *               IPACKB(1200), IPACKT(77), IPACKH(1200)
      CHARACTER      BANGFG*80, BANRM2*80, BANCON*80
      CHARACTER*77 TITLE, TGFGEN
C-
      COMMON /LAB/ TITLE, TGFGEN
C     -  - - - - - - - - - - - -
C

C  READ THE 3D GEOMETRY INFORMATION
C     NPTOT = TOTAL NUMBER OF NODES
C     NETOT = TOTAL NUMBER OF ELEMENTS
C        NP = NUMBER OF SURFACE NODES
C        NE = NUMBER OF SURFACE ELEMENTS
C
      LUIN1 = 33
      READ (LUIN1)  NPTOT, NETOT, NP, NE
      WRITE (*,140) NPTOT, NETOT, NP, NE
  140 FORMAT ( /, ' FOR THE INPUT RMA10 GEOMETRY FILE:', /,
     *            '     TOTAL NUMBER OF NODES      = ', I20,/,
     *            '     TOTAL NUMBER OF ELEMENTS   = ', I20,/,
     *            '     NUMBER OF SURFACE NODES    = ', I20,/,
     *            '     NUMBER OF SURFACE ELEMENTS = ', I20,// )
      IF (NPTOT.GT.MNN) PRINT *,' ==> Increase max node param'
      IF (NPTOT.GT.MNN) Stop
      IF (NETOT.GT.MEN) PRINT *,' ==> Increase max elem param'
      IF (NETOT.GT.MEN) Stop
C
      REWIND LUIN1
C
      READ (LUIN1) NPTOT, NETOT, NP, NE,
     *     ((CORD(J,K), SPECDUM, K=1,3),
     *     ALFA(J), NXDUM, AO(J), NSDUM, J = 1, NPTOT),
     *     (NDEP(J), NREF(J), J = 1, NP),
     *     ((NOP(J,K),K=1,20), NCORN(J), IMAT(J), THDUM,
     *     NXHDUM, J = 1, NETOT),
     *     (WIDUM, J = 1, NPTOT)
C
      PRINT *,' --> Finished 3D Geo read ... '
C-
      RETURN
      END
C ---------------------------------------------------------------------

      SUBROUTINE R10READ
      SAVE
C ... Purpose:  Read RMA10 binary solution files
C    
C     -  - - - - - - - - - - - -
      PARAMETER(MNN=200000,  MEN=90000,
     *          MAXL=50,            ! cosmetic
     *          MAXT=20000,          ! max time steps
     *          MCC=50,MCCN=1500)    ! max GC lines and nodes per line
      COMMON /GEOM/  NE,NP,CORD(MNN,3),AO(MNN),NOP(MEN,20),
     *               IMAT(MEN), AT(MEN),DEP(MNN),AVDEP(MEN),
     *               NETOT,NPTOT,NREF(MNN),NDEP(MNN),
     *               ALFA(MNN),NCORN(MNN)
      COMMON /MESH1/ WIDTH(MNN),SS1(MNN),SS2(MNN),WIDS(MNN)
      COMMON /LINES/ LMT(MCC),LINE(MCC,MCCN),NCL
      COMMON /HYDRO/ TET, VEL(6,MNN), WSEL(MNN), NDRY(MNN)
      COMMON /FLUX/  TIME(MAXT), WFLUX(MAXL,MAXT,3)
      COMMON /CNTRL/ ICYC, IHAVE, NCALL, TETSTART,
     *               TETSTOP, ONCER2
C-    DATA MANAGEMENT REQUIREMENTS
      COMMON /DMSCHR/BANGFG(15),BANRM2(15),BANCON(15)
      COMMON /DMSREC/IREC(40),FREC(40),
     *               IPACKB(1200), IPACKT(77), IPACKH(1200)
      CHARACTER      BANGFG*80, BANRM2*80, BANCON*80
      CHARACTER*77 TITLE, TGFGEN
C-
      COMMON /LAB/ TITLE, TGFGEN
C
      DIMENSION IRESAV(9)
C     -  - - - - - - - - - - - -
C   
C -------
        LUIN2 = 1
        NDF = 6
        NDFP1 = NDF + 1
C ...   RMA10-WES binary solution read
  500   READ (LUIN2,END=2000) TET, NP1, NDF, NE1,
     &       NDFS,(IRESAV(K),K=1,NDFS),
     &       ((VEL(K,J),J = 1, IRESAV(K)),K=1,NDF),
     &       (WSEL(J),J = 1, IRESAV(3)),
     &       (IMAT(J), J = 1, NE1), (NDRY(J), J = 1, NP1),
     &       (RDUM,J=1,IRESAV(NDFP1)),(RDUM,J=1,IRESAV(NDFS)),
     &       (RDUM, J = 1, NP1), (RDUM,
     &       J = 1, NE1)
      IF (NP1 .NE. NPTOT .OR. NE1 .NE. NETOT) THEN
        Write(*,*) 'Error: solution and 3-d geometry do not correspond'
        STOP
      END IF
      IF (TET .LT. TETSTART) GO TO 500
      IF (TET .GT. TETSTOP) GO TO 2000
 
 1000 WRITE (*,'('' --> processing RMA10 hour='',F12.5)') TET
      IHAVE = IHAVE + 1
      TIME(IHAVE) = TET
 2000 RETURN
      END

C ----------------------------------------------------------------

      SUBROUTINE PRESET
      SAVE
C
C     -  - - - - - - - - - - - -
      PARAMETER(MNN=200000,  MEN=90000,
     *          MAXL=50,            ! cosmetic
     *          MAXT=20000,          ! max time steps
     *          MCC=50,MCCN=1500)    ! max GC lines and nodes per line
      COMMON /GEOM/  NE,NP,CORD(MNN,3),AO(MNN),NOP(MEN,20),
     *               IMAT(MEN), AT(MEN),DEP(MNN),AVDEP(MEN),
     *               NETOT,NPTOT,NREF(MNN),NDEP(MNN),
     *               ALFA(MNN),NCORN(MNN)
      COMMON /MESH1/ WIDTH(MNN),SS1(MNN),SS2(MNN),WIDS(MNN)
      COMMON /LINES/ LMT(MCC),LINE(MCC,MCCN),NCL
      COMMON /HYDRO/ TET, VEL(6,MNN), WSEL(MNN), NDRY(MNN)
      COMMON /FLUX/  TIME(MAXT), WFLUX(MAXL,MAXT,3)
      COMMON /CNTRL/ ICYC, IHAVE, NCALL, TETSTART,
     *               TETSTOP, ONCER2
C-    DATA MANAGEMENT REQUIREMENTS
      COMMON /DMSCHR/BANGFG(15),BANRM2(15),BANCON(15)
      COMMON /DMSREC/IREC(40),FREC(40),
     *               IPACKB(1200), IPACKT(77), IPACKH(1200)
      CHARACTER      BANGFG*80, BANRM2*80, BANCON*80
      CHARACTER*77 TITLE, TGFGEN
C-
      COMMON /LAB/ TITLE, TGFGEN
C     -  - - - - - - - - - - - -
C
      COMMON /CARD/ JREC
      DIMENSION INT(200),REA(200),TH(MEN),NGWN(MNN)
      CHARACTER JREC(80)*1,idummy(Maxl)*80,COMT*80,ic1*2,ic3*1

      IECHO = 0
      NCOM = 0
      ncl = 0
  250 READ(4,260,END=900) IC1,IC3,(JREC(I),I=1,80)
  260 FORMAT(A2,A1,80A1)
  261 FORMAT(1x,A2,A1,80A1)
      I1 = 1
      PRINT 261, IC1,IC3,(JREC(I),I=1,80)
C
      IF (IC1 .EQ. 'GW') THEN
C ...   NODAL CROSS SECTIONAL WIDTHS                           (GW CARD)
        NWD = 4
        DO 980 I = 1, NWD
          REA(I) = 0.0
          INT(I) = 0
  980   CONTINUE
        NWD1 = 1
        CALL CRACK(I1, NWD1, 0, INT, 0, 'INTEGER  ', IERR)
        NWD2 = 4
        CALL CRACK(I1, NWD2, REA, 0, 0, 'REAL    ', IERR)
        NODER = INT(1)
          IF (IC3 .NE. ' ') THEN
C ...       INITIALIZE BY NODE                               
            NGWN(NODER)  = NODER
            WIDTH(NODER) = REA(1)
            SS1(NODER)   = REA(2)
            SS2(NODER)   = REA(3)
            WIDS(NODER)  = REA(4)
          ELSE 
C ...       INITIALIZE ENTIRE RANGE OF VALUES                
            DO 1000 II = NODER, MNN
              NWID = NWID + 1
              WIDTH(II) = REA(1)
              SS1(II)   = REA(2)
              SS2(II)   = REA(3)
              WIDS(II)  = REA(4)
 1000       CONTINUE
          END IF 
C-
      ELSE IF (IC1 .EQ. 'GC') THEN
C
C**                              CONTINUITY LINE CHECK CARD   (GC  CARD)
C
C ...   (ONLY USE CORNER NODES ON THE CARD)
C ===   IF (IRVIZ .GT. 0) GO TO 1280
        NCLTOT = NCLTOT + 1
        IF (NCLTOT .GT. MCC) THEN
          PRINT *,' ==> ERROR, too many continuity check lines '
          IERR = IERR + 1
        ELSE
          NWD = 200
          CALL CRACK(I1, NWD, REA, INT, CHA, 'INTEGER  ', IERC)
          NCL = INT(1)
          IF (NCL.GT. MCC) THEN
            PRINT *, ' ==> ERROR, exceeded max# continuity checks=',MCC
            IERR = IERR + 1
            GO TO 295
          ENDIF
          NGCCRD = 0
          DO 245 I = 1, NWD - 1
              LINE(NCL,I) = INT(I+1)
              IF (LINE(NCL,I) .LT. 1) GO TO 270
              IF (LINE(NCL,I) .GT. MNN) THEN
                PRINT 240, IC1, IC3, NCL, LINE(NCL,I)
  240           FORMAT (' ==> ERROR, An out of bounds node was read ',/
     *                  '     on ',A2,A1,' CARD',
     *                  '     continuity line #=',I6,'  node=',I10)
                IERR = IERR + 1
                GO TO 295
              END IF
              NGCCRD = NGCCRD + 1
  245     CONTINUE
  251     ISTART = NGCCRD
C
C ...     NEED TO READ GC-CARD CONTINUATION CARD INFORMATION
          READ (4,260,END=900) IC1, IC3, (JREC(I), I = 1, 77)
          I1 = 1
          IF (IC1 .NE. 'GC') THEN
                PRINT *, ' ==> Found a ', IC1, IC3,' Card ...'
                PRINT *, ' ==> ERROR, EXPECTED GC continuation'
                PRINT *, ' ==> NO end-of-GC string encountered'
                PRINT *, ' ==> A negative or zero marks end of string'
                IERR = IERR + 1
                BACKSPACE 4 
                GO TO 295
          END IF
          NWD = 200
          CALL CRACK(I1, NWD, REA, INT, CHA, 'INTEGER  ', IERC)
          DO 255 I = 1, NWD
               IF( INT(I).LT.1 ) GO TO 270
               ISTART = ISTART + 1
               LINE(NCL,ISTART) = INT(I)
               IF ( LINE(NCL,ISTART) .GT. MNN) THEN
                  IERR = IERR + 1
                  GO TO 295
               END IF
               NGCCRD = NGCCRD + 1
  255     CONTINUE
          GO TO 251
        END IF
C-
  270   LMT(NCL) =  NGCCRD
C-      NOTE: SUB CHECK will insert midside nodes into continuity lines
C-      ----------------
  295   GO TO 250 
C-
        ELSE IF(IC1 .EQ. 'CO') THEN
          NCOM = NCOM + 1
          COMT(1:2) = IC1
          COMT(3:3) = IC3
          DO 480 JC = 4,80
          COMT(JC:JC) = JREC(JC-3)(1:1)
  480     CONTINUE
          idummy(NCOM)(1:80) = COMT(1:80)
        ELSE
C         WRITE(20,2000) IC1,IC3,(JREC(II),II=1,77)
 2000  FORMAT(A2,A1,77A1)
          goto 250
        ENDIF
        GOTO 250
  900   continue

        PRINT 2870, IERR
 2870   FORMAT (' CARD input completed with ', I6, ' errors.',/)
        IF (IERR .GT. 0)  STOP 'ERROR'
        RETURN
 2320   FORMAT (1X, A2, A1, 80A1)
 2330   FORMAT (' *** ERROR, ILLEGAL CARD TYPE ***')
        END
C
C -----------------------------------------------------------------
C
      SUBROUTINE CRACK(I1,NWD,REA,INT,CHA,TYPE,IERR)
C ... AUTHOR = DON BACH
C ... CRACKS DATA CARDS SIMULATING LIST DIRECTED READS
C ... THIS SUBROUTINE CONFORMS TO ANSI X3.9-1978 (FORTRAN 77)
C ... CARD IS PASSED THROUGH COMMON BLOCK
      IMPLICIT REAL (A-H,O-Z)
      CHARACTER*1  IBLANK,ICMT,ICMD,IPERID,ICOMMA
      CHARACTER*1  ICG,IDT,ISI 
      COMMON /CARD/ JREC 
      DIMENSION REA(NWD), INT(NWD)
      CHARACTER JREC(80)*1, CHA(NWD)*20,TYPE*9,IBUF*77,IFOR(1)*10
      DATA IBLANK /' '/
      DATA ICMT   /'*'/
      DATA ICMD   /'$'/
      DATA IPERIOD/'.'/
      DATA ICOMMA /','/
C-
C ... LOOP FOR EACH WORD TO BE READ
      IF(I1.GT.77) RETURN
      NWD1=NWD
      DO 900 I=1,NWD1
C ... FIND START OF DATA (FIRST NON-BLANK CHARACTER)
   10 CONTINUE
      IF(JREC(I1).NE.IBLANK.AND.JREC(I1).NE.ICOMMA) GO TO 15
      I1=I1+1
      IF(JREC(I1).EQ.ICOMMA) THEN
          I1=I1+1
          GO TO 900
        ENDIF
      IF(I1.GT.77) THEN
          NWD=I-1
          RETURN
        ENDIF
      GO TO 10
   15 CONTINUE
C  FIND END OF DATA
      I2=I1+1
   20 CONTINUE
      IF(JREC(I1).EQ.'''') THEN
          IF(JREC(I2).EQ.'''') GO TO 25
        ELSE
          IF(JREC(I2).EQ.IBLANK.OR.JREC(I2).EQ.ICOMMA) GO TO 25
        ENDIF
      I2=I2+1
      IF(I2.LT.78) GO TO 20
      I2=77
   25 CONTINUE
      IF(JREC(I1).EQ.'''') I1=I1+1
      LENGTH=I2-I1
      IF(LENGTH.LT.1) LENGTH=1
      I2=I2-1
C ... PACK DATA INTO BUFFER
      WRITE(IBUF,30)(JREC(J),J=I1,I2)
   30 FORMAT(77A1)
C ... SET UP CORRECT FORMAT AND READ DATA
      IF(TYPE(1:4).EQ.'REAL') THEN
          WRITE(IFOR,35) LENGTH
   35     FORMAT('(F',I2,'.0)')
          READ(IBUF,IFOR) REA(I)
        ELSE IF(TYPE(1:7).EQ.'INTEGER') THEN
          WRITE(IFOR,40) LENGTH
   40     FORMAT('(I',I2,')')
          READ(IBUF,IFOR) INT(I)
        ELSE IF(TYPE(1:9).EQ.'CHARACTER') THEN
          WRITE(IFOR,45) LENGTH
   45     FORMAT('(A',I2,')')
          READ(IBUF,IFOR) CHA(I)
        ELSE
          PRINT 50,TYPE
   50     FORMAT(' *** ERROR, ',A10,' IS ILLEGAL IN CALL TO CRACK ***')
          IERR=IERR+1
          RETURN
        ENDIF
      IF(JREC(I2+1).EQ.'''') I2=I2+1
      I1=I2+1
  900 CONTINUE
      RETURN
      END
C
C --------------------------------------------------------------------------
      SUBROUTINE CHECK(ICK)
      SAVE
C ... routine was extracted from rma10  03/2000
C-
C     -  - - - - - - - - - - - -
      PARAMETER(MNN=200000,  MEN=90000,
     *          MAXL=50,            ! cosmetic
     *          MAXT=20000,          ! max time steps
     *          MCC=50,MCCN=1500)    ! max GC lines and nodes per line
      COMMON /GEOM/  NE,NP,CORD(MNN,3),AO(MNN),NOP(MEN,20),
     *               IMAT(MEN), AT(MEN),DEP(MNN),AVDEP(MEN),
     *               NETOT,NPTOT,NREF(MNN),NDEP(MNN),
     *               ALFA(MNN),NCORN(MNN)
      COMMON /MESH1/ WIDTH(MNN),SS1(MNN),SS2(MNN),WIDS(MNN)
      COMMON /LINES/ LMT(MCC),LINE(MCC,MCCN),NCL
      COMMON /HYDRO/ TET, VEL(6,MNN), WSEL(MNN), NDRY(MNN)
      COMMON /FLUX/  TIME(MAXT), WFLUX(MAXL,MAXT,3)
      COMMON /CNTRL/ ICYC, IHAVE, NCALL, TETSTART,
     *               TETSTOP, ONCER2
C-    DATA MANAGEMENT REQUIREMENTS
      COMMON /DMSCHR/BANGFG(15),BANRM2(15),BANCON(15)
      COMMON /DMSREC/IREC(40),FREC(40),
     *               IPACKB(1200), IPACKT(77), IPACKH(1200)
      CHARACTER      BANGFG*80, BANRM2*80, BANCON*80
      CHARACTER*77 TITLE, TGFGEN
C-
      COMMON /LAB/ TITLE, TGFGEN
C     -  - - - - - - - - - - - -
      DIMENSION FLUXX(MNN),FLUXY(MNN)
C-
      DIMENSION ITEMP(MCCN), FLW(2), AREAC(2)
C
      IF (NCL .LE. 0) RETURN
C
      IF (NCALL .GT. 0) GO TO 160
      NCALL = 1
      DO J = 1, MNN
        FLUXX(J) = 0.0
        FLUXY(J) = 0.0
      END DO
C-
C-..... Augment continuity lists.....
C-
      DO 150 J = 1, NCL
        M = LMT(J)
        DO 100 K = 1, M
          ITEMP(K) = LINE(J,K)
  100   CONTINUE
        LMT(J) = 2 * LMT(J) - 1
        NN = LMT(J)
        N = 0
        IF (M .EQ. 1) GO TO 150
        DO 140 L = 1, NN, 2
          N = N + 1
          NA = ITEMP(N)
          NC = ITEMP(N+1)
          LINE(J,L) = NA
          LINE(J,L+2) = NC
          DO 130 JJ = 1, NETOT
            NCN = NCORN(JJ)
            DO 120 KK = 1, NCN, 2
              KKK = MOD(KK+2,NCN)
              IF (KKK .EQ. 0) KKK = 3
              N1 = IABS(NOP(JJ,KK))
              N2 = IABS(NOP(JJ,KKK))
              IF (NA .EQ. N1 .AND. NC .EQ. N2) GO TO 110
              IF (NC .EQ. N1 .AND. NA .EQ. N2) GO TO 110
             GO TO 120
  110         LINE(J,L+1) = IABS(NOP(JJ,KK+1))
              GO TO 140
  120       CONTINUE
  130     CONTINUE
  140   CONTINUE
  150 CONTINUE
  160 CONTINUE
      DO 230 J = 1, NCL
        NTL = 1
        SUMX = 0.0
        SUMY = 0.0
        TOTAL = 0.
C       TOTALS DEALS WITH SALINITY FLUX( OR WHATEVER IS DOF 4)
        TOTALS=0.
        IF (LMT(J) .EQ. 1) THEN
          NA = LINE(J,1)
          IF (NDEP(NA) .LT. 2) THEN
            SUMX = SQRT(VEL(1,NA)**2+VEL(2,NA)**2) * VEL(3,NA) *
     &           (2. * WIDTH(NA) + (SS1(NA) + SS2(NA)) *
     &           VEL(3,NA)) / 2.
C GLB add the following line to include constituent flux for 1-D elements
           totals = sumx*vel(ick,na)
          ELSE
            K = NREF(NA)
            L = K + NDEP(NA) - 3
            SUMX = 0.0
            ELEV = CORD(NA,3)
            XHT = ELEV - AO(NA)
            DO 170 M = K, L, 2
              IF (M .EQ. K) THEN
                M1 = NA
              ELSE
                M1 = M
              END IF
              FL = (CORD(M1,3) - CORD(M+2,3)) / XHT
              CA = COS(ALFA(NA))
              SN = SIN(ALFA(NA))
              V1 = VEL(1,M1) * CA + VEL(2,M1) * SN
              V2 = 4. * (VEL(1,M+1) * CA + VEL(2,M+1) * SN)
              V3 = VEL(1,M+2) * CA + VEL(2,M+2) * SN
              SUMX = SUMX + (V1 + V2 + V3) / 6. * VEL(3,NA) *
     &             FL * WIDTH(NA)
C GLB add the following line to include constituent flux for 1-D elements
              totals = totals + sumx*vel(ick,na)
  170       CONTINUE
          END IF
        ELSE
          MAX = LMT(J) - 2
          DO 220 K = 1, MAX, 2
            NA = LINE(J,K)
            NB = LINE(J,K+1)
            NC = LINE(J,K+2)
            DX = (CORD(NC,1) - CORD(NA,1))
            DY = (CORD(NC,2) - CORD(NA,2))
            IF (NDEP(NB) .GT. 1) THEN
              ELEV = CORD(NA,3)
              IDPI = NDEP(NA)
              IDPK = NDEP(NC)
              I1 = NA
              J1 = NB
              K1 = NC
C-
C...... Count elements downward
C-
              NEL = NDEP(NA) - 2
              IF (NDEP(NC) - 2 .GT. NEL) NEL = NDEP(NC) - 2
              DO 210 M = 1, NEL, 2
C-
C...... Test for triangle
C-
                IF (IDPK .GT. 1) THEN
                  IF (IDPI .GT. 1) THEN
C-
C...... This is quadrilateral
C-
                    I2 = NREF(NA) + M
                    I3 = I2 + 1
                    J2 = NREF(NB) + (M + 1) / 2
                    K2 = NREF(NC) + M
                    K3 = K2 + 1
                    VF1 = VEL(3,NA) / (ELEV - AO(NA))
                    VF2 = VEL(3,NC) / (ELEV - AO(NC))
                    VDIST = ((CORD(I1,3)-CORD(I3,3)) * VF1 +
     &                   (CORD(K1,3) - CORD(K3,3)) * VF2) / 2.
                    AREAC(1) = VDIST * DY
                    AREAC(2) = VDIST * DX
                    DO 180 LA = 1, 2
                      FLW(LA) =
     &                     ((VEL(LA,I2)+VEL(LA,J2)+VEL(LA,K2)+VE
     &                     L(LA,J1)) / 3. - (VEL(LA,I1) +
     &                     VEL(LA,I3) + VEL(LA,K3) + VEL(LA,K1)) /
     &                     12.) * AREAC(LA)
  180               CONTINUE
                    SUMX = SUMX + FLW(1)
                    SUMY = SUMY + FLW(2)
                    TOTAL = TOTAL + FLW(1) - FLW(2)
C GLB the following algorithm is modofied to handle
C GLB the 5th and 6th degreees of freedom (6OF)
                    U1=VEL(1,I1)
                    U2=VEL(1,I3)
                    U3=VEL(1,K3)
                    U4=VEL(1,K1)
                    U5=VEL(1,I2)
                    U6=VEL(1,J2)
                    U7=VEL(1,K2)
                    U8=VEL(1,J1)
                    V1=VEL(2,I1)
                    V2=VEL(2,I3)
                    V3=VEL(2,K3)
                    V4=VEL(2,K1)
                    V5=VEL(2,I2)
                    V6=VEL(2,J2)
                    V7=VEL(2,K2)
                    V8=VEL(2,J1)
                    S1=VEL(ICK,I1)
                    S2=VEL(ICK,I3)
                    S3=VEL(ICK,K3)
                    S4=VEL(ICK,K1)
                    S5=VEL(ICK,I2)
                    S6=VEL(ICK,J2)
                    S7=VEL(ICK,K2)
                    S8=VEL(ICK,J1)
cc                    FLWS=((-(U2+U3+U4)+2.*(U6+U7)+4.*(U5+U8))*S1
cc     .                  +(-(U1+U3+U4)+2.*(U7+U8)+4.*(U5+U6))*S2
cc     .                  +(-(U1+U2+U4)+2.*(U5+U8)+4.*(U6+U7))*S3
cc     .                  +(-(U1+U2+U3)+2.*(U5+U6)+4.*(U7+U8))*S4)
cc     .                  *AREAC(1)/36.
cc                    FLWS=FLWS-((-(V2+V3+V4)+2.*(V6+V7)+4.*(V5+V8))*S1
cc     .                  +(-(V1+V3+V4)+2.*(V7+V8)+4.*(V5+V6))*S2
cc     .                  +(-(V1+V2+V4)+2.*(V5+V8)+4.*(V6+V7))*S3
cc     .                  +(-(V1+V2+V3)+2.*(V5+V6)+4.*(V7+V8))*S4)
cc     .                  *AREAC(2)/36.
cc                    TOTALS=TOTALS+FLWS
                    FLWS1=(U1*(8.*(S2+S4)+12.*S3+24.*(S1-S5-S8)
     .                  -32.*(S6+S7))
     .                  +U2*(8.*(S1+S3)+12.*S4+24.*(S2-S5-S6)
     .                  -32.*(S7+S8))
     .                  +U3*(8.*(S2+S4)+12.*S1+24.*(S3-S6-S7)
     .                  -32.*(S5+S8))
     .                  +U4*(8.*(S1+S3)+12.*S2+24.*(S4-S7-S8)
     .                  -32.*(S5+S6))
     .                  +U5*(-24.*(S1+S2)-32.*(S3+S4)+64.*S7
     .                  +80.*(S6+S8)+128.*S5)
     .                  +U6*(-24.*(S2+S3)-32.*(S1+S4)+64.*S8
     .                  +80.*(S5+S7)+128.*S6)
     .                  +U7*(-24.*(S3+S4)-32.*(S1+S2)+64.*S5
     .                  +80.*(S6+S8)+128.*S7)
     .                  +U8*(-24.*(S1+S4)-32.*(S2+S3)+64.*S6
     .                  +80.*(S5+S7)+128.*S8))*AREAC(1)/720.
                    FLWS2=(V1*(8.*(S2+S4)+12.*S3+24.*(S1-S5-S8)
     .                  -32.*(S6+S7))
     .                  +V2*(8.*(S1+S3)+12.*S4+24.*(S2-S5-S6)
     .                  -32.*(S7+S8))
     .                  +V3*(8.*(S2+S4)+12.*S1+24.*(S3-S6-S7)
     .                  -32.*(S5+S8))
     .                  +V4*(8.*(S1+S3)+12.*S2+24.*(S4-S7-S8)
     .                  -32.*(S5+S6))
     .                  +V5*(-24.*(S1+S2)-32.*(S3+S4)+64.*S7
     .                  +80.*(S6+S8)+128.*S5)
     .                  +V6*(-24.*(S2+S3)-32.*(S1+S4)+64.*S8
     .                  +80.*(S5+S7)+128.*S6)
     .                  +V7*(-24.*(S3+S4)-32.*(S1+S2)+64.*S5
     .                  +80.*(S6+S8)+128.*S7)
     .                  +V8*(-24.*(S1+S4)-32.*(S2+S3)+64.*S6
     .                  +80.*(S5+S7)+128.*S8))*AREAC(2)/720.
                    TOTALS=TOTALS+FLWS1-FLWS2
                    IDPI = IDPI - 2
                    IDPK = IDPK - 2
                    I1 = I3
                    J1 = J2
                    K1 = K3
                  ELSE
C-
C...... Triangle type 1
C-
                    J2 = NREF(NB) + (M + 1) / 2
                    K2 = NREF(NC) + M
                    K3 = K2 + 1
                    VDIST = (CORD(K1,3) - CORD(K3,3)) *
     &                   VEL(3,NC) / (2. * (ELEV - AO(NC)))
                    AREAC(1) = VDIST * DY
                    AREAC(2) = VDIST * DX
                    DO 190 LA = 1, 2
                      FLW(LA) = (VEL(LA,J1) + VEL(LA,J2) +
     &                     VEL(LA,K2)) / 3. * AREAC(LA)
  190               CONTINUE
                    SUMX = SUMX + FLW(1)
                    SUMY = SUMY + FLW(2)
                    TOTAL = TOTAL + FLW(1) - FLW(2)
C GLB the following algorithm is modified to
C GLB handle the 5th and 6th degrees of freedom (6DOF)
                    U1=VEL(1,I1)
                    U2=VEL(1,K3)
                    U3=VEL(1,K1)
                    U4=VEL(1,J2)
                    U5=VEL(1,K2)
                    U6=VEL(1,J1)
                    V1=VEL(2,I1)
                    V2=VEL(2,K3)
                    V3=VEL(2,K1)
                    V4=VEL(2,J2)
                    V5=VEL(2,K2)
                    V6=VEL(2,J1)
                    S1=VEL(ICK,I1)
                    S2=VEL(ICK,K3)
                    S3=VEL(ICK,K1)
                    S4=VEL(ICK,J2)
                    S5=VEL(ICK,K2)
                    S6=VEL(ICK,J1)
CC                    FLWS=((-U1-U2+2.*U3+4.*U4+8.*(U5+U6))*S1
CC     .                  +(2.*U1-U2-U3+8.*(U4+U6)+4.*U5)*S2
CC     .                  +(-U1+2.*U2-U3+8.*(U4+U5)+4.*U6)*S3)
CC     .                  *AREAC(1)/60.
CC                    FLWS=FLWS-((-V1-V2+2.*V3+4.*V4+8.*(V5+V6))*S1
CC     .                  +(2.*V1-V2-V3+8.*(V4+V6)+4.*V5)*S2
CC     .                  +(-V1+2.*V2-V3+8.*(V4+V5)+4.*V6)*S3)
CC     .                  *AREAC(2)/60.
CC                    TOTALS=TOTALS+FLWS
                    FLWS1=(U1*(-(S2+S3)-4.*S5+6.*S1)
     .                +U2*(-(S1+S3)-4.*S6+6.*S2)
     .                +U3*(-(S1+S2)-4.*S4+6.*S3)
     .                +U4*(-4.*S3+16.*(S5+S6)+32.*S4)
     .                +U5*(-4.*S1+16.*(S4+S6)+32.*S5)
     .                +U6*(-4.*S2+16.*(S4+S5)+32.*S6))*AREAC(1)/180.
                    FLWS2=(V1*(-(S2+S3)-4.*S5+6.*S1)
     .                +V2*(-(S1+S3)-4.*S6+6.*S2)
     .                +V3*(-(S1+S2)-4.*S4+6.*S3)
     .                +V4*(-4.*S3+16.*(S5+S6)+32.*S4)
     .                +V5*(-4.*S1+16.*(S4+S6)+32.*S5)
     .                +V6*(-4.*S2+16.*(S4+S5)+32.*S6))*AREAC(2)/180.
                    TOTALS=TOTALS+FLWS1-FLWS2
                    GO TO 220
                  END IF
                ELSE
C-
C...... Triangle type 2
C-
                  J2 = NREF(NB) + (M + 1) / 2
                  I2 = NREF(NA) + M
                  I3 = I2 + 1
                  VDIST = (CORD(I1,3) - CORD(I3,3)) * VEL(3,NA) /
     &                 (2. * (ELEV - AO(NA)))
                  AREAC(1) = VDIST * DY
                  AREAC(2) = VDIST * DX
                  DO 200 LA = 1, 2
                    FLW(LA) = (VEL(LA,J1) + VEL(LA,J2) +
     &                   VEL(LA,I2)) / 3. * AREAC(LA)
  200             CONTINUE
                  SUMX = SUMX + FLW(1)
                  SUMY = SUMY + FLW(2)
                  TOTAL = TOTAL + FLW(1) - FLW(2)
C GLB the following algorithm is modified to handle
C GLB the 5th and 6th degrees of freeedom (6DOF)
                  U1=VEL(1,K1)
                  U2=VEL(1,I3)
                  U3=VEL(1,I1)
                  U4=VEL(1,J2)
                  U5=VEL(1,I2)
                  U6=VEL(1,J1)
                  V1=VEL(2,K1)
                  V2=VEL(2,I3)
                  V3=VEL(2,I1)
                  V4=VEL(2,J2)
                  V5=VEL(2,I2)
                  V6=VEL(2,J1)
                  S1=VEL(ICK,K1)
                  S2=VEL(ICK,I3)
                  S3=VEL(ICK,I1)
                  S4=VEL(ICK,J2)
                  S5=VEL(ICK,I2)
                  S6=VEL(ICK,J1)
CC                    FLWS=((-U1-U2+2.*U3+4.*U4+8.*(U5+U6))*S1
CC     .                  +(2.*U1-U2-U3+8.*(U4+U6)+4.*U5)*S2
CC     .                  +(-U1+2.*U2-U3+8.*(U4+U5)+4.*U6)*S3)
CC     .                  *AREAC(1)/60.
CC                    FLWS=FLWS-((-V1-V2+2.*V3+4.*V4+8.*(V5+V6))*S1
CC     .                  +(2.*V1-V2-V3+8.*(V4+V6)+4.*V5)*S2
CC     .                  +(-V1+2.*V2-V3+8.*(V4+V5)+4.*V6)*S3)
CC     .                  *AREAC(2)/60.
CC                    TOTALS=TOTALS+FLWS
                    FLWS1=(U1*(-(S2+S3)-4.*S5+6.*S1)
     .                +U2*(-(S1+S3)-4.*S6+6.*S2)
     .                +U3*(-(S1+S2)-4.*S4+6.*S3)
     .                +U4*(-4.*S3+16.*(S5+S6)+32.*S4)
     .                +U5*(-4.*S1+16.*(S4+S6)+32.*S5)
     .                +U6*(-4.*S2+16.*(S4+S5)+32.*S6))*AREAC(1)/180.
                    FLWS2=(V1*(-(S2+S3)-4.*S5+6.*S1)
     .                +V2*(-(S1+S3)-4.*S6+6.*S2)
     .                +V3*(-(S1+S2)-4.*S4+6.*S3)
     .                +V4*(-4.*S3+16.*(S5+S6)+32.*S4)
     .                +V5*(-4.*S1+16.*(S4+S6)+32.*S5)
     .                +V6*(-4.*S2+16.*(S4+S5)+32.*S6))*AREAC(2)/180.
                    TOTALS=TOTALS+FLWS1-FLWS2
                  GO TO 220
                END IF
  210         CONTINUE
            ELSE
C-
C........ Two dimensional element
C-
              D1 = VEL(3,NA)
              D3 = VEL(3,NC)
C              IF (D1 .LE. DSET .OR. D3 .LE. DSET) GO TO 220
              D2 = (D1 + D3) / 2.
              SUMX = SUMX + DY * (VEL(1,NA) * D1 + 4.0 *
     &             VEL(1,NB) * D2 + VEL(1,NC) * D3) / 6.
              SUMY = SUMY + DX * (VEL(2,NA) * D1 + 4.0 *
     &             VEL(2,NB) * D2 + VEL(2,NC) * D3) / 6.
C GLB the following algorithm is modified to handle the
C GLB 5th and 6th degrees of freedom (6DOF)
              H1=VEL(3,NA)
              H2=VEL(3,NC)
              S1=VEL(ICK,NA)
              S2=VEL(ICK,NC)
              S3=VEL(ICK,NB)
              FX1=FLUXX(NA)
              FY1=FLUXY(NA)
              FX2=FLUXX(NC)
              FY2=FLUXY(NC)
              FX3=FLUXX(NB)
              FY3=FLUXY(NB)
              U1=VEL(1,NA)
              U2=VEL(1,NC)
              U3=VEL(1,NB)
              V1=VEL(2,NA)
              V2=VEL(2,NC)
              V3=VEL(2,NB)
CC              FLWS=((U1*(9.*S1*H1+S1*H2+S2*H1-S2*H2)
CC     .          +U2*(-S1*H1+S1*H2+S2*H1+9.*S2*H2)
CC     .          +U3*(12.*(S1*H1+S2*H2)+8.*(S1*H2+S2*H1)))*DY
CC     .          -(V1*(9.*S1*H1+S1*H2+S2*H1-S2*H2)
CC     .          +V2*(-S1*H1+S1*H2+S2*H1+9.*S2*H2)
CC     .          +V3*(12.*(S1*H1+S2*H2)+8.*(S1*H2+S2*H1)))*DY)/60.
CC              TOTALS=TOTALS+FLWS
            FLWS1=(U1*(S1*(7.*H1+H2)-S2*(H1+H2)+4.*S3*H1)
     .        +U2*(-S1*(H1+H2)+S2*(H1+7.*H2)+4.*S3*H2)
     .        +U3*(4.*S1*H1+4.*S2*H2+16.*S3*(H1+H2)))*DY/60.
            FLWS2=(V1*(S1*(7.*H1+H2)-S2*(H1+H2)+4.*S3*H1)
     .        +V2*(-S1*(H1+H2)+S2*(H1+7.*H2)+4.*S3*H2)
     .        +V3*(4.*S1*H1+4.*S2*H2+16.*S3*(H1+H2)))*DX/60.
             FLWS1=FLWS1+ DY * (FX1 * D1 + 4.0 *
     &             FX2 * D2 + FX3 * D3) / 6.
             FLWS2=FLWS2 + DX * (FY1 * D1 + 4.0 *
     &             FY2 * D2 + FY3 * D3) / 6.

            TOTALS=TOTALS+FLWS1-FLWS2
            END IF
  220     CONTINUE
        END IF
        TOTAL = SUMX - SUMY
        IF (J .EQ. 1) REF = TOTAL
        IF (ABS(REF) .LT. 0.0001) REF = 1.
        PCT = 100.0 * TOTAL / REF
        MX = LMT(J)
      WFLUX(J,ICYC,1) = TOTAL
      WFLUX(J,ICYC,2) = TOTALS 
  230 CONTINUE
      RETURN
  240 FORMAT ( // 10X, 'CONTINUITY CHECKS' // 10X, 'LINE          '
     &     , 'TOTAL          X FLOW         Y FLOW   PERCENT'
     &     , '     SALT FLUX   ')
  250 FORMAT (10X, I4, 1P3E15.3, 0PF10.1,1PE17.5)
      END

C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      SUBROUTINE CONVRT ( ISTRNG, LENGTH, INTVAL, ISIZE, ISWTCH )
      SAVE
C-
C ... AUTHOR: BOB EVANS/BARBARA DONNELL              01-22-1993
C ... ISWTCH:  1 CONVERT FROM CHARACTER TO ASCII INTEGER VALUE
C ...          2 CONVERT FROM INTEGER VALUE TO CHARACTER EQUIVALENT
C ...          3 CONSOLIDATE BANNERS
C ... PURPOSE:  Avoid FORTRAN to C-Language binary file read problems
C-
      PARAMETER (MM8=1200)
C     -  - - - - - - - - - - - -
C     -  - - - - - - - - - - - -
      PARAMETER(MNN=200000,  MEN=90000,
     *          MAXL=50,            ! cosmetic
     *          MAXT=20000,          ! max time steps
     *          MCC=50,MCCN=1500)    ! max GC lines and nodes per line
      COMMON /GEOM/  NE,NP,CORD(MNN,3),AO(MNN),NOP(MEN,20),
     *               IMAT(MEN), AT(MEN),DEP(MNN),AVDEP(MEN),
     *               NETOT,NPTOT,NREF(MNN),NDEP(MNN),
     *               ALFA(MNN),NCORN(MNN)
      COMMON /LINES/ LMT(MCC),LINE(MCC,MCCN),NCL
      COMMON /HYDRO/ TET, VEL(6,MNN), WSEL(MNN), NDRY(MNN)
      COMMON /FLUX/  TIME(MAXT), WFLUX(MAXL,MAXT,3)
      COMMON /CNTRL/ ICYC, IHAVE, NCALL, TETSTART,
     *               TETSTOP, ONCER2
C-    DATA MANAGEMENT REQUIREMENTS
      COMMON /DMSCHR/BANGFG(15),BANRM2(15),BANCON(15)
      COMMON /DMSREC/IREC(40),FREC(40),
     *               IPACKB(1200), IPACKT(77), IPACKH(1200)
      CHARACTER      BANGFG*80, BANRM2*80, BANCON*80
      CHARACTER*77 TITLE, TGFGEN
C-
      COMMON /LAB/ TITLE, TGFGEN
C     -  - - - - - - - - - - - -
      CHARACTER  ISTRNG(15)*80
      DIMENSION  INTVAL(MM8)
C-
C-
      ISTOP = 0
      IF ( (ISIZE*LENGTH) .GT. MM8 ) THEN
          WRITE(*,*)' Error in CONVRT, LENGTH & SIZE are inconsistant'
          WRITE(*,*)'          with dimension capabilities'
          ISTOP= 1
      ENDIF
      IF ( ISIZE .GT. MM8 ) THEN
          WRITE(*,*)' Error in CONVRT ... size larger than 1200'
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
C ...   consolidate BANNER section   BANDMS = BANGFG + BANRM2 + BANCON
C-
	ISTRNG(1)(1:80)  = BANGFG(1)(1:80)
	ISTRNG(2)(1:80)  = BANGFG(2)(1:80)
	ISTRNG(3)(1:80)  = BANGFG(3)(1:80)
	ISTRNG(4)(1:80)  = BANGFG(4)(1:80)
	ISTRNG(5)(1:80)  = BANGFG(5)(1:80)
C
	ISTRNG(6)(1:80)  = BANRM2(1)(1:80)
	ISTRNG(7)(1:80)  = BANRM2(2)(1:80)
	ISTRNG(8)(1:80)  = BANRM2(3)(1:80)
	ISTRNG(9)(1:80)  = BANRM2(4)(1:80)
	ISTRNG(10)(1:80) = BANRM2(5)(1:80)
C
	ISTRNG(11)(1:80) = BANCON(1)(1:80)
	ISTRNG(12)(1:80) = BANCON(2)(1:80)
	ISTRNG(13)(1:80) = BANCON(3)(1:80)
	ISTRNG(14)(1:80) = BANCON(4)(1:80)
	ISTRNG(15)(1:80) = BANCON(5)(1:80)
        WRITE(*,*)' --> Character Banners have been consolidated'
C-
      ELSE
C-
          WRITE(*,*)' --> Value of ISWTCH invalid in CONVRT'
      ENDIF
      RETURN
      END



