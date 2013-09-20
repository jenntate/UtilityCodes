      PROGRAM SLICE_S
C 
C   THIS PROGRAM WILL ACCESS THE OUTPUT FILE OF AN RMA-10 RUN
C   AND TAKE HORIZONTAL "SURFACE SLICES". THE DATA WILL
C   BE WRITTEN TO SEPARATE FILES FOR EACH DEPTH.  BOB EVANS 9-1991
C   Revised in Jan 1995 to accept seperate 3Dgeo read & write
C   updated 'fake' RMA2/RMA4 type of solution files (BpD)
C  
C   Modified by BARBARA DONNELL 09-20-1997
C 
C******************************************************************
C 
      PARAMETER (MAXNE = 50000, MAXN = 100000, MAXNDF = 6,
     *           ITMAX=500, MAXDEP = 10, LUIN1 = 55, LUIN2 = 56)

      COMMON  /BLKIN/ NP, NE, NDF, NDF2DO, ZSLICE(MAXDEP),
     *                NOP(MAXNE,20), NDEP(MAXN), NREF(MAXN), 
     *                CORD(MAXN,3),  XVEL(MAXNDF,MAXN), AO(MAXN),
     *                DELBED(MAXN),BSHR(MAXN)

      COMMON  /BLKOUT/VEL(MAXNDF,MAXN), 
     *                VVEL(MAXN), WSEL(MAXN), ZSEL(MAXN),
     *                NDRY(MAXN), IMAT(MAXNE)
 
      COMMON  /ALL/ IPUTIT, IWANT_ALL, NSAVE, NSTEPS,   
     *              HOURLY, TMIN, TMAX, TIME2GET(ITMAX)

      COMMON  /REF/ IREF,REFEL

      COMMON  /BUG/ ITRACE

C ... needed for banners
      COMMON  /DMSCHR/ HEADER, IDMS(15), BANSLC(2),
     *                 BANGFG(5), BANRM2(5), BANRM4(5), EMPTY(5)
      COMMON  /DMSREC/ IREC(40),FREC(40), IPACKB(1200), IPACKT(77)
      CHARACTER        HEADER*77,  IDMS*80, BANSLC*80,
     *                 BANGFG*80, BANRM2*80, BANRM4*80, EMPTY*80

      CHARACTER *80 INFILE, OUTFILV(MAXDEP),OUTFILS(MAXDEP),
     *              PREFIX1, PREFIX2
      CHARACTER CDEPTH*6, CSLICE*2, CHECKV*1, CHECKS*1, REPLY*1
      CHARACTER *1 VECTOR(2)
 
      INTEGER TRMLEN
C ... REAL star 8 needed for HEC version of RMA10 solution files
C     REAL*8  AO
C      
      LOGICAL HERE
C GLB-NSF add this
      DIMENSION IRESAV(9)
C 
C******************************************************************
C 
C                               
      DATA IREC/40*0/, FREC/40*0.0/, VECTOR/'X','Y'/
      DATA IDEBUG /0/, IEOF /0/
C-
C                      123456789-123456789-123456789-123456789-
      DATA BANSLC(1)/'CEWES-CHL PROGRAM SLICE SURFACE 2.1     '/
      DATA BANSLC(2)/'Last modified  09-20-1997               '/
C
      DATA BANGFG(1)/'CEWES-HE DMS BANNER HEADINGS FOR GFGEN  '/
      DATA BANGFG(2)/'ENGLISH UNITS APPLIED IN THIS GEOMETRY  '/
      DATA BANGFG(3)/'GFGEN VERSION 4.35         1-D AND 2-D  '/
      DATA BANGFG(4)/'CAPABILITY.   LAST MOD DATE 09-20-1997  '/
      DATA BANGFG(5)/'        Run thru SLICE                  '/
C
C                      123456789-123456789-123456789-123456789-
      DATA BANRM2(1)/'CEWES-HE DMS BANNER HEADINGS FOR RMA2-V '/
      DATA BANRM2(2)/'ENGLISH UNITS APPLIED IN THIS HYDRO     '/
      DATA BANRM2(3)/'RMA2 VERSION 4.35          1-D AND 2-D '/
      DATA BANRM2(4)/'CAPABILITY.  LAST MOD DATE  09-05-1997  '/
      DATA BANRM2(5)/'        Run through SLICE               '/
C
C                      123456789-123456789-123456789-123456789-
      DATA BANRM4(1)/'CEWES-HE DMS BANNER HEADINGS FOR RMA4   '/
      DATA BANRM4(2)/'ENGLISH UNITS APPLIED IN THESE RESULTS  '/
      DATA BANRM4(3)/'RMA4 VERSION 3.00+          1-D AND 2-D '/
      DATA BANRM4(4)/'CAPABILITY.   LAST MOD DATE  04-01-1990 '/
      DATA BANRM4(5)/'        Run through SLICE               '/
C
      DATA EMPTY(1) /'........................................'/ 
      DATA EMPTY(2) /'........................................'/ 
      DATA EMPTY(3) /'........................................'/ 
      EMPTY(4)(1:40)  = BANSLC(1)(1:40) 
      EMPTY(5)(1:40)  = BANSLC(2)(1:40)

      IPUTIT = 0
      ITRACE = 1
      IWANT_ALL = 0                             
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
      WRITE(*,20)  BANSLC(1), BANSLC(2)
   20 FORMAT(1X,A,/,1X,A,/,
     *   ' --> Beware REAL*8 needed for <= 32 word length',//,
     *   ' --> Echo of program activities on file=slice_sv21.txt',/)
      OPEN(99,FILE='slice_sv21.txt',FORM='FORMATTED',STATUS='UNKNOWN')
      WRITE(99,20) BANSLC(1), BANSLC(2)
C
      IDOSAL = 0    
      PREFIX1(1:20)    = '                   '
      PREFIX2(1:20)    = '                   '
C
   30 WRITE (*,*) ' Enter the rma10 output 3D geometry file name '
      READ (*,'(A)',END = 3000) INFILE
C  
      INQUIRE(FILE=INFILE,EXIST=HERE)
      IF ( .NOT. HERE) THEN
        WRITE (*,*) ' You entered a non-existant file>>',INFILE
        GO TO 30          
      END IF 
      WRITE (99,32) INFILE
   32 FORMAT(A50, ' 3dgeo')
      OPEN(LUIN1,FILE=INFILE,FORM='UNFORMATTED',STATUS='OLD')
C 
   40 WRITE (*,*) ' Enter the rma10 output solution file name '
      READ (*,'(A)',END = 3000) INFILE
C 
      INQUIRE(FILE=INFILE,EXIST=HERE)
      IF ( .NOT. HERE) THEN
        WRITE (*,*) ' You entered a non-existant RMA10 file>>',INFILE
        GO TO 40
      END IF 
      WRITE (99,42) INFILE
   42 FORMAT(A50, ' 3dsol')
      OPEN(LUIN2,FILE=INFILE,FORM='UNFORMATTED',STATUS='OLD')
C
   50 WRITE(*,60)
   60 FORMAT(/,' Enter the number of time steps to retrieve.',
     *   /,'       HINT: time window allows more I/O control.'
     *   /,'         Put  0          to specify all time steps.'
     *   /,'         Put  positive # to specify only n time step(s)'
     *   /,'         Put  negative # to specify a time window.')
      READ (*,*,END=50) NSTEPS
      WRITE (99,62) NSTEPS
   62 FORMAT(I8, ' nsteps')
                                            
      IF (NSTEPS .GT. 0) THEN
C ...    specified time steps      
         WRITE (*,70) NSTEPS
   70    FORMAT(' Enter ',I5,' time values to retrieve (decimal hrs)'
     *                ,/,12X,' enter 99999 to retrieve last step.')
         READ (*,*) (TIME2GET(I), I = 1, NSTEPS)
         NSAVE = 1
         WRITE (99,72) (TIME2GET(I), I = 1, NSTEPS)
   72    FORMAT(5F12.6, ' hours to get')

      ELSE IF (NSTEPS .LT.0) THEN
C ...    time window                    
   80    WRITE(*,*) ' Enter the minimum and maximum time values'
         READ (*,*,END=80) TMIN, TMAX
         WRITE (*,*) ' Enter the save interval n '
         WRITE (*,*) ' (i.e. save every nth step within time interval)'
         READ (*,*) NSAVE
         NSAVE = ABS(NSAVE)
         WRITE (99,82) TMIN, TMAX, NSAVE
   82    FORMAT(2F12.4 ' tmin and tmax time window',/,I8, ' nsave')
   90    WRITE(*,*) 'Enter switch to retrieve only hourly values'
         WRITE(*,*) '      Put  0 to de-activate the switch' 
         WRITE(*,*) '      Put  1 to get only hourly solutions' 
         HOURLY = 0.0
         CALL DCODE(HOURLY,HOURLY)
         WRITE (99,92) HOURLY
   92    FORMAT(F8.1,' hourly modulo switch')

      ELSE IF (NSTEPS.EQ.0) THEN
         WRITE (*,*) ' ... All time steps will be retrieved ' 
         IWANT_ALL = 1
         NSTEPS = -1
         NSAVE = 1
         TMIN = -99.
         TMAX = 1E+6
         WRITE (99,93) IWANT_ALL, NSTEPS, NSAVE, TMIN, TMAX
   93    FORMAT( ' Auto setting to process all time steps',/,
     *   'want_all  nsteps   nsave           tmin           tmax',/,
     *   3I8, 2F15.4)
      END IF

C      WRITE (*,*) ' Enter the number of degrees of freedom (DEF=6)'
C      READ (*,'(BN,I20)') NDF2DO
C      IF (NDF2DO .LE. 0)  NDF2DO = 6
      NDF2DO = 6
      WRITE (99,94) NDF2DO
   94 FORMAT(I8,' ndf2do')
C 
C      WRITE (*,*) ' Want to run a check on any of the deg of freedom?'
C      READ (*,'(A)') REPLY
C      IF (REPLY .EQ. 'n' .OR. REPLY .EQ. 'N') GO TO 100  
      GO TO 100     
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
      IF (CHECKS .EQ. 'n' .OR. CHECKS .EQ. 'N') GO TO 100        
        WRITE(*,*) ' Enter the maximum expected concentration (DEF=35).'
        IDEBUG = 1
        CHECKS = 'Y'
        SLIMIT = 35.
        CALL DCODE (SLIMIT, SLIMIT)

C  100 WRITE (*,*) ' Enter the base elevation (DEF = 100 FEET)'
C      ELEV0 = 100.0
 100  CONTINUE
      ELEV0 = 0.0001
C      CALL DCODE(ELEV0, ELEV0)
C GLB-LDR enter request to show shallow elements as dry
      DRTHSH = 0.0
      WRITE (*,*) 'Want to show elements with depths less than'
      WRITE (*,*) 'a given threshold as if they were dry ?  (DEF=NO)'
      READ (*,'(A)') REPLY
      IF (REPLY .EQ. 'y' .OR. REPLY .EQ. 'Y') THEN
        WRITE(*,*) 'Enter the threshold depth'
        READ(*,*) DRTHSH
      END IF
C GLB-LDR end addition
      WRITE (*,*) ' Enter the prefix for new "2D" HYDRO OUTPUT files ',
     *            '(DEF="SS_VEL")'
      READ (*,'(A)') PREFIX1
      IF (PREFIX1(1:3) .EQ. '   ') PREFIX1 = 'SS_VEL'
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
        ICK = 0
        WRITE (*,*) ' Enter a 1 to slice for salinity'
        WRITE (*,*) '         2 to slice for temperature'
        WRITE (*,*) '         3 to slice for sediment'
        WRITE (*,*) '         4 to slice for delbed (RMA10-SED only)'
        WRITE (*,*) '         5 to slice for bed shear (RMA10-SED only)'

        READ  (*,*) ICK
        IF (ICK .LT. 1 .OR. ICK .GT. 5) ICK = 1
        IF (ICK .EQ. 1) WRITE(*,*) 'Slicing for SALINITY'
        IF (ICK .EQ. 2) WRITE(*,*) 'Slicing for TEMPERATURE'
        IF (ICK .EQ. 3) WRITE(*,*) 'Slicing for SEDIMENT'
        IF (ICK .EQ. 4) WRITE(*,*) 'Slicing for DELBED'
        IF (ICK .EQ. 5) WRITE(*,*) 'Slicing for BED SHEAR'
        WRITE (*,*)
        WRITE (*,*) ' Enter the prefix for "2D" CONSTITUENT OUTPUT ',
     *              'files  (DEF="SS_SAL")'
        READ (*,'(A)') PREFIX2
        IF (PREFIX2(1:3) .EQ. '   ') PREFIX2 = 'SS_SAL'
      ENDIF
C
C 
      NSLICE = 1
      ZSLICE(1) = 0.0
      PRINT *,' --> Will make one slice at depth = 0.0  (surface)'
C 
      IREF = 0
C      WRITE(*,*) ' Want to reference slices to a fixed elevation?'
C      READ (*,'(A)') REPLY
C      IF (REPLY .EQ. 'Y' .OR . REPLY .EQ. 'y' ) THEN
C        WRITE(*,*)  ' Enter the reference elev below which to slice' 
C        READ (*,*)  REFEL
C        IREF = 1
C      ENDIF
C 
C 
C  READ THE 3D GEOMETRY INFORMATION
C     NPTOT = TOTAL NUMBER OF NODES
C     NETOT = TOTAL NUMBER OF ELEMENTS
C        NP = NUMBER OF SURFACE NODES
C        NE = NUMBER OF SURFACE ELEMENTS
C 
      READ (LUIN1)  NPTOT, NETOT, NP, NE
      WRITE (*,140) NPTOT, NETOT, NP, NE
  140 FORMAT ( /, ' FOR THE INPUT RMA10 GEOMETRY FILE:', /,
     *            '     TOTAL NUMBER OF NODES      = ', I20,/,
     *            '     TOTAL NUMBER OF ELEMENTS   = ', I20,/, 
     *            '     NUMBER OF SURFACE NODES    = ', I20,/, 
     *            '     NUMBER OF SURFACE ELEMENTS = ', I20,// )
      IF (NPTOT.GT.MAXN)  PRINT *,' ==> Increase max node param'
      IF (NETOT.GT.MAXNE) PRINT *,' ==> Increase max elem param'
C 
      REWIND LUIN1
C 
      READ (LUIN1,ERR=4000) NPTOT, NETOT, NP, NE,
     *     ((CORD(J,K), SPECDUM, K=1,3), 
C GLB remove nx1dum
     *     ADUM, NXDUM, AO(J), NSDUM, J = 1, NPTOT), 
     *     (NDEP(J), NREF(J), J = 1, NP), 
     *     ((NOP(J,K),K=1,20), NCDUM, IMAT(J), THDUM,
     *     NXHDUM, J = 1, NETOT), 
     *     (WIDUM, J = 1, NPTOT)
C  
      PRINT *,' --> Finished 3D Geo read ... '
      IF (ITRACE.GE.1) THEN
      print *, ' --------------------------------------------'      
      print *, ' 3D Geometry check for node 1='
      print *, ' ndep(1)-nref(1)= ',ndep(1),nref(1)
      print *, ' node  ao(1)    cord-1    cord-2     cord-3'
      print *,   '1',  ao(1),cord(1,1),cord(1,2),cord(1,3)              
      print *, ' --------------------------------------------'
      print *, ' 3D Geometry check for node NP=', np
      print *, ' ndep(np)-nref(np)= ',ndep(np),nref(np)
      print *, ' node  ao(np)    cord-1    cord-2     cord-3'
      print *,   np,   ao(np),cord(np,1),cord(np,2),cord(np,3)              
      print *, ' --------------------------------------------'
      print *, ' 3D Geometry check for node nptot=', nptot
      print *, ' ndep(nptot)-nref(nptot)= ',ndep(nptot),nref(nptot)
      print *, ' node  ao()   cord-1    cord-2     cord-3'
      print *,   nptot, ao(nptot),
     *           cord(nptot,1),cord(nptot,2),cord(nptot,3)             
      print *, ' --------------------------------------------' 
      ENDIF    
C 
C 
C** OPEN EACH OUTPUT FILE & WRITE BANNERS & HEADERS IN STANDARD RMA2
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
  150   FORMAT(' --> Opening Unit =',i3,'  File Name=',A )
C 
C-...   Process INTEGER type CHARACTER DMS BANNERS on Fake HYDRO 
        CALL PUT_BANNER (NE, NP, I, 2)          
C
C-      ------------------
        IF (IDOSAL.EQ.0) GO TO 200
        II = I + 10
C ...   SALINITY 2D LOGICAL UNIT NUMBERS TO OPEN (RMA4 LOOK=A=LIKE)
        OUTFILS(I) = PREFIX2(1:TRMLEN(PREFIX2)) // '_' // 
     *       CSLICE(1:IL) // '.sol'
        write(*,150) ii, outfils(i)
        OPEN(II,FILE=OUTFILS(I),FORM='UNFORMATTED',STATUS='UNKNOWN')
C
C-...   Process INTEGER type CHARACTER DMS BANNERS on Fake RMA4           
        CALL PUT_BANNER (NE, NP, II, 4) 
C-      ------------------ 
  200 CONTINUE
C 
C 
C ... NOW READ EACH TIME STEP IN THE INPUT DATA FILE, SLICE OUT A
C ...      HORIZONAL SECTION, & WRITE IT TO THE OUTPUT FILE
C 
C                             -------------------------- Time Step Loop
      NTIMES = 0
      DO 900 ITIME = 1, 999999
C
        ILOOP = ITIME
        IF (IEOF.GE.1 ) GO TO 1000
        NDF = 6
        NDFP1 = NDF + 1
C ...   RMA10-WES binary solution read
        READ (LUIN2,END=850) TIMERD, NP1, NDF, NE1,
     &       NDFS,(IRESAV(K),K=1,NDFS),
     &       ((XVEL(K,J),J = 1, IRESAV(K)),K=1,NDF),
     &       (WSEL(J),J = 1, IRESAV(3)),
     &       (IMAT(J), J = 1, NE1), (NDRY(J), J = 1, NP1),
     &       (DELBED(J),J=1,IRESAV(NDFP1)),(BSHR(J),J=1,IRESAV(NDFS)),
     &       (VVEL(J), J = 1, NP1), (DUM,
     &       J = 1, NE1)
C
        WRITE(*,210) NDF, TIMERD
  210   FORMAT(' --> Number of Degrees of Freedom in the'
     *         ' solution file =', I2, ' Time read=',F12.5)
        IF (NDF2DO .GT. NDF) THEN
            WRITE(*,*)' --> User ERROR .. cannot request to do more'
            WRITE(*,*)'     degrees of freedom than are available'
            WRITE(*,*)'     Hence NDF will be =',NDF
            NDF2DO = NDF
        ENDIF
        IF( NDF .NE. NDF2DO) THEN
            WRITE(*,*) ' --> User has over-ridden NDF to be=',NDF2DO
            NDF = NDF2DO
        ENDIF
C
        TIME = TIMERD
         
        IWRITE = 0
        IF (NSTEPS.GT.0) THEN
C ...       the user requested a discrete set of time steps
            DO 220 J = 1, NSTEPS
               IF (ABS(TIMERD - TIME2GET(J)).LT..001) IWRITE = 2
  220       CONTINUE
        ELSE
C ...       NSTEPS is negative, means a time window was requested
            IF (TIMERD .GT. TMAX) GO TO 1000
            IF (TMIN.LE.TIMERD .AND. TMAX.GE.TIMERD) IWRITE = 1

            IF (NSAVE.GE.1 .AND. IWRITE.EQ.1) THEN
C ...          NSAVE = the save interval (every 2nd or whatever)
               IF (IPUTIT.LE.0) THEN
C ...             get 1st time step in the window then nsaveth
                  IWRITE = IWRITE + 1
                  IF (ITRACE.GE.1) 
     *            WRITE(99,*) ' -> iputit=',IPUTIT,' iwrite=',IWRITE
                  INEXT = ILOOP + NSAVE
               ELSE
C ...             get every nsaveth time step
                  IF (ILOOP.NE.INEXT) THEN
                      IF (ITRACE.GE.1) 
     *                WRITE(99,222) ILOOP, INEXT, IWRITE
  222                 FORMAT(' -> iloop=',I5,' inext=',I5,' iwrite=',I3)
                      GO TO 230
                  ELSE
                      IWRITE = IWRITE + 1
                      IF (ITRACE.GE.1) 
     *                WRITE(99,222) ILOOP, INEXT, IWRITE
                      INEXT = ILOOP + NSAVE
                  ENDIF
               ENDIF
            ENDIF

            IF (HOURLY.GT.0. .AND. IWRITE.EQ.2) THEN
               IWRITE = IWRITE - 1   
C ...          User wants to get all even hours within time window
               ITEST = mod(int(TIMERD*10.),10)
               IF (ITEST .EQ. 0) THEN
                   IWRITE = IWRITE + 1
               ENDIF
               IF (ITRACE.GE.1) WRITE(99,224) ITEST, IWRITE, INEXT
  224          FORMAT(' -> itest=',I3,' iwrite=',I3,' inext=',I5)
            ENDIF
        ENDIF

  230   CONTINUE 

        IF (ITRACE.GE.1) WRITE(99,232) TMIN, TIMERD, TMAX, 
     *                   NSTEPS, NSAVE, IPUTIT, ILOOP, 
     *                   INEXT, HOURLY, ITEST, IWRITE
  232   FORMAT(/5X,F12.3,'<=?',F12.3,'<=?',F12.3,/,5X,'nsteps=',I4,
     *   ' nsave=',I5,' iputit=',I5,' iloop=',I5,' inext=',I5,/,
     *   5X,'hourly toggle=',F3.1,' itest modulo=',I3,' iwrite=',I3,/)

        IF (IWRITE.GE.2) THEN
            WRITE(*,*) ' Process this RMA10 record. Hour=',TIME
            IPUTIT = IPUTIT + 1
        ELSE 
            WRITE(*,*) ' ...Skip this RMA10 record. Hour=',TIME
            GO TO 900
        END IF 

C       ========================= 
C 
C ...   DO A MAX VELOCITY/SALINITY CHECK, IF DESIRED
C 
  240   IF (IDEBUG.GT.0 ) THEN
            WRITE(*,*)' --> Read RMA-10 record ... time= ', TIMERD
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
            WRITE(*,*)' Check on Min and Max SALINITY at time= ',TIME
            WRITE(*,*)' Minimum occurs at node=',nodmin,'  was=',SALMIN
            WRITE(*,*)' Maximum occurs at node=',nodmax,'  was=',SALMAX
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
            WRITE(*,*)' Check Min and Max VELOCITY at time = ',TIME
            WRITE(*,*)' Minimum occurs at node=',nodmin,' was=',VELMIN
            WRITE(*,*)' Maximum occurs at node=',nodmax,' was=',VELMAX
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
  530   WRITE (*,*) ' Working on time step=', ITIME, ' TIME =', TIME
        DO 600 NSURF = 1, NP
C          WSEL(NSURF) = AO(NSURF) + XVEL(3,NSURF)
          VEL(3,NSURF) = XVEL(3,NSURF)
          IF (NREF(NSURF) .GT. 0) THEN
            NBOT = NREF(NSURF) + NDEP(NSURF) - 1
            ZWHOLE = ELEV0 - CORD(NBOT,3)
            XVEL(3,NSURF) = 0.0
            DO 570 IZ = 2, NDEP(NSURF)
              NODE = NREF(NSURF) + IZ - 1
              ZFRACT = (ELEV0 - CORD(NODE,3)) / ZWHOLE
              XVEL(3,NODE) = ZFRACT * XVEL(3,NODE)
  570       CONTINUE
          END IF 
  600   CONTINUE
C 
        NTIMES = NTIMES + 1
C 
        DO 700 ISLICE = 1, NSLICE
          CALL surface (ISLICE, IDEBUG, SLIMIT, VLIMIT)
C GLB-LDR add this to make elements where d < drthsh at
C GLB-LDR every node appear dry
          DO K = 1, NE
            IDELE = 1
            IMAT(K)=IABS(IMAT(K))
            DO J = 1, 20 
              IF (NOP(K,J) .NE. 0) THEN
                IF (VEL(3,NOP(K,J)) .GT. DRTHSH) THEN
                  IDELE = 0
                END IF 
              END IF
            END DO 
            IF (IDELE .EQ. 1) THEN
              IMAT(K)=-IABS(IMAT(K)) 
            END IF
          END DO 
C GLB-LDR end addition
C ...     RMA2 LOOK-A-LIKE WRITE 
          WRITE (ISLICE) TIME, NP, 
     *                  ((VEL(J,K),J=1,3), K = 1, NP), 
     *                  (NDRY(K), K = 1, NP), 
     *                  NE, (IMAT(K), K = 1, NE), 
     *                  (WSEL(K),K=1, NP)
          WRITE(*,620)  TIME, ISLICE, ZSLICE(ISLICE)
          WRITE(99,620) TIME, ISLICE, ZSLICE(ISLICE)
  620     FORMAT(' ** WRITE TIME =',F12.8,'  HYDRO I-O UNIT   =',I3,
     *           ' SLICE DEPTH=',F7.3)
          IF (IDOSAL.EQ.0) GO TO 700
          IISLICE = ISLICE + 10
          NQAL = 1
C ...     RMA4 LOOK-A-LIKE-WRITE
          IF (ICK .LE. 3) THEN
              WRITE (IISLICE) TIME, NQAL, NP,
     *        (VEL(ICK+3,K),K=1,NP),NE,(IMAT(K),K=1,NE)
          ELSE IF (ICK .EQ. 4) THEN
              WRITE (IISLICE) TIME, NQAL, NP,
     *        (DELBED(K),K=1,NP),NE,(IMAT(K),K=1,NE)
          ELSE IF (ICK .EQ. 5) THEN
              WRITE (IISLICE) TIME, NQAL, NP,
     *        (BSHR(K),K=1,NP),NE,(IMAT(K),K=1,NE)
          END IF
          WRITE(*,640)  TIME, IISLICE, ZSLICE(ISLICE)
          WRITE(99,640) TIME, IISLICE, ZSLICE(ISLICE)
  640     FORMAT('    WRITE TIME =',F12.8,'  SALINITY I-O UNIT=',I3,
     *           ' SLICE DEPTH=',F7.3)          
  700   CONTINUE

      PRINT *,' ***** WROTE SLICED RESULT TO RMA4 Look-a-like'


      IF (IWANT_ALL.EQ.1)  GO to 900 

      GO TO 900
C
  850 IF (ABS(TIME2GET(1)-99999.).LT. .01) THEN
          TIME2GET(1) = TIME
          WRITE(*,*) ' Found the last record ... time (hrs)=',TIME
          WRITE(*,*) '                           time step =',ILOOP
          IEOF = IEOF + 1
          GO TO 240
      ELSE
          WRITE(*,*) ' END-OF-FILE Hit. Last hour read = ',TIME
          WRITE(*,*) '                       time step =',ILOOP
          GO TO 1000
      END IF

C-
  900 CONTINUE
C 
C 
 1000 CLOSE (LUIN1)
      CLOSE (LUIN2)
      CLOSE (99)
      WRITE(*,*) ' '
      WRITE(*,*) ' ..... Close ALL active files .....'
      DO 2000 ISLICE = 1, NSLICE
         CLOSE(ISLICE)
 2000 CONTINUE
C 
      WRITE(*,*)  ' --> Finished'
      WRITE (*,*) ' --> For this input file, ', NTIMES,
     *            ' Time Step(s) processed'
 3000 CONTINUE
      GO TO 5000
C 
 4000 PRINT *,' ***************** Warning ************************'
      PRINT *,' UNEXPECTED E-O-F or ERROR on RMA10 Geometry Record'

 5000 END
C 
C********************************************************************
C 
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
      PARAMETER (MAXNE = 50000, MAXN = 100000, MAXNDF = 6,
     *           ITMAX=500, MAXDEP = 10, LUIN1 = 55, LUIN2 = 56)
C
      COMMON  /BLKIN/ NP, NE, NDF, NDF2DO, ZSLICE(MAXDEP),
     *                NOP(MAXNE,20), NDEP(MAXN), NREF(MAXN), 
     *                CORD(MAXN,3),  XVEL(MAXNDF,MAXN),AO(MAXN),
     *                DELBED(MAXN),BSHR(MAXN)
C
      COMMON  /BLKOUT/VEL(MAXNDF,MAXN), 
     *                VVEL(MAXN), WSEL(MAXN), ZSEL(MAXN),
     *                NDRY(MAXN), IMAT(MAXNE)

      COMMON  /REF/ IREF,REFEL

      COMMON  /BUG/ ITRACE 

      DIMENSION NOPSUR(8)
C ... REAL star 8 needed for HEC version of RMA10 solution files
C     REAL*8  AO
C 
      IF (ITRACE.GE.1) PRINT *,' --> routine SLICE'
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
C
C    THE FOLLOWING LINE MAKES THE SLICE A SCONCH ABOVE THE BOTTOM
C            JOE LETTER  -  JULY 93
C
        ZZZSLICE = XVEL(3,I) - 1.
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
            IF (ZZZSLICE .LT. XVEL(3,NODES)) THEN
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
          IF (ZZZSLICE .GE. XVEL(3,NDEEP)) THEN
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
      IF (STRING(1:7) .EQ. 'NO MORE') THEN
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
C
C
C 
C********************************************************************
C 
      SUBROUTINE surface (ISLICE, IDEBUG, SLIMIT, VLIMIT)
      SAVE
C 
C  THIS ROUTINE WILL TAKE THE DATA FROM AN RMA10 RUN & RETURN
C  A HORIZONTAL SLICE (AT AN DEPTH = surface)
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
      PARAMETER (MAXNE = 50000, MAXN = 100000, MAXNDF = 6,
     *           ITMAX=500, MAXDEP = 10, LUIN1 = 55, LUIN2 = 56)
C
      COMMON  /BLKIN/ NP, NE, NDF, NDF2DO, ZSLICE(MAXDEP),
     *                NOP(MAXNE,20), NDEP(MAXN), NREF(MAXN), 
     *                CORD(MAXN,3),  XVEL(MAXNDF,MAXN),AO(MAXN),
     *                DELBED(MAXN),BSHR(MAXN)
C
      COMMON  /BLKOUT/VEL(MAXNDF,MAXN), 
     *                VVEL(MAXN), WSEL(MAXN), ZSEL(MAXN),
     *                NDRY(MAXN), IMAT(MAXNE)

      COMMON  /REF/ IREF,REFEL

      COMMON  /BUG/ ITRACE 

      DIMENSION NOPSUR(8)
C ... REAL star 8 needed for HEC version of RMA10 solution files
C      REAL*8  AO      
C 
C*****************************************************************
C 
      IF (ITRACE.GE.1) PRINT *,' --> routine surface'

      NPS      = NP
      NOFIND   = 0
C 
      DO 260 I = 1, NPS
C         VEL(3,I) = WSEL(I) - AO(I)
         IF (NREF(I) .EQ. 0) THEN
C 
C ...      FIRST FOR 2-D OR 1-D NODES
C 
C          PRINT *,'     ... THIS IS A 1-D OR 2-D(HORIZ) NODE'
           DO 180 NDFF = 1, NDF
              IF(NDFF.NE.3) THEN
                 VEL(NDFF,I) = XVEL(NDFF,I)
              ENDIF
              NDRY(I) = 1
  180      CONTINUE
           GO TO 260
        ELSE 
C 
C ...      NEXT, 3-D DEPTHS
C 
          
C          PRINT *,' THIS IS A 3-D NODE'
C          PRINT *,'     DEEPEST DEPTH = ',CORD(NDEEP,3)
C          PRINT *,'     NUMBER OF NODES IN DEPTH =', NDEP(NODES)
C 
           NDRY(I) = 1
C 
C 
          DO 250 NDFF = 1, NDF
             IF (NDFF .NE. 3) THEN
               VEL(NDFF,I) = XVEL(NDFF,i)  
             END IF 
  250     CONTINUE
C
       END IF 
C 
C ... DO THE NEXT NODE POINT
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
C ----------------
      SUBROUTINE PUT_BANNER (NEGEO, NPGEO, LUOUT, IKIND)
C  
C ... LUOUT = Logical unit output #
C ... IKIND = 2 for RMA2    =4 for RMA4 file
C
      COMMON  /BUG/ ITRACE 
     
C ... needed for banners
      COMMON  /DMSCHR/ HEADER, IDMS(15), BANSLC(2),
     *                 BANGFG(5), BANRM2(5), BANRM4(5), EMPTY(5)
      COMMON  /DMSREC/ IREC(40),FREC(40), IPACKB(1200), IPACKT(77)
      CHARACTER        HEADER*77,  IDMS*80, BANSLC*80,
     *                 BANGFG*80, BANRM2*80, BANRM4*80, EMPTY*80
      CHARACTER  IFAKE2(15)*80
C
      IF (ITRACE.GE.1) PRINT *,' --> routine put_banner'

      IF ( IKIND.EQ.2 ) THEN      
C-...      Process INTEGER type CHARACTER DMS BANNERS on Fake RMA2           
           IREC(1) = 435
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
      COMMON  /BUG/ ITRACE
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
      IF( ITRACE.GE.1 ) PRINT *,' --> routine convrt (ISIZE=',ISIZE,
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
          PRINT *,' --> Value of ISWTCH invalid in CONVRT'
      ENDIF
      RETURN
      END
C***************************************************************


