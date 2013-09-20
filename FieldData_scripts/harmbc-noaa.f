       PROGRAM HARMBC
C ... PURPOSE: TO CONVERT TIDAL HARMONIC EPOCHS INTO TIMESERIES
C ...          OF HARMONIC BC ALONG A SERIES OF NODES ALONG A BOUNDARY
C
      INCLUDE 'harmbc-noaa.inc'
C
      CHARACTER CARD*3,BANNER(15)*80,FNAME*80,JREC(80)*1,ITYP*3,IDUM*80
      INTEGER*4 NE,NP,NBX,NBOUND,NCON,NT,IDUH,IGO,NSKIP,NOP,NMAX,NODE
      CHARACTER REPLY*3,HARM*65
c
      DIMENSION  BQ(50),IBQ(50),BH(50),IBH(50),TFUNC(MNN),FUNC(MNN,2)
	DIMENSION  AP(NH), UV(NH), HARM(NH), ZF(NMX)
	DIMENSION  AF1(NBND),AF2(NBND),EPF1(NBND),EPF2(NBND)
C
      DATA ITAG/MNN*0/
	DATA HARM/'    M2',
     *'    S2',
     *'    N2',
     *'    K1',
     *'    M4',
     *'    O1',
     *'    M6',
     *'   MK3',
     *'    S4',
     *'   MN4',
     *'   Nu2',
     *'    S6',
     *'   Mu2',
     *'   2N2',
     *'   OO1',
     *'  Lam2',
     *'    S1',
     *'    M1',
     *'    J1',
     *'    Mm',
     *'   Ssa',
     *'    Sa',
     *'   Msf',
     *'    Mf',
     *'   Rho',
     *'    Q1',
     *'    T2',
     *'    R2',
     *'   2Q1',
     *'    P1',
     *'  2SM2',
     *'    M3',
     *'    L2',
     *'  2MK3',
     *'    K2',
     *'    M8',
     *'   MS4',
     *'Sigma1',
     *'   MP1',
     *'    X1'    /

      DATA  TPER/ 12.42060122, 12, 12.65834824, 23.93446966,
     *  6.21030061, 25.81934166, 4.140200402, 8.177139947,
     *            6, 6.269173909, 12.62600441,          4,
     *  12.87175763, 12.90537448, 22.3060742, 12.22177415,
     *           24, 24.83324836, 23.09847677, 661.3092049,
     *  4382.905209, 8765.82109, 354.3670522, 327.8589689,
     *   26.7230533, 26.86835667, 12.0164492, 11.98359578,
     *  28.00622255, 24.06589016, 11.60695156, 8.280400814,
     *   12.19162021, 8.38630297, 11.96723479,3.10515031,
     *   6.103339279,  1.,  1.,  1.  /

C
C ... READ ORIGINAL GE CARD LIST (LAST OCCURANCE SAVED)
C
      IFLAG =0
      NBX = 0
      TWOPI = 8.*ATAN(1.)
c
      PRINT *,'ENTER THE GEOMETRY INPUT FILE NAME FOR MESH'
      PRINT *,'(A FORMATTED SMS GEOMETRY INPUT FILE NAME):'
      READ(*,'(A)') FNAME
      OPEN(10,FILE=FNAME,FORM='FORMATTED',STATUS='OLD')
	PRINT *,' GEMETRY FILE SUCCESSFULLY OPENED'
	REWIND 10
C
      CALL GEOREAD
      PRINT *,'TABS-MD GEOMETRY READ: ',NE,' ELEMENTS AND ',NP,' NODES'

	PRINT *,' Will the output BC file format be:'
	PRINT *,'  Enter (1) for RMA2 or (2) for TABBS-MDS '
	READ *, IMOUT
C
      PRINT *,'ENTER THE FILE NAME FOR BC DATA OUTPUT FROM THIS CODE'
      READ(*,'(A)') FNAME
      OPEN(4,FILE=FNAME,FORM='FORMATTED',STATUS='UNKNOWN')
C
 1    PRINT *,'ENTER THE NUMBER OF TIDAL BOUNDARIES TO BE DEFINED:'
      READ *,NBOUND
C
      IF(NBOUND.GT.NBND) THEN
         PRINT *,'ERROR -  SORRY, BUT THIS CODE IS DIMENSIONED TO'
         PRINT *,' HANDLE ONLY  5  SEPARATE BOUNDARIES '
         GOTO 1
      ENDIF
C
      PRINT *,'DO YOU WANT TO INCLUDE A FUNCTIONAL COMPONENT'
      PRINT *,'(SOMETHING OTHER THAN A HARMONIC COMPONENT)??'
      READ(*,'(A)')  REPLY
      IF (REPLY .EQ. 'YES'   .OR.
     *    REPLY .EQ. 'yes'   .OR.
     *    REPLY .EQ. 'y  '   .OR.
     *    REPLY .EQ. 'Y  '   .OR.
     *    REPLY .EQ. 'Yes'   .OR.
     *    REPLY .EQ. 'Si ' ) THEN
         PRINT *,'OK, ENTER THE FILE WITH THE FUNCTIONAL TIME SERIES'
         PRINT *,'NOTE;  WE ARE ASSUMING THAT THE TIME STEP OF THE'
         PRINT *,'   FUNCTION MATCHES THE DESIRED OUTPUT TIME STEP!!'
         READ(*,'(A)') FNAME
         OPEN(33,FILE=FNAME,FORM='FORMATTED',STATUS='OLD')
         NFUNC = 0
         IFLAG = 1
         DO 46 I=1,MNN
           READ(33,*,END=47) TFUNC(I),FUNC(I,1),FUNC(I,2)
           NFUNC = I
   46    CONTINUE
   47    LML = 1 
         PRINT *,'READ IN ',NFUNC,' DATA VALUES'
	   DO I=1,NFUNC
           PRINT 48,TFUNC(I),(FUNC(I,K),K=1,2)
   48      FORMAT( 3F15.3)
         END DO
      ENDIF
C
C   CHECK TO SEE IF WE ARE USING THE STANDARD NOAA 37 CONSTITUENTS
C
      PRINT *,' ARE WE PROCESING THE STANDARD NOAA 37 CONSTITUENTS?'
	PRINT *,'  ENTER A 1 FOR YES,  0 FOR NO:'
	READ *, NOAA
	IF( NOAA .EQ. 1 ) THEN
	  NCON = 37
	  PRINT *,'DO YOU WANT TO ACCOUNT FOR THE ACTUAL YEAR K-PRIME?'
	  PRINT *,'  (THAT IS THE YEARLY VARIATION IN HARMIONICS)'
	  PRINT *,' ENTER A 1 FOR YES OR 0 FOR NO:'
	  READ *, KPRIME
	  IF( KPRIME .EQ. 1 ) THEN
          PRINT *,' ENTER THE FILENAME CONTAINING THE '
		PRINT *,' K-PRIME NODAL INFO FOR YOUR YEAR OF INTEREST:'
          READ(*,'(A)') FNAME
          OPEN(34,FILE=FNAME,FORM='FORMATTED',STATUS='OLD')
          READ(34,3400)  IYR, ILEAP, II, ((AP(K),UV(K)),K=1,8)
 3400     FORMAT(I4,2I2,8(F4.3,F4.1) )  
	    IYRO = IYR
          READ(34,3400)  IYR, ILEAP, II, ((AP(K),UV(K)),K=9,16)
	     IF( IYR .NE. IYRO) STOP ' BAD K-PRIME FILE'
	     IYRO = IYR
          READ(34,3400)  IYR, ILEAP, II, ((AP(K),UV(K)),K=17,24)
	     IF( IYR .NE. IYRO) STOP ' BAD K-PRIME FILE'
	     IYRO = IYR
          READ(34,3400)  IYR, ILEAP, II, ((AP(K),UV(K)),K=25,32)
	     IF( IYR .NE. IYRO) STOP ' BAD K-PRIME FILE'
	     IYRO = IYR
          READ(34,3400)  IYR, ILEAP, II, ((AP(K),UV(K)),K=33,37)
	     IF( IYR .NE. IYRO) STOP ' BAD K-PRIME FILE'

         Print *,' Summary of the K-prime values just read'
         Print *,'   constituent   K-prime      UV   '
	   Print *,'     No.          Value   (degrees)'
	   do k=1,37
           print 3402, k, ap(k), uv(k) 
 3402      format(i7,f15.3,f11.1)
         end do


          PRINT *,' ENTER THE OFFSET TIME TO BEGIN TIMESERIES IN ',IYR
	    PRINT *,' THAT IS HOURS AFTER 1 JAN ',IYR,' AT 0000 HRS'
	    READ *, TSTART
	  ELSE IF (KPRIME .EQ. 0) THEN
           LML = 1
	  ELSE
          PRINT *,' ERROR: INVALID RESPONSE: K-PRIME = ',KPRIME
      	STOP
	  END IF 

	  PRINT *,'  HARMONIC    K-PRIME     PHASE (uv)  '
	  DO K = 1,NH
	    PRINT 3401, HARM(K),  AP(K), UV(K)
 3401     FORMAT (3X,A6,F12.3,F12.1)
	  END DO 
          PRINT *,'HOW MANY BC TIMESTEPS WILL BE GENERATED'
          READ *,NT
          PRINT *,'...AND WHAT IS THE TIMESTEP TO BE (HOURS)??'
          READ *,TS
          PRINT *,'ENTER THE INTERVAL TO RAMP UP THE TIDAL AMPLITUDE???'
          READ *, TRAMP
          IF(TRAMP .LT. 0.00000000001 ) TRAMP = -1
          PRINT *,'WHAT IS THE DATUM FOR THE MEAN WATER SURFACE ABOUT'
          PRINT *,' WHICH TO VARY THE TIDAL HARMONICS???'
          READ *, ZDATUM


C
	ELSE IF(NOAA .EQ. 0 ) THEN

	
 2        PRINT *,'ENTER THE NUMBER OF HARMONIC CONSTITUENTS TO BE USED'
          PRINT *,' IN THE GENERATION OF THE BOUNDARY CONDITION:'
          READ *, NCON
C
          IF(NCON.GT.NH) THEN
            PRINT *,'ERROR -  SORRY, BUT THIS CODE IS DIMENSIONED TO'
            PRINT *,' HANDLE ONLY', NH,' HARMONIC CONSTITUENTS '
            GOTO 2
          ENDIF
C
          PRINT *,'HOW MANY BC TIMESTEPS WILL BE GENERATED'
          READ *,NT
          PRINT *,'...AND WHAT IS THE TIMESTEP TO BE (HOURS)??'
          READ *,TS
          PRINT *,'ENTER THE INTERVAL TO RAMP UP THE TIDAL AMPLITUDE???'
          READ *, TRAMP
          IF(TRAMP .LT. 0.00000000001 ) TRAMP = -1
          PRINT *,'WHAT IS THE DATUM FOR THE MEAN WATER SURFACE ABOUT'
          PRINT *,' WHICH TO VARY THE TIDAL HARMONICS???'
          READ *, ZDATUM
C
          IF(NCON .EQ. 0) GOTO 81
          PRINT *,'YOU HAVE SAID ',NCON,' CONSTIUENTS ARE TO BE USED.'
          PRINT *,'PROVIDE NOW THE PERIOD AND REFERENCE EPOCH FOR EACH'
          PRINT *,'CONSTITUENT. (THE ORDER WHICH YOU CHOOSE NOW MUST'
          PRINT *,'MATCH OTHER INPUT TO FOLLOW - TAKE NOTE)'
          PRINT *,' UNITS OF PERIOD ARE HOURS - EPOCHS ARE IN DEGREES'
          PRINT *,'    '
          PRINT *,' ENTER',NCON,' PERIODS AND REFENECE EPOCHS IN PAIRS:'
          READ *,(TPER(NN),REFEP(NN),NN=1,NCON)
C
C         READ(3,3333) (REFEP(NN),NN=1,NCON)
 3333     FORMAT(8F10.0)
      END IF
C
   81 LML =1
      IDUH = 0
      PRINT *,'WILL YOU DEFINE THE BOUNDARIES FOR TIDES BY:'
      PRINT *,'      1)   "GCL" STRINGS    or'
      PRINT *,'      2)   "GC" STRINGS'
      PRINT *,'   ENTER EITHER 1 OR 2 :'
  444 READ *, IGO
c
      IF(IGO.EQ.2) THEN
        PRINT *,'ENTER THE RMA-2 INPUT FILE WHICH CONTAINS THE '
        PRINT *,' BOUNDARY SEGMENTS, PRESCRIBED AS "GC" LINES :'
        PRINT *,'(THE ORDER OF THE SPECIFICATION OF THE LINES WILL HAVE'
        PRINT *,' TO MATCH THE ORDER OF OTHER STUFF TO FOLLOW):::'
        READ (*,'(A)') FNAME
        OPEN(11,FILE=FNAME,FORM='FORMATTED',STATUS='OLD')
C
C    THIS SEGMENT OF CODE READS THE GFGEN INPUT FILE AND FINDS THE GC
C    LINES FOR USE LATER
C  
        PRINT *,'HOW MANY "GC" LINES DO YOU WANT TO SKIP?'
        READ *,NSKIP
C
        IGC = 0
  111   CONTINUE
        READ(11,1111,END=887) ITYP,IDUM
 1111   format(a3,a80)
 1112   format(a80)
        IF(ITYP.NE.'GC ') GOTO 115
        WRITE(9,1112) IDUM
        IGC = 1
        GOTO 111
  115   IF(IGC.EQ.0) GOTO 111 
C-
  887   REWIND 9
        IB = 0
        NBB = nbound+NSKIP
        do 116 ibc= 1,NBB
          IB = IB+1
          NN=NMAX(IB)
  888     NN = NN + 1 
C      PRINT *, 'IB,NMAX,NN = ', IB,NMAX(IB),NN
          read(9,*,end=117) NN,(node(ib,k),k=1,nn)
C      PRINT *,'      NODE(ib,NN) = ',NODE(ib,NN)
          NMAX(IB) = NN
          IF(NSKIP.GT.0) IB=IB-1
          NSKIP = NSKIP-1
  116   continue
        goto 118
  117   continue
        print *,'ERROR - could not find',nbound,' GC lines in RMA2 file'
        stop
  118   continue
c
        CALL MIDSID
c
      ELSE IF(IGO.EQ.1) THEN
        CALL GOSTR
      ELSE
        IDUH = IDUH + 1
        IF(IDUH .GT.3) THEN
          PRINT *,'THREE TIMES IS THE CHARM!!! STOP !!!'
          STOP
        ENDIF
        PRINT *,'WRONG ANSWER -- TRY AGAIN -- ENTER 1 OR 2 :'
        GOTO 444
      ENDIF
      IF(IFLAG .EQ.1) THEN
        PRINT *,'ENTER THE FUNCTIONAL CONSTITUENT INFORMATION FOR EACH'
        PRINT *,'OF THE ',NBOUND,' BOUNDARIES IN PAIRS OF (AMP, TLAG) '
        PRINT *,'FOR THE FIRST AND THEN LAST NODES ON EACH BOUNDARY:'
        PRINT *,'      '
        PRINT *,' AMP(FIRST), TIMELAG(FIRST), AMP(LAST), TIMELAG(LAST)'
        PRINT *,'      ...'
        DO 50 I=1,NBOUND
C
C     
        PRINT *,'BOUNDARY NO. ', I,' INPUT DATA:'
        PRINT *,'ENTER FUNCTION DATA AS ABOVE FOR '
        PRINT *,'FUNCTIONAL TIME-SERIES'
C***************************************************************************
        READ *, AF1(I),EPF1(I),AF2(I),EPF2(I)
c
        NBX = NBX + NMAX(I)
C
        DO 45 K=1,NMAX(I)
        NFIX(I,K) = 200
   45   CONTINUE
        NFIX(I,1) = 1200
        NFIX(I,NMAX(I)) = 1200
   50   CONTINUE
c
         
      ENDIF
C

      IF(NOAA .EQ. 0) THEN
        PRINT *,'ENTER THE TIDAL CONSTITUENT DATA FOR EACH OF THE'
        PRINT *,NBOUND,' BOUNDARIES IN PAIRS OF (AMPLITUDE, EPOCH) FOR'
        PRINT *,'THE FIRST AND THEN LAST NODES ON EACH BOUNDARY:'
        PRINT *,'      '
        PRINT *,' AMP(FIRST), EPOCH(FIRST), AMP(LAST), EPOCH(LAST) '
        PRINT *,'      ...'
        PRINT *,'  ENTER THESE DATA; ONE LINE PER TIDAL CONSTITUENT'
C
        IF(IFLAG .EQ.1) NBX = 0
        DO 100 I=1,NBOUND
C
C     
          IF(NCON .EQ. 0 ) GOTO 99
          PRINT *,'BOUNDARY NO. ', I,' INPUT DATA:'
          PRINT *,'ENTER HARMONIC DATA AS ABOVE FOR '
          DO 98 J=1,NCON
            PRINT *,'TIDAL PERIOD OF ',TPER(J),' HOURS:'
            READ *, A1(I,J),EPOCH1(I,J),A2(I,J),EPOCH2(I,J)
   98     continue
   99     LML =1
c
          NBX = NBX + NMAX(I)
C
          DO 75 K=1,NMAX(I)
            NFIX(I,K) = 200
            NN = NODE(I,K)
            ITAG(NN) = ITAG(NN) + 1
   75     CONTINUE
          NFIX(I,1) = 1200
          IF( ITAG(NODE(I,1)) .GT. 1) NFIX(I,1) = 200
          NFIX(I,NMAX(I)) = 1200
          IF( ITAG(NODE(I,NMAX(I))) .GT. 1) NFIX(I,1) = 200
  100   CONTINUE

      END IF    

	IF( NOAA .EQ. 1 ) THEN
        PRINT *,' ENTER THE FILE NBAME WITH THE HARMONICS FOR'
	  PRINT *,'  THE FIRST AND LAST NODE ON THE BOUNDARY'
	  PRINT *,'  AMP1, EPOCH1,  AMP2,  EPOCH2 '
        READ(*,'(A)') FNAME
        OPEN(35,FILE=FNAME,FORM='FORMATTED',STATUS='UNKNOWN')
	  I=1
	  DO J=1,37
        READ(35,*) A1(I,J),EPOCH1(I,J),A2(I,J),EPOCH2(I,J)
        END DO
	END IF 
c
c
c
      NBQL = 0
      PRINT *,'WILL YOU SPECIFY BQL BOUNDARIES FOR THE SIMULATION?'
      READ(*,'(A)') CARD
      IF(CARD.EQ.'Y  '    .OR.
     *   CARD.EQ.'y  '    .OR.
     *   CARD.EQ.'YES'    .OR.
     *   CARD.EQ.'yes'    .OR.
     *   CARD.EQ.'Yes'    .OR.
     *   CARD.EQ.'yEs'    .OR.
     *   CARD.EQ.'yES'    .OR.
     *   CARD.EQ.'YES')  THEN
         PRINT *,'HOW MANY BQL BOUNDARIES WILL BE USED?'
        READ *,NBQL
        PRINT *,'ENTER THE ',NBQL,' BQL SPECIFICATIONS NOW '
        PRINT *,' AS  (LINE NUMBER,  DISCHARGE) PAIRS:'
        PRINT *,'THESE WILL BE CONSTANT OVER THE SIMULATION'
        READ *,(IBQ(K),BQ(K),K=1,NBQL)
      ENDIF
c
      DO 400 I=1,NBOUND
        N1 = NODE(I,1)
        N2 = NODE(I,NMAX(I))
        DX = CORD(N2,1) - CORD(N1,1)
        DY = CORD(N2,2) - CORD(N1,2)
        DSB = (DX*DX + DY*DY)**0.5
        print *,'first and last nodes of boundary ',I,' ARE ', N1,N2
        PRINT *,'DISTANCE ALONG BOUNDARY NO. ',I,' = ', DSB
C
        DO 300 K=1,NMAX(I)
          N = NODE(I,K)
          DX = CORD(N,1) - CORD(N1,1)
          DY = CORD(N,2) - CORD(N1,2)
          DS = (DX*DX + DY*DY)**0.5
          FACT = DS/DSB
C
          DO 200 J=1,NCON
            DELEP = EPOCH2(I,J) - EPOCH1(I,J)
            DELAMP = A2(I,J) - A1(I,J)
            EPOCH(I,J,K) = EPOCH1(I,J) + DELEP*FACT
            AMP(I,J,K) = A1(I,J) + FACT*DELAMP
  200     CONTINUE
C
  300   CONTINUE
C
  400 CONTINUE
C
C
      PRINT *,' THE INTERPOLATED BOUNDARY COREFICIENTS ARE:'
      PRINT *,'    NODE       AMPLITUDE       EPOCH/LAG'
      DO 679 KK=1,NCON
        PRINT *,'  '
C        IF(IFLAG .EQ. 1 .AND. KK .EQ. 1) THEN
C          PRINT *,'FOR FUNCTIONAL SERIES '
C        ELSE
          PRINT *,'FOR CONSTITUENT NUMBER ', KK
C        ENDIF
        DO 678 I=1,NBOUND
          DO 677 J=1,NMAX(I)
            PRINT *, NODE(I,J),AMP(I,kk,J),EPOCH(I,kk,J)
  677     CONTINUE
          PRINT *, ' '
  678   CONTINUE
  679 CONTINUE

C
      T= 0.
	NTT = 0
      DO 900 M=1,NT
      T = T + TS
	NTT = NTT + 1
      WRITE(4,4000) T
 4000 FORMAT('COMMENT          BOUNDARY CONDITIONS FOR TIME = ',f8.1)
C
      DO 500 IN = 1,MNN
      ITAG(IN) = 0
  500 CONTINUE
      DO 800 I=1,NBOUND
C
      N1 = NODE(I,1)
      N2 = NODE(I,NMAX(I))
      DX = CORD(N2,1) - CORD(N1,1)
      DY = CORD(N2,2) - CORD(N1,2)
      DSB = (DX*DX + DY*DY)**0.5


      DO 700 K=1,NMAX(I)
      Z = ZDATUM
      IF(IFLAG .EQ.1) THEN
c---------------------------------------------------------------------

        N = NODE(I,K)
        DX = CORD(N,1) - CORD(N1,1)
        DY = CORD(N,2) - CORD(N1,2)
        DS = (DX*DX + DY*DY)**0.5
        FACT = DS/DSB
	  ZF(K) = AF1(I) * FUNC(M,1) * (1-FACT) 
     *        + AF2(I) * FUNC(M,2) * FACT

        Z = Z + ZF(K)
C
c------------------------------------------------------------------------
      ENDIF
      IF(NCON .EQ. 0) GOTO 610
      TFACT = T/TRAMP
      IF( T .GT. TRAMP) TFACT = 1.
      DO 600 J=1,NCON
C      ARG = TWOPI*(T/TPER(J)-EPOCH(I,J,K)/360.-REFEP(J)/360.)
      ARG = TWOPI*((T+TSTART)/TPER(J)-EPOCH(I,J,K)/360.+UV(J)/360.)
      Z = Z + AMP(I,J,K) * cos(ARG) * TFACT * KPRIME * AP(J)
  600 CONTINUE
  610 LML =1
      ZBC(I,K,NTT) = Z
      XDUM = 0.
      YDUM = 0.
	idum1 = 1
	idum2 = 2
	idum0 = 0
	iduma = 1200
	idumb = 200
	salt = 33.0
      IF(ITAG(NODE(I,K)) .NE. 0) GOTO 700
c      WRITE(4,4001) NODE(I,K), NFIX(I,K), XDUM, YDUM, Z
      IF( IMOUT .EQ. 2) THEN
        WRITE(4,4001) NODE(I,K), idum0,idum0,idum2,idum2,idum0,idum0,
     *             XDUM, YDUM, Z, salt
	ELSE IF (IMOUT .EQ. 1) THEN
	  if(k .eq. 1  .or. k .eq. nmax(i) ) then
          WRITE(4,4003) NODE(I,K), iduma, XDUM, YDUM, Z, T
	  else
c	    kk = mod(k,2)
c          if(kk .ne. 0) WRITE(4,4003) NODE(I,K), idumb, XDUM, YDUM, Z, T
          WRITE(4,4003) NODE(I,K), idumb, XDUM, YDUM, Z, T
	  endif
	else
        print *,'error; imout must be either 1 or 2;  imout = ',imout
	  stop
	endif
c 4001 FORMAT('BCN',2I10,3F10.3)
 4001 FORMAT('BCN',i10,5x,6i2,4F10.3)
 4003 FORMAT('BCN',i10,5x,i6,3F10.3,5X,' TIME = ', F9.2)
      ITAG(NODE(I,K)) = 1
  700 CONTINUE
      IF(IFLAG .GT. 0) WRITE (21,2001) T, (ZF(K),K=1,NMAX(I))
C
  800 CONTINUE
C
      DO 850 I=1,NBQL
      WRITE(4,4002)  IBQ(I),BQ(I)
 4002 FORMAT('BQL',I10,F12.2)
  850 CONTINUE
C
      WRITE(4,4004)
 4004 FORMAT('END')
  900 CONTINUE
      T = 0
	DO I=1,NBOUND
      DO J=1,NT
	T = T + TS
      WRITE (20,2001)T,(ZBC(I,K,J),K=1,NMAX(I)) 
 2001 FORMAT(F9.2, 50 F 8.3 )
	END DO
	END DO
      END
c
c-------------------------------------------------------------------
c
      SUBROUTINE GEOREAD
C
      INCLUDE 'harmbc-noaa.inc'
C
      DIMENSION INT(200),REA(200)
      CHARACTER JREC(80)*1,IC1*2,IC3*1,CHA(200)*20
      INTEGER*4 NE,NP,NBX,NBOUND,NCON,NT,IDUH,IGO,NSKIP,NOP,NMAX,NODE
C
      ICNTE = 0
  250 READ(10,260,END=900) IC1,IC3,(JREC(I),I=1,80)
  260 FORMAT(A2,A1,80A1)
       I1 = 1
c      PRINT 260, IC1,IC3,(JREC(I),I=1,80)
      IF (IC1 .EQ. 'GE') THEN
C ...   READ ELEMENT CONNECTION TABLE   -GE CARD              (GE CARD)
        NWD = 10
        DO 730 I = 1, NWD
          INT(I) = 0
  730   CONTINUE
        REA(1) = 0.0
        CALL CRACK(I1, NWD, REA, INT, CHA, 'INTEGER  ', IERR)
        NWD1 = 1
        CALL CRACK(I1, NWD1, REA, INT, CHA, 'REAL     ', IERR)
        J = INT(1)
          LOW = 0
c          IELE(J) = J
          ICNTE = ICNTE + 1
          DO 760 I = 8, 1,  - 1
            NODER = INT(I+1)
            NOP(J,I) = NODER
            if(noder.gt.np2) np2 = noder
  760     CONTINUE
          IF (NWD .GT. 9) THEN
C ...       IMAT WAS READ ON THIS CARD
            IMAT(J) = INT(10)
C ...       WAS TH (ANGLE OF ROTATION FOR EDDY VIS) READ ON CARD?
            IF (NWD1 .GT. 0) TH(J) = REA(1)
          END IF 
C-
      ELSE IF (IC1 .EQ. 'GN' .AND. IC3 .EQ. 'N') THEN
C ...   NODE COORDINATE AND ELEVATION (JUST LIKE GFGEN)       (GNN CARD)
        INT(1) = 0
        DO 840 I = 1, NWD
          REA(I) = 0.0
  840   CONTINUE
        NWD = 1
        CALL CRACK(I1, NWD, REA, INT, CHA, 'INTEGER  ', IERR)
        NWD = 7
        CALL CRACK(I1, NWD, REA, INT, CHA, 'REAL     ', IERR)
        J = INT(1)
C        NODE(J) = J
          CORD(J,1) = REA(1)
          CORD(J,2) = REA(2)
          A0(J) = REA(3)
          WIDTH(J) = REA(4)
          SS1(J) = REA(5)
          SS2(J) = REA(6)
          WIDS(J) = REA(7)
c        IF(REA(4).NE.0.) NGWN(J) = J
        IF(J.GT.NP2) NP2 = J
C-
      ELSE IF (IC1 .EQ. 'GW') THEN
C ...   NODAL CROSS SECTIONAL WIDTHS                           (GW CARD)
        NWD = 4
        DO 980 I = 1, NWD
          REA(I) = 0.0
          INT(I) = 0
  980   CONTINUE
        NWD1 = 1
        CALL CRACK(I1, NWD1, REA, INT, CHA, 'INTEGER  ', IERR)
        NWD2 = 4
        CALL CRACK(I1, NWD2, REA, INT, CHA, 'REAL    ', IERR)
        NODER = INT(1)
          IF (IC3 .NE. ' ') THEN
C ...       INITIALIZE BY NODE                               
c            NGWN(NODER) = NODER
            WIDTH(NODER) = REA(1)
            ss1(NODER) = REA(2)
            ss2(NODER) = REA(3)
            WIDs(NODER) = REA(4)
          ELSE 
C ...       INITIALIZE ENTIRE RANGE OF VALUES                
            DO 1000 II = NODER, MNN
              NWID = NWID + 1
              WIDTH(II) = REA(1)
              ss1(II) = REA(2)
              ss2(II) = REA(3)
              WIDs(II) = REA(4)
 1000       CONTINUE
          END IF 
C-
        ELSE
         WRITE(20,2000) IC1,IC3,(JREC(II),II=1,80)
 2000  FORMAT(A2,A1,77A1)
        ENDIF
        GOTO 250
  900   NE = ICNTE
        np = np2
        RETURN
        END
c
c-------------------------------------------------------------------
c
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
      INTEGER*4 NE,NP,NBX,NBOUND,NCON,NT,IDUH,IGO,NSKIP,NOP,NMAX,NODE
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
C-
C-------------------------------------------------------------------------
C
      SUBROUTINE GOSTR
C
      INCLUDE 'harmbc-noaa.inc'
C
      CHARACTER CARD*3,BANNER(15)*80,FNAME*80,JREC(80)*1,ITYP*3,IDUM*80
      INTEGER*4 NE,NP,NBX,NBOUND,NCON,NT,IDUH,IGO,NSKIP,NOP,NMAX,NODE
C
      REWIND 9
C
      PRINT *,'WILL THE "GO" STRINGS BE INCLUDED IN THE GEOMETRY'
      PRINT *,'FILE THAT YOU DEFINED ABOVE OR WILL YOU PROVIDE ANOTHER'
      PRINT *,'FILE WITH THE "GC" STRINGS DEFINED???'
      PRINT *,'     1)  SAME FILE AS ABOVE '
      PRINT *,'     2)  ANOTHER FILE '
      PRINT *,' ENTER EITHER A 1 OR 2 :'
      IDUH = 0
  100 READ *, IGO
C
       IF (IGO.EQ.1) THEN
          INP = 10
       ELSE IF( IGO.EQ.2) THEN
          INP = 20
      PRINT *,'ENTER THE GFGEN INPUT FILE WHICH CONTAINS THE '
      PRINT *,'BOUNDARY SEGMENTS, PRESCRIBED AS "GO" LINES :'
      PRINT *,'(THE ORDER OF THE SPECIFICATION OF THE LINES WILL HAVE'
      PRINT *,' TO MATCH THE ORDER OF OTHER STUFF TO FOLLOW):::'
      READ (*,'(A)') FNAME
      OPEN(20,FILE=FNAME,FORM='FORMATTED',STATUS='OLD')
C
      ELSE
        IDUH = IDUH +1
        IF(IDUH.GT.3) THEN
           PRINT *,'THREE TIMES IS THE CHARM  -- STOP!!!!'
           STOP
        ENDIF
        PRINT *,'WRONG RESPONSE... ENTER EITHER 1 OR 2:'
        GOTO 100
      ENDIF
C-
      REWIND INP
C-
      NB = 0
      DO 150 I=1,NBOUND
      NMAX(I) = 1
  150 CONTINUE
  155 CONTINUE
      DO 200 I=1,NBOUND
      NN = NMAX(I)+1
      READ(INP,*) (NODE(I,K),K=1,NN)
      IF(NODE(I,NN).NE.-1) THEN
        NMAX(I) = NN
        PRINT *,'BOUNDARY ',I,' NMAX=',NMAX,' NODE(I,NN)= ',NODE(I,NN)
        GOTO 155
      ENDIF
      NMAX(I) = NN-1
  200 CONTINUE
C-
      PRINT *,'FOUND THE FOLLOWING BOUNDARIES'
      DO 300 I=1,NBOUND
      PRINT *,'  BOUNDARY NO. ',I
      PRINT *,(NODE(I,K),K=1,NMAX(I))
  300 CONTINUE
C-
      RETURN
      END
c
C========================================================================
C
      SUBROUTINE MIDSID
C
C     THIS ROUTINE FIGURES OUT WHAT THE MIDSIDE NODES ARE THAT LIE ON
C     GC STRINGS SO THAT THEY CAN BE SPECIFIED ALONG WITH THE CORNER
C     NODES AS "BCN" SPECS
C
C
      INCLUDE 'harmbc-noaa.inc'
C
C
      DIMENSION MID(NMX)
      CHARACTER CARD*3,BANNER(15)*80,FNAME*80,JREC(80)*1,ITYP*3,IDUM*80
      INTEGER*4 NE,NP,NBX,NBOUND,NCON,NT,IDUH,IGO,NSKIP,NOP,NMAX,NODE
c
      DO 400 I=1,NBOUND 
      PRINT *,'TIDAL BOUNDARY NUMBER ',I,' - CORNER NODES READ:'
      PRINT 10, (NODE(I,K),K=1,NMAX(I))
  10  FORMAT(80I8)
      IF(NMAX(I).EQ.1) GOTO 400
      DO 300 J= 1,NMAX(I)-1
      N1 = NODE(I,J)
      N2 = NODE(I,J+1)
      DO 200 M=1,NE
C      if(m.eq.1) print *,(nop(m,k),k=1,8)
      NCN = 8
      IF(NOP(M,8).EQ.0) NCN=6
      DO 100 N=1,NCN-1,2
      IF(NOP(M,N).EQ.N1 ) THEN
c       print *,'searching midside # ',j,' found node ',n1,' in elem ',m
c       print *,'   in the NOP position  ',n
           NU = MOD(N+2,NCN)
c         print *,'nop position for NU = ',nu,'  nop(m,nu) = ',NOP(M,NU)
             IF(NOP(M,NU) .EQ. N2) THEN
                 NMID = N +1
                 MID(J) = NOP(M,NMID)
                 GOTO 300
             ENDIF
           NM = N - 2
           IF(NM.LE.0) NM = NM + NCN
c         print *,'nop position for NM = ',nm,'  nop(m,nm) = ',NOP(M,NM)
             IF(NOP(M,NM) .EQ. N2) THEN
                 NMID = NM + 1
                 MID(J) = NOP(M,NMID)
                 GOTO 300
             ENDIF
      ENDIF
  100 CONTINUE
  200 CONTINUE
  300 CONTINUE
C
      DO 350 J=NMAX(I),2,-1
      IC = 2*J - 1
      IM = IC-1
      NODE(I,IC) = NODE(I,J)
      NODE(I,IM) = MID(J-1)
  350 CONTINUE
C
      NMAX(I) = 2*NMAX(I) - 1
      PRINT *,'TIDAL BOUNDARY NUMBER ',I,' WITH MIDSIDE NODES:'
      PRINT 10, (NODE(I,K),K=1,NMAX(I))
  400 CONTINUE
      PRINT *,' MIDSIDE NODES HAVE BEEN DEFINED FOR GC STRINGS'
      DO 800 I=1,NBOUND
      DO 600 J = 2,NMAX(I)-1,2
      N1 = NODE(I,J-1)
      N2 = NODE(I,J)
      N3 = NODE(I,J+1)
      CORD(N2,1) = (CORD(N1,1) + CORD(N3,1))/2.
      CORD(N2,2) = (CORD(N1,2) + CORD(N3,2))/2.
  600 CONTINUE
  800 CONTINUE
      RETURN
      END
