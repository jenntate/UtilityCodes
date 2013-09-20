      PROGRAM R4ICR10
c
c    Thsi program is designed to convert rma4 salinities into an IC for
c    use in RMA10.  It requires that the RMA4 results be developed for the
c    identical 2D mesh as is to be used in RMA10
c
       INCLUDE  'blk1.hydromd'
C
       COMMON /RMA4/TOLD(1,MNP)
C
      IGB = 10
      IHO=13
      READ(IGB) NP,NE,NPM,NEM,
     *     ((CORD(J,K),SPEC(J,K),K=1,3), ALFA(J), NFIX(J), AO(J), 
     *     NSURF(J), J = 1, NP), (NDEP(J), NREF(J), J = 1, NPM), 
     *     ((NOP(J,K),K=1,20), NCORN(J), IMAT(J), TH(J), 
     *     NFIXH(J), J = 1, NE), (WIDTH(J), J = 1, NP)
C
       CALL SALGET
C
      WRITE(12,1200) NP,NE,NPM,NEM
 1200 FORMAT(///,'      VALUES FROM 3D BINARY GEOMETRY FILE',/
     *           '        NP =  ',I6,/
     *           '        NE =  ',I6,/
     *           '        NPM = ',I6,/
     *           '        NEM = ',I6)
      WRITE(12,1201)
 1201 FORMAT(//,    
     * '  SURFACE      X             Y        BOTTOM     Z',/,
     * '   NODE    COORDINATE   COORDINATE  ELEVATION  COORD    NDEP',
     * '   NREF  SALIN',/)
      DO 500 I=1,NPM
      WRITE(12,1202) I,CORD(I,1),CORD(I,2),AO(I),CORD(I,3),NDEP(I),
     *  NREF(I),TOLD(1,I)
 1202 FORMAT(I6,3F12.2,F10.2,2I6,F10.2)
  500 CONTINUE
      WRITE(12,1205)
 1205 FORMAT(//,
     *'                  NODAL COORDINATES      BOTTOM  ',/
     *'  NODE        X           Y        Z      ELEV   NSURF     ALFA',
     *'     NFIX    SAL',/)
      DO 800 I=1,NPM
      WRITE(12,1204) I,(CORD(I,K),K=1,3),AO(I),NSURF(I),ALFA(I),NFIX(I),
     *  VEL(4,I)
 1203 FORMAT(I6,2F12.2,2F8.2,I8,2X,F8.4,I8,F7.2)
 1204 FORMAT(/,I6,2F12.2,2F8.2,I8,2X,F8.4,I8,F7.2)
      JJ= NREF(I) + 1
      KK= JJ + NDEP(I) -2
      DO 700 J=JJ,KK
      WRITE(12,1203) J,(CORD(J,K),K=1,3),AO(J),NSURF(J),ALFA(J),NFIX(J),
     *  VEL(4,J)
  700 CONTINUE
  800 CONTINUE
      WRITE(12,1208)
 1208 FORMAT(//,
     *'              ELEMENT CONNECTIONS FOR THIS ELEMENT (2 LINES)',/,
     *' ELEM     1     2     3     4     5     6     7     8     9    ',
     *    '10   IMAT  NCORN',/,
     *' NO.     11    12    13    14    15    16    17    18    19    ',
     *     '20  NFIXH    TH',//)
      DO 900 I=1,NE
      WRITE(12,1206) I,(NOP(I,K),K=1,10), IMAT(I),NCORN(I)
      WRITE(12,1207)   (NOP(I,K),K=11,20),NFIXH(I),TH(I)
 1206 FORMAT(13I6)
 1207 FORMAT(6X,11I6,F8.3)
  900 CONTINUE
      STOP
      END
C
      SUBROUTINE SALGET
      INCLUDE 'blk1.hydromd'
       COMMON /RMA4/TOLD(1,MNP)
       NOPT = 11
       ICYC = 0
       NCYC = 799
C
       READ  (NOPT) IDUM 
       READ  (NOPT) IDUM
C         READ (NOPT) (BANGEOM(I), I = 1, 5), (BANHYD(I), I = 1, 5),
C    *        (BANSED(I), I = 1,5)
       READ  (NOPT) IDUM
       READ  (NOPT) IDUM
C         READ (NOPT) TITLE
C-
C
C       PROCESS TIME SERIES DATA FOR DYNAMIC RUN
C
C ...   READ TIME STEP INFORMATION
       DO 400 ICYC = 1,NCYC
      READ (NOPT,END=401) TET, NQAL, NPX,((TOLD(K,J),J=1,NPX),K=1,NQAL)
C
  400 CONTINUE
  401 CONTINUE
      CALL SALFIX
      DO 950 I=1,NPM
      DEP = CORD(I,3) - AO(I)
      SAL = TOLD(1,I)
      DSDZ = 0.0016*SAL
      IF(SAL .GT. 25. ) THEN
            DSDZ = 0.007328 - 0.000229*SAL
      ENDIF
      IF(SAL.GT.32. ) DSDZ = 0.
      VEL(4,I) = SAL
      VEL(1,I) = 0.01
      JJ = NREF(I) + 1
      KK = JJ + NDEP(I) -2
      VEL(3,I) = DEP
      DO 940 J=JJ,KK
      DZ = CORD(I,3) - CORD(J,3)
      FACT = 0.0
      VEL(4,J) = VEL(4,I) + FACT*DSDZ*DZ
c     VEL(4,J) = SAL
      VEL(3,J) = DEP
  940 CONTINUE
  950 CONTINUE
      TET = 0.
      NDF = 6 
      IHO = 13
      WRITE(IHO) TET,NP,NDF,((VEL(J,I),VDOT(J,I),J=1,NDF),
     *           VVEL(I),I=1,NP)
      RETURN
      END
      SUBROUTINE SALFIX
C
      INCLUDE 'blk1.hydromd'
      COMMON /RMA4/TOLD(1,MNP)
      DIMENSION  MFIX(MNP),ILIT(MNP),IMLEFT(MNP)
      DATA MFIX/MNP*0/
C
      IGO = 0
      DO 100 I=1,NPM
      IF(TOLD(1,I).EQ.0.) THEN
        MFIX(I) = 1
        IGO = 1
      ENDIF
  100 CONTINUE
C
      IF(IGO.EQ.0) RETURN
c
      print *,' NEM = ',NEM,  '  and NPM = ', npm
c
      IFIT = 0
  200 IGO = 0
C
      DO 600 I=1,NPM
      IF(MFIX(I).LE.IFIT) GOTO 600
      PRINT *,'ATTEMPTING TO FIX SALINITY AT NODE ',I
      NAVG = 0
      SSUM = 0.
C
         DO 500 J=1,NEM
C  
             DO 250 K=1,20
             IF (I.EQ.NOP(J,K)) GOTO 300
  250        CONTINUE
            GOTO 500
C
  300 CONTINUE
             DO 400 K=1,20
             IF(NOP(J,K).EQ.0) GOTO 500
             IF(NOP(J,K).GT.NPM) GOTO 400
             IF(MFIX(NOP(J,K)).GT.IFIT) GOTO 400
             NAVG = NAVG +1
             SSUM = SSUM + TOLD(1,NOP(J,K))
  400        CONTINUE
C
  500   CONTINUE
C
        IF(NAVG.EQ.0) THEN
         MFIX(I) = MFIX(I) + 1
         IGO = IGO + 1
         GOTO 600
        ENDIF
        TOLD(1,I) = SSUM/NAVG
  600 CONTINUE
C
      IF(IGO.GE.1) THEN
        IFIT=IFIT+1
        NNF=0
        DO 700 I=1,NPM
        IF(MFIX(I).GT.IFIT) THEN
          NNF=NNF + 1
          IMLEFT(NNF) = I
        ENDIF
  700   CONTINUE
        PRINT * ,'SALFIX ITERATION ', IFIT,' WITH ',IGO,' NODES UNFIXED'
        IF(IFIT.GT.5) THEN
         PRINT *, 'SALFIX RETURN AFTER ',IFIT,' ITERATIONS'
         PRINT *,' NODES LEFT ARE:'
         PRINT 770,(IMLEFT(K),K=1,NNF)
  770    FORMAT(10I6)
         RETURN
         ENDIF
        GOTO 200
      ENDIF
      RETURN
      END
