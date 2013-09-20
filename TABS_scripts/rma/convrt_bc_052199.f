      program convrt_bc

      COMMON /CARD/ JREC
      DIMENSION REA(200), INT(200), CHA(200)
      character*80 infile, outfile
      CHARACTER JREC(80)*1, CHA*20, IC1*2, IC3*1, ILAST*2, IC3OLD*1

      write(*,*)
      write(*,*) '          ***  CONVRT_BC ***'
      write(*,*)
      write(*,*) 'This program reads in a RMA10 boundary condition'
      write(*,*) 'file given in English units and writes the ' 
      write(*,*) 'boundary conditions out to another file with the'
      write(*,*) 'following cards converted from English to Metric'
      write(*,*) 'units:'
      write(*,*)
      write(*,*) 'BH BQ CN DF DM DZ EV' 
      write(*,*) 'EZ IC PE RA RD SI ZB'
      write(*,*)


      write(*,*) 'enter input b.c. file name'
      read(*,*) infile
      write(*,*) 'enter output b.c. file name'
      read(*,*) outfile

      open(10,file=infile,status='old',form='formatted')
      open(20,file=outfile,status='unknown',form='formatted')


      I1 = 1
  150 DO 160 I = 1, 200 
         REA(I) = 0.0
         INT(I) = 0
  160 CONTINUE
      I1 = 1
C-
      READ (10,170,END =200) IC1, IC3, (JREC(I), I = 1, 77)
  170 FORMAT (A2, A1, 77A1)

      IF (IC1 .EQ. 'BH') THEN
C
C**                                          BOUNDARY, HEAD   (BH  CARD)
C
        NWD = 1
        CALL CRACK(I1, NWD, REA, INT, CHA, 'INTEGER  ', IERC)
C
        NWD = 4
        CALL CRACK(I1, NWD, REA, INT, CHA, 'REAL     ', IERC)
C
        ICONJ = INT(1)
        ELEV    = REA(1)*.3048
        QQALB1 = REA(2)
        QQALB2 = REA(3)
        QQALB3 = REA(4)
        write(20,180) IC1, IC3, ICONJ, ELEV, QQALB1, QQALB2, QQALB3 
 180    format(A2, A1, I6, 4F11.5)
C-
      ELSE IF (IC1 .EQ. 'BQ') THEN
C
C**                          CONTROL FOR SPECIAL BOUNDARIES   (BQ  CARD)
C
        NWD = 1
        CALL CRACK(I1, NWD, REA, INT, CHA, 'INTEGER  ', IERC)
C         
        ICONJ = INT(1)
C         
        NWD = 5
        CALL CRACK(I1, NWD, REA, INT, CHA, 'REAL     ', IERC)
C
        TOTBQ    = REA(1)*0.0283168
        DIRBQ    = REA(2)
        QQALB1   = REA(3)
        QQALB2   = REA(4)
        QQALB3   = REA(5)
        write(20,185) IC1,IC3,ICONJ,TOTBQ,DIRBQ,QQALB1,QQALB2,QQALB3
 185    format(A2, A1, I6, 5F11.5)
      ELSE IF (IC1 .EQ. 'SI') THEN
C
C**                              SYSTEM INTERNATIONAL UNITS   (SI  CARD)
C
        IDUM = 1
        write(20,1000) IC1,IC3,IDUM
 1000   format(A2, A1, I6)

      ELSE IF (IC1 .EQ. 'ZB') THEN
C
C**                                ZERO BOTTOM VELOCITY CARD   (ZB  CARD)
C
        NWD = 3
        CALL CRACK(I1, NWD, REA, INT, CHA, 'REAL     ', IERC)
C
        BLRTH = REA(1)
        BLVEV = REA(2)*47.88
        BLVDF = REA(3)*0.0929
        write(20,1010) IC1,IC3,BLRTH,BLVEV,BLVDF
 1010   format(A2, A1, 2F11.5, F11.7)

      ELSE IF (IC1 .EQ. 'IC') THEN
C
C**                                      INITIAL CONDITIONS   (IC  CARD)
C
        NWD = 9
        CALL CRACK(I1, NWD, REA, INT, CHA, 'REAL     ', IERC)
C
        ELEV  = REA(1)*.3048
        UNOM  = REA(2)*.3048
        UDIR  = REA(3)
        HMINR = REA(4)*.3048
        SALI  = REA(5)
        TEMPI = REA(6)
        SEDI  = REA(7)
        UINP  = REA(8)*.3048
        VINP  = REA(9)*.3048
        write(20,1020) IC1,IC3,ELEV,UNOM,UDIR,
     +                 HMINR,SALI,TEMPI,SEDI,UINP,VINP
 1020   format(A2, A1, F11.5, 2F7.3, F7.2, 2F8.3, 
     +         F10.6, 2F6.3)

      ELSE IF (IC1 .EQ. 'CN') THEN
C
C**                    CONVERGANCE TEST FOR MAXIMUM CHANGES   (CN  CARD)
C
        NWD = 1
        CALL CRACK(I1, NWD, REA, INT, CHA, 'INTEGER  ', IERC)
C
        NCONV = INT(1)
        NWD = 6
        CALL CRACK(I1, NWD, REA, INT, CHA, 'REAL     ', IERC)
        CONV1 = REA(1)*.3048
        CONV2 = REA(2)*.3048
        CONV3 = REA(3)*.3048
        CONV4 = REA(4)
        CONV5 = REA(5)
        CONV6 = REA(6)
        write(20,1030) IC1,IC3,NCONV,CONV1,CONV2,CONV3,
     +                 CONV4,CONV5,CONV6 
 1030   format(A2, A1, I6, 6F8.5)

      ELSE IF (IC1 .EQ. 'DF') THEN
C
C**           FULL DIFFUSION COEFFICIENTS IN THE HORIZONTAL   (DF  CARD)
C
        NWD = 1
        CALL CRACK(I1, NWD, REA, INT, CHA, 'INTEGER  ', IERC)
C
        NWD = 3
        CALL CRACK(I1, NWD, REA, INT, CHA, 'REAL     ', IERC)
        IMSTRT = INT(1)
        ORT1   = REA(1)*.0929
        ORT2   = REA(2)*.0929
        ORT3   = REA(3)
        write(20,1040) IC1,IC3,IMSTRT,ORT1,ORT2,ORT3
 1040   format(A2, A1, I6, 3F11.5)

      ELSE IF (IC1 .EQ. 'DZ') THEN
C
C**          TURBULENT DIFFUSION COEFICIENT IN THE VERTICAL   (DZ  CARD)
C
        NWD = 1
        CALL CRACK(I1, NWD, REA, INT, CHA, 'INTEGER  ', IERC)
C
        IMSTRT = INT(1)

        NWD = 1
        CALL CRACK(I1, NWD, REA, INT, CHA, 'REAL     ', IERC)
C
        ORT1 = REA(1)*.0929
        write(20,1050) IC1,IC3,IMSTRT,ORT1
 1050   format(A2, A1, I6, F10.7)

      ELSE IF (IC1 .EQ. 'EV') THEN
C
C**                 HORIZ PARMS FOR TURBULENT EX,MANN,CHEZY   (EV  CARD)
C  
        NWD = 1
        CALL CRACK(I1, NWD, REA, INT, CHA, 'INTEGER  ', IERC)
C
        NWD = 5
        CALL CRACK(I1, NWD, REA, INT, CHA, 'REAL     ', IERC)
C
        IMSTRT = INT(1)
        ORT1   = REA(1)*47.88
        ORT2   = REA(2)*47.88
        ORT3   = REA(3)*47.88
        ORT4   = REA(4)*47.88
        ORT5   = REA(5)
        write(20,1060) IC1,IC3,IMSTRT,ORT1,
     +                 ORT2,ORT3,ORT4,ORT5
 1060   format(A2, A1, I6, 4F11.1, F7.4)

      ELSE IF (IC1 .EQ. 'EZ') THEN
C
C**                       VERTICAL EDDY AND DIFFUSION COEFS   (EZ  CARD)
C
        NWD = 1
        CALL CRACK(I1, NWD, REA, INT, CHA, 'INTEGER  ', IERC)
C
        NWD = 2
        CALL CRACK(I1, NWD, REA, INT, CHA, 'REAL     ', IERC)
C
        IMSTRT = INT(1)
        ORT1 = REA(1)*47.88
        ORT2 = REA(2)*47.88
        write(20,1070) IC1,IC3,IMSTRT,ORT1,ORT2
 1070   format(A2, A1, I6, 2F10.6)

      ELSE IF (IC1 .EQ. 'RD') THEN
C
C**                           ROUGHNESS ASSIGNMENT BY DEPTH   (RD  CARD)
C
        NWD = 2
        CALL CRACK(I1, NWD, REA, INT, CHA, 'INTEGER  ', IERC)
C
        ISTART = INT(1)
        IRUFF  = INT(2)
C
        NWD = 4
        CALL CRACK(I1, NWD, REA, INT, CHA, 'REAL     ', IERC)
C
        RDRO   = REA(1)
        RDDO   = REA(2)*.3048
        RDRM   = REA(3)
        RDCOEF = REA(4)
C
        NWD = 1
        CALL CRACK(I1, NWD, REA, INT, CHA, 'INTEGER  ', IERC)
C
        IZBA = INT(1)
        write(20,1080) IC1,IC3,ISTART,IRUFF,
     +                 RDRO,RDDO,RDRM,RDCOEF,IZBA
 1080   format(A2, A1, 2I6, 4F11.7, I4)

      ELSE IF (IC1 .EQ. 'DM') THEN
C
C**                    WETTING AND DRYING BY MARCH POROSITY   (DM  CARD)
        NWD1 = 1
        CALL CRACK(I1, NWD1, REA, INT, CHAR, 'INTEGER  ', IERC)
        NWD2 = 4
        CALL CRACK(I1, NWD2, REA, INT, CHAR, 'REAL     ', IERC)
C
        NODE1 = INT(1)
        WDMC1 = REA(1)*.3048
        WDMC2 = REA(2)*.3048
        WDMC3 = REA(3)
        WDMC4 = REA(4)*.3048
        write(20,1090) IC1,IC3,NODE1,
     +                 WDMC1,WDMC2,WDMC3,WDMC4
 1090   format(A2, A1, I6, 4F11.5)

      ELSE IF (IC1 .EQ. 'PE') THEN
C
C**    CONTROL OF HORZ EDDY VISCOSITY AND DIFFUSION BY PECLET NUMBER   (PE  CARD)
C
        NWD1 = 1
        CALL CRACK(I1, NWD1, REA, INT, CHA, 'INTEGER  ', IERC)
C
        IPEC = INT(1)
        NWD2 = 4
        CALL CRACK(I1, NWD2, REA, INT, CHA, 'REAL     ', IERC)
C
        GPEC = REA(1)
        GPECD = REA(2)
        VPEC = REA(3)*.3048
        SFPEC = REA(4)
        write(20,1100) IC1,IC3,IPEC,
     +                 GPEC,GPECD,VPEC,SFPEC 
 1100   format(A2, A1, I6, 4F11.5)

C-
      ELSE IF (IC1 .EQ. 'RA' ) THEN
C ...   RAINFALL/EVAPORATION CARD (POINT SOURCE OF FLOW)    (RA TYPE)
C
        NWD1 = 1
        CALL CRACK(I1, NWD1, REA, INT(1), CHAR, 'INTEGER  ', IERC)
C
        NWD1 = 1
        CALL CRACK(I1, NWD1, REA(1), INT, CHAR, 'REAL     ', IERC)
C
        IDUM = INT(1)
        RDUM = REA(1)*2.54
        write(20,1110) IC1,IC3,IDUM,RDUM
 1110   format(A2, A1, I6, F11.7)


      ELSE
        write(20,170) IC1, IC3, (JREC(I), I = 1, 77)
      END IF
      GO TO 150


  200 CONTINUE
      close(10)
      close(20)
      STOP
      END



      SUBROUTINE CRACK (I1,NWD,REA,INT,CHA,TYPE,IERRC)
C-
C ... AUTHOR = DON BACH
C ... CRACKS DATA CARDS SIMULATING LIST DIRECTED READS
C ... THIS ROUTINE CONFORMS TO ANSI X3.9-1978 (FORTRAN 77)
C ... CARD IS PASSED THROUGH COMMON BLOCK
C
C GLB this routine is identical to the HEC version of the routine
C 
      IMPLICIT REAL (A-H,O-Z)
      CHARACTER*1  IBLANK, ICOMMA
      COMMON /CARD/ JREC 
      DIMENSION REA(200), INT(200)
      CHARACTER JREC(80)*1, TYPE*9,IBUF*77,IFOR(1)*10
      CHARACTER CHA(200)*20
C-
      DATA IBLANK /' '/
      DATA ICOMMA /','/
C-
      IF (NWD.GT.200) PRINT *,' ARRAY overrun in CRACK -REA/INT/CHA'
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
          IERRC =IERRC + 1
          RETURN
        ENDIF
      IF(JREC(I2+1).EQ.'''') I2=I2+1
      I1=I2+1
  900 CONTINUE
      RETURN
      END
