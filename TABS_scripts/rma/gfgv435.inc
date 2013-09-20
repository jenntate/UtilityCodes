C ...           INCLUDE FOR GFGV435    Effective 8-21-1997
C     PARAMETER (MAXE=3500,  MAXP=10000,  MM4=4*MAXE, 
C    *           MM1=100,    MM2=500,      MM3=MAXE,
C    *           NL=20,      NBN=MAXP/3,   MAXFC=100)
c
c     PARAMETER (MAXE=10000, MAXP=30000,   MM4=4*MAXE, 
c    *           MM1=100,    MM2=500,      MM3=MAXE,
c    *           NL=20,      NBN=MAXP/3,   MAXFC=50)
C
      PARAMETER (MAXE=20000,  MAXP=60000,  MM4=4*MAXE, 
     *           MM1=100,    MM2=500,      MM3=MAXE,
     *           NL=20,      NBN=MAXP/3,   MAXFC=100)

      COMMON /BASIC/
     *       NE, NP, NPM, NR, NC, MIST, KN4, 
     *       IBATCH, IRO, IHEC, IMETRIC, ITRACE, NSTOP,
     *       ALPHA(MAXP),   CORD(MAXP,2),   IEM(MAXE),
     *       IEL(MAXP),     IES(MM4,3),     INUM(MAXP),
     *       IRDGNN(MAXP),  MSN(MAXP),      NCORN(MAXP),
     *       SS1(MAXP),     SS2(MAXP),      WD(MAXP),
     *       WIDTH(MAXP),   WIDS(MAXP),
     *       NADM(MM1),     ICOL(MM1,MM2),  IHOLD(MM3),   
     *       LIST(MAXP),    LISTN(NL,MM2),  LISTO(NL),
     *       MLIST(MAXE),   IMAT(MAXE),    TH(MAXE),    
     *       NCNODES(MAXE), NOP(MAXE,8)

C-
      COMMON /IO/ IALTGM, ICOLL, IGIN, IN, LP, LUNIT, NPRT
C-
      COMMON /STRING/ TITLE
C-
      CHARACTER TITLE*77
C-
      COMMON /SVMID/ MSNKP(MAXE),NMSN
C-
      COMMON /ATRAN/ ICON(MAXP,60)
C-
      COMMON /BLKB/ NCM,NCMI,KNT,NDP,NNEW,MP,MSUM,NAD,NAE,
     *              NITST, NFWS, NFWSAV, 
     *              MPQ, MPQ_CLICK, MPQ_KEEP, MPQ_CLICK_KEEP,
     *              MTSUM, MTSUM1, MRSUM, MRSUM1, 
     *              IENXT(MAXE), LNEW(8), NDELM(MAXP), 
     *              NDROP(8), NINC(MAXP), NELIM(MAXE)
C-
      COMMON /BLKB2/ NECON(MAXP,20)
C-
      COMMON /PLTVAR/ IPO, IPP,
     *                IPNN, IPEN, HORIZ, VERT, XSCALE, YSCALE, AR,
     *                HITEL, HITNN, NXPMIN, NXPMAX, NYPMIN, NYPMAX
C-
