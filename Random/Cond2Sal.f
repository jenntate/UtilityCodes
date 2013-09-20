        PROGRAM Cond2Sal 
          REAL Sal78, CND, T, SALINITY, P
          CHARACTER*20 name
          READ(*,'(A)') name 
          OPEN(10,file=name,form='formatted',status='old')   
         DO i=1,999999
          READ(10,*,END=900) CND,T,P
          SALINITY = Sal78(CND,T,P)
          PRINT *, 'Salinity = ', Salinity
         ENDDO
900     CONTINUE
        END

        REAL FUNCTION Sal78(COND,t,dp) 
C       FORTRAN CODE
C       THE CONDUCTIVITY RATIO (CND)=1.0000000 FOR SALINITY=35 PSS-78
C       TEMPERATURE=15.0 DEG. CELSIUS AND ATMOSPHERIC PRESSURE.
C       FUNCTION TO CONVERT CONDUCTIVITY RATIO TO SALINITY (M=0)
C       SALINITY TO CONDUCTIVITY RATIO (M=1, CND BECOMES INPUT SALINITY)
C       REFERENCES: ALSO LOCATED IN UNESCO REPORT NO. 37 1981
C       PRACTICAL SALINITY SCALE 1978: E.L. LEWIS IEEE OCEAN ENG. JAN. 1980
C       ----------------------------------------------------------
C       UNITS:
C         PRESSURE      P      DECIBARS (1 decibar = 10000 Pa)
C         TEMPERATURE   T      DEG CELSIUS IPTS-68
C         CONDUCTIVITY  COND   mS/cm
C         CONDUCTIVITY  CND    RATIO                (M=0)
C         SALINITY      SAL78  PSS-78               (M=0)
C       ----------------------------------------------------------
C       CHECKVALUES:
C         1.) SAL78=1.888091 for CND=40.0000, T=40 DEG C, P=10000 DECIBARS:   M=1
C         2.) SAL78=40.00000 for CND=1.888091, T=40 DEG C, P=10000 DECIBARS:  M=0
C       ----------------------------------------------------------
C       SAL78 RATIO: RETURNS ZERO FOR CONDUCTIVITY RATIO: < 0.0005
C       SAL78: RETURNS ZERO FOR SALINITY: < 0.02
C       ----------------------------------------------------------
C       Original fortran code is found in:
C         UNESCO technical papers in marine science 44 (1983) -
C         'Algorithms for computation of fundamental properties of seawater'
C       ----------------------------------------------------------


        REAL a1,a2,a3,a4,a5,a6,b1,b2,b3,b4,b5,b6,c1,c2,c3,c4,c5,k,dt
        REAL r_t,del_S,Sal,RT,A,B,C,dp,COND

C       PRACTICAL SALINITY SCALE 1978 DEFINITION WITH TEMPERATURE    

        c1=0.6766097
        c2=0.0200564
        c3=0.0001104259
        c4=-0.00000069698
        c5=0.0000000010031
        dt=t-15.0
        r_t35 = c1+(c2*dt)+(c3*dt*dt)+(c4*dt*dt*dt)+(c5*dt*dt*dt*dt)
C       r_t35 = C(35,t,0)/C(35,15,0)


C       CONDUCTIVITY RATIO BASED FROM MEASURED NOAA CONDUCTIVITY in mS/cm
C       R = R_p*R_t*r_t
C       C(35,15,0) = 42.914 mS/cm

C        R=COND/r_t35
        R=COND/42.914

        A = -0.003107*dt+0.4215
        B = (0.000446*dt+0.03426)*dt+1.0

C       FOR SURFACE ASSUME ATMOSPHERIC PRESSURE = 10.1325 decibars
C        C = 0.000209677
C        C=0
        C = ((0.000000000000003989*dp-0.000000000637)*dp+0.0000207)*dp

        RT=(ABS(R/(r_t35*(1.0+C/(B+A)))))**0.5

        k=0.0162
        b1=0.0005
        b2=-0.0056
        b3=-0.0066
        b4=-0.0375
        b5=0.0636
        b6=-0.0144
        del_S = dt/(1+k*dt)*(b1+(b2*RT**0.5)+(b3*RT)+(b4*RT**1.5)
     #          +(b5*RT**2)+(b6*RT**2.5))

        a1=0.0080
        a2=-0.1692
        a3=25.3851
        a4=14.0941
        a5=-7.0261
        a6=2.7081


        Sal = a1+(a2*RT**0.5)+(a3*RT)+(a4*RT**1.5)+(a5*RT**2)
     #        +(a6*RT**2.5)+del_S
 
       END
