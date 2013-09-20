*      DATA FILTER
*      PROGRAM TO FILTER AND WRITE OUT FILTERED DATA 
*      ORIGINAL AUTHOR: UNKNOWN
*      THIS VERSION UPDATED BY MARÌA SORAYA SARRUFF, CEWES-HE-TS (03/26/2001)
*      SIGNIFICANT PORTIONS OF THIS CODE ARE TAKEN FROM 'NUMERICAL RECIPES'	
*
	parameter (norder=18,nsize=2**norder,nsize2=2*nsize)
*
	dimension tide(nsize),time(nsize),itide(24),
     &		fdata1(nsize),fdata2(nsize),fftdat(nsize2),
     &		rfd1(nsize),irfd1(24),
     &		fsata(nsize),sata(nsize),
     &		data1(nsize),data2(nsize)
*
        dimension tidef(nsize,50),timef(nsize,50)
        dimension tideo(nsize,50),timeo(nsize,50)
*
	dimension nodata(nsize)
	integer trmlen
	character filename*40, fileout*40
*	
*
	
*	ENTER TIME WHEN DATA WILL START BEING READ
*	ENTER 0 IF PROGRAM SHOULD READ INPUT FILE FROM TOP
*
	write(*,*) ' what is timeoffset..? '
	read(*,*) timeoffset
*
*	ESTABLISH FILTER BOXCAR LIMITS BY ASSIGNING THE SHORTEST
*	AND LONGEST PERIODS OF INTEREST.
*
 	write(*,*) 'select the high frequency limit'
 	write(*,*) 'by selecting the shortest period'
 	write(*,*) 'of interest (real number):'
 	write(*,*)
 	read(*,*) shper
*
	frqhi = 1./shper
*
 	write(*,*) 'select the low frequency limit'
	write(*,*) 'by selecting the longest period'
 	write(*,*) 'of interest (real number):'
	write(*,*)
 	read(*,*) lgper
*
	frqlo = 1./lgper
*
*
*	ENTER FILENAME CONTAINING VECTORS TO BE FILTERED
*	FIRST COLUMN SHOULD CONTAIN TIMES, SECOND COLUMN SHOULD CONTAIN DATA
*
	write(*,*)'what is the input filename? '
	read(*,'(a)') filename
        write(*,*) 'how many columns of data are on the file?'
        read(*,*) ncoldat
        write(*,*) 'choose an imput file format'
        write(*,*) 'enter a 1 if the file format is as follows:'
        write(*,*) '  time, data(1), data(2),...,data(n)'
        write(*,*) 'enter a 2 if the file format is as follows:'
        write(*,*) '  time(1), data(1), time(2), data(2),',
     +             '...,time(n), data(n)'
        write(*,*) 'NOTE: if format 2 is chosen, the code will'
        write(*,*) 'expect all void data to be represented with'
        write(*,*) 'a value of 9999'
        read(*,*) idatform
        if (idatform .ne. 1 .and. idatform .ne. 2) then
          write(*,*) 'error: you must enter a 1 or a 2 for the'
          write(*,*) 'data format.  Try harder, loser'
          stop
        end if
        write(*,*) 'finally, what is the output file name'
        read(*,'(a)') fileout
*
*	READING FILE AND WRITING READ VALUES TO DATA.OUT FILE...
*	
	open(15,file=filename, form='formatted',status='old')
        nvalm = 0
        if (idatform .eq. 1) then
          do i = 1, 99000 
            nvalm = nvalm + 1
            read(15,*,end=100) timef(i,1),(tidef(i,j),j=1,ncoldat) 
            if (ncoldat .gt. 1) then
              do j = 2, ncoldat
                timef(i,j) = timef(i,1)
              end do
            end if
          end do
        else if (idatform .eq. 2) then
          do i = 1, 99000 
            nvalm = nvalm + 1
            read(15,*,end=100) (timef(i,j),tidef(i,j),j=1,ncoldat)
          end do
        end if
100     continue
        close (15)
        nvalm = nvalm - 1
*
        open(10,file=fileout,form='formatted',
     +      status='unknown')
*
        do ijk = 1, ncoldat
*
	nval = 0
	jm = 0
        do i = 1, nvalm
          if (tidef(i,ijk) .lt. 999999.0) then
            nval = nval + 1
            time(nval) = timef(i,ijk) - timeoffset
            tide(nval) = tidef(i,ijk)
          end if
        end do  
*
****
*
*   ACCOUNT FOR WATER LEVEL VARIATION
*
*     1. FIND AVERAGE VALUE OF ALL VALID DATA POINTS
*	AVEZ=AVERAGE, ZN=NUMBER OF DATA POINTS
*
        avez = 0.0
        zn = 0.0
        do 501 n = 1, nval
            avez = avez + tide(n)
            zn = zn + 1.
            data1(n) = tide(n)
	    nodata(n) = 1
            data2(n) = 0.0
 501    continue
	write(*,*) 'zn = ',zn
*
        avez = avez / zn
*
*     2. CENTER THE DATA AT THE AVERAGE (AVEZ)
*
        do 502 n = 1, nval
		data1(n) = (data1(n) - avez)
 502    continue
*
*     3. PUT ZEROES IN REMAINING NSIZE-NVAL VALUES OF DATA1.
*	 DATA2, AND NODATA
*
		do 601 i=nsize-nval,nsize
*
			data1(i) = 0.0
			data2(i) = 0.0
			nodata(i) = 1
601		continue
*
*   FILTER ELEVATION DATA (HERE IS THE MEAT, GUYS!)
*
        call filter(data1,data2,nsize,time,frqlo,frqhi,
     &               fftdat, fdata1, fdata2,reduct)
**
**
	do 801 i=1,nval
	sata(i)=data1(i)+avez
	fsata(i)=fdata1(i)+avez
801	continue
**
**	WRITE OUTPUT FILTERED TIDES 
**
*
	do n=1,nval
          timeo(n,ijk) = time(n)+timeoffset
          tideo(n,ijk) = fsata(n)
        end do
        if (nval .lt. nvalm) then
          do n=nval+1,nvalm
            timeo(n,ijk) = 9999.0
            tideo(n,ijk) = 9999.0
          end do
        end if
        end do
C
        if (idatform .eq. 1) then
        do i = 1, nvalm 
          write(10,698) timeo(i,1),(tideo(i,j),j=1,ncoldat)
        end do
        else
        do i = 1, nvalm 
          write(10,699) (timeo(i,j),tideo(i,j),j=1,ncoldat)
        end do
        end if
698     format(f10.3,50(f11.4))
699     format(50(f10.3,f9.3))
        close (10)
       end
*
*
      SUBROUTINE FILTER(DATA1,DATA2,NTIME,TIME, FRQLOW, FRQHI,
     &                   FFTDAT, FDATA1, FDATA2,reduct)
*
*   THIS SUBROUTINE WILL FILTER UP TO 2 SETS OF DATA AT A TIME.
*
*     INPUTS:
*        DATA1(*) = ARRAY OF DATA SET 1 VALUES
*        DATA2(*) = ARRAY OF DATA SET 2 VALUES
*                   IF NO 'DATA2', SET ALL VALUES IN DATA2(*) TO 0.0
*           NTIME = NUMBER OF VALUES IN DATA1(*), DATA2(*), AND TIME(*)
*         TIME(*) = THE TIME EACH SAMPLE IN DATA1 & DATA2 WAS TAKEN
*          FRQLOW = LOW FREQUENCY CUTOFF
*           FRQHI = HIGH FREQUENCY CUTOFF
* 
*     OUTPUTS:
*       FFTDAT(*) = ARRAY OF VALUES OF THE FOURIER TRANSFORM OF DATA1 & DATA2
*       FDATA1(*) = FILTERED VALUES OF DATA1(*)
*       FDATA2(*) = FILTERED VALUES OF DATA2(*)
*
*
      PARAMETER (NORDER=18, NSIZE=2**NORDER, NSIZE2=2*NSIZE)
      REAL FFTDAT(NSIZE2),TIME(NSIZE), FREQ(NSIZE),
     &     DATA1(NSIZE), DATA2(NSIZE), FFTDT(NSIZE2), FDATA1(NSIZE),
     &     FDATA2(NSIZE)
*
*  PUT  DATA INTO FFTDAT ARRAY.. THE CONDUCTIVITY (DAT(1,*)) WILL BE IN THE
*  'REAL' PART OF FFTDAT; THE TIDE (DAT(2,*)) WILL BE IN THE 'IMAGINARY' PART.
*
      NN = NSIZE
      IF(NTIME.GT.NN)THEN
        WRITE(*,1)NTIME
 1      FORMAT(//,'  YOUR DATA HAS ',I10,' DATA POINTS...',/,
     &            '  YOU MUST CHANGE THE VALUE OF NSIZE IN FILTER TO ',/,
     &            '  EXCEED THIS',//)
        STOP
      END IF
*
      AVE1 = 0.0
      AVE2 = 0.0
      DO 3 I = 1,NN
        II = I*2 - 1
        IF(I .LE. NTIME)THEN
*
*   PUT DATA INTO THE ARRAY TO BE FFT'ed ON
*
          FFTDAT(II) = DATA1(I)
          FFTDAT(II+1) = DATA2(I)
          AVE1 = AVE1 + DATA1(I)
          AVE2 = AVE2 + DATA2(I)
        ELSE
*
*  PAD OUT THE ARRAY WITH THE AVERAGE VALUES
*
          FFTDAT(II) = AVE1/FLOAT(NTIME)
          FFTDAT(II+1) = AVE2/FLOAT(NTIME)
        END IF
 3    CONTINUE
*
*   COMPUTE THE FREQUENCY ARRAY
*
       TINC = TIME(2)-TIME(1)
*
       DENOM = FLOAT(NN) * TINC
*
       J = -1
       NHALF = NN / 2
       DO 4 NP = 1,NN
         IF(NP .LE. NHALF)THEN
*
*  POSITIVE FREQUENCY
*
           FREQ(NP) = FLOAT(NP-1) / DENOM
*
         ELSE
*
*  NEGATIVE FREQUENCY
*
           J = J + 1
           FREQ(NP) = FLOAT(J-NHALF) / DENOM
         END IF
 4     CONTINUE
*
*   DO A FORWARD FFT
*
       ISIGN = +1
       CALL FOUR1(FFTDAT,NN,ISIGN)
*
*    REMOVE CONSTITUENTS WITH FREQUENCIES HIGHER THAN 'FRQHI' AND LOWER
*    THAN 'FRQLOW' ...  USING A BOXCAR FILTER
*
*
	open(69,file='fft.coef',form='formatted',status='unknown')
	open(70,file='power_spectrum',form='formatted',status='unknown')
       DO 5 NP = 1,NN
         F = ABS(FREQ(NP))
         ii = np * 2 - 1
	if(f.gt.0.0001)then
	ti=1./f
	else
	ti=9999.
	endif
	write(69,691)np,f,ti,ii,(fftdat(ii)),(fftdat(ii+1))
691	format(i8,f20.4,f20.6,i8,2f15.2)
	pow=((abs(fftdat(ii)))**2+(abs(fftdat(ii+1)))**2)
	if(pow.lt.10000.)go to 2013
	if(ti.gt.27.)go to 2013
	if(ti.lt.2.5)go to 2013
	write(70,692) np,f,ti,pow
692	format(i8,f20.4,f20.6,f15.2)
2013	continue
         IF(F.GT. FRQHI .OR. F.LT. FRQLOW)THEN
           FACT = 0.0
         ELSE
           FACT = 1.0
         END IF
*
	fact=fact*reduct
         fftdt(ii) = fftdat(ii) * fact
         fftdt(ii+1) = fftdat(ii+1) * fact
*
 5     CONTINUE
	close(70)
	close(69)
*
*  NOW DO AN INVERSE FFT
*
       ISIGN = -1
       CALL FOUR1(FFTDT,NN,ISIGN)
*
*   NORMALIZE
*
        DO 6 I = 1,NTIME
         II = 2 * I - 1
         FDATA1(I) = FFTDT(II) / FLOAT(NN)
         FDATA2(I) = FFTDT(II+1) / FLOAT(NN)
 6     CONTINUE
*
      END
*
****
      SUBROUTINE FOUR1(DATA,NN,ISIGN)
      double precision WR,WI,WPR,WPI,WTEMP,THETA
      DIMENSION DATA(*)
      N=2*NN
      J=1
      DO 11 I=1,N,2
        IF(J.GT.I)THEN
          TEMPR=DATA(J)
          TEMPI=DATA(J+1)
          DATA(J)=DATA(I)
          DATA(J+1)=DATA(I+1)
          DATA(I)=TEMPR
          DATA(I+1)=TEMPI
        ENDIF
        M=N/2
1       IF ((M.GE.2).AND.(J.GT.M)) THEN
          J=J-M
          M=M/2
        GO TO 1
        ENDIF
        J=J+M
11    CONTINUE
      MMAX=2
2     IF (N.GT.MMAX) THEN
        ISTEP=2*MMAX
        THETA=6.28318530717959D0/(ISIGN*MMAX)
        WPR=-2.D0*DSIN(0.5D0*THETA)**2
        WPI=DSIN(THETA)
        WR=1.D0
        WI=0.D0
        DO 13 M=1,MMAX,2
          DO 12 I=M,N,ISTEP
            J=I+MMAX
            TEMPR=real(WR)*DATA(J)-real(WI)*DATA(J+1)
            TEMPI=real(WR)*DATA(J+1)+real(WI)*DATA(J)
            DATA(J)=DATA(I)-TEMPR
            DATA(J+1)=DATA(I+1)-TEMPI
            DATA(I)=DATA(I)+TEMPR
            DATA(I+1)=DATA(I+1)+TEMPI
12        CONTINUE
          WTEMP=WR
          WR=WR*WPR-WI*WPI+WR
          WI=WI*WPR+WTEMP*WPI+WI
13      CONTINUE
        MMAX=ISTEP
      GO TO 2
      ENDIF
      RETURN
      END
      INTEGER FUNCTION TRMLEN(STRING)
      CHARACTER *(*) STRING
*
      ILAST = LEN(STRING)
      DO 1 I = ILAST,1,-1
        IVAL = ICHAR(STRING(I:I))
        IF(IVAL.GE.33 .AND. IVAL.LE.126)GOTO 2
 1    CONTINUE
      TRMLEN = ILAST
      RETURN
 2    TRMLEN = I
      RETURN
      END
***********
