      PROGRAM  convert_tabs_to_adh
      parameter(nmax=300000,maxe=200000)
C
      DIMENSION CORD(NMAX,3),NOP(MAXE,20),IMAT(MAXE),
     *  XVEL(6,NMAX),ndry(NMAX),wsel(NMAX),ncorn(maxe)
      dimension nops(maxe,8), nbot(NMAX)
      dimension delbed(NMAX),bshr(NMAX)
      dimension vvel(NMAX),dfct(NMAX)
      dimension ndep(nmax), nref(nmax)
      dimension iresav(9)
      dimension ao(nmax)
      dimension iahnode(nmax),irmnode(nmax),iahelem(maxe,3)
      dimension adhvx(nmax),adhvy(nmax),adhdep(nmax)
      character filename*80 
      integer trmlen, layer
      DIMENSION IREC(40),FREC(40)
      dimension ipackt(77),ipacktb(1200)
      CHARACTER*80 BANGFGN(5),STAMP,DESC(2),BANSTUD(5),BANRMA2(5)
      CHARACTER VERRMA2(4)*40,TITLE*77,FNAME*80,FGFGEN*80
C
c
      icext = 0
      luin = 11
      ndf = 6
      nt = 0
      print *,'enter the TABS-MDS 3d-binary geometry file'
      read(*,'(A)') filename
      open(4,file=filename,form='unformatted',status='old')
      print *,'enter the TABS-MDS binary solution file '
      read(*,'(A)') filename
      open(luin,file=filename,form='unformatted',status='old')
      print *,'enter 1 for surface data and 2 for bottom data '
      read(*,*) layer 
      print *,'what is the desired adh output prefix?'
      read(*,'(A)') filename
      open(10,file=filename(1:trmlen(filename))//'.3dm',
     +form='formatted',status='unknown')
      open(20,file=filename(1:trmlen(filename))//'_ovl.dat',
     +form='formatted',status='unknown')
      open(30,file=filename(1:trmlen(filename))//'_dep.dat',
     +form='formatted',status='unknown')
      print *,'what are the minimum and maximum integer time steps'
      print *,'to be extracted? (maximum = 20000 steps)'
      read(*,*) itsmin, itsmax
      if (itsmin .lt. 0) itsmin = 0
      if (itsmax .gt. 20000) itsmax = 20000  
      print *,'finally, enter a 0 if you want the units'
      print *,'                   to stay the same'
      print *,'         enter a 1 if you want to convert'
      print *,'                   from English to Metric'
      read(*,*) iconvrt 
      rconvrt = 1.0
      if (iconvrt .eq. 1) rconvrt = 0.3048
      write(*,*)
c
      read (4) np, ne, npm, nes,
     *     ((cord(j,k), dum, k=1,3), dum ,idum,
     *     ao(j), idum, j = 1, np), (ndep(j),
     *     nref(j), j = 1, npm), ((nop(j,k), k=1,20),
     *     ncorn(j), imat(j), dum, idum, j=1, ne)

      do i = 1, np
        do j = 1, 3
          cord(i,j) = cord(i,j) * rconvrt
        end do
        ao(i) = ao(i) * rconvrt
      end do

      do n = 1, maxe
        do m = 1, 8
          nops(n,m) = 0
        end do
      end do

      do n = 1, nes
        mm = 0
        ncn = ncorn(n)
        do m = 1, ncn
          if (nop(n,m) .le. npm .and. nop(n,m) .gt. 0) then
            mm = mm + 1
            nops(n,mm) = nop(n,m)
            if (nref(nops(n,mm)) .gt.0) then
              nbot(nops(n,mm)) = nref(nops(n,mm)) + ndep(nops(n,mm)) - 1
            else
              nbot(nops(n,mm)) = nops(n,mm)
            end if
          end if
        end do
        ncorn(n) = mm
      end do

      do i = 1, nmax
        iahnode(i) = 0
        irmnode(i) = 0
      end do
      do i = 1, maxe
        do j = 1, 3
          iahelem(i,j) = 0
        end do
      end do
      k = 1
      l = 1
      do i = 1, nes
        if (imat(i) .gt. 0) then
          do j = 1, ncorn(i), 2
            iskip = 0
            if (k .ge. 1) then
              do ii = 1, k-1
                if (nops(i,j) .eq. irmnode(ii)) then
                  iskip = 1
                end if
              end do
            end if
            if (iskip .eq. 0) then
              iahnode(nops(i,j)) = k
              if (layer .eq. 1) then
                irmnode(k) = nops(i,j)
              else if (layer .eq. 2) then
                irmnode(k) = nbot(nops(i,j))
              else
                print *,' LAYER INFORMATION IS BAD: EXITING'
                go to 110
              end if
              k = k + 1
            end if
          end do
        end if
      end do
      do i = 1, nes
        if (imat(i) .gt. 0) then
          if (ncorn(i) .le. 6) then
            iahelem(l,1) = iahnode(nops(i,1))
            iahelem(l,2) = iahnode(nops(i,3))
            iahelem(l,3) = iahnode(nops(i,5))
            l = l + 1
          else
            iahelem(l,1) = iahnode(nops(i,1))
            iahelem(l,2) = iahnode(nops(i,3))
            iahelem(l,3) = iahnode(nops(i,5))
            l = l + 1
            iahelem(l,1) = iahnode(nops(i,5))
            iahelem(l,2) = iahnode(nops(i,7))
            iahelem(l,3) = iahnode(nops(i,1))
            l = l + 1
          end if
        end if
      end do

      nadhn = k-1
      nadhe = l-1
C
      write(10,900) 
      imatd = 1
      do i = 1, nadhe
        write(10,1010) i, iahelem(i,1), iahelem(i,2), 
     +  iahelem(i,3), imatd
      end do
      do i = 1, nadhn
        write(10,1100) i, cord(irmnode(i),1), 
     +  cord(irmnode(i),2), ao(irmnode(i)) 
      end do


  900 format('MESH2D')
 1010 format('E3T',1X,I8,1X,I8,1X,I8,1X,I8,1X,I8)
 1100 format('ND',1X,I8,1X,f15.2,1X,f15.2,1X,f15.2)


      write(20,1110)
      write(20,1120)
      write(20,1130)
      write(20,1200) nadhn
      write(20,1300) nadhe
      write(20,1140) 
      write(30,1110)
      write(30,1120)
      write(30,1150) 
      write(30,1200) nadhn
      write(30,1300) nadhe
      write(30,1160) 

 1110 format('DATASET')
 1120 format('OBJTYPE "mesh2d"')
 1130 format('BEGVEC')
 1140 format('NAME "Overland Velocity"')
 1150 format('BEGSCL')
 1160 format('NAME "Depth"')

 1200 format('ND',1X,I8)
 1300 format('NC',1X,I8)


      DO 100 I=1,ITSMAX
        READ (11,END=110)  TET, NP, NDf, NE,
     &       NDFS,(IRESAV(K),K=1,NDFS),
     &       ((XVEL(K,J),J = 1, IRESAV(K)),K=1,NDF),
     &       (WSEL(J),J = 1, IRESAV(3)),
     &       (IDUM, J = 1, NE), (IDUM, J = 1, NP),
     &       (DELBED(J),J=1,IRESAV(7)),(BSHR(J),J=1,IRESAV(NDFS)),
     &       (VVEL(J), J = 1, NP), (DFCT(J),
     &       J = 1, NE)
*
        if (i .lt. itsmin) go to 100
        write(*,*) 'processing time step ', i
        nt = nt + 1
        time = tet*3600.
        do j=1,nadhn
           adhvx(j)  = xvel(1,irmnode(j)) * rconvrt
           adhvy(j)  = xvel(2,irmnode(j)) * rconvrt
           adhdep(j) = xvel(3,irmnode(j)) * rconvrt
        end do

        rzdum = 0.0
        write(20,1400) time
        write(30,1400) time
        do j = 1, nadhn
          write(20,1500) adhvx(j), adhvy(j), rzdum
          write(30,1600) adhdep(j)
        end do
 1400   format('TS 0',1X,f15.2)
 1500   format(3(1X,f11.3))
 1600   format(1X,f11.4)

  100 continue
  110 continue

      write(20,*) 'ENDDS'
      write(30,*) 'ENDDS'

      close(10)
      close(20)
      close(30)
      stop
      end
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
