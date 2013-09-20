      PROGRAM  VELGET
      parameter(nmax=200000,maxe=100000)
C
      DIMENSION CORD(NMAX,3),NOP(MAXE,8),IMAT(MAXE),
     *  xVEL(6,NMAX),ndry(NMAX),wsel(NMAX)
      dimension delbed(NMAX),bshr(NMAX)
      dimension vvel(NMAX),dfct(NMAX)
      DIMENSION  NODE(1000),NODES(1000),ITHD(1000),TIME(20000)
      dimension angls(1000), angl(1000)
      dimension ndep(nmax), nref(nmax)
      dimension nsnc(20000),isc(20000),vel(120,4,20000)
      dimension iresav(9)
      dimension ao(nmax)
      character filename*80 
      integer trmlen
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
      print *,'enter the ascii file with node numbers to be extracted'
      write(*,*) '(note: this file should consist of an integer n'
      write(*,*) 'followed by n node numbers'
      read(*,'(A)') filename
      open(2,file=filename,form='formatted',status='old')
      write(*,*)
      write(*,*) 'enter the ascii file with the direction angles'
      write(*,*) 'for positive velocity that correspond to the nodes'
      write(*,*) 'At each node, the magnitude of the velocity will be,'
      write(*,*) 'calculated and the direction angle + or - 90 degrees'
      write(*,*) 'will be used to determine if the velocity is positive' 
      write(*,*) 'or negative'
      write(*,*) '(note: this file should consist of an integer n'
      write(*,*) 'followed by n direction angles'
      read(*,'(A)') filename
      open(15,file=filename,form='formatted',status='old')
      print *,'enter the rma10 3d-binary geometry file'
      read(*,'(A)') filename
      open(4,file=filename,form='unformatted',status='old')
      print *,'enter the rma10 binary solution file to extract from'
      read(*,'(A)') filename
      open(luin,file=filename,form='unformatted',status='old')
      print *,'what will you call the extract output file?'
      read(*,'(A)') filename
      open(3,file=filename,form='formatted',status='unknown')
      print *,'Three spreadsheet type files will also be created.'
      print *,'One for surface velocity, one for mid-depth velocity,'
      print *,'and one for bottom velocity'
      print *,'what will you call the output spreadsheet file prefix?'
      read(*,'(A)') filename
      open(10,file=filename(1:trmlen(filename))//'_surface.txt',
     +form='formatted',status='unknown')
      open(20,file=filename(1:trmlen(filename))//'_middepth.txt',
     +form='formatted',status='unknown')
      open(30,file=filename(1:trmlen(filename))//'_bottom.txt',
     +form='formatted',status='unknown')
      print *,'what are the minimum and maximum integer time steps'
      print *,'to be extracted? (maximum = 20000 steps)'
      read(*,*) itsmin, itsmax
      if (itsmin .lt. 0) itsmin = 0
      if (itsmax .gt. 20000) itsmax = 20000  
      write(*,*) 
c
      read (4) np, ne, npm, nes,
     *     ((dum, dum, k=1,3), dum ,dum,
     *     dum, dum, j = 1, np), (ndep(j),
     *     nref(j), j = 1, npm)
C
      read (2,*) nnodes
      read (2,*) (nodes(k),k=1,nnodes)
      read (15,*) nang
      if (nang .ne. nnodes) then
        write(*,*) 'STOP: the number of angles must'
        write(*,*) 'equal the number of nodes '
        stop
      end if
      read (15,*) (angls(k),k=1,nnodes)
      nnode = nnodes
      n = 0
      do ii = 1, nnodes
        n = n + 1
        ithd(n) = 0
        i = nodes(ii)
        rdum = angls(ii)
        node(n) = i 
        angl(n) = rdum
        k = nref(i) + 1
        if (k .gt. 1) then
          l = nref(i) + ndep(i) - 1
          if (l .ge. k) then
            ll = nref(i) + ndep(i)/2 
            ithd(n) = 1
            n = n + 1
            nnode = nnode + 1
            node(n) = ll 
            angl(n) = rdum
            ithd(n) = 2
            n = n + 1
            nnode = nnode + 1
            node(n) = l
            angl(n) = rdum
            ithd(n) = 2 
          end if
        end if
      end do
C
      DO 100 I=1,ITSMAX
        READ (11,END=110)  TET, NP, NDf, NE,
     &       NDFS,(IRESAV(K),K=1,NDFS),
     &       ((XVEL(K,J),J = 1, IRESAV(K)),K=1,NDF),
     &       (WSEL(J),J = 1, IRESAV(3)),
     &       (DUM, J = 1, NE), (DUM, J = 1, NP),
     &       (DELBED(J),J=1,IRESAV(7)),(BSHR(J),J=1,IRESAV(NDFS)),
     &       (VVEL(J), J = 1, NP), (DFCT(J),
     &       J = 1, NE)
*
        if (i .lt. itsmin) go to 100
        write(*,*) 'processing time step ', i
        nt = nt + 1
        time(nt) = tet
        do 50 j=1,nnode
          if (ithd(j) .eq. 0) then
            ry = xvel(2,node(j))
            rx = xvel(1,node(j))
            rval = sqrt(rx*rx+ry*ry+1.E-8)
            rdum = rx*cos(angl(j)/57.29578)+ry*sin(angl(j)/57.29578) 
            if (rdum .lt. 0.0) rval = -rval
            vel(j,1,nt) = rval 
            vel(j,2,nt) = rval 
            vel(j,3,nt) = rval 
          else if (ithd(j) .eq. 1) then
            ry = xvel(2,node(j))
            rx = xvel(1,node(j))
            rval = sqrt(rx*rx+ry*ry+1.E-8)
            rdum = rx*cos(angl(j)/57.29578)+ry*sin(angl(j)/57.29578) 
            if (rdum .lt. 0.0) rval = -rval
            vel(j,1,nt) = rval 
            ry = xvel(2,node(j+1))
            rx = xvel(1,node(j+1))
            rval = sqrt(rx*rx+ry*ry+1.E-8)
            rdum = rx*cos(angl(j)/57.29578)+ry*sin(angl(j)/57.29578) 
            if (rdum .lt. 0.0) rval = -rval
            vel(j,2,nt) = rval 
            ry = xvel(2,node(j+2))
            rx = xvel(1,node(j+2))
            rval = sqrt(rx*rx+ry*ry+1.E-8)
            rdum = rx*cos(angl(j)/57.29578)+ry*sin(angl(j)/57.29578) 
            if (rdum .lt. 0.0) rval = -rval
            vel(j,3,nt) = rval 
          end if
   50   continue
  100 continue
  110 continue
c
      icount = 0
      do i = 1, nnode
        if (ithd(i) .ne. 2) then
          icount = icount + 1
          nsnc(icount) = node(i)
          isc(icount)= i
        end if
      end do 
C      
      do 200 i= 1,nnodes
        write(3,2000) 
 2000   format('   node  ', 
     +       '     time    surface vel    mid-depth vel   ',
     +       'bottom vel'    )
        do 150 j=1,nt
        write(3,3001) nsnc(i),time(j),(vel(isc(i),k,j),k=1,3)
 3001   format(i8,f12.2,f8.3,7x,f8.3,7x,f8.3)
  150   continue
  200 continue
      do j=1,nt
        write(10,4000) time(j), (vel(isc(i),1,j),i=1,nnodes)
        write(20,4000) time(j), (vel(isc(i),2,j),i=1,nnodes)
        write(30,4000) time(j), (vel(isc(i),3,j),i=1,nnodes)
 4000  format(20001f12.2)
      end do
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
