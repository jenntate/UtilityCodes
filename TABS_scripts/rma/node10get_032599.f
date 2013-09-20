      PROGRAM  NODE10GET
      parameter(nmax=60000,maxe=30000)
C
      DIMENSION CORD(NMAX,3),NOP(MAXE,8),IMAT(MAXE),
     *  xVEL(6,NMAX),ndry(NMAX),wsel(NMAX)
      dimension delbed(NMAX),bshr(NMAX)
      dimension vvel(NMAX),dfct(NMAX)
      DIMENSION  NODE(1000),NODES(1000),TIME(10000)
      dimension ndep(nmax), nref(nmax)
      dimension efm(100,10000),vel(100,9,10000)
      dimension iresav(9)
      dimension ao(nmax)
      character filename*80
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
      write(*,*) '(note: this file should consist of the following:'
      write(*,*) ' an integer n, followed by n node numbers)' 
      read(*,'(A)') filename
      open(2,file=filename,form='formatted',status='old')
      print *,'enter the rma10 3d-binary geometry file'
      read(*,'(A)') filename
      open(4,file=filename,form='unformatted',status='old')
      print *,'enter the rma10 binary solution file to extract from'
      read(*,'(A)') filename
      open(luin,file=filename,form='unformatted',status='old')
      print *,'what will you call the extract output file?'
      read(*,'(A)') filename
      open(3,file=filename,form='formatted',status='unknown')
      print *,'what are the minimum and maximum integer time steps'
      print *,'to be extracted? (maximum = 10000 steps)'
      read(*,*) itsmin, itsmax
      if (itsmin .lt. 0) itsmin = 0
      if (itsmax .gt. 10000) itsmax = 10000  
      write(*,*) 
c
      read (4) np, ne, npm, nes,
     *     ((dum, dum, k=1,3), dum ,dum,
     *     dum, dum, j = 1, np), (ndep(j),
     *     nref(j), j = 1, npm)
C
      read (2,*) nnodes
      read (2,*) (nodes(k),k=1,nnodes)
      nnode = nnodes
      n = 0
      do ii = 1, nnodes
        n = n + 1
        i = nodes(ii)
        node(n) = i 
        k = nref(i) + 1
        if (k .gt. 1) then
          l = nref(i) + ndep(i) - 1
          if (l .ge. k) then
            do m = k, l
              n = n + 1
              nnode = nnode + 1
              node(n) = m 
            end do
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
          vel(j,1,nt) = xvel(1,node(j))
          vel(j,2,nt) = xvel(2,node(j))
          vel(j,3,nt) = xvel(3,node(j))
          vel(j,4,nt) = wsel(node(j))
          vel(j,5,nt) = xvel(4,node(j))
          vel(j,6,nt) = xvel(5,node(j))
          vel(j,7,nt) = xvel(6,node(j))
          vel(j,8,nt) = delbed(node(j))
          vel(j,9,nt) = bshr(node(j))
   50   continue
  100 continue
  110 continue
c
      n = 0
      do 200 i= 1,nnode
      if (node(i) .le. npm .or. i .eq. 1) n = n + 1
C      write(3,3000) node(i), nodes(n)
C 3000 format('node number = ',i10, ' surface node number = ',i10)
      write(3,2000) 
 2000 format('   node  surf node', 
     +       '     time        xvel        yvel        depth',
     +       '       wsel      salt        temp       sediment',
     +       '    delbed     bed shear')
      do 150 j=1,nt
      write(3,3001) node(i),nodes(n),time(j),(vel(i,k,j),k=1,9)
 3001 format(2i8,5f12.2,5f12.7)
  150 continue
  200 continue
      stop
      end
