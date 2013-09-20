      PROGRAM  NODE3DGET
      parameter(nmax=60000,maxe=25000)
C
      DIMENSION CORD(NMAX,3),NOP(MAXE,8),IMAT(MAXE),
     *  xVEL(3,NMAX),ndry(NMAX),wsel(NMAX)
      DIMENSION  NODE(100),TIME(5000)
      dimension efm(100,5000),vel(100,3,5000)
      dimension ao(nmax),wsmin(100),wsmax(100),trange(100)
      dimension timehw(100),timelw(100)
      character filename*80
      DIMENSION IREC(40),FREC(40)
      dimension ipackt(1200),ipacktb(1200)
      CHARACTER*80 BANGFGN(5),STAMP,DESC(2),BANSTUD(5),BANRMA2(5)
      CHARACTER VERRMA2(4)*40,TITLE*77,FNAME*80,FGFGEN*80
C
      data wsmin/100*9999./
      data wsmax/100*-9999./
c
      luin = 11
      ndf = 4
      print *,'enter the ascii file with node numbers to be extracted'
      read(*,'(A)') filename
      open(2,file=filename,form='formatted',status='old')
      print *,'what is the rma2 binary file to extract from'
       read(*,'(A)') filename
      open(luin,file=filename,form='unformatted',status='old')
      print *,'what will you call the extract output file?'
      read(*,'(A)') filename
      open(3,file=filename,form='formatted',status='unknown')
c
       read (2,*) nnode
       print *,'processing in ',nnode,' nodes'
      read (2,*) (node(k),k=1,nnode)
c
*
      read (11) itest
      rewind (11)
      if(itest.gt. 200) then
*
        READ (11) (BANGFGN(I),I=1,5),(VERRMA2(I),I=1,4),STAMP,
     1     (DESC(I),I=1,2),(BANSTUD(I),I=1,5)
        READ (11) (IREC(I),I=1,40),(FREC(I),I=1,40)
        READ (11) TITLE
	write(*,'(a)') title
      else
        read(11)mflgeo,iverid,npgeo,negeo
        print *,'mflgeo,iverid,npgeo,negeo = ',
     *           mflgeo,iverid,npgeo,negeo
        read(11)iwrt1,(ipackt(i),i=1,iwrt1)
        print *,' iwrt1 = ', iwrt1
        read(11)iwrt2,iwrt3,(irec(i),i=1,iwrt2),
     &  (frec(i),i=1,iwrt3)
        print *,' iwrt2,iwrt3 = ', iwrt2,iwrt3
     
        read(11)iwrt4,(ipackt(i),i=1,iwrt4)
        print *,' iwrt4 = ', iwrt4
      endif
C
      print *,'banners read; ne,np = ',negeo,npgeo
C 
       nt = 0
      DO 100 I=1,5000
C
          READ (11,END=110,ERR=110) TET,NP,((xVEL(J,K),J=1,3),K=1,NP),
     1       (NDRY(K),K=1,NP), NE, (IMAT(L),L=1,NE),(WSEL(K),K=1,NP)

c
       print *,'Read time step; tet = ', tet
      time(i) = tet
      nt = nt + 1
      do 50 j=1,nnode
      vel(j,1,nt) = xvel(1,node(j))
      vel(j,2,nt) = xvel(2,node(j))
      vel(j,3,nt) = wsel(node(j))
      if(vel(j,3,nt) .lt. wsmin(j)) then
            wsmin(j) = vel(j,3,nt)
            timelw(j) = tet
      endif
      if(vel(j,3,nt) .gt. wsmax(j)) then
            wsmax(j) = vel(j,3,nt)
            timehw(j) = tet
      endif
      efm(j,nt) = 1.
   50 continue
  100 continue
  110 continue
c
      print *,' read ',nt,' time steps'  
c
      tref = timehw(1)
      do 200 i= 1,nnode
      write(3,3000) node(i)
 3000 format('2d node number = ',i10)
      do 150 j=1,nt
      write(3,3001) time(j),(vel(i,k,j),k=1,3),efm(i,j)
 3001 format(6f12.2)
  150 continue
      trange(i) = wsmax(i) - wsmin(i)
      timehw(i) = timehw(i) - tref
  155 lml = 1
      if( timehw(i) .lt.  -2. )then
         timehw(i) = timehw(i) + 12.5
         goto 155
      endif
      timelw(i) = timelw(i) - tref
  170 lml = 1
      if(timelw(i) .lt. timehw(i)) then
         timelw(i) = timelw(i) + 12.5
         goto 170
      endif
  200 continue
c      goto 8887
      write(44,4401) (node(k),k=1,nnode)
 4401 format(25i6)
      do 500 i=1,nt
      write(44,4400) time(i),(vel(k,3,i),k=1,nnode) 
 4400 format(f8.1,25f7.2)
  500 continue
      write(44,4402) (trange(k),k=1,nnode)
 4402 format('range = ',25f7.2)
      write(44,4403) (timelw(k),k=1,nnode)
 4403 format('t-lw  = ',25f7.2)
      write(44,4404) (timehw(k),k=1,nnode)
 4404 format('t-hw  = ',25f7.2)
 8887 continue
      print *,'close up files'
      close (44)
      close(2)
      close(3)
      close(11)
      stop
      end
