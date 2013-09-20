      PROGRAM  NODE4GET
      parameter(nmax=50000,maxe=18000)
C
      DIMENSION CORD(NMAX,3),NOP(MAXE,8),IMAT(MAXE),
     *  xVEL(3,NMAX),ndry(NMAX),wsel(NMAX)
      DIMENSION  NODE(100),TIME(2000)
      dimension efm(100,2000),vel(100,3,2000)
      dimension ao(nmax),conc1(nmax)
      character filename*80
      DIMENSION IREC(40),FREC(40)
      dimension ipackt(77),ipacktb(1200)
      CHARACTER*80 BANGFGN(5),STAMP,DESC(2),BANSTUD(5),BANRMA2(5)
      CHARACTER VERRMA2(4)*40,TITLE*77,FNAME*80,FGFGEN*80
C
c
      luin = 11
      ndf = 4
      print *,'enter the ascii file with node numbers to be extracted'
      read(*,'(A)') filename
      open(2,file=filename,form='formatted',status='old')
      print *,'what is the rma4 binary file to extract from'
       read(*,'(A)') filename
      open(luin,file=filename,form='unformatted',status='old')
      print *,'what will you call the extract output file?'
      read(*,'(A)') filename
      open(3,file=filename,form='formatted',status='unknown')
c
       read (2,*) nnode
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
        print *,' mflgeo, iverid,npgeo,negeo = ',
     *             mflgeo,iverid,npgeo,negeo 
        read(11)iwrt1,(ipackt(i),i=1,iwrt1)
        print *,' iwrt2, imrt3 = ',iwrt2,iwrt3
        read(11)iwrt2,iwrt3,(irec(i),i=1,iwrt2),
     &  (frec(i),i=1,iwrt3)
        read(11)iwrt4,(ipackt(i),i=1,iwrt4)
        print *,' iwrt4 = ', iwrt4
      endif
C
C 
      DO 100 I=1,2000
C
        READ (11,END=110)
     &   TETB, NQAL, NPX,((CONC1(J),J=1,NPX),K=1,NQAL)
C
        print *,'  TETB = ', tetb
c
      time(i) = tetb
      nt = nt + 1
      do 50 j=1,nnode
      vel(j,1,nt) = conc1(node(j))
   50 continue
  100 continue
  110 continue
c
c      do 200 i= 1,nnode
      write(3,3000) nt,(node(i),i=1,nnode)
 3000 format('2d node number = ',i10)
      do 150 j=1,nt
      write(3,3001) time(j),(vel(i,1,j),i=1,nnode)
 3001 format(f8.2,30f6.2)
  150 continue
c  200 continue
 8887 continue
      stop
      end
