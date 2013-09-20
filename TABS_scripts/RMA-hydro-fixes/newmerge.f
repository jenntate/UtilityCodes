      PROGRAM neWMERG
      parameter(nmax=500000,maxe=500000,maxfiles=1000)
C
      real xvel(10,nmax),wsel(nmax),delbed(nmax),bshr(nmax),vvel(nmax),
     &     dfct(nmax),frec(40),tstart(maxfiles)
      integer ndep(nmax), nref(nmax),imat(maxe), ndry(nmax),iresav(10),
     &        irec(40)
      character filename*80,infile(maxfiles)*80
      logical here

      write(*,*) 'enter file with filenames to be merged'
      write(*,*) '(note: this file should consist of the following:'
      write(*,*) ' an integer n, followed by n file names)' 
      read(*,'(a)') filename

      write(*,*)
      write(*,*)

      open(8,file=filename,form='formatted',status='old')
      rewind(8)
      read(8,*) nfiles
      if(nfiles.gt.maxfiles) then
         write(*,*) 'Too many file names in ',filename
         stop
      endif
      do n=1,nfiles
         read(8,'(a)') infile(n)
         inquire(file=infile(n), exist=here)
         if(.not. here) then
           write(*,*) "Cannot find: ",infile(n)
           stop
         else
           open(10,file=infile(n),form='unformatted',status='old')
           rewind(10)
           read(10)  tstart(n)
           close(10)
           ilgth=index(infile(n),' ')-1
           write(*,*) infile(n)(1:ilgth)," tstart: ",tstart(n)
         endif
      enddo
      tstart(nfiles+1)=100000000.
      close(8)

      write(*,*)
      write(*,*)

      write(*,*) 'enter merged file name:'
      read(*,'(a)') filename
      open(12,file=filename,form='unformatted',status='unknown')
      rewind(12)

      write(*,*)
      write(*,*)

      do n=1,nfiles
        open(10,file=infile(n),form='unformatted',status='old')
        rewind(10)
        istrt=0
        do i=1,9999999
          read(10,end=220)
     &        tet, np, ndf, ne, ndfs,(iresav(k),k=1,ndfs),
     &        ((xvel(k,j),j = 1, iresav(k)),k=1,ndf),
     &        (wsel(j),j=1,iresav(3)),(imat(j),j=1,ne),(ndry(j),j=1,np),
     &        (delbed(j),j=1,iresav(7)),(bshr(j),j=1,iresav(ndfs)),
     &        (vvel(j), j = 1, np), (dfct(j),j = 1, ne)

          if (tet .lt. tstart(n+1)) then
            if(istrt.eq.0) then
              tfst=tet
              istrt=1
            endif
            write(12)
     &        tet, np, ndf, ne, ndfs,(iresav(k),k=1,ndfs),
     &        ((xvel(k,j),j = 1, iresav(k)),k=1,ndf),
     &        (wsel(j),j=1,iresav(3)),(imat(j),j=1,ne),(ndry(j),j=1,np),
     &        (delbed(j),j=1,iresav(7)),(bshr(j),j=1,iresav(ndfs)),
     &        (vvel(j), j = 1, np), (dfct(j),j = 1, ne)
              tlst=tet
          else
            go to 220
          endif

        enddo
  220   continue
        close(10)
        il=index(infile(n),' ')-1
        write(*,'(a)') "***** "//infile(n)(1:il)
        write(*,902) tfst,tlst
  902   format("Starttime: ",f8.2,"    Endtime: ",f8.2)

      enddo

  110 continue

      end
