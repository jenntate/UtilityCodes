      PROGRAM LININTERPOLATE  
      parameter(nmax=500000,maxe=500000,maxfiles=1000)
C
      real time(nmax),tide(nmax),
     &     new_time(nmax),tstart, ndata, tinc, tend,
     &     ntimes, new_tide(nmax)
      integer i, j, n
      character filename*80,infile(maxfiles)*80
      character time_filename*80,tide_filename*80
      logical here

      write(*,*) 'enter number of time fields'
      read(*,*) ndata
      write(*,*)'enter file with original data to be
     &interpolated; time value'
      read(*,'(a)') time_filename
      open(8,file=time_filename,form='formatted',status='old')
      rewind(8)
      do n=1, ndata 
        read(8,*) time(n), tide(n)
      enddo


      write(*,*) 'enter the desired start time'
      read(*,*) tstart 
      write(*,*) 'enter the time increment'
      read(*,*) tinc 
      write(*,*) 'enter the desired end time'
      read(*,*) tend

      ntimes = (tend-tstart)/tinc 
      new_time(1) = tstart
      do i=1, ntimes
        new_time(i+1) = new_time(i) + tinc
      enddo

5     do 10 i=1, ntimes+1
       do 20 j=1,ndata
           if(new_time(i).LT.time(j+1) .AND. new_time(i).GE.time(j))then
            new_tide(i) = (new_time(i)-time(j))/(time(j+1)-
     &          time(j))*tide(j+1)+(time(j+1)-new_time(i))/
     &          (time(j+1)-time(j))*tide(j)
           endif
20       continue 
10     continue 



      write(*,*)
      write(*,*)
      write(*,*) 'enter output file name:'
      read(*,'(a)') filename
      open(12,file=filename,form='formatted',status='unknown')
      rewind(12)

      write(*,*)
      write(*,*)
    
      do i=1, ntimes+1 
        write(12, 100) new_time(i), new_tide(i)
      enddo

100   format(2F10.3)

      close(8)
      close(9)

      end
