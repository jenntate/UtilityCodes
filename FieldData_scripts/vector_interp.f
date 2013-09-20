      PROGRAM VECTORINTERPOLATE  
      parameter(nmax=500000,maxe=500000,maxfiles=1000)
C
      real time(nmax),x_vect(nmax),y_vect(nmax), mag(nmax),
     &     new_time(nmax),tstart, ndata, tinc, tend, dir(nmax),
     &     ntimes, new_x_vect(nmax), new_y_vect(nmax),
     &     new_vect_speed(nmax), new_vect_dir(nmax)
      integer i, j, n
      character filename*80,infile(maxfiles)*80
      character time_filename*80,speed_filename*80,dir_filename*80
      logical here

      write(*,*) 'enter number of time fields'
      read(*,*) ndata
      write(*,*)'enter file with original data to be
     &interpolated'
      read(*,'(a)') filename
      open(8,file=filename,form='formatted',status='old')
      rewind(8)
      do n=1, ndata 
        read(8,*) time(n),mag(n),dir(n),x_vect(n),y_vect(n)
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
            new_x_vect(i) = (new_time(i)-time(j))/(time(j+1)-
     &          time(j))*x_vect(j+1)+(time(j+1)-new_time(i))/
     &          (time(j+1)-time(j))*x_vect(j)
            new_y_vect(i) = (new_time(i)-time(j))/(time(j+1)-
     &          time(j))*y_vect(j+1)+(time(j+1)-new_time(i))/
     &          (time(j+1)-time(j))*y_vect(j)
           endif
20       continue 
10     continue 

      do i=1,ntimes+1
        new_vect_speed(i)=SQRT(new_x_vect(i)*new_x_vect(i)+
     &          new_y_vect(i)*new_y_vect(i))
        new_vect_dir(i)=ATAN(new_y_vect(i)/new_x_vect(i))
     &          *(180.0/3.14159)
        if(new_x_vect(i).EQ.0.0)then
          new_vect_dir(i)=0.0
        endif
        if(new_x_vect(i).LT.0.0)then
          new_vect_dir(i) = new_vect_dir(i)+180
        endif
      enddo
        

      write(*,*)
      write(*,*)
      write(*,*) 'enter output file name:'
      read(*,'(a)') filename
      open(12,file=filename,form='formatted',status='unknown')
      rewind(12)

      write(*,*)
      write(*,*)
    
      do i=1, ntimes+1 
        write(12, 100) new_time(i), new_vect_speed(i), new_vect_dir(i),
     &      new_x_vect(i), new_y_vect(i)
      enddo

100   format(5F10.3)

      close(8)
      close(12)

      end
