      PROGRAM WINDINTERPOLATE  
      parameter(nmax=500000,maxe=500000,maxfiles=1000)
C
      real time(nmax),wind_x(nmax),wind_y(nmax),wind_speed(nmax),
     &     wind_dir(nmax),new_time(nmax),tstart, ndata, tinc, tend,
     &     ntimes, new_wind_speed(nmax), new_wind_dir(nmax),
     &     new_wind_x(nmax), new_wind_y(nmax)
      integer i, j, n
      character filename*80,infile(maxfiles)*80
      character time_filename*80,speed_filename*80,dir_filename*80
      logical here

      write(*,*) 'enter number of time fields'
      read(*,*) ndata
      write(*,*)'enter file with original time data to be
     &interpolated'
      read(*,'(a)') time_filename
      open(8,file=time_filename,form='formatted',status='old')
      rewind(8)
      do n=1, ndata 
        read(8,*) time(n)
      enddo
      write(*,*)'enter file with original wind speed to be
     &interpolated'
      read(*,'(a)') speed_filename
      open(9,file=speed_filename,form='formatted',status='old')
      rewind(9)
      do n=1,ndata 
        read(9,*) wind_speed(n)
      enddo
      write(*,*)'enter file with original wind direction 
     &to be interpolated'
      read(*,'(a)') dir_filename
      open(10,file=dir_filename,form='formatted',status='old')
      rewind(10)
      do n=1,ndata 
        read(10,*) wind_dir(n)
      enddo

      do n=1, ndata
        wind_x(n)= wind_speed(n)*COS(wind_dir(n)*3.14159/180.0)
        wind_y(n)= wind_speed(n)*SIN(wind_dir(n)*3.14159/180.0)
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
            new_wind_x(i) = (new_time(i)-time(j))/(time(j+1)-
     &          time(j))*wind_x(j+1)+(time(j+1)-new_time(i))/
     &          (time(j+1)-time(j))*wind_x(j)
            new_wind_y(i) = (new_time(i)-time(j))/(time(j+1)-
     &          time(j))*wind_y(j+1)+(time(j+1)-new_time(i))/
     &          (time(j+1)-time(j))*wind_y(j)
           endif
20       continue 
10     continue 

      do i=1,ntimes+1
        new_wind_speed(i)=(new_wind_x(i)*new_wind_x(i)+new_wind_y(i)
     &        *new_wind_y(i))**0.5
        new_wind_dir(i)=ATAN(new_wind_y(i)/new_wind_x(i))
     &     *(180.0/3.14159)
        if(new_wind_x(i).EQ.0.0)then
          new_wind_dir(i)=0.0
        endif
        if(new_wind_x(i).LT.0.0)then
          new_wind_dir(i) = new_wind_dir(i)+180
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
        write(12, 100) new_time(i), new_wind_speed(i), new_wind_dir(i),
     &      new_wind_x(i), new_wind_y(i)
      enddo

100   format(5F10.3)

      close(8)
      close(9)
      close(10)
      close(12)

      end
