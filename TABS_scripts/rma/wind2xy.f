      program wind2xy
C
      character*80 filein, fileout 
C
      write(*,*) 'enter a 1 to convert from mag,dir to x,y' 
      write(*,*) 'enter a 2 to convert from x,y to mag,dir'
      read(*,*) iconv
      if (iconv .eq. 1) then
        write(*,*) 'enter input file (in time,mag,dir format)'
        read(*,*) filein
        open(10,file=filein,form='formatted',status='old')
        write(*,*) 'enter output file (in time,x,y,format)'
        read(*,*) fileout
        open(20,file=fileout,form='formatted',status='unknown')
        do i = 1, 90000
          read(10,*,end=10) time,rmag,rdir
          rx = rmag*cos((270-rdir)/57.29578)
          ry = rmag*sin((270-rdir)/57.29578)
          write(20,100) time,rx,ry
 100      format(3f15.2)
        end do
 10     continue
      else if (iconv .eq. 2) then
        write(*,*) 'enter input file (in time,x,y format)'
        read(*,*) filein
        open(10,file=filein,form='formatted',status='old')
        write(*,*) 'enter output file (in time,mag,dir,format)'
        read(*,*) fileout
        open(20,file=fileout,form='formatted',status='unknown')
        do i = 1, 90000
          read(10,*,end=20) time,rx,ry
          rmag = sqrt(rx*rx+ry*ry+1.E-8)
          rdum = atan(ry/(rx+1.e-8))*57.29578
          if (rx .lt. 0.0) rdum = rdum + 180.
          rdir = 270. -  rdum
          write(20,200) time,rmag,rdir
 200      format(3f15.3)
        end do
 20     continue
      else
         write(*,*) 'enter a 1 or a 2, genius'
      end if
      close (10)
      close (20)
      stop
      end
