      program grab_bed_fraction
      character*80 chardum, fname
      character *6 ftype
      dimension gc(10)
      data ftype /"BEGSCL"/
C
      write(*,*) 'enter input file name'
      read(*,*) fname
      open (10,file=fname,form='formatted',status='old')
      write(*,*) 'enter output file name'
      read(*,*) fname
      open (20,file=fname,form='formatted',status='unknown')
      write(*,*) 'enter number of grain classes in the file'
      read(*,*) ngc
      write(*,*) 'enter desired grain class'
      read(*,*) ndgc
C
      read(10,1000) chardum
      write(20,1000) chardum 
      read(10,1000) chardum
      write(20,1000) chardum
      read(10,1000) chardum
      write(20,1000) ftype 
      read(10,1000) chardum
      write(20,1000) chardum
      read(10,1000) chardum
      write(20,1000) chardum
      read(10,1000) chardum
      write(20,1000) chardum
      read(10,1000) chardum
      write(20,1000) chardum

      do i = 1, 90000
        do j = 1, 900000
          read(10,*,end=200,err=100) (gc(k),k=1,ngc)
          write(20,*) gc(ndgc)
        end do
 100    backspace (10) 
        read(10,1000) chardum
        write(20,1000) chardum
      end do
 200  continue

 1000 format (a)
      
      stop
      end
