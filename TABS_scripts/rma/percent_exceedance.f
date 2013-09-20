      program percent_exceedance 
C
C     this program creates a percent exceedance analysis of data 
C
      parameter(nts=10000,nop=100,nth=101)
      dimension time(nts), value(nts,nop), valthr(nth)
      dimension rexceed(nth,nts)
      character*80 fname
C
      write(*,*)
      write(*,*) 'HSA '
      write(*,*)
      write(*,*) 'Enter the input file name'
      write(*,*) 'It should be a spreadsheet style txt file'
      read(*,*) fname
      open(10,file=fname,form='formatted',status='old')
      write(*,*) 'Enter the number of columns of data in the file'
      write(*,*) '(not including the time column)'
      read(*,*) ncol
      write(*,*) 'Enter the minimum and maximum range for the value'
      read(*,*) vmin, vmax
      write(*,*) 'If you want to evaluate absolute values ',
     +           '(as for velocity) enter a 1'
      write(*,*) 'Otherwise, enter a 0'
      read(*,*) iavflag
      write(*,*) 'Enter the percent exceedance output file name'
      read(*,*) fname
      open(20,file=fname,form='formatted',status='unknown')
C
      do i = 1, nts
        read(10,*,end=100) time(i), (value(i,j),j=1,ncol) 
      end do
 100  numstep = i - 1
C
      valinc = (vmax - vmin)/float(nth-1)
      do i = 1, nth
        valthr(i) = vmin+float(i-1)*valinc
      end do
C
      if (iavflag .eq. 1) then
        do i = 1, numstep
          do j = 1, ncol
            value(i,j) = abs(value(i,j))
          end do
        end do
      end if
C
      do i = 1, nth
        do j = 1, ncol
          rexceed(i,j) = 0.0
        end do
      end do
C
      do j = 1, ncol
        do i = 1, numstep
          do k = 1, nth
            if (value(i,j) .gt. valthr(k)) then
              rexceed(k,j) = rexceed(k,j) + 1.0 
            end if 
          end do
        end do
        do k = 1, nth
          rexceed(k,j) = 100.*rexceed(k,j)/float(numstep)
        end do
      end do
C
      do i = 1, nth
        write(20,1000) valthr(i),(rexceed(i,j),j=1,ncol)
      end do
 1000 format(101f8.2)
C
      close(10)
      close(20)
C
      stop
      end
