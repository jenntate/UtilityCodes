      program adh_grab_desired_column 
      dimension rgcd(900000,3),rgcdi(100),rpfv(100),rdia(100)
      dimension rgsout(100)
      dimension isizor(100)
      character*80 chardum, fname
      character*1 chone
      character*2 chtwo
      character*4 chfour
C
      write(*,*)
      write(*,*) 'opens a grain distribution file and'
      write(*,*) 'writes another file with requested'
      write(*,*) 'percent finer values'
      write(*,*)
      write(*,*) 'enter input file name'
      read(*,*) fname
      open(10,file=fname,form='formatted',status='old')
      write(*,*) 'enter output file name'
      read(*,*) fname
      write(*,*) 'enter the number of columns of data'
      read (*,*) ncol
      write(*,*) 'enter the diameter asscoated with'
      write(*,*) 'each column of data'
      read(*,*) (rdia(i),i=1,ncol)
      write(*,*) 'enter desired number of output'
      write(*,*) 'percent finer values'
      read (*,*) ipfval
      write(*,1100) ipfval
 1100 format('enter ', I2, ' percent finer values')
      read(*,*) (rpfv(i),i=1,ipfval)
      open(30,file=fname,form='formatted',status='unknown')
      write(*,*) 'enter the beginning time '
      read(*,*) tstart 
      write(*,*) 'enter the final time '
      read(*,*) tstop 

      do i=1, ipfval
        rpfv(i) = rpfv(i)/100.
      end do

      rdlim = 0.0
      rlf = 0.0
      ruf = 0.0

      do i = 1, ncol
        temp = 1.E8
        do j = 1, ncol
          if (rdia(j) .lt. temp .and. rdia(j) .gt. rdlim) then
            isizor(i) = j
            temp = rdia(j)
          end if
        end do
        rdlim = rdia(isizor(i))
      end do
      do i = 1, ncol
        write(*,*) i, isizor(i), rdia(isizor(i))
      end do
C
      do i = 1, 2
        read(10,'(A)') chardum
        write(30,'(A)') chardum
      end do
      read(10,'(A)') chardum
      chardum = 'BEGALD'
      write(30,'(A)') chardum
      read(10,1500) chtwo, nnode
      write(30,1500) chtwo, nnode
      do i = 1, 3
        read(10,'(A)') chardum
        write(30,'(A)') chardum
      end do
      do i = 1, 100000
        read(10,1600,end=1000,err=1000) chfour, time, chone, iexp
        time = time*(10.**float(iexp))
        if(time .ge. tstart .and. time .le. tstop) then
          write(*,1650) time
          write(30,1700) chfour, time
          do j = 1, nnode 
            read(10,*,end=1000, err=900) (rgcdi(k),k=1,ncol)
            do k = 1, ipfval
              rlgs = 0.0
              rlf = 0.0
              ruf = 0.0
              do kk = 1, ncol
                 rlf = ruf
                 ruf = ruf + rgcdi(isizor(kk))
                 rdif = AMAX1((ruf-rlf),1.E-8)
                 if (rpfv(k) .gt. rlf .and. rpfv(k) .le. ruf) then
                   rgsout(k) = (rdia(isizor(kk)) * (rpfv(k) - rlf) 
     +                         + rlgs * (ruf - rpfv(k))) / rdif
                 end if
                 rlgs = rdia(isizor(kk))
              end do
            end do
            write(30,2000) (rgsout(k),k=1,ipfval)
          end do
        else if (time .gt. tstop) then
         go to 1000
        else
          do j = 1, nnode
            read(10,*,end=1000, err=900) (rgcdi(k),k=1,ncol)
          end do
        end if 
      end do
  900 write(*,*) 'Last time step is incomplete'
 1000 continue
      write(30,'A') 'ENDDS'
 1500 format(A,1X,I8)
 1600 format(A,f12.8,A,I3)
 1650 format('processing time = ', f15.3)
 1700 format(A,E16.8)
 2000 format(10f15.8)
      close(10)
      close(30)
      stop
      end
