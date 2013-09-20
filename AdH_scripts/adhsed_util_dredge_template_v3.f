      program adhsed_util_dredge_template 
      dimension ient(500000,4), rnct(500000,3), rarea(500000)
      dimension mnum(100)
      dimension rdata(500000,20), tetime(100000)
      dimension rwdata(100,20,10000,3)
      character*80 chardum, fname, fpre, fpreo
      character*1 chone,chint(10)
      character*2 chtwo
      character*4 chfour

      data chint/'0','1','2','3','4','5','6','7','8','9'/
C
      write(*,*)
      write(*,*) 'grabs deposition data from an ADH'
      write(*,*) 'dat file for given material types'
      write(*,*)
      write(*,*) 'enter file containing material numbers'
      write(*,*) 'the file should have an integer '
      write(*,*) 'number of materials, followed by the'
      write(*,*) 'material numbers'
      write(*,*) '(max of 100 material types)'
      read(*,*) fname 
      open(10,file=fname,
     +  form='formatted',status='old')
      write(*,*) 'enter the node table filename' 
      read(*,*) fname
      open(20,file=fname,
     +  form='formatted',status='old')
      write(*,*) 'enter the element table filename'
      read(*,*) fname
      open(30,file=fname,
     +  form='formatted',status='old')
      write(*,*) 'enter the ADH dat file prefix' 
      read(*,*) fpre
      write(*,*) 'enter output file prefix'
      read(*,*) fpreo
C
      read(10,*) nmt
      do i=1, nmt
        read(10,*) mnum(i)
      end do
      close(10)
C

      do i = 1, 500000
        read(30,*,end=100) ielem, ient(ielem,1), 
     +  ient(ielem,2), ient(ielem,3), ient(ielem,4)
      end do
 100  nelem = ielem
      close(30)

      do i = 1, 500000
        read(20,*,end=200) inode, rnct(inode,1),
     +  rnct(inode,2), rnct(inode,3)
      end do
 200  nnode = inode
      close(20)

      do i = 1, nelem
        rarea(i) = 0.5 * abs(
     +   (rnct(ient(i,1),1) - rnct(ient(i,3),1)) * 
     +   (rnct(ient(i,2),2) - rnct(ient(i,1),2)) - 
     +   (rnct(ient(i,1),1) - rnct(ient(i,2),1)) * 
     +   (rnct(ient(i,3),2) - rnct(ient(i,1),2)))
      end do

      open(10,file=trim(fpre)//'_dpl.dat',
     +  form='formatted',status='old')

      do i = 1, 2
        read(10,'(A)') chardum
      end do
      read(10,'(A)') chardum
      read(10,'(A)') chardum
      do i = 1, 3
        read(10,'(A)') chardum
      end do
      ntstep = 0
      tstart = 0.0
      tstop = 1.e+16
      ncol = 1
      ngc = 1
      do i = 1, 100000
        read(10,1600,end=1000,err=1000) chfour, time, chone, iexp
        time = time*(10.**float(iexp))
        if(time .ge. tstart .and. time .le. tstop) then
          ntstep = ntstep + 1
          write(*,1650) time
          do j = 1, nnode 
           read(10,*,end=1000, err=900) (rdata(j,k),k=1,ncol)
          end do

          do jj = 1, nmt
            do k = 1, ngc+1
              do l = 1, 3
                rwdata(jj,k,ntstep,l) = 0.0
              end do
            end do
          end do 
          do j = 1, nelem
            do jj = 1, nmt
              if (mnum(jj) .eq. ient(j,4)) then
               tetime(ntstep) = time
               rwdata(jj,1,ntstep,1) = rwdata(jj,1,ntstep,1) +
     +         rarea(j)
               rwdata(jj,1,ntstep,2) = rwdata(jj,1,ntstep,1)
               rwdata(jj,1,ntstep,3) = rwdata(jj,1,ntstep,1)

               do k = 2, ngc+1
                 rwdata(jj,k,ntstep,1) = rwdata(jj,k,ntstep,1) + 
     +           (1./3.) * rarea(j) * amax1(( 
     +            rdata(ient(j,1),1)
     +          + rdata(ient(j,2),1)  
     +          + rdata(ient(j,3),1)),0.0)
                 rwdata(jj,k,ntstep,2) = rwdata(jj,k,ntstep,2) +
     +           (1./3.) * rarea(j) * amin1((
     +            rdata(ient(j,1),1)
     +          + rdata(ient(j,2),1)
     +          + rdata(ient(j,3),1)),0.0)
                 rwdata(jj,k,ntstep,3) = rwdata(jj,k,ntstep,3) +
     +           (1./3.) * rarea(j) * (
     +            rdata(ient(j,1),1)
     +          + rdata(ient(j,2),1)
     +          + rdata(ient(j,3),1))

               end do
             end if
           end do
          end do
        else if (time .gt. tstop) then
         go to 1000
        else
          do j = 1, nnode
            read(10,*,end=1000, err=900) (rdata(j,k),k=1,ncol)
          end do
        end if 
      end do


  900 write(*,*) 'Last time step is incomplete'
 1000 continue
      close(10)
C
      do i = 1, nmt
      if (i .lt. 10) then
        open(10,file=trim(fpreo)//'_mat_'//chint(i+1)//'.txt',
     +  form='formatted',status='unknown')
      else if (i .lt. 20) then
        open(10,file=trim(fpreo)//'_mat_1'//chint(i-9)//'.txt',
     +  form='formatted',status='unknown')
      else if (i .lt. 30) then
        open(10,file=trim(fpreo)//'_mat_2'//chint(i-19)//'.txt',
     +  form='formatted',status='unknown')
      else if (i .lt. 40) then
        open(10,file=trim(fpreo)//'_mat_3'//chint(i-29)//'.txt',
     +  form='formatted',status='unknown')
      else if (i .lt. 50) then
        open(10,file=trim(fpreo)//'_mat_4'//chint(i-39)//'.txt',
     +  form='formatted',status='unknown')
      else if (i .lt. 60) then
        open(10,file=trim(fpreo)//'_mat_5'//chint(i-49)//'.txt',
     +  form='formatted',status='unknown')
      else if (i .lt. 70) then
        open(10,file=trim(fpreo)//'_mat_6'//chint(i-59)//'.txt',
     +  form='formatted',status='unknown')
      else if (i .lt. 80) then
        open(10,file=trim(fpreo)//'_mat_7'//chint(i-69)//'.txt',
     +  form='formatted',status='unknown')
      else if (i .lt. 90) then
        open(10,file=trim(fpreo)//'_mat_8'//chint(i-79)//'.txt',
     +  form='formatted',status='unknown')
      else if (i .lt. 100) then
        open(10,file=trim(fpreo)//'_mat_9'//chint(i-89)//'.txt',
     +  form='formatted',status='unknown')
      end if
      do j = 1, ntstep
        write(10,2000) mnum(i), tetime(j), 
     +  (rwdata(i,k,j,1),k = 1, ngc+1),
     +  (rwdata(i,k,j,2),k = 2, ngc+1),
     +  (rwdata(i,k,j,3),k = 2, ngc+1)
      end do
      close(10)
      end do
        

C
 1500 format(A,1X,I8)
 1600 format(A,f12.8,A,I3)
 1650 format('processing time = ', f15.3)
 2000 format(I8,21(E16.8,1X))
      close(10)
      stop
      end
