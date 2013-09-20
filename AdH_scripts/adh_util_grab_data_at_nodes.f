      program adh_util_grab_data_at_nodes 
      dimension rwdata(100,20,100000),tetime(100000), nnum(100)
      dimension rdata(100)
      character*80 chardum, fnamei, fnamen, fnameo
      character*1 chone,chint(9)
      character*2 chtwo
      character*4 chfour

      data chint/'1','2','3','4','5','6','7','8','9'/
C
      write(*,*)
      write(*,*) 'grabs data from an ADH'
      write(*,*) 'dat file for a given'
      write(*,*) 'set of nodes'
      write(*,*)
      write(*,*) 'enter file containing node numbers'
      write(*,*) 'the file should have an integer '
      write(*,*) 'number of nodes, followed by the'
      write(*,*) 'node numbers'
      write(*,*) '(max of 100 nodes)'
      read(*,*) fnamen 
      write(*,*) 'enter input file name'
      read(*,*) fnamei
      write(*,*) 'enter number of columns of'
      write(*,*) 'data in the file'
      write(*,*) '(max of 20)'
      read(*,*) ncol
      write(*,*) 'enter output file prefix'
      read(*,*) fnameo
      write(*,*) 'enter the beginning time'
      read(*,*) tstart 
      write(*,*) 'enter the final time '
      read(*,*) tstop 
C
      open(10,file=fnamen,
     +  form='formatted',status='old')
      read(10,*) nnn
      do i=1, nnn
        read(10,*) nnum(i)
      end do
      close(10)
C
      open(10,file=fnamei,
     +form='formatted',status='old')

      do i = 1, 2
        read(10,'(A)') chardum
      end do
      read(10,'(A)') chardum
      read(10,1500) chtwo, nnode
      do i = 1, 3
        read(10,'(A)') chardum
      end do
      ntstep = 0
      do i = 1, 100000
        read(10,1600,end=1000,err=1000) chfour, time, chone, iexp
        time = time*(10.**float(iexp))
        if(time .ge. tstart .and. time .le. tstop) then
          ntstep = ntstep + 1
          write(*,1650) time
          do j = 1, nnode 
            read(10,*,end=1000, err=900) (rdata(k),k=1,ncol)
             do jj = 1, nnn
               if (j .eq. nnum(jj)) then
                 tetime(ntstep) = time
                 do k = 1, ncol
                   rwdata(jj,k,ntstep) = rdata(k)
                 end do
               end if
             end do
           end do
        else if (time .gt. tstop) then
         go to 1000
        else
          do j = 1, nnode
            read(10,*,end=1000, err=900) (rdata(k),k=1,ncol)
          end do
        end if 
      end do
  900 write(*,*) 'Last time step is incomplete'
 1000 continue
      close(10)
C
      do i = 1, nnn
      if (i .lt. 10) then
        open(10,file=trim(fnameo)//'_node_'//chint(i)//'.txt',
     +  form='formatted',status='unknown')
      else if (i .lt. 20) then
        open(10,file=trim(fnameo)//'_node_1'//chint(i)//'.txt',
     +  form='formatted',status='unknown')
      else if (i .lt. 30) then
        open(10,file=trim(fnameo)//'_node_2'//chint(i)//'.txt',
     +  form='formatted',status='unknown')
      else if (i .lt. 40) then
        open(10,file=trim(fnameo)//'_node_3'//chint(i)//'.txt',
     +  form='formatted',status='unknown')
      else if (i .lt. 50) then
        open(10,file=trim(fnameo)//'_node_4'//chint(i)//'.txt',
     +  form='formatted',status='unknown')
      else if (i .lt. 60) then
        open(10,file=trim(fnameo)//'_node_5'//chint(i)//'.txt',
     +  form='formatted',status='unknown')
      else if (i .lt. 70) then
        open(10,file=trim(fnameo)//'_node_6'//chint(i)//'.txt',
     +  form='formatted',status='unknown')
      else if (i .lt. 80) then
        open(10,file=trim(fnameo)//'_node_7'//chint(i)//'.txt',
     +  form='formatted',status='unknown')
      else if (i .lt. 90) then
        open(10,file=trim(fnameo)//'_node_8'//chint(i)//'.txt',
     +  form='formatted',status='unknown')
      else if (i .lt. 100) then
        open(10,file=trim(fnameo)//'_node_9'//chint(i)//'.txt',
     +  form='formatted',status='unknown')
      end if
      do j = 1, ntstep
        write(10,2000) nnum(i), tetime(j), 
     +  (rwdata(i,k,j),k = 1, ncol)
      end do
      close(10)
      end do
        

C
 1500 format(A,1X,I8)
 1600 format(A,f12.8,A,I3)
 1650 format('processing time = ', f15.3)
 2000 format(I8,21(E16.8,1X))
      close(10)
      close(30)
      stop
      end
