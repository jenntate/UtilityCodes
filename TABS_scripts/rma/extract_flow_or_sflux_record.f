      program extract_flow_or_sflux_record

      character*80 fname,fname1
      dimension cl(30)
      
      write(*,*) 'enter flow or sflux record filename'
      read(*,5)  fname
      write(*,*) 'enter continuity line number you want extracted'
      read(*,*) clext
      write(*,*) 'enter output file name'
      read(*,5) fname1
 5    format(A)

      open(10,file=fname,status='old',form='formatted')
      open(15,file=fname1,status='unknown',form='formatted')
    
      do n = 1, 30
        cl(n) = 0.0
      end do
 
      do n = 1, 1000000 
        read(10,*,end=30) ncl,tet,(cl(i),i=1,ncl)
        if (tet .ge. 0.0) then
          write(15,20) tet,cl(clext)
 20       format(f8.2,f15.2)
        end if
      end do
 30   continue
      stop
      end
