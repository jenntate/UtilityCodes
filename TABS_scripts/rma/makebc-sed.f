      program create

      dimension bql_flows(2000), bql_inflows(2000)

      dimension hr(50000),wdir(50000),wspd(50000)
      integer date(50000)


      character*15 bqe_names(1)
      character*8 cdate
      dimension bqe_flows(1,2000), bqe_areas(1), bqe_inflows(1,2000)
      integer bqe_elem(4), bqe_pnt(2)
 
      data bqe_names / "SAN JACINTO    " /

      data bqe_areas /512682.0/

      data bqe_elem /6999, 6983, 6982, 6981/

      data bqe_pnt /1,5 /


      open(20,FILE='sed-inflows.dat',STATUS='OLD',FORM='FORMATTED')
      rewind(20)
      open(21,FILE='winds.dat',STATUS='OLD',FORM='FORMATTED')
      rewind(21)
      open(22,FILE='h-sed-inflows.dat',STATUS='OLD',FORM='FORMATTED')
      rewind(22)
      open(30,FILE='newbc.bc',STATUS='UNKNOWN',FORM='FORMATTED')
      rewind(30)
      
*     *** read daily values

      id=0
      read(20,*)
      read(20,*)
      read(22,*)
      read(22,*)
      do i = 1,2000
        read(20,*,end=1000) idate,bqe_flows(1,i),bql_flows(i)
        read(22,*,end=1000) idate,bqe_inflows(1,i),bql_inflows(i)
        id=id+1
      enddo

 1000 write(*,*) "Days Read: ",id

*     *** read hourly values

      ih=0
      read(21,*)
      read(21,*)
      do i = 1,50000
        read(21,*,end=2000) hr(i),wdir(i),wspd(i),date(i)
        hr(i)=hr(i)-1.0
        ih=ih+1
      enddo

 2000 write(*,*) "Hours Read: ",ih




      id=0
      date_last=0

      do i=1,ih
        if(date(i) .ne. date_last) then
	  write(cdate,'(I8.8)') date(i)
          id=id+1
          phr=0.0
          date_last=date(i)
          write(30,800) cdate(1:2),cdate(3:4),cdate(5:8),phr
          write(30,801) 1,0,0,2,2
          j=1
          do k=bqe_pnt(j),bqe_pnt(j+1)-1
              write(30,803) bqe_elem(k),bqe_inflows(j,id)/bqe_areas(j),
     &                      -1.0, -1.0, bqe_flows(j,id), bqe_names(j)
          enddo
          write(30,804) 2,bql_inflows(id),4.235,-1.0,-1.0,bql_flows(id)
        else
          phr=phr+.5
          write(30,800) cdate(1:2),cdate(3:4),cdate(5:8),phr
        endif
        write(30,806) 1,wspd(i),wdir(i)
        write(30,807) 14,0.0,0.0
        write(30,807) 11,wspd(i),wdir(i)
        write(30,808)
*        if(i.ne.ih) then
*          phr=phr+.5
*          write(30,800) cdate(1:2),cdate(3:4),cdate(5:8),phr
*          write(30,806) 1,wspd(i),wdir(i)
*          write(30,807) 14,0.0,0.0
*          write(30,807) 11,wspd(i),wdir(i)
*          write(30,808)
*        endif
      enddo     
      write(30,*)

  800 format('CO ********************************** Date: ',
     &       A2,'/',A2,'/',A4,' Hour: ',F4.1)
  801 format('TID ',I3,1X,16(4I3,2X))
  803 format('BQE ',I8,1X,F10.7,2(1X,F6.2),F10.7,5X,"!",15A)
  804 format('BQL ',I6,4(1X,F12.3),F10.7)
  805 format('BHL ',I6,4(1X,F12.3))
  806 format('BW  ',I6,2(1X,F12.3))
  807 format('BWT ',I6,2(1X,F12.3))
  808 format('END')

      close(20)
      close(21)
      close(22)
      close(30)

      end
