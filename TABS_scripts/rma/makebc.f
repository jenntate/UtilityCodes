      program create	
	
      dimension bql_flows(2000)	
"      dimension ocean_sal(2000),sal_ms_atch(2000)"	
	
"      dimension tide(50000),wdir(50000),wspd(50000),hr(50000)"	
      integer date(50000)	
	
	
      character*15 bqe_names(11)	
      character*8 cdate	
"      dimension bqe_flows(11,2000), bqe_areas(11)"	
"      integer bqe_elem(40),bqe_pnt(12)"	
 	
"      data bqe_names / ""BUFFALO BAYOU  "", ""SAN JACINTO    "","	
"     &                 ""OYSTER BAYOU   "", ""CEDAR BAYOU    "","	
"     &                 ""HIGHLAND BAYOU "", ""DICKINSON BAYOU"","	
"     &                 ""CLEAR CREEK    "", ""CHOCOLATE BAYOU"","	
"     &                 ""ROBINSON BAYOU "", ""LIVE OAK BAYOU "","	
"     &                 ""MS ATCH        "" /"	
	
"      data bqe_areas /1622848.8,"	
"     &                5518440.8,"	
"     &                2445173.5,"	
"     &                 860631.1,"	
"     &                4201904.0,"	
"     &                3165600.1,"	
"     &                3383190.5,"	
"     &                1551623.9,"	
"     &                 697593.1,"	
"     &                 753946.9,"	
     &                      0.0 /	
	
"      data bqe_elem /7057,"	
"     .              6999, 6983, 6982, 6981,"	
"     .               503,"	
"     .               4680,"	
"     .               1756,"	
"     .               3218,"	
"     .               5051,"	
"     .               1614, 1613,"	
"     .               734,"	
"     .               3273, 3274,"	
"     .               87,90,56,55,51,77,49,50,76,48,75,119,155,118,154,"	
"     &               47,46,74,115,117,153,45,72,73,114                /"	
	
"      data bqe_pnt /1,2,6,7,8,9,10,11,13,14,16,41/"	
	
"      open(20,FILE='Tfinal_daily.prn',STATUS='OLD',FORM='FORMATTED')"	
      rewind(20)	
"      open(21,FILE='Tfinal_hourly.prn',STATUS='OLD',FORM='FORMATTED')"	
      rewind(21)	
"      open(30,FILE='newbc.bc',STATUS='UNKNOWN',FORM='FORMATTED')"	
      rewind(30)	
      	
*     *** read daily values	
	
      id=0	
"      read(20,*)"	
"      read(20,*)"	
"      do i = 1,2000"	
"        read(20,*,end=1000) idate,bqe_flows(11,i),sal_ms_atch(i),"	
"     &                ocean_sal(i),bql_flows(i),(bqe_flows(j,i),j=1,10)"	
        id=id+1	
      enddo	
	
" 1000 write(*,*) ""Days Read: "",id"	
	
*     *** read hourly values	
	
      ih=0	
"      read(21,*)"	
"      read(21,*)"	
"      do i = 1,50000"	
"        read(21,*,end=2000) hr(i),tide(i),wdir(i),wspd(i),date(i)"	
	hr(i)=hr(i)-1.0
        ih=ih+1	
      enddo	
	
" 2000 write(*,*) ""Hours Read: "",ih"	
	
      id=0	
      date_last=0	
	
"      do i=1,ih"	
        if(date(i) .ne. date_last) then	
	"  write(cdate,'(I8.8)') date(i)"
          id=id+1	
          phr=0.0	
          date_last=date(i)	
"          write(30,800) cdate(1:2),cdate(3:4),cdate(5:8),phr,hr(i)"	
"          write(30,801) 3,2,0,1,0,0,0,1,0,0,0,2,0"	
"          do j=1,10"	
"            do k=bqe_pnt(j),bqe_pnt(j+1)-1"	
"              write(30,803) bqe_elem(k),bqe_flows(j,id)/bqe_areas(j),"	
"     &                      0.0,0.0,0.0,bqe_names(j)"	
            enddo	
          enddo	
"          do k=bqe_pnt(11),bqe_pnt(12)-1"	
"            write(30,803) bqe_elem(k),bqe_flows(11,id),"	
"     &                    sal_ms_atch(id),0.0,0.0,bqe_names(11)"	
          enddo	
        else	
          phr=phr+.5	
"          write(30,800) cdate(1:2),cdate(3:4),cdate(5:8),phr,hr(i)"	
"          write(30,801) 3,2,0,1,0,0,0,1,0,0,0,2,0"	
        endif	
"        write(30,804) 2,bql_flows(id),4.235,0.0,-1.0,-1.0"	
"        write(30,805) 1,tide(i)+100.,ocean_sal(id),-1.0,-1.0"	
"        write(30,806) 1,wspd(i),270.-wdir(i)"	
"        write(30,807) 14,0.0,0.0"	
"        write(30,807) 11,wspd(i),270.-wdir(i)"	
"        write(30,808)"	
        if(i.ne.ih) then	
          phr=phr+.5	
"          write(30,800) cdate(1:2),cdate(3:4),cdate(5:8),phr,hr(i)+.5"	
"          write(30,801) 3,2,0,1,0,0,0,1,0,0,0,2,0"	
"          write(30,804) 2,bql_flows(id),4.235,0.0,-1.0,-1.0"	
"          write(30,805) 1,0.5*(tide(i)+tide(i+1))+100.,ocean_sal(id),"	
"     &                 -1.0,-1.0"	
"          write(30,806) 1,wspd(i),270.-wdir(i)"	
"          write(30,807) 14,0.0,0.0"	
"          write(30,807) 11,wspd(i),270.-wdir(i)"	
"          write(30,807) 20,wspd(i),0.0,0.0"	
"          write(30,808)"	
        endif	
      enddo     	
"      write(30,*)"	
	
"  800 format('CO ********************************** Date: ',"	
"     &       A2,'/',A2,'/',A4,' Hour: ',F4.1,' Total Time: ',F7.1)"	
"  801 format('TID ',I3,1X,16(4I3,2X))"	
"  803 format('BQE ',I8,1X,F10.7,3(1X,F6.2),5X,""!"",15A)"	
"  804 format('BQL ',I6,5(1X,F12.3))"	
"  805 format('BHL ',I6,4(1X,F12.3))"	
"  806 format('BW  ',I6,2(1X,F12.3))"	
"  807 format('BWT ',I6,2(1X,F12.3))"	
  808 format('END')	
	
      close(20)	
      close(21)	
      close(30)	
	
      end	
