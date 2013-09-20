!     ******************************************************************
!     bcn_2_adh.f90
!     Copyright(c) USACE-ERDC-CHL 2000
!
!     Created: 8/13/2008 3:39:19 PM
!     Author : Keith Martin
!     Last change: KM 8/22/2008 11:01:27 AM
!     ******************************************************************

      INCLUDE "junk_mod.f90"

      PROGRAM CNVRT_BCN

      USE JUNK

      INTEGER           :: i,j,k,jj,ios1,nn,en=0,nt,nq=0,qnum(10),wnum
      INTEGER           :: bcn(50),vx,vy,el,sa,tm,sd,nmax,enmax
      INTEGER           :: gc_str(10,15),gc_crd(15),gcn=0,gc_num(10)
      INTEGER           :: start,finish,gcend(10),nds=0,lastnode
      REAL              :: velx(50),vely(50),wsl(50,15000)
      REAL              :: bw(2,15000)=-99999.0,bql(10,15000)=-99999.0
      REAL              :: salt(50,15000),egs(50,15000),dt=3600,ts
      CHARACTER(LEN=80) :: line1, line2, filename
      CHARACTER(LEN=78) :: line3
      CHARACTER(LEN=3)  :: card

      PRINT*,'Enter the TABS-MDS filename:  '
      READ*,filename
      OPEN(21,FILE=filename,STATUS='OLD',FORM='FORMATTED',DELIM='NONE')

      PRINT*,'Enter the ADH output filename:  '
      READ*,filename
      OPEN(22,FILE=filename,STATUS='REPLACE',FORM='FORMATTED')

      PRINT*,'Enter the last node number in the string:  '
      READ*,lastnode

      READ(21,'(A)',IOSTAT=ios1) line1
      nt = 1

      DO WHILE (ios1 .EQ. 0)
          IF (line1(1:3) .EQ. 'BCN') THEN
              BACKSPACE(21)
              nn = 1
              READ(21,9001) card,bcn(nn),vx,vy,el,sa,tm,sd,             &
     &                      velx(nn),vely(nn),wsl(nn,nt),salt(nn,nt)
              DO WHILE (card .EQ. 'BCN')
                  READ(21,'(A)') line2   !read mid-side node as dummy
                  en = en + 1
                  nn = nn + 1
                  READ(21,9001) card,bcn(nn),vx,vy,el,sa,tm,sd,         &
     &                          velx(nn),vely(nn),wsl(nn,nt),salt(nn,nt)

! ****** Calculate edge string value for the wsel *****************
                  egs(en,nt) = (wsl(nn,nt) + wsl(nn-1,nt))/2.0
                  IF (bcn(nn) .EQ. lastnode) EXIT  !replace 49 with var
              END DO
          ELSE IF (line1(1:3) .EQ. 'BW ') THEN
              BACKSPACE(21)
              READ(21,9003) card,wnum,bw(1,nt),bw(2,nt)

          ELSE IF (line1(1:3) .EQ. 'BQL') THEN
              BACKSPACE(21)
              nq = nq + 1
              READ(21,9004) card,qnum(nq),bql(nq,nt)

          ELSE IF (line1(1:3) .EQ. 'GC ') THEN
              gcn = gcn + 1
              line3 = line1(3:80)
              PRINT*,'line 1 = ',line1
              PRINT*,'line 3 = ',line3
              READ(line3,*) (gc_crd(j),j=1,15)
!              PRINT*,'Trying to enter ParseLineGC'
!              CALL ParseLineGC(line3, j, gc_crd)
!              PRINT*,'Completed ParseLineGC'
              DO i = 1, j
                  Print*, 'integer value:', i,gc_crd(i)
              END DO

              PRINT*,'GC node 1 = ',gc_crd(1)
              gc_num(gcn) = gc_crd(1)
              DO k = 2,15
                  IF (gc_crd(k) .EQ. -1) THEN
                      gcend(gcn) = k - 2
                      EXIT
                  ELSE
                      gc_str(gcn,k-1) = gc_crd(k)
                  END IF
              END DO

          ELSE IF (line1(1:3) .EQ. 'END') THEN

              IF (nt .EQ. 1) THEN
                  card = 'EGS'
                  print*,'Dumb'
                  WRITE(22,9002) card,nn-1,nn,en
                  print*,'Dumber',card,nn-1,nn,en
              END IF
              nt = nt + 1
              nmax = nn
              nn = 1
              enmax = en
              en = 0
              nq = 0
              gcn = 0
          END IF

          READ(21,'(A)',IOSTAT=ios1) line1  !read next line

      END DO

      nt = nt - 1

! ****** Build Edge Strings for tide and inflow boundaries
      start = bcn(1)
      DO i = 2,nmax
          en = en + 1
          finish = start + 1
          WRITE(22,*)'EGS',start,finish,en
          print*,'This Loop',start,finish,en
          start  = finish
      END DO

      DO i = 1,gcn

          en = en + 1
          start = gc_str(gcn,1)
          DO j = 2,gcend(gcn)
              finish = gc_str(gcn,j)
              WRITE(22,*) 'EGS',start,finish,en
              PRINT*,'That Loop'
              start = finish
          END DO

      END DO
! ****** Build Node Strings salinity boundary
      start = bcn(1)
      DO i = 1,nmax
          nds = nds + 1
          WRITE(22,*)'NDS',start,nds
          start = start + 1
      END DO
      
      en = 1
      nmax = nmax -1
      DO j = 1,nmax
          WRITE(22,*) 'XY1',en,8760,0,0
          DO i = 1,nt
              ts = i * dt
              WRITE(22,9005) ts, egs(en,i)
          END DO
          en = en + 1
      END DO

 9001 FORMAT(A3,I10,5X,6I2,4F10.3)
 9002 FORMAT(A3,3I3)
 9003 FORMAT(A3,I2,2I5)
 9004 FORMAT(A3,I3,F12.3)
 9005 FORMAT(F9.0,F10.4)

      END PROGRAM
