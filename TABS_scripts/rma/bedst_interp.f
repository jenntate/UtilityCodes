      program bedst_interp
*
*  gary brown 7/22/99
*
* this routine will interpoolate a bed structure file 
* from one grid to another 
*
*

      parameter (mel=50000,mnp=100000,mnlay=10,mngs=10)


      dimension  cs(mnp,mnlay,mngs), hs(mnp,mnlay)
      dimension  nops(mel,8),mt(mel),th(mel)
      dimension  xo(mnp), yo(mnp), zo(mnp)
      dimension  xn(mnp), yn(mnp), zn(mnp)
      dimension width(mnp),ss1(mnp),ss2(mnp),wids(mnp)
      character cline*100
      character*100  bsiname,bsoname,giname,goname,fname
      logical openfl, here
C
      do i = 1, mnp 
        zo(i) = -100.
        zn(i) = -100.
      end do
C
      write(*,*) ' *** BEDST_INTERP ***'
      write(*,*)
      write(*,*) 'This program interpolates a bed structure file'
      write(*,*) 'from one grid to another'
      write(*,*)
      write(*,*) 'enter the input bed structure file name'
      read(*,111) bsiname
      write(*,*) 'enter the geo file corresponding to the'
      write(*,*) 'input bed structure file'
      read(*,111) giname
      write(*,*) 'enter the output bed structure file name'
      read(*,111) bsoname
      write(*,*) 'enter the geo file corresponding to the'
      write(*,*) 'output bed structure file'
      read(*,111) goname
 111  format (a)
C
C
      open(1,file=bsiname,form='formatted',status='old')
      open(2,file=giname,form='formatted',status='old')
      open(7,file=goname,form='formatted',status='old')
      open(5,file=bsoname,form='formatted',status='unknown')
C
      inquire(30,opened=openfl,name=fname)
      if(.not.openfl)then
        open(30,file='tmp.geo',form='formatted',status='unknown')
      else
        write(*,*)' unit 30 opened elsewhere'
        write(*,*)' name is >'//fname
        stop
      end if

      ige = 0
      ignn = 0
      do i = 1, 999999
        read(2,'(a)',end=1) cline
        if(cline(1:2).eq.'GE')then
          if(ige.eq.0)then
            write(*,*)' reading GE cards'
          end if
          ige = ige + 1
          write(30,'(a)') cline(3:100)
        else if(cline(1:3).eq.'GNN')then
          if(ignn .eq.0)then
            write(*,*)' reading GNN cards'
          end if
          ignn = ignn + 1
          write(30,'(a)') cline(4:100)
        end if
      end do
 1    close(2)
      rewind(30)
      do i = 1, ige
        read(30,*) ne2d,(nops(ne2d,j),j=1,8),mt(ne2d),th(ne2d)
      end do
      do i = 1, ignn
        read(30,*) node, xo(node), yo(node), zo(node)
      end do
      close(30,status='delete')
C
C
      do ie = 1, ne2d
        if(nops(ie,6).eq.0)then
*
* a 1d element  set ncnn to 0 to skip at this point.
*
          ncnn = 0

        else if(nops(ie,7).eq.0)then
*
* triangle element
*
          ncnn = 6
        else
*
* quadralateral element
*
          ncnn = 8

        end if

        if(ncnn.gt.0)then
          do mid = 2, ncnn, 2
            nd = nops(ie,mid)

            if(nd.gt.node) node = nd

            if(zo(nd) .lt. -10.)then
              ndm1 = nops(ie,mid-1)
              if(mid.eq.ncnn)then
                ndp1 = nops(ie,1)
              else
                ndp1 = nops(ie,mid+1)
              end if

              zo(nd) = 0.5 * (zo(ndp1) + zo(ndm1))
              xo(nd) = 0.5 * (xo(ndp1) + xo(ndm1))
              yo(nd) = 0.5 * (yo(ndp1) + yo(ndm1))

            end if
          end do
        end if

      end do

*
* now process all the 1-d elements now.
*
      do ie = 1, ne2d

        if(nops(ie,6).eq.0)then
*
          if(mt(ie).lt.900)then
            ncnn = 3
          else
            ncnn = 0
          end if

        else
*
* 2d elements are already processed, so skip these
*
          ncnn = 0

        end if

        if(ncnn.gt.0)then
          do mid = 2, ncnn, 2
            nd = nops(ie,mid)

            if(nd.gt.node) node = nd

            if(zo(nd) .lt. -10.)then
              ndm1 = nops(ie,mid-1)
              if(mid.eq.ncnn)then
                ndp1 = nops(ie,1)
              else
                ndp1 = nops(ie,mid+1)
              end if

              zo(nd) = 0.5 * (zo(ndp1) + zo(ndm1))
              xo(nd) = 0.5 * (xo(ndp1) + xo(ndm1))
              yo(nd) = 0.5 * (yo(ndp1) + yo(ndm1))

            end if
          end do
        end if

      end do
C
      no = node
C
      inquire(30,opened=openfl,name=fname)
      if(.not.openfl)then
        open(30,file='tmp.geo',form='formatted',status='unknown')
      else
        write(*,*)' unit 30 opened elsewhere'
        write(*,*)' name is >'//fname
        stop
      end if

      ige = 0
      ignn = 0
      do i = 1, 999999
        read(7,'(a)',end=2) cline
        if(cline(1:2).eq.'GE')then
          if(ige.eq.0)then
            write(*,*)' reading GE cards'
          end if
          ige = ige + 1
          write(30,'(a)') cline(3:100)
        else if(cline(1:3).eq.'GNN')then
          if(ignn .eq.0)then
            write(*,*)' reading GNN cards'
          end if
          ignn = ignn + 1
          write(30,'(a)') cline(4:100)
        end if
      end do
 2    close(7)
      rewind(30)
      do i = 1, ige
        read(30,*) ne2d,(nops(ne2d,j),j=1,8),mt(ne2d),th(ne2d)
      end do
      do i = 1, ignn
        read(30,*) node, xn(node), yn(node), zn(node)
      end do
      close(30,status='delete')
C
      do ie = 1, ne2d
        if(nops(ie,6).eq.0)then
*
* a 1d element  set ncnn to 0 to skip at this point.
*
          ncnn = 0

        else if(nops(ie,7).eq.0)then
*
* triangle element
*
          ncnn = 6
        else
*
* quadralateral element
*
          ncnn = 8

        end if

        if(ncnn.gt.0)then
          do mid = 2, ncnn, 2
            nd = nops(ie,mid)

            if(nd.gt.node) node = nd

            if(zn(nd) .lt. -10.)then
              ndm1 = nops(ie,mid-1)
              if(mid.eq.ncnn)then
                ndp1 = nops(ie,1)
              else
                ndp1 = nops(ie,mid+1)
              end if

              zn(nd) = 0.5 * (zn(ndp1) + zn(ndm1))
              xn(nd) = 0.5 * (xn(ndp1) + xn(ndm1))
              yn(nd) = 0.5 * (yn(ndp1) + yn(ndm1))

            end if
          end do
        end if

      end do

*
* now process all the 1-d elements now.
*
      do ie = 1, ne2d

        if(nops(ie,6).eq.0)then
*
          if(mt(ie).lt.900)then
            ncnn = 3
          else
            ncnn = 0
          end if

        else
*
* 2d elements are already processed, so skip these
*
          ncnn = 0

        end if

        if(ncnn.gt.0)then
          do mid = 2, ncnn, 2
            nd = nops(ie,mid)

            if(nd.gt.node) node = nd

            if(zn(nd) .lt. -10.)then
              ndm1 = nops(ie,mid-1)
              if(mid.eq.ncnn)then
                ndp1 = nops(ie,1)
              else
                ndp1 = nops(ie,mid+1)
              end if

              zn(nd) = 0.5 * (zn(ndp1) + zn(ndm1))
              xn(nd) = 0.5 * (xn(ndp1) + xn(ndm1))
              yn(nd) = 0.5 * (yn(ndp1) + yn(ndm1))

            end if
          end do
        end if

      end do
C
      nn = node
C
C
      READ(1,2000) NPMX,NBLAX,NGSX
      DO J = 1, NPMX
        DO K =  1, NBLAX
          DO L = 1, NGSX
            READ(1,2010) CS(J,K,L)
          END DO
        END DO
      END DO
      DO J = 1, NPMX
        DO K = 1, NBLAX
          READ(1,2020) HS(J,K)
        END DO
      END DO
 2000 FORMAT (3I8)
 2010 FORMAT (F12.4)
 2020 FORMAT (F12.8)
C
      if (npmx .ne. no) then
        write(*,*) 'error: input geometry file'
        write(*,*) 'and input bed structure file'
        write(*,*) 'are not compatible.'
        write(*,*) 'nodes from geo = ', no
        write(*,*) 'nodes from bs = ', npmx
        stop
      end if
C
      write(*,*) 'first do cs values'
      nt = 1
      do i = 1, nblax
        do j = 1, ngsx
          do k = 1, no
            zo(k) = cs(k,i,j)
          end do
          call interph(nt,no,nn,xo,yo,zo,xn,yn,zn)
          write(*,*) 'finished cycle ', nt
          nt = nt + 1
          do k = 1, nn
            cs(k,i,j) = zn(k)
          end do
        end do
      end do
C 
      write(*,*) 'now do hs values'
      do i = 1, nblax
        do j = 1, no
          zo(j) = hs(j,i)
        end do
        call interph(nt,no,nn,xo,yo,zo,xn,yn,zn)
        write(*,*) 'finished cycle ', nt
        nt = nt + 1
        do j = 1, nn
          hs(j,i) = zn(j)
          hs(j,i) = amax1(hs(j,i),1.E-6)
        end do
      end do
C
C
      WRITE(5,2000) NN,NBLAX,NGSX
      DO J = 1, NN
        DO K =  1, NBLAX
          DO L = 1, NGSX
            WRITE(5,2010) CS(J,K,L)
          END DO
        END DO
      END DO
      DO J = 1, NN
        DO K = 1, NBLAX
          WRITE(5,2020) HS(J,K)
        END DO
      END DO
C
      stop
      end





      subroutine interph(nt,no,nn,xo,yo,zo,xn,yn,zn) 
C
C     This subroutine does horizontal interpolation
C     by inverse distance weighted averaging.  It uses the
C     8 nearest nodes from the original grid to do
C     the averaging. 
C
      parameter (mnp=100000)
C
      dimension xo(mnp),yo(mnp),zo(mnp)
      dimension xn(mnp),yn(mnp),zn(mnp)
      dimension nmin(mnp,8),dmin(mnp,8)
C
      do i = 1, nn
        if (nt .gt. 1) go to 10
        do j = 1, 8
          dmin(i,j) = 9999999.
          nmin(i,j) = 0
        end do
        do j = 1, no
          dist = sqrt((xo(j)-xn(i))**2+
     +                (yo(j)-yn(i))**2)
          if (dist .lt. 0.1) then
            do k = 1, 8
              dmin(i,k) = 1.0
              nmin(i,k) = j
            end do
            go to 10
          end if
          do k = 1, 8
            ifill = 1
            if (dist .lt. dmin(i,k)) then
              if (k .gt. 1) then
                do l = k-1,1,-1
                  if (abs(dmin(i,l)-dist) .lt. 0.0001) then
                    ifill = 0
                  end if
                end do
              end if
              if (ifill .eq. 1) then
                dmin(i,k) = dist
                nmin(i,k) = j
              end if
            end if
          end do
        end do
 10     continue
        vdsum = 0.0
        dsum  = 0.0
        do j = 1, 8
          vdsum = vdsum + zo(nmin(i,j))/dmin(i,j)
          dsum  = dsum  + 1./dmin(i,j) 
        end do
        zn(i) = vdsum/dsum  
      end do  
C
      return
      end
         

