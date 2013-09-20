      parameter (maxne = 40000, maxn = 60000, maxndf = 8)

      real cord(maxn,3)
      real spec(maxn,3)
      real alfa(maxn)
      integer nfix(maxn)
      integer nfix1(maxn)
      real ao(maxn)
      integer nsurf(maxn)
      integer ndep(maxn)
      integer nref(maxn)
      integer ndry(maxn)
      integer nop(maxne,20)
      integer ncorn(maxne)
      integer imat(maxne)
      real th(maxne)
      integer nfixh(maxne)
      real width(maxn)
      real xvel(maxndf,maxn), vvel(maxn), dfct(maxne)
      
      character*3 shape(4:8)
      character*24 dattyp(6)
      
      integer isave(maxn), nopgms(maxne,8), necorn(maxne),matgms(maxne)
      real xgms(maxn), ygms(maxn), zgms(maxn)
      integer nodenw(maxn), nodold(maxn), lu(2:6), iexist(2:6),
     & iresav(maxn)
      

      character fil3db*80, filgms*80, filbin*80, filhd*80, filvel*80,
     &          filslt*80, filtmp*80, filsed*80
      
      data isave/maxn*0/, shape/'E4T','E5P','E6W','???','E8H'/
      
      data nodenw/maxn*0/, nodold/maxn*0/, lu/11,12,13,14,15/,
     &     iexist/5*0/
      
      data dattyp/'X-velocity','Y-velocity','Head', 'Salinity',
     &            'Temperature','Sediment Concentration'/
      
      

      write(*,*)' enter the RMA10 3d binary geometry file'
      read(*,'(a)') fil3db
      open(10,file=fil3db,form='unformatted',status='old')
      
      write(*,*)' enter the GMS style 3d ASCII output geometry file'
      read(*,'(a)') filgms
      open(11,file=filgms,form='formatted',status='unknown')
      write(11,'(6hMESH3D)')
      
      
c 
c.....read the 3d geometry information
c.....np= total number of nodes
c.....ne= total number of elements
c.....npm = number of surface nodes
c.....nes = number of surface elements
c
      
      read(10)np, ne, npm, nes,
     &    ((cord(j,k),spec(j,k),k=1,3), alfa(j), nfix(j), 
     &     ao(j),
     &    nsurf(j), j = 1, np), (ndep(j), nref(j), j = 1, npm),
     &    ((nop(j,k),k=1,20), ncorn(j), imat(j),th(j),
     &    nfixh(j), j = 1, ne), (width(j), j = 1, np)

      write(*,*)' binary file read in, total 3d nodes = ',np
      write(*,*)'                   total 3d elements = ',ne
      write(*,*)'                      total 2d nodes = ',npm
      write(*,*)'                   total 2d elements = ',nes
C GLB add the following as per Bob Evans
*
* not sure what NCORN is from RMA10, so reinitialize it to be
* exactly the number of nodes for each element
*
      n1d = 0
      njunc = 0
      n1d2d = 0

      do n = 1, nes

        do k = 1, 20
          if(nop(n,k).ne.0) ncorn(n) = k
        end do

        if(ncorn(n) .lt. 6 .and. imat(n).lt.900) n1d = n1d + 1
        if(ncorn(n) .eq. 5 .and. imat(n).lt.900) n1d2d = n1d2d + 1
        if(imat(n).gt.900 .and. imat(n).lt.910) njunc = njunc + 1
      end do

      write(*,*)'  number of non-junction 1d elements = ',n1d
      write(*,*)'      number of junction 1d elements = ',njunc
      write(*,*)' number of 1D/2D transition elements = ',n1d2d

C GLB end additions
      
      if(np.gt.maxn)then
        write(*,*)' increase MAXN >=',np
        stop
      end if
      if(ne.gt.maxne)then
        write(*,*)' increase MAXNE >=',ne
        stop
      end if

      iwedge = 1
      ibrick = 2
      ipyrmd = 3
      itetra = 4
      itri2d = 5
      iqud2d = 6
      
      ndgms = 0
      
      negms = 0
      ndgms = 0

C GLB add the following as per Bob Evans

      n1d = 0
      n1d2d = 0
      njunc = 0

      
      
      j = 0
*
* only the first NES elements are true 2-d/1-d elements for display
*


      do while(j.lt.ne)
        j = j + 1
*

        if(nop(j,4).eq.0 .and. imat(j).lt.900)then
*
* simple 3 node 1d element
*
          n1d = n1d + 1

        else if(nop(j,6).eq.0 .and. imat(j).lt.900)then
*
* 1d to 2d transition element
*
          n1d2d = n1d2d + 1

        else if(imat(j).gt.900 .and. imat(j).lt.910)then
*
* 1d junction element
*
          njunc = njunc + 1
*

        else if(nop(j,7).eq.0)then
C GLB end changes
*
* simple 2-d triangle..trick gms

          if(j.gt.nes)goto 999
               
          negms = negms + 1
          matgms(negms) = itri2d
          necorn(negms) = 6
          
          m = 0
          do k = 1, 5, 2  
            m = m + 1
            nopgms(negms,m) = nop(j,k)
            nopgms(negms,m+3) = nop(j,k)
            
            n = nop(j,k)
            x = cord(n,1)
            y = cord(n,2)
            z = cord(n,3)
            if(isave(n).eq.0)then

              ndgms = ndgms + 1
              nodenw(n) = ndgms
              xgms(ndgms) = x
              ygms(ndgms) = y
              zgms(ndgms) = z
              
              isave(n) = 1
            end if
          end do
                      
*
        else if(nop(j,9).eq.0)then
*
* simple 2-d quad..trick gms

          if(j.gt.nes)goto 999
          
          negms = negms + 1
          matgms(negms) = iqud2d
          necorn(negms) = 8
          m = 0
          
          do k = 1, 7, 2  
            m = m + 1
            nopgms(negms,m) = nop(j,k)
            nopgms(negms,m+4) = nop(j,k)
            n = nop(j,k)
            x = cord(n,1)
            y = cord(n,2)
            z = cord(n,3)
            if(isave(n).eq.0)then

              ndgms = ndgms + 1
              nodenw(n) = ndgms
              xgms(ndgms) = x
              ygms(ndgms) = y
              zgms(ndgms) = z
              
              isave(n) = 1
            end if
          end do
                      
*
           
        
        else if(nop(j,11).eq.0)then
*
* tetrahedron element


          negms = negms + 1
          matgms(negms) = itetra
          necorn(negms) = 4
          m = 0
          
          do k = 1, 5, 2  
            m = m + 1
            nopgms(negms,m) = nop(j,k)
            n = nop(j,k)
            x = cord(n,1)
            y = cord(n,2)
            z = cord(n,3)
            if(isave(n).eq.0)then

              ndgms = ndgms + 1
              nodenw(n) = ndgms
              xgms(ndgms) = x
              ygms(ndgms) = y
              zgms(ndgms) = z
              
              isave(n) = 1
            end if
          end do
          m = m + 1
          nopgms(negms,m) = nop(j,10)

          n = nop(j,10)
          x = cord(n,1)
          y = cord(n,2)
          z = cord(n,3)
          if(isave(n).eq.0)then

              ndgms = ndgms + 1
              nodenw(n) = ndgms
              xgms(ndgms) = x
              ygms(ndgms) = y
              zgms(ndgms) = z
              
            isave(n) = 1
          end if
            
            
        else if(nop(j,14).eq.0) then
*
* pyramid element
*
         
           
          negms = negms + 1
          matgms(negms) = ipyrmd
          necorn(negms) = 5
          m = 0
          do ic = 1, 5
            if(ic.eq.1)then
              k = 3
            else if(ic.eq.2)then
              k = 1
            else if(ic.eq.3)then
              k = 9
            else if(ic.eq.4)then
              k = 11
            else 
              k = 5
            end if
            
            m = m + 1
            nopgms(negms,m) = nop(j,k)
             
            n = nop(j,k)
            x = cord(n,1)
            y = cord(n,2)
            z = cord(n,3)
            if(isave(n).eq.0)then

              ndgms = ndgms + 1
              nodenw(n) = ndgms
              xgms(ndgms) = x
              ygms(ndgms) = y
              zgms(ndgms) = z
              
              isave(n) = 1
            end if
          end do
                      
        else if(nop(j,16).eq.0)then
*
*   wedge elements
*
*
          negms = negms + 1
          matgms(negms) = iwedge
          necorn(negms) = 6
          m = 0
          do k = 1, 5, 2
            
            m = m + 1
            nopgms(negms,m) = nop(j,k)
             
            n = nop(j,k)
            x = cord(n,1)
            y = cord(n,2)
            z = cord(n,3)
            if(isave(n).eq.0)then

              ndgms = ndgms + 1
              nodenw(n) = ndgms
              xgms(ndgms) = x
              ygms(ndgms) = y
              zgms(ndgms) = z
              
              isave(n) = 1
            end if
          end do
           do k = 10, 14, 2
           
            m = m + 1
            nopgms(negms,m) = nop(j,k)
             
            n = nop(j,k)
            x = cord(n,1)
            y = cord(n,2)
            z = cord(n,3)
            if(isave(n).eq.0)then

              ndgms = ndgms + 1
              nodenw(n) = ndgms
              xgms(ndgms) = x
              ygms(ndgms) = y
              zgms(ndgms) = z
              
              isave(n) = 1
            end if
          end do
          
           
        
        else 
        
*  brick element


          negms = negms + 1
          matgms(negms) = ibrick
          necorn(negms) = 8
          m = 0
          
          do k = 1, 7, 2
          
            m = m + 1
            nopgms(negms,m) = nop(j,k)
             
            n = nop(j,k)
            x = cord(n,1)
            y = cord(n,2)
            z = cord(n,3)
            if(isave(n).eq.0)then

              ndgms = ndgms + 1
              nodenw(n) = ndgms
              xgms(ndgms) = x
              ygms(ndgms) = y
              zgms(ndgms) = z
              
              isave(n) = 1
            end if
          end do
          do k = 13, 19, 2
            m = m + 1
            nopgms(negms,m) = nop(j,k)
             
            n = nop(j,k)
            x = cord(n,1)
            y = cord(n,2)
            z = cord(n,3)
            if(isave(n).eq.0)then

              ndgms = ndgms + 1
              nodenw(n) = ndgms
              xgms(ndgms) = x
              ygms(ndgms) = y
              zgms(ndgms) = z
              
              isave(n) = 1
            end if
          end do

        end if
        
 999   continue       
        
      end do
      
      close(10)
            
     
      do ie = 1, negms
        k = necorn(ie)
*
* put new node # in element connection table
*
        do j = 1, k
          nopgms(ie,j) = nodenw( nopgms(ie,j) )
        end do
        
        write(11,'(a3,1x,10i6)')shape(k),
     &                   ie, (nopgms(ie,j),j=1,k),matgms(ie)
      end do
      
      do n = 1, ndgms
        write(11,'(2hND,i7,3e16.8)') n,xgms(n), ygms(n), zgms(n)
      end do
      close(11)
      write(*,*)' GMS-type 3d mesh complete, number of nodes saved =',
     &                          ndgms
      write(*,*)'                         number of elements saved = ',
     &                          negms
C GLB add this write statement as per Bob Evans
      write(*,*)'                     number of 1-D elements found = ',
     &                          n1d
*
* set array NODOLD(*) such that when the index = new node value, the
* array value = old node value
*
      do n = 1, np
        newnd = nodenw(n)
        if(newnd.gt.0) nodold(newnd) = n
      end do
      
c      write(*,*)' new node 9853 corresponds to old node ',nodold(9853)
      
*
*  check that the new node actually corresponds to the old
*  node location
*
      ipass = 0
      do ngms = 1, ndgms
        nold = nodold(ngms)
        x1 = xgms(ngms)
        x2 = cord(nold,1)
        
        y1 = ygms(ngms)
        y2 = cord(nold,2)
        
        z1 = zgms(ngms)
        z2 = cord(nold,3)
        
        r = sqrt((x1-x2)**2.+(y1-y2)**2.+(z1-z2)**2.)
        if(r.gt.0.25)then
          write(*,*)' nodes (old,new) =',nold,ngms,' r=',r
          ipass = ipass + 1
        end if
        
      end do
      
      if(ipass.eq.0)then
        write(*,*)' old node/new nodes checked OK'
      else
        write(*,*)' old node/new node mismatch, number =',ipass
      end if
      
     
      write(*,*)' enter the RMA10 3d binary output file'
      read(*,'(a)')filbin
      
      if(filbin(1:1).ne.' ')then
      
        open(10,file=filbin,form='unformatted',status='old')
*
* read the first record to get the number of degrees of freedom
*
       read(10)tet,np,ndf
       backspace 10
       write(*,*)tet,np,ndf
        write(*,*)' number of degrees of freedom =',ndf
        
*
*   ndf     data saved
*   ---     --------------------------------------------
*    3      xvel, yvel, head
*    4      xvel, yvel, head, salinity
*    5      xvel, yvel, head, salinity, temperature
*    6      xvel, yvel, head, salinity, temperature, sediment conc.
*
*  the above values are stored in xvel(k,np), where k = ndf
*
        
        write(*,*)' enter the GMS style ASCII head file'
        read(*,'(a)') filhd
        open(11,file=filhd,form='formatted',status='unknown')
        write(11,'(11hSCALAR head,/,2hND,i6,/,6hSTAT 0)') ndgms
        iexist(2) = 1
        
        write(*,*)' enter the GMS style ASCII velocity file'
        read(*,'(a)') filvel
        open(12,file=filvel,form='formatted',status='unknown')
        write(12,'(15hVECTOR velocity,/,2hND,i6,/,6hSTAT 0)') ndgms
        iexist(3) = 1
        
        if(ndf.ge.4)then
          write(*,*)' enter the GMS style ASCII salinity file'
          read(*,'(a)') filslt
          if(filslt(1:1).ne.' ')then
          open(13,file=filslt,form='formatted',status='unknown')
          write(13,'(15hSCALAR salinity,/,2hND,i6,/,6hSTAT 0)') ndgms
          iexist(4) = 1
       write(*,*)'test'
          end if
        end if
        if(ndf.ge.5)then
          write(*,*)' enter the GMS style ASCII temperature file'
          read(*,'(a)') filtmp
          if(filtmp(1:1).ne.' ')then
          open(14,file=filtmp,form='formatted',status='unknown')
          write(14,'(18hSCALAR temperature,/,2hND,i6,/,6hSTAT 0)')ndgms
          iexist(5) = 1
          end if
        end if
         if(ndf.ge.6)then
          write(*,*)' enter the GMS style ASCII sediment conc. file'
          read(*,'(a)') filsed
          if(filsed(1:1).ne.' ')then
          open(15,file=filsed,form='formatted',status='unknown')
          write(15,'(15hSCALAR sed.conc,/,2hND,i6,/,6hSTAT 0)') ndgms
          iexist(6) = 1
          end if
         end if
      else
      
        write(*,*)'  geometry converted'
        stop     
        
      end if
*
* at this point the user has chosen to convert a solution file..
*
      tmax = -10e10
      tmin = -tmax
      
      do itime = 1, 99999
        read(10,end=2) tet
        if(tet.gt.tmax)tmax = tet
        if(tet.lt.tmin)tmin = tet
        ntime = itime
      end do
 2    rewind 10
      write(*,*)' time steps go from 1 to ',ntime
      write(*,*)' time ranges from ',tmin,' to ',tmax
      
              
      write(*,*)' enter the beginning & ending time step to save'
      write(*,*)'         (enter -1 -1 to save all)'
      read(*,*) ifirst, ilast
      if(ifirst.lt.1)then
        ifirst = 1
        ilast = 99999
      end if
       
      
c.....scans through the beginning unwanted steps

      if(ifirst .gt. 1)then
        do itime = 1,ifirst-1
          read(10,end=2)tet, np, ndf,ne,
     &      ndfs,(iresav(k),k=1,ndfs),
     &      ((xvel(k,j),j=1,iresav(k)),k=1,ndf),
     &       (DUM1,J = 1, IRESAV(3)),
     &       (DUM, J = 1, NE), (DUM, J = 1, NP),
     &       (DUM2,J=1,IRESAV(7)),(DUM3,J=1,IRESAV(NDFS)),
     &      (vvel(j),j=1,np), (dfct(j),j=1,ne)
        end do
      endif

      do itime = ifirst, ilast
          
          read(10,end=2)tet, np, ndf,ne,
     &      ndfs,(iresav(k),k=1,ndfs),
     &      ((xvel(k,j),j=1,iresav(k)),k=1,ndf),
     &       (DUM1,J = 1, IRESAV(3)),
     &       (DUM, J = 1, NE), (DUM, J = 1, NP),
     &       (DUM2,J=1,IRESAV(7)),(DUM3,J=1,IRESAV(NDFS)),
     &      (vvel(j),j=1,np), (dfct(j),j=1,ne)
     
        write(*,*)' saving data at time = ',tet
        
        do ll = 2,ndf
          if(iexist(ll).eq.1) write(lu(ll),'(2hTS,f15.4)') tet
        end do
        
        do n = 1, ndgms
*
* get the old node # that corresponds to this new node #
*
          nd = nodold(n)
          
          if(nd.gt.0)then
*
* this is a gms node, save the data
*
            write(11,'(e16.8)') xvel(3,nd)
            
            write(12,'(3e16.8)') xvel(1,nd), xvel(2,nd), vvel(nd)
            
            
            if(iexist(4) .eq. 1) write(13,'(e16.8)') xvel(4,nd)
            if(iexist(5) .eq. 1) write(14,'(e16.8)') xvel(5,nd)
            if(iexist(6) .eq. 1) write(15,'(e16.8)') xvel(6,nd)
            
          end if
          
        end do
      end do
      
 1    close(10)
      close(11)
      close(12)
      if(ndf.ge.4) close(13)
      if(ndf.ge.5) close(14)
      if(ndf.ge.6) close(15)
      
      write(*,*)' all done'
      
      end
      
      
      
        
 
