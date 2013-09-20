      program thist_2_bc
C
C      this program reads time history files and converts
C      them to TABS_MDS bc files
C
      parameter(nsmax=20000,nclmax=20,nmtmax=100,nemax=200)
C
      dimension timin(90000),valin(90000),valin2(90000),valout(90000)
      dimension valout2(90000), bqt(nsmax,nmtmax)
      dimension bhl(nsmax,nclmax), bql(nsmax,nclmax), bqe(nsmax,nemax)
      dimension salt(nsmax,nclmax), icls(nclmax), icls2(nclmax)
      dimension wspeed(nsmax,10),wdir(nsmax,10),mtbwt(nmtmax)
      dimension rfang(nclmax), salt2(nclmax), salt3(nemax)
      dimension salt4(nmtmax), ifpua1(nemax), ifpua2(nmtmax)
      dimension mtrat(nmtmax),rain(nsmax,nclmax),ibqe(nemax)
      dimension mtbqt(nmtmax)
C
      character*80 fname, fileout, chardum
C
      nbw = 0
      nbhl = 0
      nbql = 0
      nra = 0
C
      write(*,*)
      write(*,*) ' *** THIST 2 BC *** '
      write(*,*)
      write(*,*) 'this program reads time history files and converts'
      write(*,*) 'them to TABS_MDS bc files'
      write(*,*)
      write(*,*) 'The time history files must satisfy these conditions:'
      write(*,*) ' (1) all files should be referenced to the same'
      write(*,*) '     time datum'
      write(*,*) ' (2) the first and last time records on each file'
      write(*,*) '     should be less than and greater than the time'
      write(*,*) '     window requested for the TABS-MDS bc file'
      write(*,*)
      write(*,*) 'If there are gaps in the data record of any file, the'
      write(*,*) 'values written to the bc file will be interpolated'
      write(*,*)
      
      write(*,*) 'enter the output bc file name'
      read(*,*) fileout
      write(*,*) 'enter the starting time and ending time for the'
      write(*,*) 'simulation, in hours'
      read(*,*) tstart, tstop
      write(*,*) 'enter the desired time step to be used for the' 
      write(*,*) 'simulation, in hours'
      read(*,*) tstep
      ntsteps = (tstop-tstart)/tstep + 1
      write(*,*)
      write(*,10) ntsteps
 10   format('there will be', I8,' time steps generated')
      write(*,*)
      write(*,*) 'enter the number of BHL cards to be created'
      write(*,*) 'enter a 0 for no BHL cards'
      read(*,*) nbhl
      write(*,*) 'enter the number of BW type cards to be created'
      write(*,*) 'this includes the BW card, and any BWT cards'
      write(*,*) 'delinating material types with wind speed'
      write(*,*) 'taken from additional wind file(s)'
      write(*,*) 'enter a 0 for no BW cards'
      read(*,*) nbw
      write(*,*) 'enter the number of BQL cards to be created'
      write(*,*) 'enter a 0 for no BQL cards'
      read(*,*) nbql
      write(*,*) 'enter the number of RA type cards to be created'
      write(*,*) 'this includes the RA card, and any RAT cards'
      write(*,*) 'delinating material types with rainfall'
      write(*,*) 'taken from additional rain file(s)'
      write(*,*) 'enter a 0 for no RA cards'
      read(*,*) nra
      write(*,*) 'enter the number of BQE cards to be created'
      write(*,*) 'enter a 0 for no BQE cards'
      read(*,*) nbqe
      write(*,*) 'enter the number of BQT cards to be created'
      write(*,*) 'enter a 0 for no BQT cards'
      read(*,*) nbqt
      if (nbhl .gt. 0) then
        do i = 1, nbhl
          write(*,*) 
          write(*,*) ' *** beginning a new entry ***' 
          write(*,*)
          write(*,*) 'enter the continuity line number'
          write(*,*) 'corresponding to the desired BHL card'  
          read(*,*) icl
          icls(i) = icl
          write(*,*) 'enter the filename for the tide data'
          write(*,20) icl
 20       format ('corresponding to continuity line #', I3) 
          read(*,*) fname
          open(10,file=fname,form='formatted',status='old')
          read(10,*) chardum
          write(*,*) 'enter the desired datum shift to be applied to'
          write(*,*) 'this tide data, in feet or meters'
          read(*,*) rdatum
          write(*,*) 'enter the desired phase shift to be applied to'
          write(*,*) 'this tide data, in hours'
          read(*,*) rshift
          write(*,*) 'enter the desired amplitude multiplier'
          write(*,*) 'to be applied to this tide data, in hours'
          read(*,*) rampm
          write(*,*) 'enter the filename for the salt data'
          write(*,20) icl
          read(*,*) fname
          open(15,file=fname,form='formatted',status='old')
          read(15,*) chardum
          k = 0
          do j = 1, 90000
            timin(j) = 0.0
            valin(j) = 0.0
          end do
          rvalmean = 0.0
          icmean = 0
          do j = 1, 90000 
            read(10,*,end=1000) timin(j), valin(j)
            timin(j) = timin(j) + rshift
            rvalmean = rvalmean + valin(j)
            icmean = icmean + 1
          end do
 1000     continue
          if (icmean .eq. 0) icmean = 1
          rvalmean = rvalmean / float(icmean)
          do j = 1, 90000
            valin(j) = (valin(j) - rvalmean)*rampm
     +                 + rvalmean + rdatum 
          end do
          call linint(tstart,tstop,tstep,timin,valin,valout)
          do j = 1, ntsteps
            bhl(j,icl) = valout(j)
          end do
          close(10)
          do j = 1, 90000
            timin(j) = 0.0
            valin(j) = 0.0
          end do
          do j = 1, 90000
            read(15,*,end=1100) timin(j), valin(j)
          end do
 1100     continue
          call linint(tstart,tstop,tstep,timin,valin,valout)
          do j = 1, ntsteps
            salt(j,icl) = valout(j)
          end do
          close(15)
        end do
      end if
C
      if (nbw .gt. 0) then
      do ii = 1, nbw
        write(*,*)
        if (ii .eq. 1) then
          write(*,*) 'enter the filename with the raw wind data'
          write(*,*) 'to be used on the BW card'
          read(*,*) fname
          open(10,file=fname,form='formatted',status='old')
          read(10,*) chardum
        else if (ii .gt. 1) then
          write(*,*) 'enter the filename with the raw wind data'
          write(*,*) 'to be used on a specified BWT card'
          read(*,*) fname
          open(10,file=fname,form='formatted',status='old')
          read(10,*) chardum
          write(*,*) 'enter the material type for this BWT card'
          read(*,*) mtbwt(ii)
        end if
        write(*,*)
        write(*,*) 'enter a 0 if the raw wind direction is given'
        write(*,*) 'as the direction towards which the wind is'
        write(*,*) 'blowing, measured counter-clockwise from the'
        write(*,*) 'east (i.e. the TABS_MDS convention)'
        write(*,*)
        write(*,*) 'Enter a 1 if the raw wind direction is given'
        write(*,*) 'as the direction from which the wind is'
        write(*,*) 'blowing, measured clockwise from the north'
        write(*,*) '(i.e. the standard convention)'
        write(*,*)
        write(*,*) 'Enter a 2 if the wind is given as components,'
        write(*,*) 'based on the direction towards which the wind'
        write(*,*) 'is blowing' 
        write(*,*)
        read(*,*) iwdcon
        write(*,*)
        write(*,*) 'enter a 0 if you do not want to increase the'
        write(*,*) 'wind speed by the land/sea factor (according'
        write(*,*) 'to Hsu)'
        write(*,*)
        write(*,*) 'enter a 1 if you do want to increase the'
        write(*,*) 'wind speed by the land/sea factor (according'
        write(*,*) 'to Hsu)'
        write(*,*)
        write(*,*) 'the land/sea factor is as follows:'
        write(*,*) 'U(sea) = 3.62 + 1.17*U(land), where U is in mph'
        write(*,*)
        read(*,*) ilsfac
        if (ilsfac .eq. 1) then
          write(*,*) 'enter the minimum and maximum angle over which to'
          write(*,*) 'apply the land/sea factor.  This is useful when '
          write(*,*) 'the wind gage is on the shoreline, and you'
          write(*,*) 'only want to modify winds that blow from onshore.'
          write(*,*) 'When specifying the angles, use the TABS-MDS'
          write(*,*) 'convention (i.e. direction towards which the wind'
          write(*,*) 'is blowing, measured counterclockwise from the'
          write(*,*) 'east'
          write(*,*)
          write(*,*) 'you can enter angles between -360 and 720'
          write(*,*) 'with a 360 degree maximum difference between'
          write(*,*) 'the angles'
          write(*,*)
          write(*,*) 'if you want to modify all winds, enter 0 360.'
          write(*,*)
          read(*,*) wangmin, wangmax
        end if
        k = 0
        do j = 1, 90000
          timin(j) = 0.0
          valin(j) = 0.0
          valin2(j) = 0.0
        end do
        do j = 1, 90000
          read(10,*,end=1300) timin(j), valin(j), valin2(j)
          if (iwdcon .eq. 1) valin2(j) = 270.0 - valin2(j) 
          if (iwdcon .lt. 2) then
            if (valin2(j) .lt. 0.0) valin2(j) = valin2(j) + 360.0
            if (valin2(j) .gt. 360.0) valin2(j) = valin2(j) - 360.0
          if (ilsfac .eq. 1) then
            twmin = 0.0
            twmax = wangmax - wangmin
            twtest = valin2(j) - wangmin
            if (twtest .lt. 0.0)  twtest=twtest + 360.0
            if (twtest .gt. 360.0) twtest=twtest - 360.0
            if (twtest .ge. twmin .and. twtest .le. twmax) then
              valin(j) = 3.62 + 1.17*valin(j) 
            end if
          end if
          end if
        end do
 1300   continue
        call linint(tstart,tstop,tstep,timin,valin,valout)
        call linint(tstart,tstop,tstep,timin,valin2,valout2)
        do j = 1, ntsteps
          wspeed(j,ii) = valout(j)
          wdir(j,ii) = valout2(j) 
          if (iwdcon .eq. 2) then
            rx = valout(j)
            ry = valout2(j)
            rmag = sqrt(rx*rx+ry*ry+1.E-8)
            rdum = atan(ry/(rx+1.e-8))*57.29578
            if (rx .lt. 0.0) rdum = rdum + 180.
            wspeed(j,ii) = rmag 
            wdir(j,ii) = rdum 
            if (ilsfac .eq. 1) then
              twmin = 0.0
              twmax = wangmax - wangmin
              twtest = wdir(j,ii) - wangmin
              if (twtest .lt. 0.0)  twtest=twtest + 360.0
              if (twtest .gt. 360.0) twtest=twtest - 360.0
              if (twtest .ge. twmin .and. 
     +            twtest .le. twmax) then
                wspeed(j,ii) = 3.62 + 1.17*wspeed(j,ii)
              end if
            end if
          end if
        end do
        close(10)
      end do
      end if
C
      if (nbql .gt. 0) then
        do i = 1, nbql
          write(*,*)
          write(*,*) ' *** beginning a new entry ***'
          write(*,*)
          write(*,*) 'enter the continuity line number'
          write(*,*) 'corresponding to the desired BQL card'
          read(*,*) icl
          icls2(i) = icl
          write(*,*) 'enter the filename for the flow data'
          write(*,70) icl
 70       format ('corresponding to continuity line #', I3)
          read(*,*) fname
          open(10,file=fname,form='formatted',status='old')
          read(10,*) chardum
          write(*,*) 'enter the desired flow angle for this'
          write(*,*) 'continuity line'
          read(*,*) rfang(icl)
          write(*,*) 'enter the desired salt specification'
          write(*,*) 'to be applied to this flow data'
          read(*,*) salt2(icl)
          write(*,*) 'enter the desired minimum flow'
          write(*,*) 'to be applied at this continuity line'
          read(*,*) flowmin 
          k = 0
          do j = 1, 90000
            timin(j) = 0.0
            valin(j) = 0.0
          end do
          do j = 1, 90000
            read(10,*,end=1500) timin(j), valin(j)
            valin(j) = amax1(valin(j),flowmin)
          end do
 1500     continue
          call linint(tstart,tstop,tstep,timin,valin,valout)
          do j = 1, ntsteps
            bql(j,icl) = valout(j)
          end do
          close(10)
        end do
      end if
C
      if (nra .gt. 0) then
        do ii = 1, nra
          write(*,*)
          if (ii .eq. 1) then
            write(*,*) 'enter the filename with the raw rain/evap data'
            write(*,*) 'to be used on the RA card'
            read(*,*) fname
            open(10,file=fname,form='formatted',status='old')
            read(10,*) chardum
          else if (ii .gt. 1) then
            write(*,*) 'enter the filename with the raw rain/evap data'
            write(*,*) 'to be used on a specified RAT card'
            read(*,*) fname
            open(10,file=fname,form='formatted',status='old')
            read(10,*) chardum
            write(*,*) 'enter the material type for this BWT card'
            read(*,*) mtrat(ii)
          end if
          k = 0
          do j = 1, 90000
            timin(j) = 0.0
            valin(j) = 0.0
          end do
          do j = 1, 90000
            read(10,*,end=1700) timin(j), valin(j)
          end do
 1700     continue
          call linint(tstart,tstop,tstep,timin,valin,valout)
          do j = 1, ntsteps
            rain(j,ii) = valout(j)
          end do
          close(10)
        end do
      end if
C
      if (nbqe .gt. 0) then
        do i = 1, nbqe
          write(*,*)
          write(*,*) ' *** beginning a new entry ***'
          write(*,*)
          write(*,*) 'enter the element number'
          write(*,*) 'corresponding to the desired BQE card'
          read(*,*) iele
          ibqe(i) = iele
          write(*,*) 'enter the filename for the flow data'
          write(*,80) iele 
 80       format ('corresponding to element #', I6)
          read(*,*) fname
          open(10,file=fname,form='formatted',status='old')
          read(10,*) chardum
          write(*,*) 'enter a 1 if the data format is'
          write(*,*) 'flow per unit area (L/T)'
          write(*,*) 'enter a 2 if the data format is'
          write(*,*) 'total flow (L^3/T)'
          read(*,*) ifpua1(i)
          write(*,*) 'enter the desired salt specification'
          write(*,*) 'to be applied to this flow data'
          read(*,*) salt3(i)
          write(*,*) 'enter the desired minimum flow'
          write(*,*) 'to be applied at this element'
          read(*,*) flowmin
          k = 0
          do j = 1, 90000
            timin(j) = 0.0
            valin(j) = 0.0
          end do
          do j = 1, 90000
            read(10,*,end=1800) timin(j), valin(j)
            valin(j) = amax1(valin(j),flowmin)
          end do
 1800     continue
          call linint(tstart,tstop,tstep,timin,valin,valout)
          do j = 1, ntsteps
            bqe(j,i) = valout(j)
          end do
          close(10)
        end do
      end if
C
      if (nbqt .gt. 0) then
        do i = 1, nbqt
          write(*,*)
          write(*,*) ' *** beginning a new entry ***'
          write(*,*)
          write(*,*) 'enter the material type number'
          write(*,*) 'corresponding to the desired BQT card'
          read(*,*) imt
          mtbqt(i) = imt
          write(*,*) 'enter the filename for the flow data'
          write(*,90) imt
 90       format ('corresponding to material type #', I6)
          read(*,*) fname
          open(10,file=fname,form='formatted',status='old')
          read(10,*) chardum
          write(*,*) 'enter a 1 if the data format is'
          write(*,*) 'flow per unit area (L/T)'
          write(*,*) 'enter a 2 if the data format is'
          write(*,*) 'total flow (L^3/T)'
          read(*,*) ifpua2(i)
          write(*,*) 'enter the desired salt specification'
          write(*,*) 'to be applied to this flow data'
          read(*,*) salt4(i)
          write(*,*) 'enter the desired minimum flow'
          write(*,*) 'to be applied at this material type'
          read(*,*) flowmin
          k = 0
          do j = 1, 90000
            timin(j) = 0.0
            valin(j) = 0.0
          end do
          do j = 1, 90000
            read(10,*,end=1900) timin(j), valin(j)
            valin(j) = amax1(valin(j),flowmin)
          end do
 1900     continue
          call linint(tstart,tstop,tstep,timin,valin,valout)
          do j = 1, ntsteps
            bqt(j,i) = valout(j)
          end do
          close(10)
        end do
      end if
C
      open(20,file=fileout,form='formatted',status='unknown')
      open(30,file='tide.txt',form='formatted',status='unknown')
      open(40,file='wind.txt',form='formatted',status='unknown')
      open(50,file='flow.txt',form='formatted',status='unknown')
      open(60,file='rain.txt',form='formatted',status='unknown')
      open(70,file='elemental-flow.txt',form='formatted',
     +status='unknown')
      open(80,file='material-type-flow.txt',form='formatted',
     +status='unknown')
      time = tstart
      do i = 1, ntsteps
        if (nbhl .gt. 0) then
          do j = 1, nbhl
            write(20,200) icls(j), bhl(i,icls(j)), salt(i,icls(j))
 200        format('BHL ', I6, 2f11.5,' -1  -1')
          end do
          write(30,205) time, (bhl(i,icls(j)),salt(i,icls(j)),j=1,nbhl)
        end if
 205    format(41f11.2)
        if (nbw .gt. 0) then 
          write(20,210) wspeed(i,1),wdir(i,1)
 210      format('BW     1', 2f9.3)
          write(40,215) time, (wspeed(i,j), wdir(i,j), j=1,nbw)
 215      format(f11.2, 20f9.3)
          if (nbw .gt. 1) then
            do j = 2, nbw 
              write(20,220) mtbwt(j), wspeed(i,j), wdir(i,j)
 220          format('BWT ', I4, 2f9.3)
            end do
          end if
        end if
        if (nbql .gt. 0) then
          do j = 1, nbql
            write(20,225) icls2(j), bql(i,icls2(j)), 
     +      rfang(icls2(j)), salt2(icls2(j))
 225        format('BQL ', I6, f11.2, 2f11.5,' -1  -1')
          end do
          write(50,230) time, (bql(i,icls2(j)),j=1,nbql)
        end if
 230    format(21f11.2)
        if (nra .gt. 0) then
          write(20,235) rain(i,1)
 235      format('RA     1', f10.5)
          write(60,240) (time, rain(i,j), j=1,nra) 
 240      format(f11.2, 10f10.5)
          if (nra .gt. 1) then
            do j = 2, nra
              write(20,245) mtrat(j), rain(i,j)
 245          format('RAT ', I4, f10.5)
            end do
          end if
        end if
        if (nbqe .gt. 0) then
          do j = 1, nbqe
            if (ifpua1(j) .eq. 1) then
              write(20,250) ibqe(j), ifpua1(j), bqe(i,j),
     +        salt3(j)
            else
              write(20,251) ibqe(j), ifpua1(j), bqe(i,j),
     +        salt3(j)
            end if
 250        format('BQE ', I8, I4, f12.9, f7.2,' -1  -1')
 251        format('BQE ', I8, I4, f11.2, f7.2,' -1  -1')
          end do
          write(70,255) time, (bqe(i,j),j=1,nbqe)
        end if
 255    format(50f9.1)
        if (nbqt .gt. 0) then
          do j = 1, nbqt
            if (ifpua2(j) .eq. 1) then
              write(20,260) mtbqt(j), ifpua2(j), bqt(i,j),
     +        salt4(j)
            else
              write(20,261) mtbqt(j), ifpua2(j), bqt(i,j),
     +        salt4(j)
            end if
 260        format('BQT ', I8, I4, f12.9, f7.2,' -1  -1')
 261        format('BQT ', I8, I4, f11.2, f7.2,' -1  -1')
          end do
          write(80,265) time, (bqe(i,j),j=1,nbqe)
        end if
 265    format(21f11.2)
        write(20,300) time
 300    format ('END at time = ', F11.2)
        time = time + tstep
       end do
       close(20)
       close(30)
C
       stop
       end      
   
      subroutine linint(tstart,tstop,tstep,timin,valin,valout)
C
      dimension timin(90000),valin(90000),timout(90000),valout(90000) 
C
      do i = 1, 90000
        timout(i) = 0.0
        valout(i) = 0.0
      end do
C
      itout = int((tstop - tstart)/tstep) + 1
      kstart = 1
 10   continue
      if (timin(kstart) .lt. tstart) then
        kstart = kstart + 1
        go to 10
      end if
      time = tstart
      do i = 1, itout
        timout(i) = time
 20     continue
        if (timin(kstart) .eq. time) then
          timout(i) = time
          valout(i) = valin(kstart)
          kstart = kstart + 1
          time = time + tstep
        else if (timin(kstart) .lt. time) then
          kstart = kstart + 1
          go to 20
        else if (timin(kstart) .gt. time) then
          timout(i) = time
          valout(i) = valin(kstart-1) + (valin(kstart)-valin(kstart-1))*
     +                (time-timin(kstart-1))/
     +                (timin(kstart)-timin(kstart-1))
          time = time + tstep
        end if
      end do
      return
      end
