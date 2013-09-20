      PROGRAM CHOPIT
c      
c      This program was developed to provide 2d vertical chops of the 3D
c      finite element meshes used in RMA-10.  The modeler can then use 
c      previously develped 2D plotting tools to present the 3D data relative
c      to the chopped plane.  The vertical scale is distorted as defined in
c      the input to become the Y-coordinate in the X-Y 2D plane.  The X-coord
c      is the cummulative horizontal distance along a series of surface nodes
c      of the 3D mesh.  The X-component of velocity is the projection the 3D
c      horizontal velocity onto the plane of the chop and the Y-component of
c      velocity in the 2D plane is the 3D vertical velocity (also scaled).  
c      The water depth for the RMA2 binary file is set up to be the normal
c      component of the 3D velocity to the 2D plane; i.e. the portion of the 
c      horizontal 3D velocity perpendicular to the2D plane.  Because of the
c      need for the 2D geometry file to remain static with time the 2D plane 
c      is relative to the "z-prime" transformed 3D mesh with the water surface
c      at a fixed elevation.
c
c      Coded by J. V. Letter, Jr.  for the CRAY-YMP
c
c      The program is set up to add the TABS-MD banners with the minimum 
c      information needed to keep the TABS-MD plotting utilities from dying.
c
C GLB change to INCLUDE 'blkcom_v2.chopit
      INCLUDE 'blkcom_v2.chopit'
C GLB add the following common block
C ... needed for banners
      COMMON  /DMSCHR/ HEADER, IDMS(15), BANSLC(2),
     *                 BANGFG(5), BANRM2(5), BANRM4(5), EMPTY(5)
      COMMON  /DMSREC/ IREC(40),FREC(40), IPACKB(1200), IPACKT(77)
      CHARACTER        HEADER*77,  IDMS*80, BANSLC*80,
     *                 BANGFG*80, BANRM2*80, BANRM4*80, EMPTY*80
C
C GLB change filename*15 to filename*80
      CHARACTER DOTS*20,FILENAME*80
      DATA DOTS/'....................'/
      DATA TITLE(1:44)/'CHOPIT OUTPUT FROM RMA10 FOR POST-PROCESSING'/
C GLB add the following 38 lines
C******************************************************************
C
C
      DATA IREC/40*0/, FREC/40*0.0/
      DATA IDEBUG /0/, IEOF /0/
C-
C                      123456789-123456789-123456789-123456789-
      DATA BANSLC(1)/'CEWES-HE PROGRAM CHOPIT 2.0             '/
      DATA BANSLC(2)/'Last modified  10-12-1995               '/
C
      DATA BANGFG(1)/'CEWES-HE DMS BANNER HEADINGS FOR GFGEN  '/
      DATA BANGFG(2)/'ENGLISH UNITS APPLIED IN THIS GEOMETRY  '/
      DATA BANGFG(3)/'GFGEN VERSION 4.25         1-D AND 2-D  '/
      DATA BANGFG(4)/'CAPABILITY.   LAST MOD DATE 05-02-1991  '/
      DATA BANGFG(5)/'        Run thru CHOPIT                 '/
C
C                      123456789-123456789-123456789-123456789-
      DATA BANRM2(1)/'CEWES-HE DMS BANNER HEADINGS FOR RMA2-V '/
      DATA BANRM2(2)/'ENGLISH UNITS APPLIED IN THIS HYDRO     '/
      DATA BANRM2(3)/'RMA2-V VERSION 4.25         1-D AND 2-D '/
      DATA BANRM2(4)/'CAPABILITY.  LAST MOD DATE  10-01-1991  '/
      DATA BANRM2(5)/'        Run through CHOPIT              '/
C
C                      123456789-123456789-123456789-123456789-
      DATA BANRM4(1)/'CEWES-HE DMS BANNER HEADINGS FOR RMA4   '/
      DATA BANRM4(2)/'ENGLISH UNITS APPLIED IN THESE RESULTS  '/
      DATA BANRM4(3)/'RMA4-V VERSION 3.00+        1-D AND 2-D '/
      DATA BANRM4(4)/'CAPABILITY.   LAST MOD DATE  04-01-1990 '/
      DATA BANRM4(5)/'        Run through CHOPIT              '/
C
      DATA EMPTY(1) /'........................................'/
      DATA EMPTY(2) /'........................................'/
      DATA EMPTY(3) /'........................................'/
      EMPTY(4)(1:40)  = BANSLC(1)(1:40)
      EMPTY(5)(1:40)  = BANSLC(2)(1:40)
C-
C-
C*******************************************************************
c
c     Initialize TABS-MD banners
c
      DO 8 J=1,15
      DO 6 K=1,4
      K1 = (K-1)*20 + 1
      K2 = K1 + 19
      BANNER (J)(K1:K2) = DOTS
    6 CONTINUE
    8 CONTINUE
      BANNER (1)(1:6) = ' DMS ='
c
c    Initialize integers
c
c    NQ=no. of quality variables
c    NDF = no. of degrees of freedom
c    NTS = no. of timesteps
c
      IBR2 = 0
      IBR4 = 0
      I2OUT = 7
      I4OUT = 8
      IGOUT = 9
C GLB change IGIN = 3 to IGIN = 4
      IGIN = 4
      I10IN = 3      
      NQ = 1
C GLB change NDF from 4 to 6
      NDF = 6
C      NTS = 500 
C GLB-NSF add the following initializations
      DO I = 1, NMAX 
        DO J = 1,NDF
          VEL(J,I) = 0.0
        END DO
        DELBED(I) = 0.0
        BSHR(I) = 0.0
      END DO
C GLB allow the user to specify NTS
      PRINT *,'ENTER THE NUMBER OF TIME STEPS TO BE USED'  
      PRINT *,'(NOTE: TREAT STEADY STATE AS A TIME STEP)'
      READ (*,220) NTS
 220  FORMAT(I5)
c
c     enter the filenames for input and output
c
C GLB add the following read statement
c
      PRINT *,'ENTER THE RMA10 OUTPUT 3D-BINARY GEOMETRY FILE'
      READ (*,221) FILENAME
 221  FORMAT(A15)
      OPEN(UNIT=IGIN,FILE=FILENAME,FORM='UNFORMATTED',STATUS='OLD')
C GLB
      PRINT *,'ENTER THE RMA10 BINARY OUTPUT FILE FOR PROCESSING'
      READ (*,222) FILENAME
 222  FORMAT(A15)
      OPEN(UNIT=I10IN,FILE=FILENAME,FORM='UNFORMATTED',STATUS='OLD')
c
c       go ahead and read the 3D geometry data
c
      CALL R10GEOIN
c
      print *,'The 3D geometry has been read; no. of elements & nodes'
      print *, 'ne= ',ne , 'np = ', np
c
c     Output files
c
c          The 2D  TABS-MD binary geometry file
c
      print *,'ENTER THE OUTPUT 2D BINARY GEOMETRY FILE NAME (TABS-MD)'
      READ (*,222)  FILENAME
      OPEN(UNIT=IGOUT,FILE=FILENAME,FORM='UNFORMATTED',STATUS='UNKNOWN')
c
c     The 2D RMA2 look-alike binary file
c
      PRINT *,'ENTER THE OUTPUT 2D BINARY RMA2-TYPE FILE NAME'
      READ (*,222) FILENAME
      OPEN(UNIT=I2OUT,FILE=FILENAME,FORM='UNFORMATTED',STATUS='UNKNOWN')
c
c    The 2D RMA-4 look-alike binary file
c
      PRINT *,'ENTER THE OUTPUT 2D BINARY RMA4-TYPE FILE NAME'
      READ (*,222) FILENAME
      OPEN(UNIT=I4OUT,FILE=FILENAME,FORM='UNFORMATTED',STATUS='UNKNOWN')
C GLB-MCS alter the program to allow chopping for salinity
C GLB-MCS tempterature or sediment.  Changes for this purpose
C GLB-MCS are dentoed with C GLB-MCS
C GLB-MCS add the following 10 lines
        ICK = 0
        WRITE (*,*) ' Enter a 1 to chop for salinity'
        WRITE (*,*) '         2 to chop for temperature'
        WRITE (*,*) '         3 to chop for sediment'
C GLB-NSF add the following 2 lines
        WRITE (*,*) '         4 to chop for delbed (RMA10-SED only)'
        WRITE (*,*) '         5 to chop for bed shear (RMA10-SED only)'
        READ  (*,*) ICK
C GLB-NSF change GT 3 to GT 5
        IF (ICK .LT. 1 .OR. ICK .GT. 5) ICK = 1
        IF (ICK .EQ. 1) WRITE(*,*) 'chopping for SALINITY'
        IF (ICK .EQ. 2) WRITE(*,*) 'chopping for TEMPERATURE'
        IF (ICK .EQ. 3) WRITE(*,*) 'chopping for SEDIMENT'
C GLB-NSF add the following 2 lines
        IF (ICK .EQ. 4) WRITE(*,*) 'chopping for DELBED'
        IF (ICK .EQ. 5) WRITE(*,*) 'chopping for BED SHEAR'
        WRITE (*,*)
c
c   summary output in ASCII format with diagnostics
c
      PRINT *,'NOW ENTER THE FILE NAME FOR PRINTED SUMMARY OUTPUT'
      READ (*,222) FILENAME
      OPEN(UNIT=10,FILE=FILENAME,FORM='FORMATTED',STATUS='UNKNOWN')
c
c    The ASCII output file for possible use in other programs
c    of the 2D distorted chopped geometry mesh
c
      PRINT *,'OK .. ONE MORE FILE NAME ...'
      PRINT *,'ENTER THE FILENAME FOR THE TABS-MD FORMATTED GEOMETRY'
      PRINT *,' OUTPUT FILE FOR THE CHOPPED 2D PLANE FROM THE 3D MESH'
      READ (*,222) FILENAME
      OPEN(UNIT=11,FILE=FILENAME,FORM='FORMATTED',STATUS='UNKNOWN')
c
c   initialize the IMATs for reconstruction of the 2D mesh
c
       DO 10 I=1,NE
       IMAT(I) = 0
   10 CONTINUE
      IF(ISTOP.EQ.1) STOP
   20 CONTINUE 
c
c   At this point the user inputs the series of surface nodes of the 3D mesh
c   which will be used to drop vertically downward to create the chopped 
c   mesh.  
c
c     first the number of nodes, then the string of nodes
c
       PRINT *, ' ENTER THE NUMBER OF NODES IN YOUR CUT LINE'
      PRINT *,'   COUNT BOTH CORNERS AND MIDSIDE NODES '
      READ *, NS
c
c     Check to see if the number of nodes is odd - if not ask again
c
      IF(MOD(NS,2).NE.1) THEN
       PRINT *,
     * 'NUMBER OF SURFACE NODES MUST BE ODD TO FOR ELEMENT STRUCTURE'
      GOTO 20
      ENDIF
c
c    Now ask for the string of surface nodes
c
      PRINT *, 'ENTER A STRING OF  SURFACE NODES ALONG WHICH TO CUT'
      PRINT *, '  ****** INCLUDE MIDSIDE NODES *********'
      READ *, (NODES(K), K=1,NS)
c
c    Get the vertical distortion to be applied to the vertical coordinate
c    from the 3D mesh to become the Y-coordinate of the chopped mesh.
c
      PRINT *, ' ENTER THE VERTICAL DISTORTION DESIRED IN PLOTS '
      PRINT *, '   DEPTHS WILL BE STRETCHED  '
      READ *,  VPULL
c
c    Determine whether the chopped mesh will use the 3D mesh node numbers
c     or will renumber.  Using the 3D node numbers has not been fully tested.
c
      PRINT *,'IF YOU WANT TO USE 3D NODE NUMBERS IN THE CHOPPED MESH'
      PRINT *,'ENTER THE VALUE 1 (CREATES BIG FILES); OR IF YOU WANT TO'
      PRINT *,'USE NEW SEQUENTIAL NUMBERS ENTER THE VALUE 0 (A CROSS-'
      PRINT *,'REFERENCE TABLE WILL BE OUTPUT ON SUMMARY ASCII FILE)'
      READ *, IXREF
      PRINT *,'IXREF ENTERED AS  ',IXREF
C
C    Initialize the distance variables
c
      X = 0.0
      DS = 0.
c
c   set NPN (number of nodes in new mesh) initially to NS (no. surface nodes)
c
      NPN = NS
c
c    loop over all surface nodes to generate subsurface node numbers and
c    referencing
c
      DO 175 J = 1,NS
c
c           the incremental distance DS (initially 0.) is computed later
c
      X = X + DS
c
c   NV is the number of nodes vertically in 3d mesh at this surface node NODES
c
      NV = NDEP(NODES(J))
      print *, J, 'NV = ', NV
c
c   NODEC  gives the new node numbers for the old 3D node numbers NODES
c
      NODEC(NODES(J)) = J
      NN = NODES(J)
c
c   set new coordinates for the 3d surface nodes
c
      ELEV = CORD(NN,3)
      CORDN(J,1) = X
c
c       the vertical coordinates from the 3d model are now distorted
c       for the surface nodes (these will become the maximum y-coordinate
c       in the new mesh)
c
      CORDN(J,2) = ELEV * VPULL
      write (10,1010) j,nn, cordn(j,1),cordn(j,2)
 1010 FORMAT('J = ',I6,'  NODES(J)= ', I6,'   X=',F12.2,'   Y=  ',F12.2)
c
c    Now set coordinates for the 3d subsurface nodes, renumbering as we go
c
      DO 150 K = 1, NV-1
      NPN = NPN + 1
c
c    NREF is the refernce node number for the 3d subsurface nodal numbers
c    which comes from the 3d binary geometry file
c
      NN = NREF(NODES(J)) + K
      CORDN(NPN,1) = X
      CORDN(NPN,2) = CORD(NN,3)*VPULL
      NODES(NPN) = NN
      NODEC(NN) = NPN
      WRITE(10,1010) NPN,NODES(NPN),(CORDN(NPN,KK),KK=1,2)
  150 CONTINUE
c
c    now we must compute the incremental distance along the chop line for the
c    next surface node's X-coordinate in the new mesh
c
      DX = CORD(NODES(J+1),1)-CORD(NODES(J),1)
      DY = CORD(NODES(J+1),2)-CORD(NODES(J),2)
      DS = (DX*DX + DY*DY)**0.5
c
c    The orientation angle for the chop line must be computed for use in the 
c    projection of 3d horizontal velocities to the chop plane
c
      THETAS(J) = ATAN2(DY,DX)
c
c    The orientation angle is now adjusted to account for bends in the chop
c    line, based on the incremental distances. DSP is the DS for the previous
c    step along the chop line.
c
      IF(J.GT.1) THEN
        DSS = DS + DSP
        THETAS(J) = (THETAS(J)*DS+THETAS(J-1)*DSP)/DSS
C GLB add the following to fix sign problem at the downstream boundary
        if (j .eq. ns) thetas(j) = thetas(j-1)
      ENDIF
      DSP = DS
  175 CONTINUE
C %%%
      do i = 1, 50000
        do j = 1, 8
          nopn(i,j) = 0
          nopnt(i,j) = 0
        end do
      end do
      neuse = 0
      do i = 1, ne
        niect = 0
        if (nop(i,9) .gt. 0) then
          do j = 1, 20 
            do k = 1, npn
              if (nop(i,j) .eq. nodes(k)) then
                niect = niect + 1
                ntemp(niect) = nodec(nodes(k)) 
                nsurf(nodec(nodes(k))) =
     +            iabs(nodec(nsurf(nodes(k))))
              end if  
            end do
          end do
        end if
        if (niect .ge. 6) then
          neuse = neuse + 1
          nnie(neuse) = niect
          do j = 1, niect
            nopnt(neuse,j) = ntemp(j)
          end do
          imat(neuse) = 1
          imatn(neuse) = 1    
        end if
      end do 
      do i = 1, neuse
        do j = 1, 8 
          do k = 1, 8
            if ( nopnt(i,j) .ne. 0 .and. nopnt(i,k).ne. 0 
     +           .and. j .lt. k) then
              if (nopnt(i,j) .gt. nopnt(i,k)) then 
                 itemp = nopnt(i,j)
                 nopnt(i,j) = nopnt(i,k)
                 nopnt(i,k) = itemp 
              end if
            end if
          end do
        end do
        iflg = 0
        do j = 1, 8
          if ((nopnt(i,j) .eq. nsurf(nopnt(i,j)))
     +       .and. nopnt(i,j) .ne. 0) iflg = 1
        end do 
        if (iflg .eq. 1) then
        if (nnie(i) .eq. 6) then
          if (nsurf(nopnt(i,1)) .eq. nsurf(nopnt(i,4))) then
            nopn(i,1) = nopnt(i,1)
            nopn(i,2) = nopnt(i,4)
            nopn(i,3) = nopnt(i,5)
            nopn(i,4) = nopnt(i,6)
            nopn(i,5) = nopnt(i,3)
            nopn(i,6) = nopnt(i,2)
          else
            nopn(i,1) = nopnt(i,1)
            nopn(i,2) = nopnt(i,4)
            nopn(i,3) = nopnt(i,6)
            nopn(i,4) = nopnt(i,5)
            nopn(i,5) = nopnt(i,3)
            nopn(i,6) = nopnt(i,2)
          end if
        else
          nopn(i,1) = nopnt(i,1)
          nopn(i,2) = nopnt(i,4)
          nopn(i,3) = nopnt(i,5)
          nopn(i,4) = nopnt(i,6)
          nopn(i,5) = nopnt(i,8)
          nopn(i,6) = nopnt(i,7)
          nopn(i,7) = nopnt(i,3)
          nopn(i,8) = nopnt(i,2)
        end if
        else
        if (nnie(i) .eq. 6) then
          if (nsurf(nopnt(i,1)) .eq. nsurf(nopnt(i,2))) then
            nopn(i,1) = nopnt(i,1)
            nopn(i,2) = nopnt(i,2)
            nopn(i,3) = nopnt(i,3)
            nopn(i,4) = nopnt(i,5)
            nopn(i,5) = nopnt(i,6)
            nopn(i,6) = nopnt(i,4)
          else
            nopn(i,1) = nopnt(i,1)
            nopn(i,2) = nopnt(i,3)
            nopn(i,3) = nopnt(i,6)
            nopn(i,4) = nopnt(i,5)
            nopn(i,5) = nopnt(i,4)
            nopn(i,6) = nopnt(i,2)
          end if
        else
          nopn(i,1) = nopnt(i,1)
          nopn(i,2) = nopnt(i,2)
          nopn(i,3) = nopnt(i,3)
          nopn(i,4) = nopnt(i,5)
          nopn(i,5) = nopnt(i,8)
          nopn(i,6) = nopnt(i,7)
          nopn(i,7) = nopnt(i,6)
          nopn(i,8) = nopnt(i,4)
        end if 
        end if
      end do
      nel = neuse 
      do i = 1, neuse
        do j = 1, neuse
          if (i .lt. j .and. i .le. nel .and. j .le. nel) then
            irpflg = 1
            do k = 1, 8
              if (nopn(i,k) .ne. nopn(j,k)) irpflg = 0 
            end do
            if (irpflg .eq. 1) then
              do k = 1,8
                nopnt(1,k) = nopn(nel,k)  
                nopn(nel,k) = 0 
                nopn(j,k) = nopnt(1,k)
              end do
              nel = nel - 1
            end if
          end if
        end do      
      end do  
C %%%
c
      DO 320 I=1,NEL
      WRITE(10,1011) I,(NOPN(I,K),K=1,8)
 1011 FORMAT(10I7)
c
c    Loop 310 computes the coordinates for the midside nodes.  This actually
c    corrects the coordinates for special cases where you may have cheated
c    in the selection of your surface nodes
c
      DO 310 J=2,8,2
      JP = J+1
      IF(J.EQ.8. OR .NOPN(I,JP).EQ. 0) JP = 1
C GLB add the following clause
      if (nopn(i,j-1) .eq. 0) goto 310
      NJ = NODEC(NOPN(I,J))
      NM = NODEC(NOPN(I,J-1))
      NP = NODEC(NOPN(I,JP))
      CORDN(NJ,1) = (CORDN(NM,1)+CORDN(NP,1) )/2.
      CORDN(NJ,2) = (CORDN(NM,2) + CORDN(NP,2))/2.
  310 CONTINUE
  320 CONTINUE
      print *, 'nel = ', nel
      DX = CORD(NODES(NS),1) - CORD(NODES(1),1)
      DY = CORD(NODES(NS),2) - CORD(NODES(1),2)
c    
c     This section handles the geometry output
c
c      If IXREF = 1 then the user has asked that the new mesh have the same 
c      node numbers as the 3D mesh.  So we have to store the new mesh 
c      coordinates in the old mesh CORD array.  The approach creates a mesh 
c      with a whole bunch of unused nodes.
c
      IF(IXREF.EQ.1) THEN
         DO 350 I= 1,NMAX
         CORD(I,1) = 0.
         CORD(I,2) = 0.
  350    CONTINUE
         DO 400 I = 1,NPN
         CORD(NODES(I),1) = CORDN(I,1)
         CORD(NODES(I),2) = CORDN(I,2)
  400    CONTINUE
         print *, 'ixref= ',ixref,'  nel= ',nel
         CALL GEOWRITE
      ENDIF
c
c   However, if IXREF = 0  the user has asked for a new mesh with no unused
c    nodes.  So everything is cool and just got to GEOWRIT2 to output file
c
      IF(IXREF.EQ.0) THEN
         CALL GEOWRIT2
c
c   Output the cross-refernce nodal list for new vs old mesh to summary file
c
         WRITE(10,1020) 
         DO 333 JL=1,NPN
         WRITE(10,1021) JL, NODES(Jl),NODEC(NODES(JL))
  333    CONTINUE
 1020    FORMAT(/,'  CHOPIT       RMA10',
     *    /,      '   NODE         NODE',/)
 1021    FORMAT ( I7,2I13)
c
c      List the new element connections and nodal coordinates to the
c      summary file   
c
         DO 430 JEL = 1,NEL
         WRITE(10,1050)JEL,(NODEC(NOPN(JEL,K)),K=1,8),IMATN(JEL),TH(NEL)
  430    CONTINUE
 1050    FORMAT('GE',10I6,F10.2)
         DO 440 JNN = 1,NPN
         AO = 0.
         WRITE(10,1051) JNN,(CORDN(JNN,K),K=1,2),ao 
  440    CONTINUE
 1051    FORMAT('GNN',I6,3F12.2)
c
      ENDIF
c
c    Now we process the timeseries of model results.  NTS = number of timesteps
c
      DO 500 I=1,NTS
c
c    Each call to R10READ reads a single timestep of results
c
      CALL R10READ
c
c    loop 450 spans all of the new mesh nodes to process and store model results
c
      DO 450 J=1,NPN
      NN= NODES(J)
C GLB replace the following line with the next one
C       K = NODEC(NSURF(NN))
       K = NODEC(ABS(NSURF(NN)))
c
c    project the 3D horizontal velocities onto the chop plane (U) and get the
c    normal component to the chop plane (UP)
c
      U = VEL(1,NN)*COS(THETAS(K)) + VEL(2,NN)*SIN(THETAS(K))
      UP = VEL(1,NN)*SIN(THETAS(K)) + VEL(2,NN)*COS(THETAS(K))
c      UP = 10.
c  *************************************************
c    WARNING - I had some trouble with some postprocessing of the UP array
c    when I stored the UP values as the depth variable in the dummy RMA2 binary
c    file.  If the normal velocity UP goes negative some of our codes assume
c    the element is dry and don't plot anything.  So for plotting salinity or
c    in-plane velocities I had to set UP=10 (or any finite positive value).  I 
c    haven't come up with a solution yet.
c  *****************************************************************
c   
c    The vertical velocity is stored into the y-component for the chop mesh
c    and must also be scaled appropriately with the vertical distortion
c
      V = VVEL(NN)*VPULL
C GLB-MCS change VEL(4,NN) to VEL(ICK+3,NN)
C GLB-NSF replace the following line with the next 
C      S = VEL(ICK+3,NN) 
      IF (ICK .LE. 3) THEN
        S = VEL(ICK+3,NN)
      ELSE IF (ICK .EQ. 4) THEN
        S = DELBED(NN)
      ELSE IF (ICK .EQ. 5) THEN
        S = BSHR(NN)
      END IF 
      VELN(J,1) = U
      VELN(J,2) = V
      VELN(J,3) = UP+100.
      SAL(J) = S   
  450 CONTINUE
c
c   Now output this timestep to the dummy RMA2 and RMA4 binary files     
c
c     First for the case with 3D node numbers
c
      IF(IXREF.EQ.1) THEN
        DO 460 J=1,NPN
        NN = NODES(J)
        VEL(1,NN) = VELN(J,1)
        VEL(2,NN) = VELN(J,2)
        VEL(3,NN) = VELN(J,3)
        VEL(4,NN) = SAL(J)
  460   CONTINUE
        BANNER(6)(1:6) = ' DMS ='
      CALL R2OUT
       BANNER(11)(1:6) = ' DMS ='
      CALL R4OUT
      ENDIF
c
c    And also for the case of no unused node numbers
c
      IF(IXREF.EQ.0) THEN
       BANNER(6)(1:6) = ' DMS ='
      CALL R2OUT2
       BANNER(11)(1:6) = ' DMS ='
      CALL R4OUT4
      ENDIF
  500 CONTINUE
c
c    Mission accomplished, captain
c
      STOP
      END
c
      SUBROUTINE R10GEOIN
C GLB change to INCLUDE 'blkcom_v2.chopit
      INCLUDE 'blkcom_v2.chopit'
      READ (IGIN) NP,NE,NPM,NES,((CORD(J,K),SPEC(J,K),K=1,3),ALFA(J),
     *  NFIX(J),A0(J),NSURF(J),J=1,NP), (NDEP(J),NREF(J), J=1,NPM),
     *((NOP(J,K),K=1,20),NCORN(J),IMAT(J),TH(J),NFIXH(J),J=1,NE)
       print *, 'np,ne,npm,nes', np,ne,npm,nes
C GLB add this statement
      NPORIG = NP
      RETURN 
      END
c
      SUBROUTINE GEOWRITE
C GLB change to INCLUDE 'blkcom_v2.chopit
      INCLUDE 'blkcom_v2.chopit'
C GLB add the following common block
C ... needed for banners
      COMMON  /DMSCHR/ HEADER, IDMS(15), BANSLC(2),
     *                 BANGFG(5), BANRM2(5), BANRM4(5), EMPTY(5)
      COMMON  /DMSREC/ IREC(40),FREC(40), IPACKB(1200), IPACKT(77)
      CHARACTER        HEADER*77,  IDMS*80, BANSLC*80,
     *                 BANGFG*80, BANRM2*80, BANRM4*80, EMPTY*80
C
      AO = 0.
      IREC(1) = 425
      WRITE(IGOUT) (BANNER(I),I=1,15)
      WRITE(IGOUT) (IREC(I),I=1,40),(FREC(I),I=1,40)
      WRITE(IGOUT) TITLE
      print *,'inside GEOWRITE; NP=', np,' and NEL = ', nel
      WRITE(IGOUT) NP,NEL,((CORD(J,K),K=1,2), ALFA(J),AO,J=1,NP),
     *            ((NOPN(J,K),K=1,8),IMAT(J),TH(J),NFIXH(J),J=1,NEL)
      DUM = 0.
      WRITE(IGOUT) (DUM,DUM,DUM,DUM,J=1,NP)
      DO 650 I=1,NEL
C GLB change WRITE(10,1010) to WRITE(11,1010) and add TH(I)
      WRITE(11,1010) I,(NOPN(I,K),K=1,8),Imat(I),TH(I)
C GLB alter the following format statement to look like statement 1100
 1010 FORMAT('GE',10I6,F10.2)
  650 continue
C GLB change NP to NPORIG
       DO 700 I=1,NPORIG
C GLB add the following do loop and if-then clause
         DO II=1,NE
           DO JJ=1,8
             IF (I .EQ. NOPN(II,JJ)) THEN
C GLB change WRITE(10,1011) to WRITE(11,1011) and add AO
               WRITE(11,1011) I, CORD(I,1),CORD(I,2),AO
               GO TO 703 
             END IF
           END DO
         END DO 
703   CONTINUE
C GLB alter the following format statemt to look like statement 1101
 1011 FORMAT('GNN',I8,3F12.2)
  700  CONTINUE
      RETURN
      END
c
      SUBROUTINE GEOWRIT2
C GLB change to INCLUDE 'blkcom_v2.chopit
      INCLUDE 'blkcom_v2.chopit'
C GLB add the following common block
C ... needed for banners
      COMMON  /DMSCHR/ HEADER, IDMS(15), BANSLC(2),
     *                 BANGFG(5), BANRM2(5), BANRM4(5), EMPTY(5)
      COMMON  /DMSREC/ IREC(40),FREC(40), IPACKB(1200), IPACKT(77)
      CHARACTER        HEADER*77,  IDMS*80, BANSLC*80,
     *                 BANGFG*80, BANRM2*80, BANRM4*80, EMPTY*80
C
      AO = 0.
      IREC(1) = 425
      WRITE(IGOUT) (BANNER(I),I=1,15)
      WRITE(IGOUT) (IREC(I),I=1,40),(FREC(I),I=1,40)
      WRITE(IGOUT) TITLE
C %%% rempve nodec below
      WRITE(IGOUT) NPN,NEL,((CORDN(J,K),K=1,2), ALFA(J),AO,J=1,NPN),
     *  ((NOPN(J,K),K=1,8),IMATN(J),TH(J),NFIXH(J),J=1,NEL)
      DUM = 0.
c
c     This is where the ASCII file for new mesh with no unused nodes is output
c
      WRITE(IGOUT) (DUM,DUM,DUM,DUM,J=1,NPN)
C GLB add the following so SMS will recognize the .geo file
      WRITE(11,1096)
      WRITE(11,1097)
      WRITE(11,1098)
      WRITE(11,1099)
 1096 FORMAT('T1')
 1097 FORMAT('T2')
 1098 FORMAT('T3')
 1099 FORMAT('$L  3  0  6  0')
C GLB end additions
      DO 100 I=1,NEL
C %%% remove nodec below
      WRITE(11,1100) I,(NOPN(I,K),K=1,8),IMATN(I),TH(I)
 1100 FORMAT('GE',10I6,F10.2)
  100 CONTINUE
      DO 200 J=1,NPN
      WRITE(11,1101)  J, (CORDN(J,K),K=1,2), AO
 1101 FORMAT('GNN',I8,3F12.2)
  200 CONTINUE
      RETURN
      END
c
      SUBROUTINE R2OUT
C GLB change to INCLUDE 'blkcom_v2.chopit
      INCLUDE 'blkcom_v2.chopit'
C GLB add the following common block
C ... needed for banners
      COMMON  /DMSCHR/ HEADER, IDMS(15), BANSLC(2),
     *                 BANGFG(5), BANRM2(5), BANRM4(5), EMPTY(5)
      COMMON  /DMSREC/ IREC(40),FREC(40), IPACKB(1200), IPACKT(77)
      CHARACTER        HEADER*77,  IDMS*80, BANSLC*80,
     *                 BANGFG*80, BANRM2*80, BANRM4*80, EMPTY*80
C
      NDUM = 1
      WDUM = 0.
c
c    The first time into this routine IBR2=0, so the TABS-MD banners are written
c
      IF(IBR2.EQ.0) THEN
C GLB comment out the following and relace with CALL PUT_BANNER
C         WRITE(I2OUT) (BANNER(I),I=1,15)
C         WRITE(I2OUT) (IREC(I),I=1,40),(FREC(I),I=1,40)
C         WRITE(I2OUT) TITLE
         CALL PUT_BANNER(NE, NP, I2OUT, 2) 
         IBR2 = 1
C GLB comment out the RETURN statement
C         RETURN
      ENDIF
      WRITE  (I2OUT) TET,NP, ((VEL(J,K),J=1,3),K=1,NP),
     *   (NDUM,K=1,NP), NE,(IMAT(J),J=1,NE), (VEL(3,K),K=1,NP)
      RETURN
      END

      SUBROUTINE R4OUT
C GLB change to INCLUDE 'blkcom_v2.chopit
      INCLUDE 'blkcom_v2.chopit'
C GLB add the following common block
C ... needed for banners
      COMMON  /DMSCHR/ HEADER, IDMS(15), BANSLC(2),
     *                 BANGFG(5), BANRM2(5), BANRM4(5), EMPTY(5)
      COMMON  /DMSREC/ IREC(40),FREC(40), IPACKB(1200), IPACKT(77)
      CHARACTER        HEADER*77,  IDMS*80, BANSLC*80,
     *                 BANGFG*80, BANRM2*80, BANRM4*80, EMPTY*80
C
c
c    First time through IBR4 = 0 and the TABS-MD banners are written
c
      IF(IBR4.EQ.0) THEN
C GLB comment out the following and relace with CALL PUT_BANNER
C         WRITE(I4OUT) (BANNER(I),I=1,15)
C         WRITE(I4OUT) (IREC(I),I=1,40),(FREC(I),I=1,40)
C         WRITE(I4OUT) TITLE
         CALL PUT_BANNER(NE, NP, I4OUT, 4)
         IBR4 = 1
C GLB comment ot the return statement
C         RETURN
      ENDIF
C GLB add IMAT to the following write and remove K=1,NQ and add NE
      WRITE(I4OUT) TET,NQ,NP,(VEL(4,J),J=1,NP),NE,
     +             (IMAT(K),K=1,NE)
      RETURN
      END
c
      SUBROUTINE R2OUT2
C GLB change to INCLUDE 'blkcom_v2.chopit
      INCLUDE 'blkcom_v2.chopit'
C GLB add the following common block
C ... needed for banners
      COMMON  /DMSCHR/ HEADER, IDMS(15), BANSLC(2),
     *                 BANGFG(5), BANRM2(5), BANRM4(5), EMPTY(5)
      COMMON  /DMSREC/ IREC(40),FREC(40), IPACKB(1200), IPACKT(77)
      CHARACTER        HEADER*77,  IDMS*80, BANSLC*80,
     *                 BANGFG*80, BANRM2*80, BANRM4*80, EMPTY*80
C
      NDUM = 1
      WDUM = 0.
c
c    First time through IBR2 = 0 and the TABS-MD banners are written
c
      IF(IBR2.EQ.0) THEN
C GLB comment out the following and relace with CALL PUT_BANNER
C         WRITE(I2OUT) (BANNER(I),I=1,15)
C         WRITE(I2OUT) (IREC(I),I=1,40),(FREC(I),I=1,40)
C         WRITE(I2OUT) TITLE
         CALL PUT_BANNER(NEL, NPN, I2OUT, 2)
         IBR2 = 1
C GLB comment out the RETURN statement
C         RETURN
      ENDIF
      WRITE  (I2OUT) TET,NPN, ((VELN(K,J),J=1,3),K=1,NPN),
     *   (NDUM,K=1,NPN), NEL,(IMATN(J),J=1,NEL), (VELN(K,3),K=1,NPN)
      RETURN
      END
      SUBROUTINE R4OUT4
C GLB change to INCLUDE 'blkcom_v2.chopit
      INCLUDE 'blkcom_v2.chopit'
C GLB add the following common block
C ... needed for banners
      COMMON  /DMSCHR/ HEADER, IDMS(15), BANSLC(2),
     *                 BANGFG(5), BANRM2(5), BANRM4(5), EMPTY(5)
      COMMON  /DMSREC/ IREC(40),FREC(40), IPACKB(1200), IPACKT(77)
      CHARACTER        HEADER*77,  IDMS*80, BANSLC*80,
     *                 BANGFG*80, BANRM2*80, BANRM4*80, EMPTY*80
C
c
c    First time through IBR4 = 0 and the TABS-MD banners are written
c
      IF(IBR4.EQ.0) THEN
C GLB comment out the following and relace with CALL PUT_BANNER
C         WRITE(I2OUT) (BANNER(I),I=1,15)
C         WRITE(I2OUT) (IREC(I),I=1,40),(FREC(I),I=1,40)
C         WRITE(I2OUT) TITLE
         CALL PUT_BANNER(NEL, NPN, I4OUT, 4)
         IBR4 = 1
C GLB comment out the RETURN statement
C         RETURN
      ENDIF
C GLB add IMATN to write and remove K=1,NQ and add NEL
      WRITE(I4OUT) TET,NQ,NPN,(SAL(J),J=1,NPN),NEL,
     +             (IMATN(K),K=1,NEL)
      RETURN
      END
c
      SUBROUTINE R10READ
C GLB change to INCLUDE 'blkcom_v2.chopit
      INCLUDE 'blkcom_v2.chopit'
      DFCT = 0.
C GLB add NE to the following read
C GLB-NSF      READ(I10IN,END=900) TET,NP,NDG,NE,((VEL(K,J),K=1,NDF)
C GLB-NSF     * ,VVEL(J),J=1,NP),(DFCT,J=1,NE)
C GLB-NSF add the following 8 lines
        NDFP1 = NDF + 1
        READ (I10IN,END=900) TET, NP, NDG, NE,
     &       NDFS,(IRESAV(K),K=1,NDFS),
     &       ((VEL(K,J),J = 1, IRESAV(K)),K=1,NDF),
     &       (DUM1,J = 1, IRESAV(3)),
     &       (DUM, J = 1, NE), (DUM, J = 1, NP),
     &       (DELBED(J),J=1,IRESAV(NDFP1)),(BSHR(J),J=1,IRESAV(NDFS)),
     &       (VVEL(J), J = 1, NP), (DFCT,
     &       J = 1, NE)
      RETURN
  900 ISTOP = 1
      RETURN
      END

C GLB add SUBROUTINE PUT_BANNER and SUBROUTINE CONVRT
C
      SUBROUTINE PUT_BANNER (NEGEO, NPGEO, LUOUT, IKIND)
C  
C ... LUOUT = Logical unit output #
C ... IKIND = 2 for RMA2    =4 for RMA4 file
C     
C ... needed for banners
      COMMON  /DMSCHR/ HEADER, IDMS(15), BANSLC(2),
     *                 BANGFG(5), BANRM2(5), BANRM4(5), EMPTY(5)
      COMMON  /DMSREC/ IREC(40),FREC(40), IPACKB(1200), IPACKT(77)
      CHARACTER        HEADER*77,  IDMS*80, BANSLC*80,
     *                 BANGFG*80, BANRM2*80, BANRM4*80, EMPTY*80
C
      IF ( IKIND.EQ.2 ) THEN      
C-...      Process INTEGER type CHARACTER DMS BANNERS on Fake RMA2           
           IREC(1) = 429
           CALL CONVRT ( IFAKE2, 1, IFAKE3, 1, 3 )
C-           
C ...      WRITE INTEGER type CHARACTER BANNER ..  BINARY HYDRO
C-
           CALL CONVRT ( IDMS, 15, IPACKB, 80, 1 )
C-
           CALL CONVRT ( HEADER, 1, IPACKT, 77, 1 )
C-
           REWIND LUOUT
           MFLG = 120
           WRITE (LUOUT)   MFLG, IREC(1), NPGEO, NEGEO
           IWRT1 = 1200
           WRITE (LUOUT)   IWRT1, (IPACKB(ID),ID= 1,IWRT1)
           IWRT2 = 40  
           IWRT3 = 40
           WRITE (LUOUT)   IWRT2, IWRT3,
     *                     (IREC(ID),ID=1, IWRT2),
     *                     (FREC(ID),ID=1,IWRT3)
           IWRT4 = 77
           WRITE (LUOUT)   IWRT4, (IPACKT(ID),ID= 1,IWRT4)         
C-  
       ELSEIF ( IKIND.EQ.4 ) THEN
C-...      Process INTEGER type CHARACTER DMS BANNERS on Fake RMA4           
           IREC(1) = 427
           CALL CONVRT ( IFAKE2, 1, IFAKE3, 1, 4 )
C-           
C ...      WRITE INTEGER type CHARACTER BANNER ..  BINARY HYDRO
C-
           CALL CONVRT ( IDMS, 15, IPACKB, 80, 1 )
C-
           CALL CONVRT ( HEADER, 1, IPACKT, 77, 1 )
C-
           REWIND LUOUT
           MFLG = 140
           WRITE (LUOUT) MFLG, IREC(1), NPGEO, NEGEO
           IWRT1 = 1200
           WRITE (LUOUT) IWRT1, (IPACKB(ID),ID= 1,IWRT1)
           IWRT2 = 40
           IWRT3 = 40
           WRITE (LUOUT) IWRT2, IWRT3,
     *                   (IREC(ID),ID=1, IWRT2), 
     *                   (FREC(ID),ID=1,IWRT3)
           IWRT4 = 77
           WRITE (LUOUT) IWRT4, (IPACKT(ID),ID= 1,IWRT4)
       ELSE
           PRINT *,' --> PUT_BANNER does not recognize '
           PRINT *,'     file type= ', IKIND  
           STOP
       ENDIF
       WRITE (*,*) ' --> Integer Banners written on LU=',LUOUT
       WRITE (*,*) '     MFLG=',MFLG,' NE =',NEGEO, ' NP =',NPGEO
       RETURN
       END 
C ----------------     
      SUBROUTINE CONVRT ( ISTRNG, LENGTH, INTVAL, ISIZE, ISWTCH )
      SAVE
C-
C ... AUTHOR: BOB EVANS/BARBARA DONNELL              01-22-1993
C ... ISWTCH:  1 CONVERT FROM CHARACTER TO ASCII INTEGER VALUE
C ...           2 CONVERT FROM INTEGER VALUE TO CHARACTER EQUIVALENT
C ...           3 CONSOLIDATE BANNERS
C ... PURPOSE:  Avoid FORTRAN to C-Language binary file read problems
C-
      PARAMETER (MM8=1200)
C ... needed for banners
      COMMON  /DMSCHR/ HEADER, IDMS(15), BANSLC(2),
     *                 BANGFG(5), BANRM2(5), BANRM4(5), EMPTY(5)
      COMMON  /DMSREC/ IREC(40),FREC(40), IPACKB(1200), IPACKT(77)
      CHARACTER        HEADER*77,  IDMS*80, BANSLC*80,
     *                 BANGFG*80, BANRM2*80, BANRM4*80, EMPTY*80
C
      CHARACTER  ISTRNG(15)*80
      DIMENSION  INTVAL(MM8)
C-
      IF( ITRACE.GE.1 ) PRINT *,' =+= CALLED CONVRT (ISIZE=',ISIZE,
     *                                '...ISWTCH=',ISWTCH,')'
C-
      ISTOP = 0
      IF ( (ISIZE*LENGTH) .GT. MM8 ) THEN
          PRINT *,' Error in CONVRT ... LENGTH & SIZE are inconsistant'
          PRINT *,'          with dimension capabilities'
          ISTOP= 1
      ENDIF
      IF ( ISIZE .GT. MM8 ) THEN
          PRINT *,' Error in CONVRT ... size larger than 1200'
          ISTOP= 1
      ENDIF
      IF ( ISTOP .GE. 1 ) THEN
           STOP'CONVRT'
      ENDIF
C-
C-
      IF (ISWTCH .EQ. 1) THEN
C-
C ...     Convert from character to ascii integer value
C-
          II = 0
          DO 300 LL = 1, LENGTH
             DO 200 I = 1, ISIZE          
                II = II + 1
                INTVAL(II) = ICHAR( ISTRNG(LL)(I:I) )
  200        CONTINUE
  300     CONTINUE
C-
      ELSEIF (ISWTCH .EQ. 2) THEN
C-
C ...     Convert from ascii integer value to character 

          IIE = 0
          DO 600 LL = 1, LENGTH
             IIS = 1 + IIE
             IIE = IIS + (ISIZE-1)
             J = 0
             DO 500 I = IIS, IIE 
                J = J + 1
                ISTRNG(LL)(J:J) = CHAR( INTVAL(I) )
  500        CONTINUE
  600     CONTINUE
C-
      ELSEIF (ISWTCH .EQ. 3) THEN
C-
C ...   consolidate BANNER section for a RMA2 look alike
C-
        IDMS(1)(1:40)  = BANGFG(1)(1:40)
        IDMS(1)(41:80) = BANGFG(2)(1:40)
        IDMS(2)(1:40)  = BANGFG(3)(1:40)
        IDMS(2)(41:80) = BANGFG(4)(1:40)
        IDMS(3)(1:40)  = BANGFG(5)(1:40)
        IDMS(3)(41:80) = EMPTY(1)(1:40)
        IDMS(4)(1:40)  = EMPTY(1)(1:40)
        IDMS(4)(41:80) = EMPTY(1)(1:40)
        IDMS(5)(1:40)  = EMPTY(1)(1:40)
        IDMS(5)(41:80) = EMPTY(1)(1:40)
C
        IDMS(6)(1:40)  = BANRM2(1)(1:40)
        IDMS(6)(41:80) = BANRM2(2)(1:40)
        IDMS(7)(1:40)  = BANRM2(3)(1:40)
        IDMS(7)(41:80) = BANRM2(4)(1:40)
        IDMS(8)(1:40)  = BANRM2(5)(1:40)
        IDMS(8)(41:80) = EMPTY(1)(1:40)
        IDMS(9)(1:40)  = EMPTY(1)(1:40)
        IDMS(9)(41:80) = EMPTY(1)(1:40)
        IDMS(10)(1:40) = EMPTY(1)(1:40)
        IDMS(10)(41:80)= EMPTY(1)(1:40)
C
        IDMS(11)(1:40) = EMPTY(1)(1:40)
        IDMS(11)(41:80)= EMPTY(2)(1:40)
        IDMS(12)(1:40) = EMPTY(3)(1:40)
        IDMS(12)(41:80)= EMPTY(4)(1:40)
        IDMS(13)(1:40) = EMPTY(5)(1:40)
        IDMS(13)(41:80)= EMPTY(1)(1:40)
        IDMS(14)(1:40) = EMPTY(1)(1:40)
        IDMS(14)(41:80)= EMPTY(1)(1:40)
        IDMS(15)(1:40) = EMPTY(1)(1:40)
        IDMS(15)(41:80)= EMPTY(1)(1:40)
C-
      ELSEIF (ISWTCH .EQ. 4) THEN
C-
C ...   consolidate BANNER section for a RMA4 look alike
C-
        IDMS(1)(1:40)  = BANGFG(1)(1:40)
        IDMS(1)(41:80) = BANGFG(2)(1:40)
        IDMS(2)(1:40)  = BANGFG(3)(1:40)
        IDMS(2)(41:80) = BANGFG(4)(1:40)
        IDMS(3)(1:40)  = BANGFG(5)(1:40)
        IDMS(3)(41:80) = EMPTY(1)(1:40)
        IDMS(4)(1:40)  = EMPTY(1)(1:40)
        IDMS(4)(41:80) = EMPTY(1)(1:40)
        IDMS(5)(1:40)  = EMPTY(1)(1:40)
        IDMS(5)(41:80) = EMPTY(1)(1:40)
C
        IDMS(6)(1:40)  = BANRM2(1)(1:40)
        IDMS(6)(41:80) = BANRM2(2)(1:40)
        IDMS(7)(1:40)  = BANRM2(3)(1:40)
        IDMS(7)(41:80) = BANRM2(4)(1:40)
        IDMS(8)(1:40)  = BANRM2(5)(1:40)
        IDMS(8)(41:80) = EMPTY(1)(1:40)
        IDMS(9)(1:40)  = EMPTY(1)(1:40)
        IDMS(9)(41:80) = EMPTY(1)(1:40)
        IDMS(10)(1:40) = EMPTY(1)(1:40)
        IDMS(10)(41:80)= EMPTY(1)(1:40)
C
        IDMS(11)(1:40) = BANRM4(1)(1:40)
        IDMS(11)(41:80)= BANRM4(2)(1:40)
        IDMS(12)(1:40) = BANRM4(3)(1:40)
        IDMS(12)(41:80)= BANRM4(4)(1:40)
        IDMS(13)(1:40) = BANRM4(5)(1:40)
        IDMS(13)(41:80)= EMPTY(1)(1:40)
        IDMS(14)(1:40) = EMPTY(1)(1:40)
        IDMS(14)(41:80)= EMPTY(1)(1:40)
        IDMS(15)(1:40) = EMPTY(1)(1:40)
        IDMS(15)(41:80)= EMPTY(1)(1:40)
C-
      ELSE
C-
          PRINT *,' --> Value of ISWTCH invalid in CONVRT'
      ENDIF
      RETURN
      END
C***************************************************************


