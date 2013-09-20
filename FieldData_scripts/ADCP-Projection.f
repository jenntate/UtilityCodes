      PROGRAM  ADCP_Projection 
      parameter(maxp=200000)
C
C     READ GIS data for ADCP transects to get magnitude normal to cross-section
C
      REAL  xvel, yvel, zvel, ry, rx, rz, rval, rdum, angl
      REAL  xcood, ycood, zcoord, vmage, vdire
      CHARACTER filename*80 
C
      write(*,*) 'enter the direction angle for positive'
      write(*,*) 'For each point, the magnitude of the velocity will be'
      write(*,*) 'calculated and the direction angle + or - 90 degrees'
      write(*,*) 'will be used to determine if the velocity is positive' 
      write(*,*) 'or negative based on the flow direction'
      read(*,*)  angl  
      print *,'enter the GIS ADCP data filename'
      read(*,'(A)') filename
      open(11,file=filename,status='old')
      print *,'what will you call the converted velocity output file?'
      read(*,'(A)') filename
      open(3,file=filename,form='formatted',status='unknown')
C
        write(3,200) 
C
      DO i=1, maxp
        READ(11,*,END=100) xcoord, ycoord, zcoord, vmage, vdire, 
     &       xvel, yvel, zvel
       WRITE(*,*) xcoord, ycoord, zcoord, vmage, vdire, xvel, yvel, zvel
*
        write(*,*) 'processing point  ', i
            ry = yvel
            rx = xvel
            rz = zvel
            rval = sqrt(rx*rx+ry*ry+rz*rz+1.E-8)
            rdum = rx*cos(angl/57.29578)+ry*sin(angl/57.29578) 
            rval = rval*sin((angl-90.)/57.29578)
            if (rdum .lt. 0.0) rval = -rval
        write(3,300) xcoord, -50.0*zcoord, rval
   50 END DO 

  100 GO TO 400
c
 200  format('   x-coord    z-coord    velocity   ')
 300  format(2f16.3,f8.3)
 400  stop
      end
