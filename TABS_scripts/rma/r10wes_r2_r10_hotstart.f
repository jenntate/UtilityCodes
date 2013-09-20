      PROGRAM R2_R10_HOTSTART
C   GIVEN A RMA2 HOTSTART FILE AND A 3-D GEOMETRY FILE, THIS
C   PROGRAM GENERATES A RMA10 HOTSTART FILE.  GARY BROWN 1-1996
C
C23456
C
      PARAMETER  (MNP=25000) 
C
      DIMENSION  R2_VEL(3,MNP), R2_VDOT(3,MNP), VEL(6,MNP)
      DIMENSION  VDOT(6,MNP), NDEP(MNP), NREF(MNP), VVEL(MNP)
C
      DIMENSION  BANDUM(15), IDUM(40), FDUM(40)
      DIMENSION  IPACDUM(1200), IHDUM(40), FHDUM(40)
      DIMENSION  CDUM(MNP,3), SDUM(MNP,3), ALFDUM(MNP)
      DIMENSION  NFDUM(MNP), AODUM(MNP), NSDUM(MNP)  
C
      INTEGER NP, NE, NPT, NET, NPM, NES
C
      REAL TET
C
      CHARACTER*80  FNAME1, FNAME2, FNAME3
C
      LOGICAL HERE
C
      WRITE (*,*)
      WRITE (*,*) ' *****************************************' 
      WRITE (*,*) '     *** PROGRAM R2-R10-HOTSTART ***'
C
      WRITE (*,*)
      WRITE (*,*) 'GIVEN A RMA2 HOTSTART FILE AND A 3-D' 
     +           , ' GEOMETRY FILE,' 
      WRITE (*,*) 'THIS PROGRAM GENERATES A RMA10 HOTSTART FILE.'
      WRITE (*,*)
      WRITE (*,*) ' *****************************************' 
      WRITE (*,*)
C
C read in the RMA2 hotstart file
C
 10   WRITE (*,*) 'ENTER THE FILENAME OF THE RMA2 HOTSTART FILE'
      READ (*,*) FNAME1
      INQUIRE(FILE=FNAME1,EXIST=HERE)
      IF ( .NOT. HERE) THEN
        WRITE (*,*)
        WRITE (*,*) ' You entered a non-existant file >>',FNAME1
        WRITE (*,*)
        GO TO 10
      END IF
      OPEN(10,FILE=FNAME1,FORM='UNFORMATTED',STATUS='OLD')
C
C read in the RMA10 3-D geometry file
C
      WRITE (*,*)
 20   WRITE (*,*) 'ENTER THE FILENAME OF THE 3-D GEOMETRY FILE'
      READ (*,*) FNAME2
      INQUIRE(FILE=FNAME2,EXIST=HERE)
      IF ( .NOT. HERE) THEN
        WRITE (*,*) 
        WRITE (*,*) ' You entered a non-existant file >>',FNAME2
        WRITE (*,*)
        GO TO 20
      END IF
      OPEN(30,FILE=FNAME2,FORM='UNFORMATTED',STATUS='OLD')  
C
C read in the desired RMA10 hotstart filename
C
      WRITE (*,*) 
      WRITE (*,*) 'ENTER THE DESIRED FILENAME OF THE'
     +          , ' RMA10 HOTSTART FILE'
      READ (*,*) FNAME3
      INQUIRE(FILE=FNAME3,EXIST=HERE)
      IF ( .NOT. HERE) THEN
        OPEN(40,FILE=FNAME3,FORM='UNFORMATTED',STATUS='NEW')
      ELSE
        OPEN(40,FILE=FNAME3,FORM='UNFORMATTED',STATUS='OLD')
        WRITE (*,*)
        WRITE (*,*) 'note: overwriting file = ', FNAME3
        WRITE (*,*)
      END IF
C
C now read relevant data from the RMA2 hotstart file 
C
      READ (10)  ITEST
      REWIND 10 
C-
      IF (ITEST.GT.200) THEN
C  This is a true character variable type read
         READ (10)  (BANDUM(I),I=1,15)
         READ (10)  (IDUM(I),I=1,40), (FDUM(I),I=1,40)
      ELSE
C  This must be an integer style character-- so convert
         READ (10)   MDUM1, MDUM2, MDUM3, MDUM4
         READ (10)   IDUM1, (IPACDUM(I), I=1,IDUM1)
         READ (10)   IDUM2, IDUM3,
     *                      (IHDUM(I),I=1,IDUM2),(FHDUM(I),I=1,IDUM3)
      ENDIF
C-
      READ(10) TET,NPT,NET,NITSV,
     *                ((R2_VEL(J,K),J=1,3),K=1,NPT),
     *                ((R2_VDOT(J,K),J=1,3),K=1,NPT)
      CLOSE (10)
C
C now read relevant data from 3-D geometry file 
C
      READ (30) NP, NE, NPM, NES,
     *     ((CDUM(J,K),SDUM(J,K),K=1,3), ALFDUM(J), NFDUM(J),
     *     AODUM(J), NSDUM(J), J = 1, NP), (NDEP(J),
     *     NREF(J), J = 1, NPM)
      CLOSE (30)
C
C check for incompatible files
C
      IF (NPT .NE. NPM) THEN
        WRITE (*,*) 
        WRITE (*,*) '** ERROR: The number of surface nodes in the RMA2'
        WRITE (*,*) '          hotstart file is ', NPT,'.' 
        WRITE (*,*) '          The number of surface nodes in the 3-D'
        WRITE (*,*) '          geometry file is ', NPM,'.'   
        WRITE (*,*) '   Therefore, the files are INCOMPATIBLE'
        STOP
      END IF  
C
C  main algorithm: this loads the VEL and VDOT arrays for the RMA10
C  hotstart file      
C
      DO N = 1, NPM
        IF (NDEP(N) .GT. 1) THEN
C  this is a 3-d node, so assign values to nodes at depth
          N1 = NREF(N) + 1
          NV = NDEP(N) + N1 - 2
          DO M = N1, NV
            DO L = 1, 6 
              IF (L .LE. 3) THEN
C  let the velocities at each node at depth equal the surface node velocity
                VEL(L,M)  = R2_VEL(L,N)
                VDOT(L,M) = R2_VDOT(L,N)
              ELSE
C  these are the water quality constituents
                VEL(L,M)  = 0.0
                VDOT(L,M) = 0.0
              END IF
C  let all bottom velocities equal zero 
              IF (M .EQ. NV .AND. L .LT. 3) THEN 
                VEL(L,M)  = 0.0
                VDOT(L,M) = 0.0
              END IF
            END DO
          END DO 
        END IF
C  this loop just copies surface node values over to the 3-D arrays
        DO L = 1, 6
          IF (L .LE. 3) THEN
            VEL(L,N)  = R2_VEL(L,N)
            VDOT(L,N) = R2_VDOT(L,N)
          ELSE
C  these are the water quality constituents
            VEL(L,N)  = 0.0
            VDOT(L,N) = 0.0
          END IF
        END DO
      END DO  
C
C  let all vertical velocities equal zero
      DO N = 1, NP
        VVEL(N) = 0.0
      END DO
C
C  Finally, write the RMA10 hotstart file
C
      WRITE(40)  1,TET,NP,6,((VEL(J,I),VDOT(J,I),J=1,6),
     *                        VVEL(I),I=1,NP)
      CLOSE(40)
C
      WRITE(*,*)
      WRITE(*,*) 'The RMA10 hotstart file ',FNAME3
      WRITE(*,*) 'has been written.'
      WRITE(*,*)
C
      STOP
      END
