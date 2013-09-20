      PROGRAM ADHFEVEL 
      PARAMETER (MAXNE = 500000, MAXN = 1000000, MAXNDF = 6)
      DIMENSION VAL(MAXN)
      INTEGER NPTOT, NETOT,  NN, ITIME
      INTEGER nnodes, nodenum(MAXN)
      REAL DEPTH, WSEL, TIME, VALX, VALY, VALZ, VEL_FE
      REAL angle1, angle2, THETA
      CHARACTER filename*80,junk1*2,junk2*2,filename1*20
      CHARACTER solution_file*80		


C     open the instruction files
      WRITE(*,*)'Enter the name of the input file'
      READ(*,*) filename
      OPEN(15,file=filename,form='formatted',status='old')
      READ(15,*) nnodes, solution_file
      REWIND(15)
      DO I=1,nnodes
         READ(15,*)nodenum(I), angle1 angle2
      ENDDO

C     open the ADH solution files and read the nodal values for each time
      OPEN(25, file=solution_file, form='formatted',status='old')
      REWIND(25)
 
      READ(25,*)
      READ(25,*)
      READ(25,*)
      READ(25,*) junk1, NPTOT
      READ(25,*) junk2, NETOT 

      WRITE(*,*) 'enter the desired ADH Flood/Ebb solution filename'
      READ(*,'(A)') filename1
      OPEN(68,file=filename1,form='formatted',status='unknown')
      WRITE(68,*)'DATASET' 
      WRITE(68,*)'OBJTYPE "mesh2d"' 
      WRITE(68,*)'BEGSCL'
      WRITE(68,*)'ND ', NPTOT
      WRITE(68,*)'NC ', NETOT
      WRITE(68,*)'NAME "Flood/Ebb Velocity"'

      DO ITIME = 1, 9999999
        READ(25,*,END=900) junk1, junk2, TIME 
        WRITE(68,*) 'TS  0  ', TIME 
        DO NN=1, NPTOT
          DO I=1,nnodes
           IF(NN .EQ. nodenum(I)) THEN
              READ(25,*) VALX, VALY, VALZ
		  THETA=ATAN(VALY/VALX)*(180.0/3.14159)
              IF((angle1 .LT. THETA) .AND. (THETA .LT. angle2)) THEN
                 VEL_FE = SQRT(VALX*VALX+VALY*VALY)
               ELSE
                 VEL_FE = -1.0*SQRT(VALX*VALX+VALY*VALY)
               ENDIF
              WRITE(68,*) VEL_FE 
           ENDIF
          ENDDO
        ENDDO
       ENDDO

900   CONTINUE

        WRITE(68,*)'ENDDS' 
        CLOSE(15)
        CLOSE(68)
        CLOSE(25)

      END
