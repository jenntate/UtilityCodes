      PROGRAM ADH_2_ARC
      PARAMETER (MAXNE = 50000000, MAXN = 100000000, MAXNDF = 6)
      PARAMETER (MAXT = 10000000)
      INTEGER NPTOT, NETOT, N, NN, TIME, T,node
      REAL TIME1,TIME2,VALX,VALY,VALZ
      REAL, DIMENSION(:),ALLOCATABLE :: XCOR,YCOR,ZCOR,DEP
      CHARACTER filename1*80,dep_file*80,ovl_file*80,geo_file*80
      CHARACTER junk1*2,junk2*2

      ALLOCATE(XCOR(MAXN), YCOR(MAXN), ZCOR(MAXN))
C      ALLOCATE(DEP(MAXT)(MAXN))

      WRITE(*,*) 'This routine will develop the ARC xyz file'
      WRITE(*,*) 'and requires the depth, velocity, and geometry files'
      WRITE(*,*) 'enter the output filename'
      READ(*,'(A)') filename1
      OPEN(68,file=filename1,form='formatted',status='unknown')

      WRITE(*,*) 'what is the depth filename?' 
      READ(*,'(A)') dep_file 
        OPEN(15,file=dep_file,form='formatted',status='old')
C     WRITE(*,*) 'what is the velocity filename?' 
C     READ(*,'(A)') ovl_file 
C       OPEN(16,file=ovl_file,form='formatted',status='old')
      WRITE(*,*) 'what is the geometry filename?' 
      READ(*,'(A)') geo_file 
        OPEN(17,file=geo_file,form='formatted',status='old')



        REWIND(15)
        READ(15,*)
        READ(15,*)
        READ(15,*)
        READ(15,*) junk1, NPTOT
        READ(15,*) junk2, NETOT 
        READ(15,*)  
        READ(15,*)  
       
C       REWIND(16)
C       READ(16,*)
C       READ(16,*)
C       READ(16,*)
C       READ(16,*) junk1, NPTOT
C       READ(16,*) junk2, NETOT 
C       READ(16,*)  
C       READ(16,*)  

        REWIND(17)
          DO N=1,NETOT+1
            READ(17,*,END=900) 
          ENDDO
          DO N=1,NPTOT
           READ(17,*,END=900) junk1, node, XCOR(N), YCOR(N), ZCOR(N)
          ENDDO
       


        DO T = 1, 9999999
          READ(15,*,END=900) junk1, junk2, TIME1
          READ(16,*,END=900) junk1, junk2, TIME2
            IF(TIME1 .EQ. TIME2) THEN
              DO N=1,NPTOT
                READ(15,*,END=900) DEP(T)(N)
C               WSE = ZCOR(N) + DEP(T)(N)
C               READ(16,*,END=900) VALX(T)(N), VALY(T)(N), VALZ(T)(N)
              ENDDO
              GOTO 900
            ELSE
              WRITE(*,*)'ERROR: Times do not match in the data files.'
              STOP
            ENDIF
          TIME=T
        ENDDO
  900   CONTINUE

        DO N=1,NPTOT
          WRITE(68,*) XCOR(N),YCOR(N),(DEP(T)(N),T=1,TIME) 
        ENDDO


        CLOSE(15)
C       CLOSE(16)
        CLOSE(17)
        CLOSE(68)

      END
