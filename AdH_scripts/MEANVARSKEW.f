      PROGRAM MEANVARSKEW 
      PARAMETER (MAXNE = 500000, MAXN = 1000000, MAXNDF = 6)
      DIMENSION VAL(MAXN), COUNT(MAXN)
      INTEGER NPTOT, NETOT, COUNT, NN, I, ITIME
      INTEGER nnodes, nodenum(MAXN)
      REAL MEAN(MAXN), VAR(MAXN), Skew(MAXN), sum(MAXN)
      REAL VAL, TIME
      CHARACTER solution_file*80, junk1*2, junk2*2
      CHARACTER filename*80


C     open the instruction files
      WRITE(*,*)'Enter the name of the input file'
      READ(*,*) filename
      OPEN(15,file=filename,form='formatted',status='old')
      READ(15,*) nnodes, solution_file
      REWIND(15)
      DO I=1,nnodes
         READ(15,*)nodenum(I)
      COUNT(I)=0
      VAR(I)=0
      ENDDO

C     open the output file for the mean variance and skew for each obs. pt
      WRITE(*,*)'enter the output file for Mean, Variance, and Skew'
      READ(*,*) filename
      OPEN(20, file=filename, form='unformatted', status='unknown')
      REWIND(20)

C     open the ADH solution files and read the nodal values for each time
      OPEN(25, file=solution_file, form='formatted',status='old')
      REWIND(25)
 

      READ(25,*)
      READ(25,*)
      READ(25,*)
      READ(25,*) junk1, NPTOT
      READ(25,*) junk2, NETOT 

      DO ITIME = 1, 9999999
        READ(25,*,END=900) junk1, junk2, TIME 
        DO NN=1, NPTOT
          DO I=1,nnodes
           IF(NN .EQ. nodenum(I)) THEN
              READ(25,*) VAL(NN)
              sum(I) = sum(I)+VAL(NN)
              COUNT(I)=COUNT(I)+1
           ENDIF
          ENDDO
        ENDDO
       ENDDO

900   CONTINUE

      DO I=1,nnodes
        MEAN(I)=sum(I)/COUNT(I)
        COUNT(I)=0
      ENDDO


      REWIND(25)
      READ(25,*)
      READ(25,*)
      READ(25,*)
      READ(25,*) junk1, NPTOT
      READ(25,*) junk2, NETOT 

      DO ITIME = 1, 9999999
        READ(25,*,END=901) junk1, junk2, TIME 
        DO NN=1, NPTOT
          DO I=1,nnodes
           IF(NN .EQ. nodenum(I)) THEN
              READ(25,*) VAL(NN)
              VAR(I)=VAR(I)+(VAL(NN)-MEAN(I))**2 
           ENDIF
          ENDDO
        ENDDO
       ENDDO

901    CONTINUE

      REWIND(25)
      READ(25,*)
      READ(25,*)
      READ(25,*)
      READ(25,*) junk1, NPTOT
      READ(25,*) junk2, NETOT 

      DO ITIME = 1, 9999999
        READ(25,*,END=902) junk1, junk2, TIME 
        DO NN=1, NPTOT
          DO I=1,nnodes
           IF(NN .EQ. nodenum(I)) THEN
              READ(25,*) VAL(NN)
              SKEW(I)=SKEW(I)+((VAL(NN)-MEAN(I))/SQRT(VAR(I)))**3 
              COUNT(I)=COUNT(I)+1
           ENDIF
          ENDDO
        ENDDO
       ENDDO

902    CONTINUE

        WRITE(20,'(A)') 'Input Solution: ',  solution_file
        WRITE(20,*)'NODE      MEAN       VARIANCE       SKEWNESS' 
          DO I=1,nnodes
             WRITE(20,*) nodenum(I), MEAN(I), VAR(I),  SKEW(I)/COUNT(I) 
          ENDDO




        CLOSE(15)
        CLOSE(20)
        CLOSE(25)

      END
