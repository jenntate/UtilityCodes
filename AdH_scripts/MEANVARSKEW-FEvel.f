      PROGRAM MEANVARSKEW 
      PARAMETER (MAXNE = 500000, MAXN = 1000000, MAXNDF = 6)
      DIMENSION VAL(MAXN), COUNT(MAXN)
      INTEGER NPTOT, NETOT, COUNT, NN, I, ITIME
      INTEGER nnodes, nodenum(MAXN)
      REAL MEAN(MAXN), VAR(MAXN), Skew(MAXN), sum(MAXN)
      REAL VAL, TIME
      CHARACTER solution_file*80, junk1*2, junk2*2
      CHARACTER filename*80, filename2*80


C     open the instruction files
      WRITE(*,*)'Enter the name of the input file'
      READ(*,*) filename
      OPEN(15,file=filename,form='formatted',status='old')
      READ(15,*) nnodes, solution_file
      WRITE(*,*) nnodes, solution_file
      DO I=1,nnodes
         READ(15,*)nodenum(I)
         WRITE(*,*)nodenum(I)
      COUNT(I)=0
      VAR(I)=0
      ENDDO

C     open the output file for the mean variance and skew for each obs. pt
      WRITE(*,*)'enter the output file for Mean, Variance, and Skew'
      READ(*,*) filename2
      OPEN(20, file=filename2, form='formatted', status='unknown')
      REWIND(20)

C     open the ADH solution files and read the nodal values for each time
      OPEN(25, file=solution_file, form='formatted',status='old')
      REWIND(25)
 

      READ(25,*)
      READ(25,*)
      READ(25,*)
      READ(25,*) junk1, NPTOT
      READ(25,*) junk2, NETOT 
      READ(25,*)

      DO ITIME = 1, 9999999
        READ(25,*,END=900) junk1, junk2, TIME 
          DO I=1,nnodes
              READ(25,*) VAL(I)
              sum(I) = sum(I)+VAL(I)
              COUNT(I)=COUNT(I)+1
          ENDDO
       ENDDO

900   CONTINUE

      DO I=1,nnodes
        MEAN(I)=sum(I)/COUNT(I)
        WRITE(*,*) 'COUNT(',nodenum(I),') = ', COUNT(I)
        COUNT(I)=0
      ENDDO


      REWIND(25)
      READ(25,*)
      READ(25,*)
      READ(25,*)
      READ(25,*) junk1, NPTOT
      READ(25,*) junk2, NETOT 
      READ(25,*)

      DO ITIME = 1, 9999999
        READ(25,*,END=901) junk1, junk2, TIME 
          DO I=1,nnodes
              READ(25,*) VAL(I)
              VAR(I)=VAR(I)+(VAL(I)-MEAN(I))**2 
              COUNT(I)=COUNT(I)+1
          ENDDO
      ENDDO
901    CONTINUE

      DO I=1,nnodes
        VAR(I)=VAR(I)/(COUNT(I)-1.0) 
        COUNT(I)=0.0
      ENDDO
        


      REWIND(25)
      READ(25,*)
      READ(25,*)
      READ(25,*)
      READ(25,*) junk1, NPTOT
      READ(25,*) junk2, NETOT 
      READ(25,*)

      DO ITIME = 1, 9999999
        READ(25,*,END=902) junk1, junk2, TIME 
          DO I=1,nnodes
              READ(25,*) VAL(I)
              IF (VAR(I) .EQ. 0.0) THEN
                SKEW(I) = 0.0
              ELSE
                SKEW(I)=SKEW(I)+((VAL(I)-MEAN(I))/(VAR(I))**0.5)**3 
              ENDIF
              COUNT(I)=COUNT(I)+1
          ENDDO
       ENDDO
902    CONTINUE

      DO I=1,nnodes
        SKEW(I)=SKEW(I)*(COUNT(I)/((COUNT(I)-1.0)*(COUNT(I)-2.0))) 
      ENDDO

        WRITE(20,*) 'Input Solution: ',  solution_file
        WRITE(20,*)'         MEAN       VARIANCE       SKEWNESS' 
          DO I=1,nnodes
             WRITE(20,*) MEAN(I), VAR(I),  SKEW(I) 
          ENDDO




        CLOSE(15)
        CLOSE(20)
        CLOSE(25)

      END
